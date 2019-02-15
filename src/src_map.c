#include "src_map.h"
#include "tauc.h"
#include "utils/math_utils.h"
#include "utils/panic.h"
#include "utils/pool.h"
#include <assert.h>
#define LINE_STORE_MIN_LINES 32
static const ureg LINE_STORE_MIN_SIZE =
    LINE_STORE_MIN_LINES * sizeof(ureg) + sizeof(line_store);

#define SRC_RANGE_LENGTH_BITS 7
static const ureg SRC_RANGE_MAX_LENGTH =
    (((ureg)1) << SRC_RANGE_LENGTH_BITS) - 1;
#define SRC_RANGE_START_BITS (REG_BITS - SRC_RANGE_LENGTH_BITS - 1)
static const ureg SRC_RANGE_MAX_START = (((ureg)1) << SRC_RANGE_START_BITS) - 1;
static const ureg SRC_RANGE_NEW_MAP_BIT = ((ureg)1)
                                          << (SRC_RANGE_START_BITS - 2);
static const ureg SRC_RANGE_EXTERN_BIT = ((ureg)1)
                                         << (SRC_RANGE_START_BITS - 1);

static inline int append_line_store(src_map* m, thread_context* tc, ureg size)
{
    line_store* s = (line_store*)pool_alloc(&tc->permmem, size);
    if (!s) return -1;
    s->end = ptradd(s, size);
    s->prev = m->last_line_store;
    m->last_line_store = s;
    m->last_line = ptradd(s, sizeof(line_store));
    return 0;
}
int src_map_init(src_map* m, thread_context* tc, bool is_paste_area)
{
    m->is_paste_area = is_paste_area;
    m->last_line_store = NULL;
    int r = append_line_store(m, tc, LINE_STORE_MIN_SIZE);
    if (r) return r;
    m->last_line_store->first_line = 0;
    *m->last_line = 0;
    m->last_line++;
    return 0;
}

int src_map_fin(src_map* m)
{
    // nothing to do here for now
    return 0;
}

int src_map_add_line(src_map* m, thread_context* tc, ureg line_start)
{
    if (m->last_line >= m->last_line_store->end) {
        ureg last_size = ptrdiff(m->last_line_store->end, m->last_line_store);
        ureg prev_fst_line = m->last_line_store->first_line;
        if (append_line_store(m, tc, last_size * 2)) return -1;
        m->last_line_store->first_line =
            prev_fst_line + (last_size - sizeof(line_store)) / sizeof(ureg);
    }
    *m->last_line = line_start;
    m->last_line++;
    return 0;
}

int src_pos_get_line_bounds(
    src_map* m, ureg line_nr, ureg* start_pos, ureg* length)
{
    line_store* ls = m->last_line_store;
    line_store* next_ls = NULL;
    while (ls->first_line > line_nr) {
        next_ls = ls;
        ls = ls->prev;
        assert(ls);
    }
    ureg* line =
        ((ureg*)ptradd(ls, sizeof(line_store))) + (line_nr - ls->first_line);
    ureg* next_line = line + 1;
    if (next_ls == NULL) {
        if (next_line >= m->last_line) next_line = NULL;
    }
    else {
        if (next_line >= ls->end) {
            next_line = (ureg*)ptradd(next_ls, sizeof(line_store));
        }
    }
    *start_pos = *line;
    if (next_line != NULL) {
        *length = (*next_line - *line);
    }
    else {
        *length = 0; // otherwise impossible, because of newlines counting
                     // towards old line
    }
    return OK;
}
src_pos src_map_get_pos(src_map* m, ureg pos)
{
    line_store* ls = m->last_line_store;
    ureg* first = ptradd(ls, sizeof(line_store));
    ureg* end;
    if (*first <= pos) {
        end = m->last_line;
    }
    else {
        do {
            ls = ls->prev;
            first = ptradd(ls, sizeof(line_store));
        } while (*first > pos);
        end = ls->end;
    }
    ureg* pivot;
    ureg* start = first;
    while (true) {
        pivot = start + (end - start) / 2;
        if (*pivot > pos) {
            if (end == start) break;
            end = pivot;
        }
        else {
            pivot++;
            if (pivot == end || *pivot > pos) {
                pivot--;
                break;
            }
            start = pivot;
        }
    }
    src_pos p;
    p.line = ls->first_line + (pivot - first);
    p.column = pos - *pivot;
    return p;
}

src_range_packed src_range_pack(thread_context* tc, src_range* d)
{
    // PERF: maybe allocate these somewhere else
    if (!d->map) {
        ureg len = d->end - d->start;
        if (len > SRC_RANGE_MAX_LENGTH || d->start > SRC_RANGE_MAX_START) {
            ureg* tgt = pool_alloc(&tc->permmem, sizeof(ureg) * 2);
            if (!tgt) return SRC_RANGE_INVALID;
            *tgt = d->start;
            *(tgt + 1) = d->end;
            return SRC_RANGE_EXTERN_BIT | (((ureg)tgt) >> 2);
        }
        else {
            return (d->start << SRC_RANGE_LENGTH_BITS) | len;
        }
    }
    else {
        src_map** tgt = (src_map**)pool_alloc(&tc->permmem, sizeof(ureg) * 3);
        if (!tgt) return SRC_RANGE_INVALID;
        *tgt = d->map;
        ureg* range = (void*)(tgt + 1);
        *range = d->start;
        range++;
        *range = d->end;
        return SRC_RANGE_EXTERN_BIT | SRC_RANGE_NEW_MAP_BIT |
               (((ureg)tgt) >> 2);
    }
}

src_range_packed src_range_pack_lines(thread_context* tc, ureg start, ureg end)
{
    static src_range rng = {NULL, 0, 0};
    rng.start = start;
    rng.end = end;
    return src_range_pack(tc, &rng);
}

void src_range_unpack(src_range_packed r, src_range* d)
{
    if (r & SRC_RANGE_EXTERN_BIT) {
        if (r & SRC_RANGE_NEW_MAP_BIT) {
            src_map** tgt = (void*)(r << 2);
            d->map = *tgt;
            ureg* range = (ureg*)(tgt + 1);
            d->start = *range;
            range++;
            d->end = *range;
        }
        else {
            d->map = NULL;
            ureg* tgt = (ureg*)(r << 2);
            d->start = *tgt;
            tgt++;
            d->end = *tgt;
        }
    }
    else {
        d->map = NULL;
        d->start = r >> SRC_RANGE_LENGTH_BITS;
        ureg len = (r & SRC_RANGE_MAX_LENGTH);
        d->end = d->start + len;
    }
}

int file_init(file* f, thread_context* tc, char* path)
{
    f->path = path;
    return src_map_init(&f->src_map, tc, false);
}
void file_fin(file* f)
{
    src_map_fin(&f->src_map);
}