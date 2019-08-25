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
static const ureg SRC_RANGE_NEW_MAP_BIT = ((ureg)1) << (REG_BITS - 2);
static const ureg SRC_RANGE_EXTERN_BIT = ((ureg)1) << (REG_BITS - 1);

static inline int
append_line_store(source_map* m, thread_context* tc, ureg size)
{
    line_store* s = (line_store*)pool_alloc(&tc->permmem, size);
    if (!s) return -1;
    s->end = ptradd(s, size);
    s->prev = m->last_line_store;
    m->last_line_store = s;
    m->last_line = ptradd(s, sizeof(line_store));
    return 0;
}
int src_map_init(source_map* m, thread_context* tc)
{
    m->last_line_store = NULL;
    int r = append_line_store(m, tc, LINE_STORE_MIN_SIZE);
    if (r) return r;
    m->last_line_store->first_line = 0;
    *m->last_line = 0;
    m->last_line++;
    return 0;
}

int src_map_fin(source_map* m)
{
    // nothing to do here for now
    return 0;
}

int src_map_add_line(source_map* m, thread_context* tc, ureg line_start)
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
    source_map* m, ureg line_nr, ureg* start_pos, ureg* length)
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
src_pos src_map_get_pos(source_map* m, ureg pos)
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
void src_range_set_end(thread_context* tc, src_range* old, ureg end)
{
    src_range r = *old;
    if (r & SRC_RANGE_EXTERN_BIT) {
        if (r & SRC_RANGE_NEW_MAP_BIT) {
            *(ureg*)ptradd((void*)(r << 2), sizeof(src_file*) + sizeof(ureg)) =
                end;
        }
        else {
            ureg* p = (ureg*)ptradd((void*)(r << 2), sizeof(ureg));
            *p = end;
        }
    }
    else {
        ureg start = r >> SRC_RANGE_LENGTH_BITS;
        *old = src_range_pack_lines(tc, start, end);
    }
}
src_range src_range_large_pack(thread_context* tc, src_range_large* d)
{
    // PERF: maybe allocate these somewhere else
    if (!d->file) {
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
        src_file** tgt = (src_file**)pool_alloc(&tc->permmem, sizeof(ureg) * 3);
        if (!tgt) return SRC_RANGE_INVALID;
        *tgt = d->file;
        ureg* range = (void*)(tgt + 1);
        *range = d->start;
        range++;
        *range = d->end;
        return SRC_RANGE_EXTERN_BIT | SRC_RANGE_NEW_MAP_BIT |
               (((ureg)tgt) >> 2);
    }
}
src_range src_range_pack(thread_context* tc, ureg start, ureg end, src_file* f)
{
    src_range_large srl = {f, start, end};
    return src_range_large_pack(tc, &srl);
}
src_range src_range_pack_lines(thread_context* tc, ureg start, ureg end)
{
    src_range_large srl = {NULL, start, end};
    return src_range_large_pack(tc, &srl);
}
void src_range_unpack(src_range r, src_range_large* d)
{
    if (r & SRC_RANGE_EXTERN_BIT) {
        if (r & SRC_RANGE_NEW_MAP_BIT) {
            src_file** tgt = (void*)(r << 2);
            d->file = *tgt;
            ureg* range = (ureg*)(tgt + 1);
            d->start = *range;
            range++;
            d->end = *range;
        }
        else {
            d->file = NULL;
            ureg* tgt = (ureg*)(r << 2);
            d->start = *tgt;
            tgt++;
            d->end = *tgt;
        }
    }
    else {
        d->file = NULL;
        d->start = r >> SRC_RANGE_LENGTH_BITS;
        ureg len = (r & SRC_RANGE_MAX_LENGTH);
        d->end = d->start + len;
    }
}
src_file* src_range_get_file(src_range r)
{
    src_range_large l;
    src_range_unpack(r, &l);
    return l.file;
}
ureg src_range_get_start(src_range r)
{
    src_range_large l;
    src_range_unpack(r, &l);
    return l.start;
}
ureg src_range_get_end(src_range r)
{
    src_range_large l;
    src_range_unpack(r, &l);
    return l.end;
}
void src_range_unpack_lines(src_range r, ureg* start, ureg* end)
{
    src_range_large l;
    src_range_unpack(r, &l);
    *start = l.start;
    *end = l.end;
}
