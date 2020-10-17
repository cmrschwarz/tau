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

static inline int append_line_store(src_map* m, ureg size, ureg first_line)
{
    line_store* s = (line_store*)tmalloc(size);
    if (!s) return -1;
    s->end = ptradd(s, size);
    s->prev = m->last_line_store;
    s->first_line = first_line;
    m->last_line_store = s;
    m->last_line = ptradd(s, sizeof(line_store));
    return 0;
}
int src_map_init(src_map* m, ast_elem* source, thread_context* tc)
{
    m->last_line_store = NULL;
    int r = append_line_store(m, LINE_STORE_MIN_SIZE, 0);
    if (r) return r;
    *m->last_line = 0;
    m->last_line++;
    m->source = source;
    m->next = NULL;
    return 0;
}

void src_map_fin(src_map* m)
{
    line_store* ls = m->last_line_store;
    while (ls) {
        line_store* prev = ls->prev;
        tfree(ls);
        ls = prev;
    }
    if (m->next) src_map_fin(m->next);
}

int src_map_add_line(src_map* m, ureg line_start)
{
    if (m->last_line == m->last_line_store->end) {
        ureg last_size = ptrdiff(m->last_line_store->end, m->last_line_store);
        ureg prev_fst_line = m->last_line_store->first_line;
        int r = append_line_store(
            m, last_size * 2,
            prev_fst_line + (last_size - sizeof(line_store)) / sizeof(ureg));
        if (r) return -1;
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
void src_range_set_smap(thread_context* tc, src_range* old, src_map* smap)
{
    src_range r = *old;
    if (r & SRC_RANGE_EXTERN_BIT && r & SRC_RANGE_NEW_MAP_BIT) {
        *(src_map**)(void*)(r << 2) = smap;
    }
    else {
        src_range_large srl;
        src_range_unpack(*old, &srl);
        *old = src_range_pack(tc, srl.start, srl.end, smap);
    }
}
void src_range_set_end(thread_context* tc, src_range* old, ureg end)
{
    src_range r = *old;
    if (r & SRC_RANGE_EXTERN_BIT) {
        if (r & SRC_RANGE_NEW_MAP_BIT) {
            *(ureg*)ptradd((void*)(r << 2), sizeof(src_map*) + sizeof(ureg)) =
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
    if (!d->smap) {
        ureg len = d->end - d->start;
        if (len > SRC_RANGE_MAX_LENGTH || d->start > SRC_RANGE_MAX_START) {
            ureg* tgt = pool_alloc(&tc->permmem, sizeof(ureg) * 2);
            assert(((ureg)tgt & 0x3) == 0);
            if (!tgt) return SRC_RANGE_INVALID;
            *tgt = d->start;
            *(tgt + 1) = d->end;
            return SRC_RANGE_EXTERN_BIT | (((ureg)tgt) >> 2);
        }
        return (d->start << SRC_RANGE_LENGTH_BITS) | len;
    }
    src_map** tgt = (src_map**)pool_alloc(&tc->permmem, sizeof(ureg) * 3);
    assert(((ureg)tgt & 0x3) == 0);
    if (!tgt) return SRC_RANGE_INVALID;
    *tgt = d->smap;
    ureg* range = (void*)(tgt + 1);
    *range = d->start;
    range++;
    *range = d->end;
    return SRC_RANGE_EXTERN_BIT | SRC_RANGE_NEW_MAP_BIT | (((ureg)tgt) >> 2);
}
src_range
src_range_pack(thread_context* tc, ureg start, ureg end, src_map* smap)
{
    src_range_large srl = {smap, start, end};
    return src_range_large_pack(tc, &srl);
}
src_range src_range_pack_lines(thread_context* tc, ureg start, ureg end)
{
    src_range_large srl = {NULL, start, end};
    return src_range_large_pack(tc, &srl);
}
void src_range_unpack(src_range r, src_range_large* d)
{
    assert(r != SRC_RANGE_INVALID);
    if (r & SRC_RANGE_EXTERN_BIT) {
        if (r & SRC_RANGE_NEW_MAP_BIT) {
            src_map** tgt = (void*)(r << 2);
            d->smap = *tgt;
            ureg* range = (ureg*)(tgt + 1);
            d->start = *range;
            range++;
            d->end = *range;
        }
        else {
            d->smap = NULL;
            ureg* tgt = (ureg*)(r << 2);
            d->start = *tgt;
            tgt++;
            d->end = *tgt;
        }
    }
    else {
        d->smap = NULL;
        d->start = r >> SRC_RANGE_LENGTH_BITS;
        ureg len = (r & SRC_RANGE_MAX_LENGTH);
        d->end = d->start + len;
    }
}
src_map* src_range_get_smap(src_range r)
{
    src_range_large l;
    src_range_unpack(r, &l);
    return l.smap;
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
void src_map_print_path(src_map* smap, bool to_stderr)
{
    if (smap->source->kind == ELEM_SRC_FILE) {
        file_map_head_print_path(((file_map_head*)smap->source), to_stderr);
        return;
    }
    assert(smap->source->kind == ELEM_PASTED_SRC);
    src_range_large srl;
    src_range_unpack(((pasted_source*)smap->source)->source_pp_srange, &srl);
    srl.smap = ((pasted_source*)smap->source)->source_pp_smap;
    fprintf(to_stderr ? stderr : stdout, "<pasted: ");
    src_map_print_path(srl.smap, to_stderr);
    src_pos p = src_map_get_pos(srl.smap, srl.start);
    fprintf(to_stderr ? stderr : stdout, ":%zi:%zi>", p.line + 1, p.column + 1);
}
src_file* src_map_get_file(src_map* smap)
{
    if (smap->source->kind == ELEM_SRC_FILE) return (src_file*)smap->source;
    assert(smap->source->kind == ELEM_PASTED_SRC);
    smap = ((pasted_source*)smap->source)->source_pp_smap;
    assert(smap);
    return src_map_get_file(smap);
}
bool src_map_is_opened(src_map* smap)
{
    if (smap->source->kind == ELEM_SRC_FILE) {
        return ((src_file*)smap->source)->file_stream != NULL;
    }
    assert(smap->source->kind == ELEM_PASTED_SRC);
    return ((pasted_source*)smap->source)->read_data.read_str != NULL;
}
int src_map_open(src_map* smap)
{
    if (smap->source->kind == ELEM_SRC_FILE) {
        src_file* f = (src_file*)smap->source;
        assert(!f->file_stream);
        char pathbuff[256];
        ureg pathlen = file_map_head_get_path_len(&f->head);
        char* path;
        if (pathlen < 256) {
            file_map_head_write_path(&f->head, pathbuff);
            path = pathbuff;
        }
        else {
            path = tmalloc(pathlen + 1);
            file_map_head_write_path(&f->head, pathbuff);
        }
        f->file_stream = fopen(path, "r");
        if (path != pathbuff) tfree(path);
        if (f->file_stream == NULL) return ERR;
        if (ferror(f->file_stream)) {
            fclose(f->file_stream);
            f->file_stream = NULL;
            return ERR;
        }
        return OK;
    }
    assert(smap->source->kind == ELEM_PASTED_SRC);
    pasted_source* pe = (pasted_source*)smap->source;
    assert(pe->read_data.read_str == NULL);
    pe->read_data.read_str = pe->read_data.paste_str;
    pe->read_data.read_pos =
        pe->read_data.paste_str ? pe->read_data.paste_str->str : NULL;
    return OK;
}
int paste_eval_read(pasted_source* ps, ureg size, ureg* read_size, char* tgt)
{
    if (!ps->read_data.read_str) {
        *read_size = 0;
        return OK;
    }
    char* s = ps->read_data.read_pos;
    ureg i = 0;
    while (i < size) {
        if (*s == '\0') {
            ps->read_data.read_str = ps->read_data.read_str->next;
            if (!ps->read_data.read_str) break;
            s = ps->read_data.read_str->str;
            continue;
        }
        if (tgt) {
            *tgt = *s;
            tgt++;
        }
        s++;
        i++;
        if (i == size) break;
    }
    ps->read_data.read_pos = s;
    *read_size = i;
    return OK;
}
int src_map_seek_set(src_map* smap, ureg pos)
{
    if (smap->source->kind == ELEM_SRC_FILE) {
        src_file* f = (src_file*)smap->source;
        assert(f->file_stream);
        return fseek(f->file_stream, pos, SEEK_SET) ? ERR : OK;
    }
    assert(smap->source->kind == ELEM_PASTED_SRC);
    pasted_source* ps = (pasted_source*)smap->source;
    ps->read_data.read_str = ps->read_data.paste_str;
    ps->read_data.read_pos = ps->read_data.read_str->str;
    ureg read_size;
    paste_eval_read(ps, pos, &read_size, NULL);
    if (read_size != pos) return ERR;
    return OK;
}
int src_map_read(src_map* smap, ureg size, ureg* read_size, char* tgt)
{
    if (smap->source->kind == ELEM_SRC_FILE) {
        src_file* f = (src_file*)smap->source;
        assert(f->file_stream);
        size = fread(tgt, 1, size, f->file_stream);
        int e = ferror(f->file_stream);
        if (e) return e;
        *read_size = size;
        return OK;
    }
    assert(smap->source->kind == ELEM_PASTED_SRC);
    pasted_source* ps = (pasted_source*)smap->source;
    return paste_eval_read(ps, size, read_size, tgt);
}
void src_map_close(src_map* smap)
{
    if (smap->source->kind == ELEM_SRC_FILE) {
        src_file* f = (src_file*)smap->source;
        assert(f->file_stream);
        fclose(f->file_stream);
        f->file_stream = NULL;
        return;
    }
    assert(smap->source->kind == ELEM_PASTED_SRC);
    pasted_source* ps = (pasted_source*)smap->source;
    ps->read_data.read_str = NULL;
}
