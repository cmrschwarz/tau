#include "type_map.h"
#include "utils/error.h"

int type_map_init(type_map* m, global_type_map* gtm)
{
    int r;
    m->segment_capacity = TYPE_MAP_INITIAL_SEGMENT_COUNT;
    ureg initial_size = m->segment_capacity * sizeof(type_map_segment_ref);
    m->segment_refs = tmalloc(initial_size);
    if (!m->segment_refs) return ERR;
    memset(m->segment_refs, 0, initial_size);
    return OK;
}
void type_map_fin(type_map* m)
{
    tfree(m->segment_refs);
}

int global_type_map_partial_fin(global_type_map* gtm, int i, int r)
{
    switch (i) {
        case -1: // fallthrough
        case 3: pool_fin(&gtm->segment_mem); // fallthrough
        case 2: tfree(&gtm->segments); // fallthrough
        case 1: rwlock_fin(&gtm->lock); // fallthrough
        case 0: return r;
    }
}
int global_type_map_init(global_type_map* gtm)
{
    int r;
    r = rwlock_init(&gtm->lock);
    if (r) return global_type_map_partial_fin(gtm, 0, r);
    gtm->segment_capacity = TYPE_MAP_INITIAL_SEGMENT_COUNT;
    ureg initial_size = gtm->segment_capacity * sizeof(type_map_segment*);
    gtm->segments = tmalloc(initial_size);
    if (!gtm->segments) return global_type_map_partial_fin(gtm, 1, ERR);
    r = pool_init(&gtm->segment_mem);
    if (r) return global_type_map_partial_fin(gtm, 2, r);
    gtm->type_ids = 0;
    memset(gtm->segments, 0, initial_size);
    return OK;
}
void global_type_map_fin(global_type_map* gtm)
{
    global_type_map_partial_fin(gtm, -1, 0);
}

static inline ast_elem** get_segment_offset(type_map_segment* ts, ureg idx)
{
    return (ast_elem**)ptradd(
        ts, sizeof(type_map_segment) + idx * sizeof(ast_elem*));
}
static inline type_pointer*
get_segment_type_ptr(type_map_segment_ref* seg_ref, ureg idx)
{
    if (seg_ref->filled_bits >> idx & 1) {
        return *(type_pointer**)get_segment_offset(seg_ref->segment, idx);
    }
    return NULL;
}

type_pointer* type_map_get_pointer(
    type_map* tm, ast_elem* base_type, ureg ptr_id, bool is_const,
    pool* type_mem)
{
    ureg segment = ptr_id / TYPE_MAP_SEGMENT_CAPACITY;
    ureg seg_idx = ptr_id % TYPE_MAP_SEGMENT_CAPACITY;
    type_pointer* res;
    if (segment >= tm->segment_capacity) {
        ureg cap_new = tm->segment_capacity;
        while (cap_new <= segment) cap_new *= 2;
        ureg size_old = tm->segment_capacity * sizeof(type_map_segment_ref);
        ureg size_new = cap_new * sizeof(type_map_segment_ref);
        type_map_segment_ref* refs_new =
            trealloc(tm->segment_refs, size_old, size_new);
        if (!refs_new) return NULL;
        memset(ptradd(refs_new, size_old), 0, size_new - size_old);
        tm->segment_refs = refs_new;
        tm->segment_capacity = cap_new;
    }
    type_map_segment_ref* seg_ref = &tm->segment_refs[segment];
    res = get_segment_type_ptr(seg_ref, seg_idx);
    if (res) return res;
    bool write_aquired = false;
    if (!seg_ref->segment) {
        bool realloced = false;
        rwlock_read(&tm->gtm->lock);
        if (segment >= tm->gtm->segment_capacity) {
            rwlock_end_read(&tm->gtm->lock);
            write_aquired = true;
            rwlock_write(&tm->gtm->lock);
            if (segment >= tm->gtm->segment_capacity) {
                ureg cap_new = tm->gtm->segment_capacity;
                while (cap_new <= segment) cap_new *= 2;
                ureg size_old =
                    tm->gtm->segment_capacity * sizeof(type_map_segment*);
                ureg size_new = cap_new * sizeof(type_map_segment_ref);
                type_map_segment** segs_new =
                    trealloc(tm->gtm->segments, size_old, size_new);
                if (!segs_new) {
                    rwlock_write(&tm->gtm->lock);
                    return NULL;
                }
                memset(ptradd(segs_new, size_old), 0, size_new - size_old);
                tm->gtm->segments = segs_new;
                tm->gtm->segment_capacity = cap_new;
                realloced = true;
            }
        }
        type_map_segment** seg_ptr = &tm->gtm->segments[segment];
        if (!realloced) {
            while (true) {
                if (*seg_ptr) {
                    seg_ref->segment = *seg_ptr;
                    seg_ref->filled_bits = seg_ref->segment->filled_bits;
                    res = get_segment_type_ptr(seg_ref, seg_idx);
                    if (res) {
                        if (write_aquired) {
                            rwlock_end_write(&tm->gtm->lock);
                        }
                        else {
                            rwlock_end_read(&tm->gtm->lock);
                        }
                        return res;
                    }
                }
                if (write_aquired) break;
                rwlock_end_read(&tm->gtm->lock);
                write_aquired = true;
                rwlock_write(&tm->gtm->lock);
            }
        }
        if (realloced || !seg_ref->segment) {
            seg_ref->segment =
                pool_alloc(&tm->gtm->segment_mem, TYPE_MAP_SEGMENT_SIZE);
            if (!seg_ref->segment) {
                rwlock_end_write(&tm->gtm->lock);
                return NULL;
            }
            *seg_ptr = seg_ref->segment;
            memset(seg_ref->segment, 0, TYPE_MAP_SEGMENT_SIZE);
        }
    }
    else {
        rwlock_read(&tm->gtm->lock);
        while (true) {
            seg_ref->filled_bits = seg_ref->segment->filled_bits;
            res = get_segment_type_ptr(seg_ref, seg_idx);
            if (res) {
                if (write_aquired) {
                    rwlock_end_write(&tm->gtm->lock);
                }
                else {
                    rwlock_end_read(&tm->gtm->lock);
                }
                return res;
            }
            rwlock_end_read(&tm->gtm->lock);
            write_aquired = true;
            rwlock_write(&tm->gtm->lock);
        }
    }
    assert(write_aquired);
    type_pointer** pos =
        (type_pointer**)get_segment_offset(seg_ref->segment, seg_idx);
    res = pool_alloc(type_mem, sizeof(type_pointer));
    if (!res) {
        rwlock_end_write(&tm->gtm->lock);
        return NULL;
    }
    res->kind = TYPE_POINTER;
    res->base = base_type;
    res->ptr_to_id = tm->gtm->type_ids++;
    res->flipped_const_id = tm->gtm->type_ids++;
    res->is_const = is_const;
    *pos = res;
    seg_ref->segment->filled_bits |= 1 << seg_idx;
    seg_ref->filled_bits = seg_ref->segment->filled_bits;
    rwlock_end_write(&tm->gtm->lock);
    return res;
}