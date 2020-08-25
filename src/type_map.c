#include "type_map.h"
#include "utils/error.h"

int type_map_init(type_map* m, global_type_map* gtm)
{
    m->segment_capacity = TYPE_MAP_INITIAL_SEGMENT_COUNT;
    ureg initial_size = m->segment_capacity * sizeof(type_map_segment_ref);
    m->segment_refs = tmalloc(initial_size);
    if (!m->segment_refs) return ERR;
    memset(m->segment_refs, 0, initial_size);
    m->free_type_ids_start = 0;
    m->free_type_ids_end = 0;
    m->gtm = gtm;
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
        case 2: tfree(gtm->segments); // fallthrough
        case 1: rwlock_fin(&gtm->lock); // fallthrough
        case 0: break;
    }
    return r;
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
    memset(gtm->segments, 0, initial_size);
    atomic_ureg_init(&gtm->type_ids, 0);
    return OK;
}
void global_type_map_fin(global_type_map* gtm)
{
    global_type_map_partial_fin(gtm, -1, 0);
}
ureg type_map_claim_id(type_map* tm)
{
    if (tm->free_type_ids_start == tm->free_type_ids_end) {
        ureg claim_amount = TYPE_MAP_SEGMENT_CAPACITY; // this is arbitrary
        tm->free_type_ids_start =
            atomic_ureg_add(&tm->gtm->type_ids, claim_amount);
        tm->free_type_ids_end = tm->free_type_ids_start + claim_amount;
    }
    return tm->free_type_ids_start++;
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
type_map_segment_ref* type_map_get_segment_ref(type_map* tm, ureg seg_idx)
{
    if (seg_idx >= tm->segment_capacity) {
        ureg cap_new = tm->segment_capacity;
        while (cap_new <= seg_idx) cap_new *= 2;
        ureg size_old = tm->segment_capacity * sizeof(type_map_segment_ref);
        ureg size_new = cap_new * sizeof(type_map_segment_ref);
        type_map_segment_ref* refs_new =
            trealloc(tm->segment_refs, size_old, size_new);
        if (!refs_new) return NULL;
        memset(ptradd(refs_new, size_old), 0, size_new - size_old);
        tm->segment_refs = refs_new;
        tm->segment_capacity = cap_new;
    }
    return &tm->segment_refs[seg_idx];
}
type_map_segment*
global_type_map_get_segment(global_type_map* gtm, ureg seg_idx)
{
    bool write_aquired = false;
    rwlock_read(&gtm->lock);
    if (seg_idx >= gtm->segment_capacity) {
        rwlock_end_read(&gtm->lock);
        write_aquired = true;
        rwlock_write(&gtm->lock);
        if (seg_idx >= gtm->segment_capacity) {
            ureg cap_new = gtm->segment_capacity;
            while (cap_new <= seg_idx) cap_new *= 2;
            ureg size_old = gtm->segment_capacity * sizeof(type_map_segment*);
            ureg size_new = cap_new * sizeof(type_map_segment_ref);
            type_map_segment** segs_new =
                trealloc(gtm->segments, size_old, size_new);
            if (!segs_new) {
                rwlock_end_write(&gtm->lock);
                return NULL;
            }
            memset(ptradd(segs_new, size_old), 0, size_new - size_old);
            gtm->segments = segs_new;
            gtm->segment_capacity = cap_new;
        }
    }
    type_map_segment** seg_ptr = &gtm->segments[seg_idx];
    type_map_segment* ts;
    while (true) {
        ts = *seg_ptr;
        if (ts) {
            if (write_aquired) {
                rwlock_end_write(&gtm->lock);
            }
            else {
                rwlock_end_read(&gtm->lock);
            }
            return ts;
        }
        if (write_aquired) break;
        rwlock_end_read(&gtm->lock);
        write_aquired = true;
        rwlock_write(&gtm->lock);
    }
    assert(write_aquired);
    ts = pool_alloc(&gtm->segment_mem, TYPE_MAP_SEGMENT_SIZE);
    if (!ts) {
        rwlock_end_write(&gtm->lock);
        return NULL;
    }
    int r = rwlock_init(&ts->lock);
    if (r) {
        pool_undo_last_alloc(&gtm->segment_mem, TYPE_MAP_SEGMENT_SIZE);
        rwlock_end_write(&gtm->lock);
        return NULL;
    }
    memset(
        ptradd(ts, sizeof(type_map_segment)), 0,
        TYPE_MAP_SEGMENT_SIZE - sizeof(type_map_segment));
    ts->filled_bits = 0;
    *seg_ptr = ts;
    rwlock_end_write(&gtm->lock);
    return ts;
}

type_pointer* type_map_get_pointer(
    type_map* tm, ast_elem* base_type, ureg ptr_id, bool is_const,
    pool* type_mem)
{
    ureg seg_idx = ptr_id / TYPE_MAP_SEGMENT_CAPACITY;
    ureg seg_entry_idx = ptr_id % TYPE_MAP_SEGMENT_CAPACITY;

    type_map_segment_ref* seg_ref = type_map_get_segment_ref(tm, seg_idx);
    // fast path
    type_pointer* res = get_segment_type_ptr(seg_ref, seg_idx);
    if (res) return res;
    if (!seg_ref->segment) {
        seg_ref->segment = global_type_map_get_segment(tm->gtm, seg_idx);
    }
    bool write_aquired = false;
    rwlock_read(&seg_ref->segment->lock);
    while (true) {
        seg_ref->filled_bits = seg_ref->segment->filled_bits;
        res = get_segment_type_ptr(seg_ref, seg_idx);
        if (res) {
            if (write_aquired) {
                rwlock_end_write(&seg_ref->segment->lock);
            }
            else {
                rwlock_end_read(&seg_ref->segment->lock);
            }
            return res;
        }
        if (write_aquired) break;
        rwlock_end_read(&seg_ref->segment->lock);
        write_aquired = true;
        rwlock_write(&seg_ref->segment->lock);
    }
    assert(write_aquired);
    type_pointer** pos =
        (type_pointer**)get_segment_offset(seg_ref->segment, seg_entry_idx);
    res = pool_alloc(type_mem, sizeof(type_pointer));
    if (!res) {
        rwlock_end_write(&seg_ref->segment->lock);
        return NULL;
    }
    res->kind = TYPE_POINTER;
    res->base = base_type;
    res->ptr_id = type_map_claim_id(tm);
    res->flipped_const_id = type_map_claim_id(tm);
    res->is_const = is_const;
    *pos = res;
    seg_ref->segment->filled_bits |= 1 << seg_entry_idx;
    seg_ref->filled_bits = seg_ref->segment->filled_bits;
    rwlock_end_write(&seg_ref->segment->lock);
    return res;
}