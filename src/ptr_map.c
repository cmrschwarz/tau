#include "ptr_map.h"
#include "utils/error.h"

int ptr_map_init(ptr_map* m, global_ptr_map* gpm)
{
    m->segment_capacity = PTR_MAP_INITIAL_SEGMENT_COUNT;
    ureg initial_size = m->segment_capacity * sizeof(ptr_map_segment_ref);
    m->segment_refs = tmalloc(initial_size);
    if (!m->segment_refs) return ERR;
    memset(m->segment_refs, 0, initial_size);
    m->free_type_ids_start = 0;
    m->free_type_ids_end = 0;
    m->gpm = gpm;
    return OK;
}
void ptr_map_fin(ptr_map* m)
{
    tfree(m->segment_refs);
}

int global_ptr_map_partial_fin(global_ptr_map* gpm, int i, int r)
{
    switch (i) {
        case -1: // fallthrough
        case 3: pool_fin(&gpm->segment_mem); // fallthrough
        case 2: tfree(gpm->segments); // fallthrough
        case 1: rwlock_fin(&gpm->lock); // fallthrough
        case 0: break;
    }
    return r;
}
int global_ptr_map_init(global_ptr_map* gpm)
{
    int r;
    r = rwlock_init(&gpm->lock);
    if (r) return global_ptr_map_partial_fin(gpm, 0, r);
    gpm->segment_capacity = PTR_MAP_INITIAL_SEGMENT_COUNT;
    ureg initial_size = gpm->segment_capacity * sizeof(ptr_map_segment*);
    gpm->segments = tmalloc(initial_size);
    if (!gpm->segments) return global_ptr_map_partial_fin(gpm, 1, ERR);
    r = pool_init(&gpm->segment_mem);
    if (r) return global_ptr_map_partial_fin(gpm, 2, r);
    memset(gpm->segments, 0, initial_size);
    atomic_ureg_init(&gpm->type_ids, 0);
    return OK;
}
void global_ptr_map_fin(global_ptr_map* gpm)
{
    global_ptr_map_partial_fin(gpm, -1, 0);
}
ureg ptr_map_claim_id(ptr_map* pm)
{
    if (pm->free_type_ids_start == pm->free_type_ids_end) {
        ureg claim_amount = PTR_MAP_SEGMENT_CAPACITY; // this is arbitrary
        pm->free_type_ids_start =
            atomic_ureg_add(&pm->gpm->type_ids, claim_amount);
        pm->free_type_ids_end = pm->free_type_ids_start + claim_amount;
    }
    return pm->free_type_ids_start++;
}
static inline ast_elem** get_segment_offset(ptr_map_segment* ts, ureg idx)
{
    return (ast_elem**)ptradd(
        ts, sizeof(ptr_map_segment) + idx * sizeof(ast_elem*));
}
static inline type_pointer*
get_segment_type_ptr(ptr_map_segment_ref* seg_ref, ureg idx)
{
    if (seg_ref->filled_bits >> idx & 1) {
        return *(type_pointer**)get_segment_offset(seg_ref->segment, idx);
    }
    return NULL;
}
ptr_map_segment_ref* ptr_map_get_segment_ref(ptr_map* pm, ureg seg_idx)
{
    if (seg_idx >= pm->segment_capacity) {
        ureg cap_new = pm->segment_capacity;
        while (cap_new <= seg_idx) cap_new *= 2;
        ureg size_old = pm->segment_capacity * sizeof(ptr_map_segment_ref);
        ureg size_new = cap_new * sizeof(ptr_map_segment_ref);
        ptr_map_segment_ref* refs_new =
            trealloc(pm->segment_refs, size_old, size_new);
        if (!refs_new) return NULL;
        memset(ptradd(refs_new, size_old), 0, size_new - size_old);
        pm->segment_refs = refs_new;
        pm->segment_capacity = cap_new;
    }
    return &pm->segment_refs[seg_idx];
}
ptr_map_segment* global_ptr_map_get_segment(global_ptr_map* gpm, ureg seg_idx)
{
    bool write_aquired = false;
    rwlock_read(&gpm->lock);
    if (seg_idx >= gpm->segment_capacity) {
        rwlock_end_read(&gpm->lock);
        write_aquired = true;
        rwlock_write(&gpm->lock);
        if (seg_idx >= gpm->segment_capacity) {
            ureg cap_new = gpm->segment_capacity;
            while (cap_new <= seg_idx) cap_new *= 2;
            ureg size_old = gpm->segment_capacity * sizeof(ptr_map_segment*);
            ureg size_new = cap_new * sizeof(ptr_map_segment_ref);
            ptr_map_segment** segs_new =
                trealloc(gpm->segments, size_old, size_new);
            if (!segs_new) {
                rwlock_end_write(&gpm->lock);
                return NULL;
            }
            memset(ptradd(segs_new, size_old), 0, size_new - size_old);
            gpm->segments = segs_new;
            gpm->segment_capacity = cap_new;
        }
    }
    ptr_map_segment** seg_ptr = &gpm->segments[seg_idx];
    ptr_map_segment* ts;
    while (true) {
        ts = *seg_ptr;
        if (ts) {
            if (write_aquired) {
                rwlock_end_write(&gpm->lock);
            }
            else {
                rwlock_end_read(&gpm->lock);
            }
            return ts;
        }
        if (write_aquired) break;
        rwlock_end_read(&gpm->lock);
        write_aquired = true;
        rwlock_write(&gpm->lock);
    }
    assert(write_aquired);
    ts = pool_alloc(&gpm->segment_mem, PTR_MAP_SEGMENT_SIZE);
    if (!ts) {
        rwlock_end_write(&gpm->lock);
        return NULL;
    }
    int r = rwlock_init(&ts->lock);
    if (r) {
        pool_undo_last_alloc(&gpm->segment_mem, PTR_MAP_SEGMENT_SIZE);
        rwlock_end_write(&gpm->lock);
        return NULL;
    }
    memset(
        ptradd(ts, sizeof(ptr_map_segment)), 0,
        PTR_MAP_SEGMENT_SIZE - sizeof(ptr_map_segment));
    ts->filled_bits = 0;
    *seg_ptr = ts;
    rwlock_end_write(&gpm->lock);
    return ts;
}

type_pointer* ptr_map_get_pointer(
    ptr_map* pm, ast_elem* base_type, ureg ptr_id, bool is_const,
    pool* type_mem)
{
    ureg seg_idx = ptr_id / PTR_MAP_SEGMENT_CAPACITY;
    ureg seg_entry_idx = ptr_id % PTR_MAP_SEGMENT_CAPACITY;

    ptr_map_segment_ref* seg_ref = ptr_map_get_segment_ref(pm, seg_idx);
    // fast path
    type_pointer* res = get_segment_type_ptr(seg_ref, seg_idx);
    if (res) return res;
    if (!seg_ref->segment) {
        seg_ref->segment = global_ptr_map_get_segment(pm->gpm, seg_idx);
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
    res->tb.kind = TYPE_POINTER;
    res->tb.type_derivs.ptr_id = ptr_map_claim_id(pm);
    res->tb.is_const = is_const;
    res->base_type = base_type;
    res->flipped_const_id = ptr_map_claim_id(pm);
    *pos = res;
    seg_ref->segment->filled_bits |= 1 << seg_entry_idx;
    seg_ref->filled_bits = seg_ref->segment->filled_bits;
    rwlock_end_write(&seg_ref->segment->lock);
    return res;
}