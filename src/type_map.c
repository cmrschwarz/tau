#include "type_map.h"
#include "ast.h"
#include "utils/fnv_hash.h"
#include "ptr_map.h"
#include "utils/panic.h"
#define TYPE_MAP_INITIAL_BITCOUNT 2

int type_map_init(type_map* tm)
{
    int r = rwlock_init(&tm->lock);
    if (r) return r;
    tm->map = (ast_elem**)NULL_PTR_PTR;
    tm->capacity_bitcount = 0;
    tm->count = 0;
    return OK;
}
void type_map_fin(type_map* tm)
{
    if (tm->capacity_bitcount != 0) {
        ast_elem** map_end = tm->map + (1 << tm->capacity_bitcount);
        for (ast_elem** i = tm->map; i != map_end; i++) {
            if (*i) {
                switch ((**i).kind) {
                    case TYPE_ARRAY: {
                        type_map_fin(
                            &((type_array*)*i)->slice_type.tb.type_derivs.tm);
                    } break;
                    case TYPE_TUPLE: {
                        assert(false); // TODO
                    } break;
                    default: panic("compiler bug");
                }
            }
        }
        tfree(tm->map);
    }
    rwlock_fin(&tm->lock);
}
static inline int
type_map_reserve(type_map* tm, ureg* capacity, ureg* mask, ureg hash, ureg* idx)
{
    ureg cap_old = *capacity;
    tm->count++;
    if (tm->count == 1) {
        tm->capacity_bitcount = TYPE_MAP_INITIAL_BITCOUNT;
        ureg cap = 1 << TYPE_MAP_INITIAL_BITCOUNT;
        tm->map = tmallocz(sizeof(ast_elem*) * cap);
        if (!tm->map) return ERR;
        *mask = cap - 1;
        *capacity = cap;
        *idx = fnv_fold(hash, tm->capacity_bitcount, *mask);
        return OK;
    }
    if (tm->count + (tm->count >> 1) < cap_old) return OK;
    ureg cap_new = 2 * *capacity;
    ast_elem** map_new = tmallocz(cap_old * 2 * sizeof(ast_elem*));
    if (!map_new) return ERR;
    tm->capacity_bitcount++;
    assert(cap_new == ((ureg)1 << tm->capacity_bitcount));
    ast_elem** map_old = tm->map;
    ureg mask_new = cap_new - 1;
    for (ast_elem** i = map_old; i != map_old + cap_old; i++) {
        if (!*i) continue;
        ureg hash;
        switch ((**i).kind) {
            case TYPE_ARRAY: {
                hash = fnv_hash_ureg(FNV_START_HASH, ((type_array*)*i)->length);
            } break;
            case TYPE_TUPLE: {
                assert(false); // TODO
            } break;
            default: panic("compiler bug"); return ERR;
        }
        ureg idx = fnv_fold(hash, tm->capacity_bitcount, mask_new);
        while (map_new[idx]) {
            idx = (idx + 1) & mask_new;
        }
        map_new[idx] = *i;
    }
    tfree(map_old);
    *capacity = cap_new;
    *mask = mask_new;
    *idx = fnv_fold(hash, tm->capacity_bitcount, *mask);
    return OK;
}
static inline bool
array_eq(type_array* arr, ast_elem* members_ctype, ureg len, bool is_const)
{
    return arr->slice_type.ctype_members == members_ctype &&
           arr->length == len && arr->slice_type.tb.is_const == is_const;
}
type_array* type_map_get_array(
    type_map* tm, ptr_map* pm, ast_elem* base_type, ureg length, bool is_const,
    pool* mem)
{
    rwlock_read(&tm->lock);
    ureg capacity = 1 << tm->capacity_bitcount;
    ureg mask = capacity - 1;

    ureg hash = fnv_hash_ureg(FNV_START_HASH, length);
    ureg idx = fnv_fold(hash, tm->capacity_bitcount, mask);
    ast_elem** e;
    type_array* res = NULL;
    while (true) {
        e = ptradd(tm->map, sizeof(ast_elem*) * idx);
        if (!*e) break;
        if ((**e).kind == TYPE_ARRAY) {
            res = (type_array*)*e;
            if (array_eq(res, base_type, length, is_const)) break;
        }
        idx = (idx + 1) & mask;
    }
    rwlock_end_read(&tm->lock);
    if (res) return res;
    rwlock_write(&tm->lock);
    int r = type_map_reserve(tm, &capacity, &mask, hash, &idx);
    if (r) {
        rwlock_end_write(&tm->lock);
        return NULL;
    }
    while (true) {
        e = ptradd(tm->map, sizeof(ast_elem*) * idx);
        if (!*e) break;
        if ((**e).kind == TYPE_ARRAY) {
            res = (type_array*)*e;
            if (array_eq(res, base_type, length, is_const)) {
                tm->count--;
                rwlock_end_write(&tm->lock);
                return res;
            }
        }
        idx = (idx + 1) & mask;
    }
    res = pool_alloc(mem, sizeof(type_array));
    if (!res) {
        rwlock_end_write(&tm->lock);
        return NULL;
    }
    res->slice_type.ctype_members = base_type;
    r = type_map_init(&res->slice_type.tb.type_derivs.tm);
    if (r) {
        pool_undo_last_alloc(mem, sizeof(type_array));
        rwlock_end_write(&tm->lock);
        return NULL;
    }
    res->slice_type.tb.kind = TYPE_ARRAY;
    res->slice_type.tb.is_const = is_const;
    res->slice_type.tb.type_derivs.ptr_id = ptr_map_claim_id(pm);
    res->slice_type.tb.type_derivs.slice_id = ptr_map_claim_id(pm);
    res->slice_type.tb.backend_id = ptr_map_claim_backend_id(pm);
    res->slice_type.ctype_members = base_type;
    res->length = length;
    *e = (ast_elem*)res;
    rwlock_end_write(&tm->lock);
    return res;
}

type_tuple* type_map_get_tuple_follower(type_map* tm, ast_elem* following_type)
{
    assert(false); // TODO
    return NULL;
}