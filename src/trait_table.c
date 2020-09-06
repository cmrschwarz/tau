#include "trait_table.h"
#include "utils/fnv_hash.h"

trait_table* trait_table_create(ureg impls, ureg generic_impls)
{
    trait_table* t = tmalloc(sizeof(trait_table));
    if (!t) return NULL;
    t->impl_lists = NULL;
    t->impl_lists_count = 0;
    t->impl_lists_bitcount = 0;
    t->impl_statuses = NULL;
    t->impl_statuses_count = 0;
    t->impl_statuses_bitcount = 0;
    if (!generic_impls) {
        dbuffer_set_invalid(&t->generic_impls);
    }
    else {
        ureg generic_impl_size =
            ceil_to_pow2(generic_impls) * sizeof(trait_impl_generic*);
        int r =
            dbuffer_init_with_capacity(&t->generic_impls, generic_impl_size);
        if (r) {
            tfree(t);
            return NULL;
        }
    }
    if (!impls) {
        dbuffer_set_invalid(&t->unresolved_impls);
    }
    else {
        ureg impl_cap = ceil_to_pow2(impls);
        if (impl_cap < 4) impl_cap = 4;
        ureg impl_bc = ulog2(impl_cap);
        int r = dbuffer_init_with_capacity(
            &t->unresolved_impls, impl_cap * sizeof(trait_impl_generic*));
        if (r) {
            trait_table_destroy(t);
            return NULL;
        }
        t->impl_statuses = tmallocz(impl_cap * sizeof(impl_status_for_type));
        if (!t->impl_statuses) {
            trait_table_destroy(t);
            return NULL;
        }
        t->impl_statuses_bitcount = impl_bc;
        t->impl_lists = tmallocz(impl_cap * sizeof(impl_list_for_type));
        if (!t->impl_lists) {
            trait_table_destroy(t);
            return NULL;
        }
        t->impl_lists_bitcount = impl_bc;
    }
    return t;
}

void trait_table_destroy(trait_table* t)
{
    if (t->impl_lists_bitcount) {
        ureg cap = 1 << t->impl_lists_bitcount;
        for (ureg i = 0; i < cap; i++) {
            if (t->impl_lists[i].type) {
                list_fin(&t->impl_lists[i].impls, true);
            }
        }
        tfree(t->impl_lists);
    }
    if (t->impl_statuses_bitcount) tfree(t->impl_statuses);
    if (!dbuffer_is_invalid(&t->unresolved_impls)) {
        dbuffer_fin(&t->unresolved_impls);
    }
    if (!dbuffer_is_invalid(&t->generic_impls)) {
        dbuffer_fin(&t->generic_impls);
    }
    tfree(t);
}
static inline ureg trait_table_impl_statuses_get_hash(
    ureg mask, ureg bitcount, ast_elem* type, sc_trait* trait)
{
    ureg hash = fnv_hash_pointer(FNV_START_HASH, type);
    fnv_hash_pointer(hash, trait);
    return fnv_fold(hash, bitcount, mask);
}
static inline int
trait_table_impl_statuses_realloc(trait_table* t, ureg cap_old, ureg* cap_new)
{
    if (t->impl_statuses_bitcount == 0) {
        ureg initial_bc = 2;
        ureg capn = 1 << initial_bc;
        t->impl_statuses = tmallocz(capn * sizeof(impl_status_for_type));
        if (!t->impl_statuses) return ERR;
        t->impl_statuses_bitcount = initial_bc;
        *cap_new = capn;
        return OK;
    }
    impl_status_for_type* map_new =
        tmallocz(*cap_new * sizeof(impl_status_for_type));
    if (!map_new) return ERR;
    ureg mask_new = *cap_new - 1;
    impl_status_for_type* map_old = t->impl_statuses;
    t->impl_statuses = map_new;
    ureg bitcount_new = ++t->impl_statuses_bitcount;
    for (ureg i = 0; i < cap_old; i++) {
        ast_elem* type = map_old[i].type;
        if (!type) continue;
        ureg idx = trait_table_impl_statuses_get_hash(
            mask_new, bitcount_new, type, map_old[i].trait);
        while (true) {
            if (map_new[idx].trait == NULL) {
                map_new[idx] = map_old[i];
                break;
            }
            idx++;
            if (idx == cap_old) idx = 0;
        }
    }
    tfree(map_old);
    return OK;
}
static inline impl_status_for_type* trait_table_get_impl_status_raw(
    trait_table* t, ast_elem* type, sc_trait* trait, ureg cap, bool take_empty)
{
    ureg mask = cap - 1;
    ureg idx = trait_table_impl_statuses_get_hash(
        mask, t->impl_statuses_bitcount, type, trait);
    while (true) {
        if (!t->impl_statuses[idx].type) {
            if (take_empty) {
                t->impl_statuses_count++;
                return &t->impl_statuses[idx];
            }
            return NULL;
        }
        if (t->impl_statuses[idx].type == type &&
            t->impl_statuses[idx].trait == trait) {
            return &t->impl_statuses[idx];
        }
        idx++;
        if (idx == cap) idx = 0;
    }
}
impl_status_for_type* trait_table_get_impl_status_for_type(
    trait_table* t, ast_elem* type, sc_trait* trait)
{
    ureg new_count = t->impl_statuses_count + 1;
    ureg cap = 1 << t->impl_statuses_bitcount;
    if (cap <= new_count + (new_count >> 1)) { // catches bitcount == 0
        ureg cap_new = cap << 1;
        if (trait_table_impl_statuses_realloc(t, cap, &cap_new)) return NULL;
        cap = cap_new;
    }
    return trait_table_get_impl_status_raw(t, type, trait, cap, true);
}
impl_status_for_type* trait_table_try_get_impl_status_for_type(
    trait_table* t, ast_elem* type, sc_trait* trait)
{
    if (!t->impl_statuses_bitcount) return NULL;
    return trait_table_get_impl_status_raw(
        t, type, trait, 1 << t->impl_statuses_bitcount, false);
}
static inline ureg
trait_table_impl_lists_get_hash(ureg mask, ureg bitcount, ast_elem* type)
{
    ureg hash = fnv_hash_pointer(FNV_START_HASH, type);
    return fnv_fold(hash, bitcount, mask);
}
static inline int
trait_table_impl_lists_realloc(trait_table* t, ureg cap_old, ureg* cap_new)
{
    if (t->impl_lists_bitcount == 0) {
        ureg initial_bc = 2;
        ureg capn = 1 << initial_bc;
        t->impl_lists = tmallocz(capn * sizeof(impl_list_for_type));
        if (!t->impl_statuses) return ERR;
        t->impl_lists_bitcount = initial_bc;
        *cap_new = capn;
        return OK;
    }
    impl_list_for_type* map_new =
        tmallocz(*cap_new * sizeof(impl_list_for_type));
    if (!map_new) return ERR;
    ureg mask_new = *cap_new - 1;
    impl_list_for_type* map_old = t->impl_lists;
    t->impl_lists = map_new;
    ureg bitcount_new = ++t->impl_lists_bitcount;
    for (ureg i = 0; i < cap_old; i++) {
        ast_elem* type = map_old[i].type;
        if (!type) continue;
        ureg idx =
            trait_table_impl_lists_get_hash(mask_new, bitcount_new, type);
        while (true) {
            if (map_new[idx].type == NULL) {
                map_new[idx] = map_old[i];
                break;
            }
            idx++;
            if (idx == cap_old) idx = 0;
        }
    }
    tfree(map_old);
    return OK;
}
static inline impl_list_for_type* trait_table_get_impl_list_raw(
    trait_table* t, ast_elem* type, ureg cap, bool take_empty)
{
    ureg mask = cap - 1;
    ureg idx =
        trait_table_impl_lists_get_hash(mask, t->impl_lists_bitcount, type);
    while (true) {
        if (!t->impl_lists[idx].type) {
            if (take_empty) {
                t->impl_lists_count++;
                return &t->impl_lists[idx];
            }
            return NULL;
        }
        if (t->impl_lists[idx].type == type) {
            return &t->impl_lists[idx];
        }
        idx++;
        if (idx == cap) idx = 0;
    }
}
impl_list_for_type*
trait_table_get_impl_list_for_type(trait_table* t, ast_elem* type)
{
    ureg new_count = t->impl_lists_count + 1;
    ureg cap = 1 << t->impl_lists_bitcount;
    if (cap <= new_count + (new_count >> 1)) { // catches bitcount == 0
        ureg cap_new = cap << 1;
        if (trait_table_impl_lists_realloc(t, cap, &cap_new)) return NULL;
        cap = cap_new;
    }
    return trait_table_get_impl_list_raw(t, type, cap, true);
}
impl_list_for_type*
trait_table_try_get_impl_lits_for_type(trait_table* t, ast_elem* type)
{
    if (!t->impl_lists_bitcount) return NULL;
    return trait_table_get_impl_list_raw(
        t, type, 1 << t->impl_lists_bitcount, false);
}

int trait_table_append_generic_impl(trait_table* t, trait_impl_generic* tig)
{
    if (dbuffer_is_invalid(&t->generic_impls)) {
        int r = dbuffer_init_with_capacity(
            &t->generic_impls, sizeof(trait_impl_generic*) * 4);
        if (r) return r;
    }
    return dbuffer_append(&t->generic_impls, &tig, sizeof(trait_impl_generic*));
}

int trait_table_append_unresolved_impl(trait_table* t, trait_impl* ti)
{
    if (dbuffer_is_invalid(&t->unresolved_impls)) {
        int r = dbuffer_init_with_capacity(
            &t->unresolved_impls, sizeof(trait_impl*) * 4);
        if (r) return r;
    }
    return dbuffer_append(&t->unresolved_impls, &ti, sizeof(trait_impl*));
}
