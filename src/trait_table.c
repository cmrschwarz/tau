#include "trait_table.h"

trait_table* trait_table_create(ureg impls, ureg generic_impls)
{
    trait_table* t = tmalloc(sizeof(trait_table));
    if (!t) return NULL;
    t->impl_lists = (impl_list_for_type*)NULL_PTR_PTR;
    t->impl_lists_count = 0;
    t->impl_lists_bitcount = 0;
    t->impl_statuses = (impl_status_for_type*)NULL_PTR_PTR;
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
        ureg unresolved_impl_size =
            ceil_to_pow2(impls) * sizeof(trait_impl_generic*);
        int r = dbuffer_init_with_capacity(
            &t->unresolved_impls, unresolved_impl_size);
        if (r) {
            tfree(t);
            if (generic_impls) dbuffer_fin(&t->generic_impls);
            return NULL;
        }
    }
    return t;
}

void trait_table_destroy(trait_table* t)
{
    if (t->impl_lists_bitcount) tfree(t->impl_lists);
    if (t->impl_statuses_bitcount) tfree(t->impl_statuses);
    if (!dbuffer_is_invalid(&t->unresolved_impls)) {
        dbuffer_fin(&t->unresolved_impls);
    }
    if (!dbuffer_is_invalid(&t->generic_impls)) {
        dbuffer_fin(&t->generic_impls);
    }
    tfree(t);
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
