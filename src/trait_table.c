#include "trait_table.h"
#include "ast.h"

trait_table* trait_table_create(ureg impls, ureg generic_impls)
{
    trait_table* t = malloc(sizeof(trait_table));
    if (!t) return NULL;
    t->impl_list_for_types = (impl_list_for_type*)NULL_PTR_PTR;
    t->impl_list_for_types_count = 0;
    t->impl_list_for_types_bitcount = 0;
    t->impl_status_for_types = (impl_status_for_type*)NULL_PTR_PTR;
    t->impl_status_for_types_count = 0;
    t->impl_status_for_types_bitcount = 0;
    if (!generic_impls) {
        t->generic_trait_impls_bitcount = 0;
        t->generic_trait_impls = (trait_impl_generic**)NULL_PTR_PTR;
    }
    else {
        //+1 for zero termination
        ureg generic_impl_cap = ceil_to_pow2(generic_impls + 1);
        t->generic_trait_impls_bitcount = ulog2(generic_impl_cap);
        t->generic_trait_impls =
            malloc(generic_impl_cap * sizeof(trait_impl_generic*));
        if (!t->generic_trait_impls) {
            trait_table_destroy(t);
            return NULL;
        }
    }
    return t;
}

void trait_table_destroy(trait_table* t)
{
    if (t->impl_list_for_types_bitcount) tfree(t->impl_list_for_types);
    if (t->impl_status_for_types_bitcount) tfree(t->impl_status_for_types);
    if (t->generic_trait_impls_bitcount) tfree(t->generic_trait_impls);
    tfree(t);
}