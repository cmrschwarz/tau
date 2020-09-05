#pragma once
#include "utils/dbuffer.h"
#include "utils/list.h"
typedef struct trait_impl_s trait_impl;
typedef struct trait_impl_generic_s trait_impl_generic;
typedef struct sc_trait_s sc_trait;
typedef struct ast_elem_s ast_elem;
typedef struct resolver_s resolver;
typedef struct ast_body_s ast_body;

// this table has to handle different kinds of questions:
// for type/trait x, is trait y implemented in this scope ?
// for type/trait x, give all implemented traits

typedef struct impl_list_for_type_s {
    ast_elem* type; // the type or trait we have impls for
    list impls;
} impl_list_for_type;

typedef struct impl_status_for_type_s {
    ast_elem* type; // NULL iff we are in unresolved impls mode
    sc_trait* trait;
    trait_impl* impl; // NULL means there is definitely no impl
} impl_status_for_type;

typedef struct trait_table_s {
    dbuffer generic_impls;
    dbuffer unresolved_impls;
    impl_list_for_type* impl_lists;
    ureg impl_lists_count;
    impl_status_for_type* impl_statuses;
    ureg impl_statuses_count;
    u8 impl_statuses_bitcount;
    u8 impl_lists_bitcount;
} trait_table;

typedef struct trait_impl_iterator_s {
    resolver* r;
    ast_body* looking_body;
    ast_elem* looking_type;
} trait_impl_iterator;

trait_table* trait_table_create(ureg impl_count, ureg generic_impl_count);
void trait_table_destroy(trait_table* t);
int trait_table_append_generic_impl(trait_table* t, trait_impl_generic* tig);
int trait_table_append_unresolved_impl(trait_table* t, trait_impl* ti);
