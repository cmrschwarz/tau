// IHT: indentifier hash table
#pragma once
#include "ast.h"
#include "utils/allocator.h"
#include "utils/types.h"

typedef struct iht {
    named_stmt** table_start;
    named_stmt** table_end;
    ureg elem_count;
    ureg grow_on_elem_count;
    ureg hash_mask;
    ureg size_bits;
    thread_allocator* tal;
} iht;

typedef ureg parent_hash;

parent_hash iht_get_parent_hash(named_stmt* parent);

int iht_init(iht* h, thread_allocator* tal);
int iht_init_with_capacity(iht* h, ureg capacity, thread_allocator* tal);
void iht_fin(iht* h);

int iht_insert(iht* h, named_stmt* val);
int iht_insert_pph(iht* h, parent_hash phash, named_stmt* val);

named_stmt* iht_get(iht* h, named_stmt* parent, const char* name);
named_stmt*
iht_get_pph(iht* h, parent_hash phash, named_stmt* parent, const char* name);

named_stmt* iht_remove(iht* h, named_stmt* parent, const char* name);
named_stmt*
iht_remove_pph(iht* h, parent_hash phash, named_stmt* parent, const char* name);

named_stmt* iht_remove_node(iht* h, named_stmt* val);
named_stmt* iht_remove_node_pph(iht* h, parent_hash phash, named_stmt* val);

int iht_grow(iht* h);
