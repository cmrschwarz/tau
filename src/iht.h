//IHT: indentifier hash table
#pragma once
#include "types.h"
#include "allocator.h"
#include "ast.h"

typedef struct iht{
    named_ast_node** table_start;
    named_ast_node** table_end;
    ureg elem_count;
    ureg grow_on_elem_count;
    ureg hash_mask;
    ureg size_bits;
    thread_allocator* tal;
}iht;

typedef ureg parent_hash;

ureg iht_hashpos(iht* h, parent_hash phash, const char* key);

parent_hash iht_get_parent_hash(named_ast_node* parent);

int iht_init(iht* h, thread_allocator* tal);
void iht_fin(iht* h);

int iht_insert(iht* h, named_ast_node* val);
int iht_insert_pph(iht* h, parent_hash phash, named_ast_node* val);

named_ast_node* iht_get(iht* h, named_ast_node* parent, const char* name);
named_ast_node* iht_get_pph(iht* h, parent_hash phash, named_ast_node* parent, const char* name);

named_ast_node* iht_remove(iht* h, named_ast_node* parent, const char* name);
named_ast_node* iht_remove_pph(iht* h, parent_hash phash, named_ast_node* parent, const char* name);

named_ast_node* iht_remove_node(iht* h, named_ast_node* val);
named_ast_node* iht_remove_node_pph(iht* h, parent_hash phash, named_ast_node* val);

int iht_grow(iht* h);
