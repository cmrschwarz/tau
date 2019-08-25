#ifndef TAUC_MDGHT_H
#define TAUC_MDGHT_H

#include "ast.h"
#include "utils/allocator.h"
#include "utils/string.h"
#include "utils/types.h"

#ifndef TAUC_MDG_H
typedef struct mdg_node_s mdg_node;
#endif

// mdght: mdg hash table
// TODO: use closed hashing to improve get performance on nonexisting entries
typedef struct mdght {
    mdg_node** table_start;
    mdg_node** table_end;
    ureg elem_count;
    ureg grow_on_elem_count;
    ureg hash_mask;
    ureg size_bits;
} mdght;

ureg mdght_get_hash_str(mdg_node* parent, string str);
ureg mdght_get_hash(mdg_node* parent, const char* str);

typedef struct mdght_iterator {
    mdg_node** head;
    mdg_node** end;
} mdght_iterator;
void mdght_iterator_begin(mdght_iterator* it, mdght* h);
mdg_node* mdght_iterator_next(mdght_iterator* it);

int mdght_init(mdght* h);
int mdght_init_with_capacity(mdght* h, ureg capacity);
void mdght_fin(mdght* h);
void mdght_fin_contained_nodes(mdght* h);

mdg_node** mdght_insert(mdght* h, mdg_node* n);
mdg_node** mdght_insert_ph(mdght* h, ureg hash, mdg_node* n);
mdg_node** mdght_insert_at(mdght* h, ureg pos, mdg_node* n);

mdg_node* mdght_get(mdght* h, mdg_node* parent, const char* name);
mdg_node* mdght_get_ph(mdght* h, ureg hash, mdg_node* parent, const char* name);

mdg_node* mdght_get_str(mdght* h, mdg_node* parent, string name);
mdg_node* mdght_get_str_ph(mdght* h, ureg hash, mdg_node* parent, string name);
mdg_node**
mdght_get_str_raw_ph(mdght* h, ureg hash, mdg_node* parent, string name);

mdg_node* mdght_remove(mdght* h, mdg_node* parent, const char* name);
mdg_node*
mdght_remove_ph(mdght* h, ureg hash, mdg_node* parent, const char* name);

mdg_node* mdght_remove_node(mdght* h, mdg_node* n);
mdg_node* mdght_remove_node_ph(mdght* h, ureg phash, mdg_node* n);

int mdght_grow(mdght* h);
#endif