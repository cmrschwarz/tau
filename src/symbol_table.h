#ifndef TAUC_SYMBOL_TABLE_H
#define TAUC_SYMBOL_TABLE_H

#include "ast_flags.h"
#include "utils/string.h"
typedef struct symbol_s symbol;
typedef struct stmt_s stmt;
typedef struct symbol_table_s symbol_table;
typedef struct ast_elem_s ast_elem;
typedef struct ast_node_s ast_node;
typedef struct src_map_s src_map;

typedef struct usings_table_s {
    ureg usings_count;
    symbol_table** using_ends[AM_ENUM_ELEMENT_COUNT];
} usings_table;

typedef struct symbol_table_s {
    // we cannot really use joined allocation since that would invalidate st
    // pointers on grow (grow is unavoidable due to the pp / macros)
    symbol** table;
    ureg hash_mask;
    // for ppsts, this points to the 'post'processing symtab
    symbol_table* parent;
    usings_table* usings;
    ast_elem* owning_node;
    // this stores the requested decl count, not the actual present amount
    // it is only updated during init and amend
    ureg decl_count;
} symbol_table;

int symbol_table_init(
    symbol_table** tgt, ureg decl_count, ureg using_count, bool force_unique,
    ast_elem* owning_node);

void symbol_table_fin(symbol_table* st);

void symbol_table_insert_use(
    symbol_table* st, access_modifier am, ast_node* use,
    symbol_table* using_node);

// if a symbol of that name already exists returns pointer to that entry
// otherwise inserts and returns NULL
symbol** symbol_table_insert(symbol_table* st, symbol* s);
// returns the positon to insert. if a symbol of that name is already present,
// the current symbol pointer at that position is not NULL
// when inserting by assigning to the returned pointer, remember to set
// the inserted symbol's next pointer to NULL!
symbol** symbol_table_find_insert_position(symbol_table* st, char* name);
void symbol_table_inc_decl_count(symbol_table* st);
// returns the symbol found or NULL if nonexistant

ureg symbol_table_prehash(const char* s);

symbol* symbol_table_lookup_raw(symbol_table* st, ureg hash, const char* name);
symbol_table**
symbol_table_get_uses_start(symbol_table* st, access_modifier am);
symbol_table** symbol_table_get_uses_end(symbol_table* st, access_modifier am);
ast_node** symbol_table_get_use_node(symbol_table* st, symbol_table** using_st);

// might return NULL, for example for mdg_node symbol tables
src_map* symbol_table_get_smap(symbol_table* st);

// used when the pp introduces additional symbols
int symbol_table_amend(symbol_table* st, ureg decl_count, ureg usings);

int init_root_symtab(symbol_table** root_st);
void fin_root_symtab(symbol_table* root_st);
symbol_table* symbol_table_skip_metatables(symbol_table* st);
symbol_table* symbol_table_get_module_table(symbol_table* st);

typedef struct symtab_it {
    symbol** pos;
    symbol* subpos;
    symbol** end;
} symtab_it;

void symtab_it_begin(symtab_it* stit, symbol_table* st);
symtab_it symtab_it_make(symbol_table* st);
symbol* symtab_it_next(symtab_it* stit);

#endif
