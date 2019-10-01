#ifndef TAUC_SYMBOL_TABLE_H
#define TAUC_SYMBOL_TABLE_H

#include "ast_flags.h"
#include "utils/string.h"
typedef struct symbol_s symbol;
typedef struct stmt_s stmt;
typedef struct symbol_table_s symbol_table;
typedef struct ast_elem_s ast_elem;
typedef struct ast_node_s ast_node;
typedef struct src_file_s src_file;

typedef struct symbol_table_s {
    ureg decl_count;
    symbol_table** usings_start;
    symbol_table* parent; // this points to the postprocessing symtab for ppsts
    ast_elem* owning_node;
    symbol_table* pp_symtab;
} symbol_table;

typedef struct symbol_table_with_usings {
    symbol_table** using_ends[AM_ENUM_ELEMENT_COUNT];
    symbol_table table;
} symbol_table_with_usings;

int symbol_table_init(
    symbol_table** tgt, ureg decl_count, ureg using_count, bool force_unique,
    ast_elem* owning_node);
void symbol_table_fin(symbol_table* st);

void symbol_table_insert_using(
    symbol_table* st, access_modifier am, ast_node* use, symbol_table* ust);

// if a symbol of that name already exists returns pointer to that entry
// otherwise inserts and returns NULL
symbol** symbol_table_insert(symbol_table* st, symbol* s);
// returns the positon to insert. if a symbol of that name is already present,
// the current symbol pointer at that position is not NULL
// when inserting by assigning to the returned pointer, remember to set
// the inserted symbol's next pointer to NULL!
symbol** symbol_table_find_insert_position(symbol_table* st, char* name);

// returns the symbol found or NULL if nonexistant
symbol** symbol_table_lookup(
    symbol_table* st, ureg ppl, access_modifier am, const char* s,
    ureg* decl_ppl);
symbol** symbol_table_lookup_limited(
    symbol_table* st, ureg ppl, access_modifier am, symbol_table* stop_at,
    const char* s, ureg* decl_ppl);
// might return NULL, for example for mdg_node symbol tables
src_file* symbol_table_get_file(symbol_table* st);

int init_root_symtab(symbol_table** root_st);
void fin_root_symtab(symbol_table* root_st);

typedef struct symtab_it {
    symbol** pos;
    symbol* subpos;
    symbol** end;
} symtab_it;

void symtab_it_begin(symtab_it* stit, symbol_table* st);
symtab_it symtab_it_make(symbol_table* st);
symbol* symtab_it_next(symtab_it* stit);

#endif
