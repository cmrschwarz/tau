#pragma once
#include "ast_node_flags.h"
#include "utils/string.h"
typedef struct symbol symbol;
typedef struct stmt stmt;
typedef struct symbol_table symbol_table;
typedef struct ast_node ast_node;
typedef struct symbol_table {
    ureg decl_count;
    stmt* usings;
    symbol_table* pp_symtab;
    symbol_table* parent;
    ast_node* owning_node;
} symbol_table;

extern symbol_table EMPTY_ST;

typedef struct symbol_table_with_usings {
    symbol_table** using_ends[AM_ENUM_ELEMENT_COUNT];
    symbol_table table;
} symbol_table_with_usings;

int symbol_table_init(
    symbol_table** st, ureg decl_count, ureg using_count, bool force_unique,
    ast_node* owning_node);
void symbol_table_fin(symbol_table* st);

// if a symbol of that name already exists returns that
// otherwise inserts and returns NULL
symbol* symbol_table_insert(symbol_table* st, symbol* s);
symbol* symbol_table_lookup(symbol_table* st, const char* s);
typedef struct src_file src_file;

// might return NULL, for example for mdg_node symbol tables
src_file* symbol_table_get_file(symbol_table* st);
