#pragma once
#include "stmt_flags.h"
#include "utils/string.h"
typedef struct symbol symbol;

typedef struct symbol_table {
    ureg decl_count;
    struct symbol_map** usings;
} symbol_table;

typedef struct symbol_table_with_usings {
    symbol_table** using_ends[AM_ENUM_ELEMENT_COUNT];
    symbol_table table;
} symbol_table_with_usings;

typedef union symbol_store {
    symbol_table* table;
    ureg decl_count;
} symbol_store;

void symbol_store_init(symbol_store* ss);

void symbol_store_inc_decl_count(symbol_store* ss, ureg dc);
ureg symbol_store_get_decl_count(symbol_store ss);
void symbol_store_merge_decls(symbol_store* tgt, symbol_store src);
void symbol_store_require_unnamed_usings(symbol_store* ss);

bool symbol_store_requires_unnamed_usings(symbol_store* ss);
int symbol_store_setup_table(symbol_store* ss);
void symbol_store_destruct_table(symbol_store* ss);
// if a symbol of that name already exists returns that
// otherwise inserts and returns NULL
symbol* symbol_store_insert(symbol_store ss, symbol* s);
symbol* symbol_store_lookup(symbol_store ss, const char* s);
