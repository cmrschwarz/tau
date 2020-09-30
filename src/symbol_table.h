#pragma once

#include "ast_flags.h"
#include "utils/string.h"
#include "trait_table.h"
typedef struct symbol_s symbol;
typedef struct ast_body_s ast_body;
typedef struct stmt_s stmt;
typedef struct symbol_table_s symbol_table;
typedef struct ast_elem_s ast_elem;
typedef struct ast_node_s ast_node;
typedef struct src_map_s src_map;

/*********************
 *   SYMTAB HEADER   *
 *   USING HEADER    *  <- the using sections are optional,
 *    USING_BODYS    *     not present if using capacity = 0
 *    USING_NODES    *
 *      SYMBOLS      *  <- table offset is the offset of this section
 *********************/

typedef struct symbol_table_s {
    // table size including headers == 2**table_bitcount * sizeof(void*)
    uregh table_bitcount;
    uregh sym_bitcount;
    ureg sym_count;
    // how much to offset the start of the actual table
    // when there are no usings this is sizeof(symbol_table)
    // in units of sizeof(void*)
    ureg table_offset;
    trait_table* tt;
} symbol_table;

typedef struct usings_table_s {
    symbol_table symtab;
    ast_body** using_ends[AM_ENUM_ELEMENT_COUNT];
} usings_table;

symbol_table* symbol_table_create(
    ureg sym_count, ureg using_count, ureg impl_count, ureg generic_impl_count);

void symbol_table_destroy(symbol_table* st);

ureg symbol_table_has_usings(symbol_table* st);
ureg symbol_table_get_symbol_capacity(symbol_table* st);
ureg symbol_table_get_using_capacity(symbol_table* st);
ureg symbol_table_get_using_count(symbol_table* st);
ureg symbol_table_get_symbol_count(symbol_table* st);

// if a symbol of that name already exists returns pointer to that entry
// otherwise inserts and returns NULL
symbol* symbol_table_insert(symbol_table* st, symbol* s);

// returns the symbol found or NULL if nonexistant
// returns the positon to insert. if a symbol of that name is already present,
// the current symbol pointer at that position is not NULL
// when inserting by assigning to the returned pointer, remember to set
// the inserted symbol's next pointer to NULL!
symbol** symbol_table_lookup(symbol_table* st, char* name);
ureg symbol_table_prehash(const char* s);
symbol** symbol_table_lookup_raw(symbol_table* st, ureg hash, const char* name);
void symbol_table_inc_sym_count(symbol_table* st);

// used when the pp introduces additional symbols
int symbol_table_amend(symbol_table** stp, ureg sym_count, ureg using_count);

void symbol_table_insert_use(
    symbol_table* st, access_modifier am, ast_node* using_node,
    ast_body* using_body, bool no_syms, bool no_impls);

ast_body** symbol_table_get_uses_start(symbol_table* st, access_modifier am);
ast_body** symbol_table_get_uses_end(symbol_table* st, access_modifier am);
ast_node** symbol_table_get_use_node(symbol_table* st, ast_body** using_st);
void symbol_table_unwrap_use(
    ast_body* use_body_wrapped, ast_body** use_body, bool* no_syms,
    bool* no_impls);

typedef struct symtab_it {
    symbol** pos;
    symbol* subpos;
    symbol** last;
} symtab_it;

void symtab_it_init(symtab_it* stit, symbol_table* st);
symtab_it symtab_it_make(symbol_table* st);
symbol* symtab_it_next(symtab_it* stit);
