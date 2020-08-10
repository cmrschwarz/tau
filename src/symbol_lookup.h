#pragma once
#include "resolver.h"
typedef struct symbol_lookup_level {
    struct symbol_lookup_level* parent;
    ast_body* lookup_body;
    ast_body** usings_head;
    ast_body** usings_end;
    ast_body* extends_body;
    open_symbol* overloaded_sym_head;
    access_modifier am_start;
    access_modifier am_end;
} symbol_lookup_level;

typedef struct resolver_s resolver;

typedef struct symbol_lookup_iterator {
    symbol_lookup_level sll_prealloc;
    resolver* r;
    symbol_lookup_level* head;
    ast_body* next_lookup_body;
    ast_body* looking_body;
    sc_struct* struct_inst_lookup;
    sc_struct* looking_struct;
    ast_body* looking_mf_body;
    ast_body* looking_mod_body;
    ureg hash;
    const char* tgt_name;
    symbol* first_hidden_match;
    // when scope contains match, don't check indirections (-> variable
    // shadowing)
    bool enable_shadowing;
    // when the symbol is an alias return the symbol the alias points to
    bool deref_aliases;
} symbol_lookup_iterator;

resolve_error symbol_lookup_iterator_init(
    symbol_lookup_iterator* sli, resolver* r, ast_body* lookup_body,
    sc_struct* struct_inst_lookup, ast_body* looking_body, const char* tgt_name,
    bool enable_shadowing, bool deref_aliases);

resolve_error
symbol_lookup_iterator_next(symbol_lookup_iterator* sli, symbol** res);
