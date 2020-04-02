#pragma once
#include "resolver.h"
typedef struct symbol_lookup_level {
    struct symbol_lookup_level* parent;
    symbol_table* lookup_st;
    symbol_table** usings_head;
    symbol_table** usings_end;
    symbol_table* extends_sc;
    open_symbol* overloaded_sym_head;
    access_modifier am_start;
    access_modifier am_end;
} symbol_lookup_level;

typedef struct resolver_s resolver;

typedef struct symbol_lookup_iterator {
    symbol_lookup_level sll_prealloc;
    resolver* r;
    symbol_lookup_level* head;
    symbol_table* next_lookup_st;
    symbol_table* looking_st;
    sc_struct* struct_inst_lookup;
    sc_struct* looking_struct;
    symbol_table* looking_mf;
    symbol_table* looking_mod;
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
    symbol_lookup_iterator* sli, resolver* r, symbol_table* lookup_st,
    sc_struct* struct_inst_lookup, symbol_table* looking_st,
    const char* tgt_name, bool enable_shadowing, bool deref_aliases);

resolve_error
symbol_lookup_iterator_next(symbol_lookup_iterator* sli, symbol** res);
