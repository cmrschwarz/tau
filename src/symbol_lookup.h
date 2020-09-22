#pragma once
#include "resolver.h"

typedef enum last_match_location_e {
    MATCH_LOCATION_PUSHED,
    MATCH_LOCATION_OVERLOAD,
    MATCH_LOCATION_TRAIT_IMPLS,
    MATCH_LOCATION_USING,
    MATCH_LOCATION_POPPED,
} last_match_location;

typedef struct symbol_lookup_level {
    list_rit trait_impls;
    ast_body** usings_head;
    ast_body** usings_end;
    ast_body* extends_body;
    open_symbol* overloaded_sym_head;
    access_modifier am_start;
    access_modifier am_end;
    bool look_for_members;
} symbol_lookup_level;

typedef struct resolver_s resolver;

typedef struct symbol_lookup_iterator {
    symbol_lookup_level sll_prealloc;
    resolver* r;
    symbol_lookup_level* head;
    ast_body* next_lookup_body;
    ast_body* looking_body;
    ast_elem* lhs_ctype;
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
    // whether looking_body != lookup_body. in that case we don't want to check
    // lookup_bodys parents (e.g. Foo::x should not search for x in Foo's
    // parent)
    bool explore_parents;
    bool explore_type;
    bool exploring_members;
    last_match_location last_match_loc;
    ureg stack_height;
} symbol_lookup_iterator;

resolve_error symbol_lookup_iterator_init(
    symbol_lookup_iterator* sli, resolver* r, ast_body* lookup_body,
    ast_elem* lhs_ctype, ast_body* looking_body, const char* tgt_name,
    bool enable_shadowing, bool deref_aliases);

void symbol_lookup_iterator_cut_off_shadowed(symbol_lookup_iterator* sli);

// level: the 'distance of the found symbol to the lookup body'
// because of scoped overloading, symbols of a lower level shadow those
// of a higher one
// lookup body: 0, usings: 1, imports: 2, imports' imports 3...
// lookup body's parent: imports' imports level + 1, usings +2, imports +3 ...
resolve_error
symbol_lookup_iterator_next(symbol_lookup_iterator* sli, symbol** res);
