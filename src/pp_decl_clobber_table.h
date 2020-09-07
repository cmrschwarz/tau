#pragma once
#include "utils/types.h"
#include "utils/freelist.h"
#include "ast.h"
typedef struct symbol_table_s symbol_table;
typedef struct resolver_s resolver;

typedef struct pp_decl_users_s {
    list users;
} pp_decl_users;

typedef struct pp_decl_clobber_s {
    symbol_table* symtab;
    char* name;
    symbol* parent_sym; //
    union {
        pp_decl_users* waiting_users; // when there is no matching parent sym
        // first use of the parent symbol in the pp
        // recorded for error reporting on attempted decls
        ast_node* parent_use;
    };
} pp_decl_clobber;

typedef struct pp_decl_clobber_table_s {
    pp_decl_clobber* table;
    ureg hash_bits;
    ureg clobber_count;
    freelist pp_decl_users_mem;
} pp_decl_clobber_table;

ppdct_init(pp_decl_clobber_table* t);

ureg ppdct_prehash_name(char* name);

pp_decl_clobber* ppdct_lookup_raw(
    pp_decl_clobber_table* t, symbol_table* st, char* name, ureg name_prehash,
    ureg cap, ureg mask);

ppdct_append(
    pp_decl_clobber_table* t, pp_decl_clobber* loc, symbol_table* st,
    char* name, ast_node* conflicting);
