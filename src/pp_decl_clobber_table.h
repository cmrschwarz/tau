#pragma once
#include "utils/types.h"
#include "ast.h"
typedef struct symbol_table_s symbol_table;

typedef struct pp_decl_clobber_s {
    symbol_table* symtab;
    char* name;
    // first use of the symbol in the pp
    // recorded for error reporting on attempted decls
    ast_node* use;
} pp_decl_clobber;

typedef struct pp_decl_clobber_table_s {
    pp_decl_clobber* table;
    ureg hash_mask;
    ureg hash_bits;
    ureg clobber_count;
    ureg grow_on_clobber_count;
} pp_decl_clobber_table;

ppdct_init(pp_decl_clobber_table* t);

ureg ppdct_prehash_name(char* name);

pp_decl_clobber* ppdct_lookup(
    pp_decl_clobber_table* t, symbol_table* st, char* name, ureg name_prehash);

ppdct_append(
    pp_decl_clobber_table* t, pp_decl_clobber* loc, symbol_table* st,
    char* name, ast_node* conflicting);
