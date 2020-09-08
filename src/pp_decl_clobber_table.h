#pragma once
#include "utils/types.h"
#include "utils/freelist.h"
#include "ast.h"
typedef struct ast_body_s ast_body;
typedef struct resolver_s resolver;

typedef struct pp_decl_clobber_s {
    ast_body* body;
    char* name;
    symbol* parent_sym; // iff NULL waiting_users is set, otherwise parent_use
    union {
        list waiting_users;

        // first use of the parent symbol in the pp is
        // recorded for error reporting on attempted decls
        struct {
            ast_node* user;
            ast_body* user_body;
        } parent_use;
    };
} pp_decl_clobber;

typedef struct pp_decl_clobber_table_s {
    pp_decl_clobber* table;
    ureg hash_bits;
    ureg hash_mask;
    ureg max_fill;
    ureg clobber_count;
    resolver* r;
} pp_decl_clobber_table;

int ppdct_init(pp_decl_clobber_table* t, resolver* r);
void ppdct_fin(pp_decl_clobber_table* t);

ureg ppdct_prehash_name(char* name);

int ppdct_add_symbol(
    pp_decl_clobber_table* t, symbol* s, ast_body* target_body);

int ppdct_use_symbol(
    pp_decl_clobber_table* t, symbol* s, ast_node* user, ast_body* user_body);