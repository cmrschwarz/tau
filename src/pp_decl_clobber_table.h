#pragma once
#include "utils/types.h"
#include "utils/freelist.h"
#include "ast.h"
typedef struct ast_body_s ast_body;
typedef struct resolver_s resolver;

typedef struct ppdct_waiting_users_s {
    list waiting_users;
    ureg refcount;
} ppdct_waiting_users;

typedef struct pp_decl_clobber_s {
    ast_body* body;
    ast_elem* associated_type; // NULL for free functions
    const char* name; // if this is NULL this entry lists all entries
    symbol* conflicting_symbol; // iff NULL waiting_users is set, otherwise
                                // parent_use
    union {
        ppdct_waiting_users* waiting_users;
        // first use of the conflicting_symbol symbol in the pp is
        // recorded for error reporting on attempted decls
        struct {
            ast_node* user;
            ast_body* user_body;
        } conflicting_use;
    };
    struct pp_decl_clobber_s* prev;
} pp_decl_clobber;

typedef struct pp_decl_clobber_table_s {
    pp_decl_clobber* table;
    ureg hash_bits;
    ureg hash_mask;
    ureg max_fill;
    ureg clobber_count;
    resolver* r;
    freelist waiting_users_mem;
} pp_decl_clobber_table;

int ppdct_init(pp_decl_clobber_table* t, resolver* r);
void ppdct_fin(pp_decl_clobber_table* t);

ureg ppdct_prehash_name(const char* name);

int ppdct_add_symbol(
    pp_decl_clobber_table* t, symbol* s, ast_body* target_body,
    ast_elem* associtated_type);

int ppdct_use_symbol(
    pp_decl_clobber_table* t, symbol* s, ast_body* user_body,
    ast_elem* associtated_type, ast_node* user);

int ppdct_require_symbol(
    pp_decl_clobber_table* t, ast_body* body, ast_elem* associated_type,
    const char* name, pp_resolve_node* dep, bool* notifier_added);