#pragma once
#include "ast.h"
#include "utils/sbuffer.h"
#include "utils/list.h"
#include "utils/freelist.h"
#include "utils/stack.h"
// this pass runs after type resolution (and preprocessing) is completed
// it is responsible for tracking lifetimes and inserting destructors

typedef enum prp_error_e {
    PRPE_OK,
    PRPE_FATAL,
    PRPE_USE_AFTER_FREE,
} prp_error;

typedef enum prp_var_state_e {
    VAR_STATE_UNKNOWN = 0,
    VAR_STATE_UNDEFINED = 1,
    VAR_STATE_DEFINED = 2,
    // if (foo) x.init(); --> maybe defined afterwards
    VAR_STATE_MAYBE_DEFINED = VAR_STATE_UNDEFINED | VAR_STATE_DEFINED
} prp_var_state;

typedef struct prp_var_node_s prp_var_node;
typedef struct prp_block_node_s prp_block_node;
typedef struct prp_var_data_s prp_var_data;

typedef struct prp_block_node_s {
    prp_block_node* parent;
    prp_block_node* prev;
    prp_block_node* children;
    ast_node* node; // NULL for meta blocks (if/else)
    ast_node** next;
    prp_var_data* owned_vars;
    prp_var_data* used_vars;
    prp_block_node* twin_branch;
    bool is_else;
    bool second_pass;
    bool inside_loop;
} prp_block_node;

typedef struct prp_var_data_s {
    prp_var_data* parent;
    prp_var_node* prev;
    prp_var_data* branch_twin; // points to the else version in if
    prp_var_state state_before_block;
    prp_var_state curr_state_in_block;
    ast_node* curr_state_inducing_expr;
    prp_var_state exit_state;
    ast_node* breaking_expr;
} prp_var_data;

typedef struct prp_var_node_s {
    sym_var* var;
    prp_var_data* curr_data;
    prp_block_node* curr_block;
    prp_var_data owner_var_data;
} prp_var_node;

typedef struct post_resolution_pass_s {
    pool mem;
    freelist var_node_mem;
    freelist var_data_mem;
    freelist block_node_mem;
    sc_func_base* curr_fn;
    prp_block_node* curr_block;
    stack nested_funcs;
    bool module_mode;
} post_resolution_pass;

int prp_init(post_resolution_pass* prp);
void prp_fin(post_resolution_pass* prp);
prp_error
prp_run_modules(post_resolution_pass* prp, mdg_node** start, mdg_node** end);
