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
    VAR_STATE_VALID = 1,
    VAR_STATE_INVALID = 2,
    // if (foo) x.init(); --> maybe defined afterwards
    VAR_STATE_MAYBE_VALID = VAR_STATE_VALID | VAR_STATE_INVALID
} prp_var_state;

typedef struct prp_var_node_s prp_var_node;
typedef struct prp_block_node_s prp_block_node;
typedef struct prp_var_data_s prp_var_data;
typedef struct thread_context_s thread_context;

typedef struct prp_block_node_s {
    prp_block_node* parent;
    prp_block_node* non_paste_context;
    ast_node* node;
    // for if/else we use the parent block
    // that way we have a symtab for error reporting
    ast_body* body;
    ast_node** next_expr; // body->elements iterator
    prp_var_data* owned_vars; // always NULL for meta blocks
    prp_var_data* used_vars;
    // so we can check wether a block is 'above' or 'beneath' another in the
    // scope tree
    ureg depth;
    prp_block_node* outermost_break_target;
    ast_node* barrier_node; // the last reachable node in the block
    bool is_else;
    bool if_end_unreachable;
    bool check_rerun;
    bool force_rerun;
    bool is_rerun; // used to avoid double warnings :/
} prp_block_node;

typedef struct prp_var_data_s {
    prp_var_data* parent;
    prp_var_node* var_node;
    prp_block_node* block;
    prp_var_data* prev;
    prp_var_state applied_entry_states;
    prp_var_state possible_entry_states; // TODO: use this for continue
    prp_var_state curr_state;
    prp_var_state exit_states;
} prp_var_data;

typedef struct prp_var_node_s {
    sym_var* var;
    prp_var_data* curr_data;
    prp_var_node* next_in_break_chain;
    prp_var_data* top_data_on_break_path;
    prp_var_data owner_var_data;
} prp_var_node;

typedef struct post_resolution_pass_s {
    sbuffer mem;
    sc_func_base* curr_fn;
    prp_block_node* curr_block;
    stack nested_funcs;
    bool module_mode;
    thread_context* tc;
} post_resolution_pass;

int prp_init(post_resolution_pass* prp, thread_context* tc);
void prp_fin(post_resolution_pass* prp);
prp_error
prp_run_modules(post_resolution_pass* prp, mdg_node** start, mdg_node** end);
