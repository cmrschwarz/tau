#ifndef TAUC_RESOLVER_H
#define TAUC_RESOLVER_H

#include "mdg.h"
#include "ast.h"
#include "utils/sbuffer.h"
#include "utils/stack.h"
#include "utils/freelist.h"
#include "utils/ptrlist.h"
#include "llvm_backend_api.h"
typedef enum resolve_error_e {
    RE_FATAL = -1,
    RE_OK = 0,
    RE_ERROR,
    RE_NOT_APPLICABLE,
    RE_TYPE_MISSMATCH,
    RE_SYMBOL_REDECLARATION,
    RE_DIFFERENT_PP_LEVEL,
    RE_UNKNOWN_SYMBOL,
    RE_TYPE_LOOP,
    RE_OVERLOADED,

    RE_REQUIRES_BODY_TYPE,
    RE_SYMBOL_NOT_FOUND_YET,
} resolve_error;

typedef struct thread_context_s thread_context;

typedef struct pp_resolve_node_s {
    ast_node* node; // either expr_pp, stmt_using or func
    symbol_table* declaring_st;
    ureg ppl;
    // PERF: use a non threadsafe list here
    aseglist required_by;
    ast_node** continue_block;
    ureg dep_count;
    ureg pending_pastes;
    bool result_used;
    bool run_when_done; // false for exprs in functions
    ANONYMOUS_UNION_START
    struct pp_resolve_node_s* last_child;
    struct pp_resolve_node_s* parent;
    ANONYMOUS_UNION_END
    ANONYMOUS_UNION_START struct pp_resolve_node_s* first_unresolved_child;
    struct pp_resolve_node_s* next;
    ANONYMOUS_UNION_END
} pp_resolve_node;

typedef struct resolver_s {
    // general stuff
    thread_context* tc;
    llvm_backend* backend;
    // current context
    mdg_node** mdgs_begin;
    mdg_node** mdgs_end;
    mdg_node* curr_mdg;
    open_scope* curr_osc;
    // temporary memory space
    sbuffer call_types;
    // dealing with type loops and type inference in expr blocks
    stack error_stack;
    ast_node* curr_expr_block_owner;
    ast_node* type_loop_start;
    u8 allow_type_loops;
    bool retracing_type_loop;
    // assigning ids
    ureg id_space;
    ureg public_sym_count;
    ureg private_sym_count;
    // dealing with the preprocessor
    freelist pp_resolve_nodes;
    ptrlist pp_resolve_nodes_pending; // dep_count = 0, but unresolved
    ptrlist pp_resolve_nodes_ready; // resolved and ready to run
    pp_resolve_node* curr_pp_node;
    pp_resolve_node* block_pp_node;
    bool multi_evaluation_ctx;
    ureg pp_generation;
} resolver;

int resolver_init(resolver* r, thread_context* tc);
void resolver_fin(resolver* r);
int resolver_resolve_and_emit(
    resolver* r, mdg_node** start, mdg_node** end, llvm_module** module);
ast_elem* get_resolved_ast_node_ctype(ast_node* n);
#endif
