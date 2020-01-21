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
    RE_PP_DEPS_LOOP,
    RE_OVERLOADED,
    // returned by the first # on the way up to let the parent know he can't
    // continue until the paste was evaluated
    RE_UNREALIZED_PASTE,
    RE_REQUIRES_BODY_TYPE,
    RE_SYMBOL_NOT_FOUND_YET,
} resolve_error;

typedef struct thread_context_s thread_context;

typedef struct pp_resolve_node_s {
    ast_node* node; // either expr_pp, stmt_using or func
    symbol_table* declaring_st;
    expr_block_base* parent_ebb;
    ureg ppl;
    // PERF: use a non threadsafe list here
    aseglist notify_when_done;
    aseglist notify_when_ready;
    ast_node** continue_block;
    ureg dep_count;
    ureg pending_pastes;
    bool run_when_ready; // false for exprs in functions
    bool block_pos_reachable;
    bool call_when_done;
    struct pp_resolve_node_s* parent; // gets informed once this is pending
    struct pp_resolve_node_s** waiting_list_entry;
    struct pp_resolve_node_s* first_unresolved_child;
    struct pp_resolve_node_s* last_unresolved_child;
    struct pp_resolve_node_s* next;
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
    ast_node* curr_block_owner;
    // temporary memory space
    sbuffer call_types;
    // dealing with type loops and type inference in expr blocks
    stack error_stack;
    ast_node* type_loop_start;
    u8 allow_type_loops;
    bool retracing_type_loop;
    bool generic_context;
    // assigning ids
    ureg id_space;
    ureg public_sym_count;
    ureg private_sym_count;

    // dealing with the preprocessor
    freelist pp_resolve_nodes;

    // dep count > 0, what remains in the end are cyclic dependencies
    sbuffer pp_resolve_nodes_waiting;

    // dep_count == 0, but unresolved.
    // when run with the parent it's not added here
    ptrlist pp_resolve_nodes_pending;

    // resolved and ready to run. cleared after every run
    ptrlist pp_resolve_nodes_ready;

    pp_resolve_node* curr_pp_node;
    pp_resolve_node* curr_block_pp_node;
    sc_func* module_group_constructor;
    sc_func* module_group_destructor;
    sym_var* curr_var_decl;
    pp_resolve_node* curr_var_pp_node;
    // this is used to determine whether the curr block is inside the decl
    // or the other way round by storing the block the func decl is in
    ast_node* curr_var_decl_block_owner;
    bool multi_evaluation_ctx;
    ureg pp_generation;
} resolver;

int resolver_init(resolver* r, thread_context* tc);
void resolver_fin(resolver* r);
int resolver_resolve_and_emit(
    resolver* r, mdg_node** start, mdg_node** end, llvm_module** module);
ast_elem* get_resolved_ast_node_ctype(ast_node* n);

resolve_error resolve_ast_node(
    resolver* r, ast_node* n, symbol_table* st, ureg ppl, ast_elem** value,
    ast_elem** ctype);
resolve_error add_body_decls(
    resolver* r, symbol_table* parent_st, symbol_table* shared_st, ureg ppl,
    ast_body* b, bool public_st);
ureg ast_node_claim_id(resolver* r, ast_node* n, bool public_st);
resolve_error report_redeclaration_error(
    resolver* r, symbol* redecl, symbol* prev, symbol_table* st);
ureg claim_symbol_id(resolver* r, symbol* s, bool public_st);
bool ctypes_unifiable(ast_elem* a, ast_elem* b);
#endif
