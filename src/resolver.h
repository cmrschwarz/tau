#pragma once

#include "mdg.h"
#include "ast.h"
#include "utils/sbuffer.h"
#include "utils/stack.h"
#include "utils/freelist.h"
#include "utils/ptrlist.h"
#include "llvm_backend_api.h"
#include "post_resolution_pass.h"
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
    RE_UNREALIZED_COMPTIME, // could be paste or a value needed for a type
    RE_REQUIRES_BODY_TYPE,
    RE_SYMBOL_NOT_FOUND_YET,
    RE_SUSPENDED,
} resolve_error;

typedef struct thread_context_s thread_context;

typedef struct pp_resolve_node_s {
    ast_node* node; // either expr_pp, stmt_use or func or var
    ast_body* declaring_body; // for continuing top level expressions
    list notify;
    // stores pointers to the pprn pointer inside the notifying node
    // this way if the notifying node's pprn gets fin'd we notice that
    list notified_by;
    ast_node** continue_block;
    ureg dep_count;
    bool nested_pp_exprs;
    bool pending_pastes;
    bool ready;
    bool activated; // prevent early ready
    bool run_individually; // false for exprs in functions
    bool block_pos_reachable; // for continuing blocks
    bool sequential_block;
    bool notify_when_ready; // by default we notify when done
    bool dummy; // clean once ready messages sent, no running required
    // struct pp_resolve_node_s* parent; // gets informed once this is pending
    bool considered_committed; // incremented 'committed waiter' count
    struct pp_resolve_node_s** waiting_list_entry;
    struct pp_resolve_node_s* first_unresolved_child;
    struct pp_resolve_node_s* last_unresolved_child;
    struct pp_resolve_node_s* next;
} pp_resolve_node;

typedef struct partial_resolution_data_s {
    pool pprn_mem;
    mdg_node* mdgs_single_store;
    ureg id_space;
    ptrlist pprns_pending;
    ptrlist pprns_waiting;
    ptrlist import_module_data_nodes;
    bool deps_required_for_pp;
    bool error_occured;
#if DEBUG
    ureg pprn_count;
#endif
} partial_resolution_data;

typedef struct resolver_s {
    // general stuff
    thread_context* tc;
    post_resolution_pass prp;
    llvm_backend* backend;
    bool deps_required_for_pp;
    // current context
    mdg_node** mdgs_begin;
    mdg_node** mdgs_end;
    // temporary memory space (used in overload resulution)
    sbuffer temp_stack;
    // dealing with type loops and type inference in expr blocks
    stack error_stack;
    ast_node* type_loop_start;
    bool allow_type_loops;
    bool retracing_type_loop;
    bool generic_context;
    bool resumed; // whether we come from a partial resolution
    bool error_occured;
    // ids distributed during declaration adding starting from PRIV_SYM_OFFSET
    ureg id_space;
    ureg public_sym_count;
    ureg private_sym_count;
    ureg glob_id_start;

    // dealing with the preprocessor
    pool pprn_mem;
    freelist pp_resolve_nodes;

    // dep count > 0, what remains in the end are cyclic dependencies
    ptrlist pp_resolve_nodes_waiting;
    ureg committed_waiters; // waiters that actually want to run

    // dep_count == 0, but unresolved.
    // when run with the parent it's not added here
    ptrlist pp_resolve_nodes_pending;

    // resolved and ready to run. cleared after every run
    ptrlist pp_resolve_nodes_ready;

    // imports of modules not yet (known to be) generated
    ptrlist import_module_data_nodes;

    pp_resolve_node* curr_pp_node;
    sc_func* module_group_constructor;
    sc_func* module_group_destructor;
} resolver;

int resolver_init(resolver* r, thread_context* tc);
void resolver_fin(resolver* r);
resolve_error resolver_resolve_and_emit(
    resolver* r, mdg_node** start, mdg_node** end, partial_resolution_data* prd,
    llvm_module** module);
ast_elem* get_resolved_ast_node_ctype(ast_node* n);

resolve_error resolve_ast_node(
    resolver* r, ast_node* n, ast_body* body, ast_elem** value,
    ast_elem** ctype);
resolve_error resolve_import_symbol(
    resolver* r, sym_import_symbol* is, ast_body* requesting_body);
resolve_error add_body_decls(
    resolver* r, ast_body* parent_body, ast_body* shared_body, ast_body* body,
    bool public_st);
ureg ast_node_claim_id(resolver* r, ast_node* n, bool public_st);
resolve_error
report_redeclaration_error(resolver* r, symbol* redecl, symbol* prev);
ureg claim_symbol_id(resolver* r, symbol* s, bool public_st);
bool ctypes_unifiable(ast_elem* a, ast_elem* b);
