#pragma once

#include "mdg.h"
#include "ast.h"
#include "utils/sbuffer.h"
#include "utils/stack.h"
#include "utils/freelist.h"
#include "utils/ptrlist.h"
#include "llvm_backend_api.h"
#include "post_resolution_pass.h"
#include "ptr_map.h"
#include "pp_decl_clobber_table.h"
typedef enum resolve_error_e {
    RE_FATAL = -1,
    RE_OK = 0,
    RE_ERROR,
    RE_TYPE_MISSMATCH,
    RE_UNKNOWN_SYMBOL,
    RE_TYPE_LOOP,
    RE_OVERLOADED,
    // returned by the first # on the way up to let the parent know he can't
    // continue until the paste was evaluated
    RE_UNREALIZED_COMPTIME, // could be paste or a value needed for a type
    RE_SUSPENDED,
} resolve_error;

typedef struct thread_context_s thread_context;

typedef enum PACK_ENUM pprn_state_e {
    PPRN_INFANT,
    PPRN_WAITING, // resolve_dep_count > 0 || run_dep_count > 0
    PPRN_WAITING_UNCOMMITTED,
    PPRN_PENDING, // resolve_dep_count == 0, resolved=false
    PPRN_READY, // run_dep_count == 0
} pprn_state;

typedef struct pp_resolve_node_s {
    ast_node* node;
    ast_body* declaring_body; // needed for continuing top level expressions
    list notify; // list of pprn*'s to notify once this is ready/done
    // stores pointers to the pprn pointer inside the notifying node
    // this way if the notifying node's pprn gets fin'd we notice that
    list notified_by;

    ureg resolve_dep_count;
    ureg run_dep_count;

    // offset into a body's elements array where we left of
    ast_node** continue_body;

    bool activated;

    // for detecting unreachable statements in continued blocks
    bool body_pos_reachable;

    // used by expr_pp to decide whether it's truly resolved
    bool pending_pastes;

    // put in pending instead of ready when resolve_dep_count == 0
    bool resolved;

    // dependants of this can resolve without it, but need it for running
    // it increases their run_dep_count instead of resolve_dep_count
    bool run_dep;

    bool dummy;

    bool added_res_deps;
    bool added_run_deps;
    bool child;
    pprn_state state;
    struct pp_resolve_node_s** pprn_list_entry;
    struct pp_resolve_node_s* first_child;
    struct pp_resolve_node_s* last_child;
    struct pp_resolve_node_s* next;
} pp_resolve_node;

typedef struct partial_resolution_data_s {
    pool pprn_mem;
    mdg_node* mdgs_single_store;
    ureg id_space;
    ptrlist pprns_pending;
    ptrlist pprns_waiting;
    ptrlist waiting_module_imports;
    bool deps_required_for_pp;
    bool error_occured;
    ureg committed_waiters;
    ureg public_sym_count;
    ureg private_sym_count;
#if DEBUG
    ureg pprn_count;
#endif
} partial_resolution_data;

typedef struct resolver_s {
    // general stuff
    thread_context* tc;
    ptr_map pm;
    pp_decl_clobber_table ppdct;
    post_resolution_pass prp;
    llvm_backend* backend;
    bool deps_required_for_pp;
    // current context
    mdg_node** mdgs_begin;
    mdg_node** mdgs_end;
    // temporary memory space (used in overload resulution)
    sbuffer temp_buffer;
    // dealing with type loops and type inference in expr blocks
    stack error_stack;
    ast_node* type_loop_start;
    bool allow_type_loops;
    bool retracing_type_loop;
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

    // list of import_module_data's for modules not yet (known to be)
    // generated
    ptrlist waiting_module_imports;

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
resolve_error resolver_run_pp_resolve_nodes(resolver* r, bool* made_progress);
resolve_error resolve_ast_node(
    resolver* r, ast_node* n, ast_body* body, ast_elem** value,
    ast_elem** ctype);
resolve_error resolve_import_symbol(
    resolver* r, sym_import_symbol* is, ast_body* requesting_body);
resolve_error add_body_decls(
    resolver* r, ast_body* body, ast_body* shared_body, bool public_st);
ureg ast_node_claim_id(resolver* r, ast_node* n, bool public_st);
resolve_error
get_curr_pprn(resolver* r, ast_body* body, pp_resolve_node** curr_pprn);
void report_redeclaration_error(resolver* r, symbol* redecl, symbol* prev);
ureg claim_symbol_id(resolver* r, symbol* s, bool public_st);
bool ctypes_unifiable(ast_elem* a, ast_elem* b);
void adjust_node_ids(resolver* r, ureg* id_space, ast_node* n);
bool ast_body_pastes_done(resolver* r, ast_body* b);
bool ast_body_is_public(ast_body* b);
bool is_body_public_st(ast_body* b);
int ast_body_propagate_error(resolver* r, ast_body* body);
int curr_body_propagate_error(resolver* r, ast_body* body);
int curr_pprn_propagate_error(resolver* r, ast_body* body);

resolve_error
pp_resolve_node_dep_done(resolver* r, pp_resolve_node* pprn, bool* progress);

resolve_error add_resolve_error(resolve_error res, resolve_error add);
pp_resolve_node* pp_resolve_node_create(
    resolver* r, ast_node* n, ast_body* declaring_body, bool run_individually,
    bool run_dep, bool dummy);
void pprn_fin(resolver* r, pp_resolve_node* pprn, bool error_occured);
resolve_error curr_pprn_depend_on(
    resolver* r, ast_body* body, pp_resolve_node** dependency_p);
int curr_context_raise_error(resolver* r, ast_body* body, ast_node* node);
