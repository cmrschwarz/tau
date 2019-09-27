#ifndef TAUC_RESOLVER_H
#define TAUC_RESOLVER_H

#include "mdg.h"
#include "ast.h"
#include "utils/sbuffer.h"
#include "utils/stack.h"
typedef enum resolve_error {
    RE_FATAL = -1,
    RE_OK = 0,
    RE_ERROR,
    RE_NOT_APPLICABLE,
    RE_TYPE_MISSMATCH,
    RE_SYMBOL_REDECLARATION,
    RE_UNKNOWN_SYMBOL,
    RE_TYPE_LOOP,
    RE_REQUIRES_BODY_TYPE,
    RE_OVERLOADED,
} resolve_error;

typedef struct thread_context_s thread_context;

typedef struct resolve_node_s {
    ast_node* node; // either expr_pp or stmt_using
    symbol_table* declaring_st;
} resolve_node_s;

typedef struct resolve_stack_s {
    sbuffer resolve_nodes;
} resolve_stack;

typedef struct pp_resolve_node_s {
    ast_node* node; // either expr_pp or stmt_using
    symbol_table* declaring_st;
} pp_resolve_node;

typedef struct resolver_s {
    stack error_stack;
    sbuffer call_types;
    thread_context* tc;
    mdg_node** mdgs_begin;
    mdg_node** mdgs_end;
    ureg public_sym_count;
    ureg private_sym_count;
    // REWORK ATTEMPT
    sbuffer pp_resolve_nodes;
    sbuffer resolve_stacks;
    resolve_stack* curr_resolve_stack;
    // TODO: rework
    open_scope* curr_osc;
    mdg_node* curr_mdg;
    symbol* curr_symbol_decl;
    ast_node* curr_expr_block_owner;
    ast_node* type_loop_start;
    bool allow_type_loops;
    bool retracing_type_loop;

} resolver;

int resolver_init(resolver* r, thread_context* tc);
void resolver_fin(resolver* r);
int resolver_resolve(
    resolver* r, mdg_node** start, mdg_node** end, ureg* startid, ureg* endid,
    ureg* prviate_sym_count);
ast_elem* get_resolved_ast_node_ctype(ast_node* n);
#endif
