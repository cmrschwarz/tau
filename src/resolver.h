#ifndef TAUC_RESOLVER_H
#define TAUC_RESOLVER_H

#include "mdg.h"
#include "ast.h"
#include "utils/sbuffer.h"
#include "utils/stack.h"
typedef enum resolve_error {
    RE_FATAL = -1,
    RE_OK = 0,
    RE_NOT_APPLICABLE,
    RE_TYPE_MISSMATCH,
    RE_SYMBOL_REDECLARATION,
    RE_UNKNOWN_SYMBOL,
    RE_TYPE_LOOP,
    RE_REQUIRES_BODY_TYPE,
    RE_OVERLOADED,
} resolve_error;

typedef struct thread_context_s thread_context;
typedef struct resolver {
    stack error_stack;
    dbuffer call_types;
    thread_context* tc;
    mdg_node** start;
    mdg_node** end;
    ureg public_sym_count;
    ureg private_sym_count;
} resolver;

int resolver_init(resolver* r, thread_context* tc);
void resolver_fin(resolver* r);
int resolver_resolve(
    resolver* r, mdg_node** start, mdg_node** end, ureg* startid, ureg* endid,
    ureg* prviate_sym_count);
resolve_error add_import_group_decls(
    thread_context* tc, mdg_node* curr_mdg_node, src_file* f,
    sym_import_group* ig, symbol_table* st);
#endif