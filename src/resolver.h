#pragma once
#include "mdg.h"
#include "utils/sbuffer.h"
typedef enum resolve_error {
    RE_FATAL = -1,
    RE_OK = 0,
    RE_TYPE_MISSMATCH,
    RE_SYMBOL_REDECLARATION,
    RE_UNKNOWN_SYMBOL,
} resolve_error;

typedef struct thread_context thread_context;
typedef struct resolver {
    thread_context* tc;
    mdg_node** start;
    mdg_node** end;
    sbuffer to_resolve;
} resolver;

int resolver_init(resolver* r, thread_context* tc);
void resolver_fin(resolver* r);
int resolver_resolve_single(resolver* r, mdg_node* node);
int resolver_resolve_multiple(resolver* r, mdg_node** start, mdg_node** end);

resolve_error resolve_delarations(
    resolver* r, symbol_table* sst, symbol_store* ss, src_file* f,
    stmt** stmt_list);
