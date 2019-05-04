#pragma once

#include "error_log.h"
#include "mdg.h"
#include "parser.h"
#include "resolver.h"
#include "utils/pool.h"

typedef struct thread_context {
    error_log error_log;
    pool permmem;
    pool tempmem;
    parser parser;
    resolver resolver;
    scc_detector sccd;
} thread_context;

int thread_context_init(thread_context* tc);
void thread_context_fin(thread_context* tc);
int thread_context_run(thread_context* tc);
