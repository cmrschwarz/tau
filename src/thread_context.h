#include "error_log.h"
#include "job_queue.h"
#include "mdg.h"
#include "parser.h"
#include "resolver.h"
#ifndef TAUC_THREAD_CONTEXT_H
#define TAUC_THREAD_CONTEXT_H
#include "utils/pool.h"
#include "utils/stack.h"
#include "llvm_backend_api.h"

typedef struct thread_context_s {
    error_log error_log;
    pool permmem;
    pool tempmem;
    list_builder listb;
    list_builder listb2;
    parser parser;
    resolver resolver;
    scc_detector sccd;
    stack stack;
    llvm_backend* llvm_backend;
} thread_context;

int thread_context_init(thread_context* tc);
void thread_context_fin(thread_context* tc);
int thread_context_run(thread_context* tc);
int thread_context_do_job(thread_context* tc, job* j);

#endif