#pragma once
#include "error_log.h"
#include "job_queue.h"
#include "scc_detector.h"
#include "mdg.h"
#include "parser.h"
#include "resolver.h"
#include "utils/pool.h"
#include "utils/stack.h"
#include "utils/sbuffer.h"
typedef struct tauc_s tauc;
typedef struct thread_context_s {
    tauc* t;
    error_log* err_log;
    pool permmem;
    pool tempmem;
    list_builder listb;
    list_builder listb2;
    parser p;
    resolver r;
    scc_detector sccd;
    sbuffer temp_buffer;
    stack temp_stack;
    sbuffer modules;
    bool has_preordered;
} thread_context;

typedef struct worker_thread {
    thread_context tc;
    // we add the thread to the list first and launch it afterwards, because
    // we it's a pain to selectively free all the resources of this tc right
    // away in case the launch fails
    bool spawned;
    bool initialized;
    thread thr;
} worker_thread;

int thread_context_init(thread_context* tc, tauc* t);
void thread_context_fin(thread_context* tc);
void thread_context_run(thread_context* tc);
int thread_context_do_job(thread_context* tc, job* j);
int thread_context_preorder_job(thread_context* tc);
