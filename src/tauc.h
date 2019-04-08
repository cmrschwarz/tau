#pragma once
#include "error_log.h"
#include "file_map.h"
#include "job_queue.h"
#include "mdg.h"
#include "parser.h"
#include "utils/allocator.h"
#include "utils/pool.h"
#include "utils/threading.h"

typedef struct thread_context {
    error_log error_log;
    pool permmem;
    pool tempmem;
    parser parser;
} thread_context;

typedef enum worker_thread_stage {
    WTS_RUNNING,
    WTS_TERMINATED,
    WTS_FAILED,
} worker_thread_stage;

typedef struct worker_thread {
    thread_context tc;
    atomic_ureg stage;
    thread thread;
} worker_thread;

typedef struct tauc {
    thread_context main_thread_context;
    aseglist worker_threads;
    mdg mdg;
    file_map file_map;
    job_queue job_queue;
} tauc;

extern struct tauc TAUC;

// MAIN THREAD ONLY
int tauc_init();
int tauc_run(int argc, char** argv);
int tauc_add_worker_thread();
void tauc_fin();

int thread_context_init(thread_context* tc);
void thread_context_fin(thread_context* tc);
