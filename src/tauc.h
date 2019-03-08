#pragma once
#include "error_log.h"
#include "mdg.h"
#include "parser.h"
#include "utils/allocator.h"
#include "utils/pool.h"
#include "utils/threading.h"

typedef struct stage_1_share {
    mutex share_lock;
    sbuffer files;
    sbi unparsed_file;
} stage_1_share;

typedef struct stage_1 {
    parser p;

} stage_1;

struct tauc;
typedef struct thread_context {
    struct tauc* tauc;
    thread_allocator tal;
    error_log error_log;
    pool permmem;
    pool stagemem;
    union {
        stage_1 s1;
    } stage;
} thread_context;

typedef struct worker_thread {
    struct worker_thread* next;
    thread_context tc;
    thread thread;
} worker_thread;

typedef struct tauc {
    thread_context main_thread_context;
    worker_thread* worker_threads;
    pool permmem;
    mdg mdg;
    union {
        stage_1_share s1;
    } stage_share;
} tauc;

extern struct tauc TAUC;

// MAIN THREAD ONLY
int tauc_init();
int tauc_run(int argc, char** argv);
void tauc_fin_temps();
void tauc_fin();

int thread_context_init(thread_context* tc);
void thread_context_fin(thread_context* tc);
void worker_thread_fin(worker_thread* wt);
