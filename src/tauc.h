#pragma once
#include "file_map.h"
#include "job_queue.h"
#include "thread_context.h"
#include "utils/threading.h"

typedef enum worker_thread_status {
    WTS_RUNNING,
    WTS_TERMINATED,
    WTS_FAILED,
} worker_thread_status;

typedef struct worker_thread {
    thread_context tc;
    atomic_ureg status;
    thread thread;
} worker_thread;

typedef struct tauc {
    thread_context main_thread_context;
    aseglist worker_threads;
    atomic_ureg thread_count;
    mdg mdg;
    file_map file_map;
    job_queue job_queue;
} tauc;

extern struct tauc TAUC;

// MAIN THREAD ONLY
int tauc_init();
int tauc_run(int argc, char** argv);
int tauc_request_parse(src_file* f);
int tauc_request_resolve_single(mdg_node* node);
int tauc_request_resolve_multiple(mdg_node** begin, mdg_node** end);
void tauc_fin();
