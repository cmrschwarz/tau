#ifndef TAUC_TAUC_H
#define TAUC_TAUC_H
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
    thread thr;
} worker_thread;

typedef struct tauc {
    thread_context main_thread_context;
    aseglist worker_threads;
    atomic_ureg thread_count;
    // number of task that need to be completed before linking can start. the
    // task that changes this to 0 does the linking.
    atomic_ureg linking_holdups;
    module_dependency_graph mdg;
    file_map filemap;
    job_queue jobqueue;
    atomic_ureg node_ids; // stores the max used id
} tauc;

extern struct tauc TAUC;

// MAIN THREAD ONLY
int tauc_init();
int tauc_run(int argc, char** argv);
int tauc_request_parse(
    src_file* f, src_file* requiring_file, src_range requiring_stmt);
int tauc_request_resolve_single(mdg_node* node);
int tauc_request_resolve_multiple(mdg_node** start, mdg_node** end);
int tauc_request_end();
int tauc_link();
void tauc_fin();
#endif