#ifndef TAUC_TAUC_H
#define TAUC_TAUC_H
#include "file_map.h"
#include "job_queue.h"
#include "thread_context.h"
#include "utils/threading.h"

typedef struct tauc {
    // these two are still needed for error reporting after
    // the compiler has run
    master_error_log mel;
    file_map filemap;

    thread_context main_thread_context;
    aseglist worker_threads;
    atomic_ureg thread_count;
    // number of task that need to be completed before linking can start. the
    // task that changes this to 0 does the linking.
    atomic_ureg linking_holdups;
    module_dependency_graph mdg;
    job_queue jobqueue;
    atomic_sreg error_code;
    atomic_ureg node_ids; // stores the max used id
} tauc;

extern struct tauc TAUC;

// THREADSAFE
int tauc_request_parse(
    src_file* f, src_file* requiring_file, src_range requiring_stmt);
int tauc_request_resolve_single(mdg_node* node);
int tauc_request_resolve_multiple(mdg_node** start, mdg_node** end);
int tauc_request_finalize();
bool tauc_success_so_far();
void tauc_error_occured(int ec);
int tauc_link();

// MAIN THREAD ONLY
int tauc_run(int argc, char** argv); // errors are returned by fin instead
#endif
