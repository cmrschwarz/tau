#include "tauc.h"
#include "error_log.h"
#include "print_ast.h"
#include "thread_context.h"
#include "utils/allocator.h"

struct tauc TAUC;

static inline int tauc_partial_fin(int r, int i)
{
    switch (i) {
        case 5: aseglist_fin(&TAUC.worker_threads);
        case 4: job_queue_fin(&TAUC.job_queue);
        case 3: file_map_fin(&TAUC.file_map);
        case 2: mdg_fin(&TAUC.mdg);
        case 1: thread_context_fin(&TAUC.main_thread_context);
        case 0: break;
    }
    if (r) master_error_log_report("memory allocation failed");
    return r;
}
int tauc_init()
{
    TAUC.worker_threads = NULL;
    int r = thread_context_init(&TAUC.main_thread_context);
    if (r) return tauc_partial_fin(r, 0);
    r = mdg_init(&TAUC.mdg);
    if (r) return tauc_partial_fin(r, 1);
    r = file_map_init(&TAUC.file_map);
    if (r) return tauc_partial_fin(r, 2);
    r = job_queue_init(&TAUC.job_queue);
    if (r) return tauc_partial_fin(r, 3);
    r = aseglist_init(&TAUC.worker_threads);
    if (r) return tauc_partial_fin(r, 4);
    return OK;
}
void tauc_fin()
{
    aseglist_iterator it;
    aseglist_iterator_begin(&it, &TAUC.worker_threads);
    worker_thread* wt;
    job jb;
    jb.type = JOB_FINALIZE;
    ureg w, j;
    job_queue_push(&TAUC.job_queue, &jb, &w, &j);
    // job_queue_stop(&TAUC.job_queue);
    while (true) {
        wt = aseglist_iterator_next(&it);
        if (!wt) break;
        worker_thread_status wts = atomic_ureg_load(&wt->status);
        if (wts != WTS_FAILED) {
            thread_join(&wt->thread);
            thread_context_fin(&wt->tc);
        }
        tfree(wt);
    }
    aseglist_iterator_fin(&it);
    tauc_partial_fin(0, 5);
}

int tauc_run(int argc, char** argv)
{
    if (argc < 2) return 0;
    for (int i = 2; i < argc; i++) {
        src_file* f = file_map_get_file_from_path(
            &TAUC.file_map, string_from_cstr(argv[i]));
        if (!f) return ERR;
        if (tauc_request_parse(f)) return ERR;
    }
    src_file* f =
        file_map_get_file_from_path(&TAUC.file_map, string_from_cstr(argv[1]));
    if (parser_parse_file(&TAUC.main_thread_context.parser, f)) return ERR;
    return thread_context_run(&TAUC.main_thread_context);
}

void worker_thread_fn(void* ctx)
{
    worker_thread* wt = (worker_thread*)ctx;
    thread_context_run(&wt->tc);
    atomic_ureg_store(&wt->status, WTS_TERMINATED);
}
int tauc_add_worker_thread()
{
    worker_thread* wt = tmalloc(sizeof(worker_thread));
    if (!wt) return ERR;
    int r = thread_context_init(&wt->tc);
    if (r) {
        tfree(wt);
        return r;
    }
    r = atomic_ureg_init(&wt->status, WTS_RUNNING);
    if (r) {
        thread_context_fin(&wt->tc);
        tfree(wt);
        return r;
    }
    r = aseglist_add(&TAUC.worker_threads, wt);
    if (r) {
        thread_context_fin(&wt->tc);
        tfree(wt);
        return r;
    }
    r = thread_launch(&wt->thread, worker_thread_fn, wt);
    if (r) {
        // all other initialization failiures are due to memory allocation,
        // which is deemed fatal for the CALLING thread
        // a thread spawn failiure isn't really though, so we make the error
        // appear in the new context, and make the old one continue like we
        // succeeded
        thread_context_fin(&wt->tc);
        error_log_report_critical_failiure(
            &wt->tc.error_log, "failed to spawn additional worker thread");
        atomic_ureg_store(&wt->status, WTS_FAILED);
        return OK; // this is intentional, see above
    }
    return OK;
}

int tauc_add_job(job* j)
{
    ureg waiters, jobs;
    int r = job_queue_push(&TAUC.job_queue, j, &waiters, &jobs);
    if (r) return r;
    // TODO: tweak spawn condition
    if (jobs > 2 * waiters) {
        ureg max_tc = plattform_get_virt_core_count();
        ureg tc = atomic_ureg_load(&TAUC.thread_count);
        if (tc < max_tc) {
            tc = atomic_ureg_inc(&TAUC.thread_count);
            if (tc < max_tc) {
                return tauc_add_worker_thread();
            }
        }
    }
    return OK;
}

int tauc_request_parse(src_file* f)
{
    job j;
    j.type = JOB_PARSE;
    j.concrete.parse.file = f;
    return tauc_add_job(&j);
}
int tauc_request_resolve_multiple(mdg_node** begin, mdg_node** end)
{
    job j;
    j.type = JOB_RESOLVE_MULTIPLE;
    j.concrete.resolve_multiple.begin = begin;
    j.concrete.resolve_multiple.end = end;
    return tauc_add_job(&j);
}
int tauc_request_resolve_single(mdg_node* node)
{
    job j;
    j.type = JOB_RESOLVE_SINGLE;
    j.concrete.resolve_single.node = node;
    return tauc_add_job(&j);
}
