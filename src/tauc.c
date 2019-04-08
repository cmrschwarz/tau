#include "tauc.h"
#include "error_log.h"
#include "print_ast.h"
#include "utils/allocator.h"

struct tauc TAUC;
static inline int thread_context_run(thread_context* tc);

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
    if (r)
        master_error_log_report(
            "fatal initialization error: memory allocation failed");
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
    while (true) {
        wt = aseglist_iterator_next(&it);
        if (!wt) break;
        worker_thread_stage wts = atomic_ureg_load(&wt->stage);
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
        if (job_queue_request_parse(&TAUC.job_queue, f)) return ERR;
    }
    src_file* f =
        file_map_get_file_from_path(&TAUC.file_map, string_from_cstr(argv[1]));
    if (parser_parse_file(&TAUC.main_thread_context.parser, f)) return ERR;
    return thread_context_run(&TAUC.main_thread_context);
}

void worker_thread_fn(void* ctx)
{
    worker_thread* wt = (worker_thread*)ctx;
    job_queue_inform_thread_added(&TAUC.job_queue);
    thread_context_run(&wt->tc);
    atomic_ureg_store(&wt->stage, WTS_TERMINATED);
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
    r = atomic_ureg_init(&wt->stage, WTS_RUNNING);
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
        atomic_ureg_store(&wt->stage, WTS_FAILED);
        return OK; // this is intentional, see above
    }
    return OK;
}

static inline int thread_context_partial_fin(thread_context* tc, int r, int i)
{
    switch (i) {
        case 4: parser_fin(&tc->parser);
        case 3: error_log_fin(&tc->error_log);
        case 2: pool_fin(&tc->tempmem);
        case 1: pool_fin(&tc->permmem);
        case 0: break;
    }
    if (r)
        master_error_log_report("thread setup error: memory allocation failed");
    return r;
}
void thread_context_fin(thread_context* tc)
{
    thread_context_partial_fin(tc, 0, 4);
}
int thread_context_init(thread_context* tc)
{
    int r = pool_init(&tc->permmem);
    if (r) return thread_context_partial_fin(tc, r, 0);
    r = pool_init(&tc->tempmem);
    if (r) return thread_context_partial_fin(tc, r, 1);
    error_log_init(&tc->error_log, &tc->permmem);
    if (r) return thread_context_partial_fin(tc, r, 2);
    r = parser_init(&tc->parser, tc);
    if (r) return thread_context_partial_fin(tc, r, 3);
    return OK;
}
static inline int thread_context_run(thread_context* tc)
{
    int r = OK;
    job_queue_result jqr;
    job j;
    while (true) {
        jqr = job_queue_pop(&TAUC.job_queue, &j);
        if (jqr == JQR_DONE) {
            break;
        }
        else if (jqr == JQR_SUCCESS_WITH_REINFORCEMENTS_REQUEST) {
            r = tauc_add_worker_thread();
            if (r) break;
        }
        else if (jqr == JQR_ERROR) {
            r = ERR;
            break;
        }
        if (j.type == JOB_PARSE) {
            r = parser_parse_file(&tc->parser, j.concrete.parse.file);
        }
        else if (j.type == JOB_RESOLVE) {
            r = ERR; // TODO: implement resolving
        }
        else {
            error_log_report_critical_failiure(
                &tc->error_log, "unknown job type");
            r = ERR;
            break;
        }
        if (r) break;
    }
    return r;
}
