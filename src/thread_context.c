#include "thread_context.h"
#include "job_queue.h"
#include "resolver.h"
#include "tauc.h"
static inline int thread_context_partial_fin(thread_context* tc, int r, int i)
{
    switch (i) {
        case 6: scc_detector_fin(&tc->sccd);
        case 5: resolver_fin(&tc->resolver);
        case 4: parser_fin(&tc->parser);
        case 3: error_log_fin(&tc->error_log);
        case 2: pool_fin(&tc->tempmem);
        case 1: pool_fin(&tc->permmem);
        case 0: break;
    }
    if (r) master_error_log_report("thread context initialization failed");
    return r;
}
void thread_context_fin(thread_context* tc)
{
    thread_context_partial_fin(tc, 0, 6);
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
    r = resolver_init(&tc->resolver, tc);
    if (r) return thread_context_partial_fin(tc, r, 4);
    r = scc_detector_init(&tc->sccd, &tc->permmem);
    if (r) return thread_context_partial_fin(tc, r, 5);
    return OK;
}
int thread_context_do_job(thread_context* tc, job* j)
{
    if (j->type == JOB_PARSE) {
        return parser_parse_file(&tc->parser, &j->concrete.parse);
    }
    else if (j->type == JOB_RESOLVE_MULTIPLE) {
        int r = resolver_resolve_multiple(
            &tc->resolver, j->concrete.resolve_multiple.start,
            j->concrete.resolve_multiple.end);
        tfree(j->concrete.resolve_multiple.start);
        return r;
    }
    else if (j->type == JOB_RESOLVE_SINGLE) {
        int r = resolver_resolve_single(
            &tc->resolver, j->concrete.resolve_single.node);
        if (r) return r;
        if (j->concrete.resolve_single.node == TAUC.mdg.root_node) {
            tauc_request_end();
        }
        return OK;
    }
    else if (j->type == JOB_FINALIZE) {
        job_queue_stop(&TAUC.job_queue);
        return OK;
    }
    else {
        error_log_report_critical_failiure(&tc->error_log, "unknown job type");
        return ERR;
    }
}
int thread_context_run(thread_context* tc)
{
    int r = OK;
    job j;
    while (true) {
        r = job_queue_pop(&TAUC.job_queue, &j);
        if (r == JQ_DONE) return OK;
        if (r != OK) return r;
        r = thread_context_do_job(tc, &j);
        if (r) return r;
    }
}
