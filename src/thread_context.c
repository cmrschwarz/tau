#include "thread_context.h"
#include "job_queue.h"
#include "resolver.h"
#include "tauc.h"
#include "print_ast.h"
static inline int thread_context_partial_fin(thread_context* tc, int r, int i)
{
    switch (i) {
        case -1:
        case 11: llvm_backend_delete(&tc->llvmb);
        case 10: list_builder_fin(&tc->listb2);
        case 9: list_builder_fin(&tc->listb);
        case 8: stack_fin(&tc->tempstack);
        case 7: scc_detector_fin(&tc->sccd);
        case 6: resolver_fin(&tc->r);
        case 5: parser_fin(&tc->p);
        case 4: error_log_fin(&tc->err_log);
        case 3: pool_fin(&tc->tempmem);
        case 2: sbuffer_fin(&tc->modules);
        case 1: pool_fin(&tc->permmem);
        case 0: break;
    }
    if (r) master_error_log_report("thread context initialization failed");
    return r;
}
void thread_context_fin(thread_context* tc)
{
    thread_context_partial_fin(tc, 0, -1);
}
int thread_context_init(thread_context* tc)
{
    int r = pool_init(&tc->permmem);
    if (r) return thread_context_partial_fin(tc, r, 0);
    r = sbuffer_init(&tc->modules, sizeof(llvm_module*) * 8);
    if (r) return thread_context_partial_fin(tc, r, 1);
    r = pool_init(&tc->tempmem);
    if (r) return thread_context_partial_fin(tc, r, 2);
    error_log_init(&tc->err_log, &tc->permmem);
    if (r) return thread_context_partial_fin(tc, r, 3);
    r = parser_init(&tc->p, tc);
    if (r) return thread_context_partial_fin(tc, r, 4);
    r = resolver_init(&tc->r, tc);
    if (r) return thread_context_partial_fin(tc, r, 5);
    r = scc_detector_init(&tc->sccd, &tc->permmem);
    if (r) return thread_context_partial_fin(tc, r, 6);
    r = stack_init(&tc->tempstack, &tc->permmem);
    if (r) return thread_context_partial_fin(tc, r, 7);
    r = list_builder_init(&tc->listb, &tc->tempmem, 64);
    if (r) return thread_context_partial_fin(tc, r, 8);
    r = list_builder_init(&tc->listb2, &tc->tempmem, 64);
    if (r) return thread_context_partial_fin(tc, r, 9);
    tc->llvmb = llvm_backend_new(tc);
    if (!tc->llvmb) return thread_context_partial_fin(tc, -1, 10);
    return OK;
}
int thread_context_do_job(thread_context* tc, job* j)
{
    if (j->kind == JOB_PARSE) {
        return parser_parse_file(&tc->p, &j->concrete.parse);
    }
    else if (j->kind == JOB_RESOLVE) {
        bool can_link = false;
        int r;
        mdg_node **start, **end;
        if (j->concrete.resolve.single_store) {
            start = &j->concrete.resolve.single_store;
            end = &j->concrete.resolve.single_store + 1;
        }
        else {
            start = j->concrete.resolve.start;
            end = j->concrete.resolve.end;
        }
        ureg startid, endid, private_sym_count;
        r = resolver_resolve(
            &tc->r, start, end, &startid, &endid, &private_sym_count);
        if (!r) {
            llvm_module* mod;
            r = llvm_backend_emit_module(
                tc->llvmb, start, end, startid, endid, private_sym_count, &mod);
            if (!r) {
                llvm_module** tgt =
                    sbuffer_append(&tc->modules, sizeof(llvm_module*));
                if (tgt) {
                    *tgt = mod;
                    ureg lh = atomic_ureg_dec(&TAUC.linking_holdups);
                    if (lh == 1) can_link = true;
                }
                else {
                    r = ERR;
                }
            }
        }
        if (!j->concrete.resolve.single_store) tfree(j->concrete.resolve.start);
        if (r) return r;
        if (can_link) return tauc_link();
        return r;
    }
    else if (j->kind == JOB_FINALIZE) {
        job_queue_stop(&TAUC.jobqueue);
        // DEBUG:
        print_mdg_node(TAUC.mdg.root_node, 0);
        puts("");
        int r = mdg_final_sanity_check(&TAUC.mdg, tc);
        if (!r) {
            ureg lh = atomic_ureg_dec(&TAUC.linking_holdups);
            if (lh == 1) return tauc_link();
        }
        return r;
    }
    else {
        error_log_report_critical_failiure(&tc->err_log, "unknown job type");
        return ERR;
    }
}
int thread_context_run(thread_context* tc)
{
    int r = OK;
    job j;
    while (true) {
        r = job_queue_pop(&TAUC.jobqueue, &j);
        if (r == JQ_DONE) return OK;
        if (r != OK) return r;
        r = thread_context_do_job(tc, &j);
        if (r) return r;
    }
}
