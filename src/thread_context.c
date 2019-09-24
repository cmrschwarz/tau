#include "thread_context.h"
#include "job_queue.h"
#include "resolver.h"
#include "tauc.h"
#include "print_ast.h"
#include "utils/debug_utils.h"
#include <assert.h>
static inline int thread_context_partial_fin(thread_context* tc, int r, int i)
{
    switch (i) {
        case -1:
        case 10: llvm_backend_delete(tc->llvmb);
        case 9: list_builder_fin(&tc->listb2);
        case 8: list_builder_fin(&tc->listb);
        case 7: stack_fin(&tc->tempstack);
        case 6: scc_detector_fin(&tc->sccd);
        case 5: resolver_fin(&tc->r);
        case 4: parser_fin(&tc->p);
        case 3: pool_fin(&tc->tempmem);
        case 2: sbuffer_fin(&tc->modules);
        case 1:
            pool_fin(&tc->permmem);
            // these don't need to be initialized and they are thread local,
            // so this seems like the best place to free them
            debug_utils_free_res();
        case 0: break;
    }
    if (r)
        master_error_log_report(
            &tc->t->mel, "thread context initialization failed");
    return r;
}
void thread_context_fin(thread_context* tc)
{
    thread_context_partial_fin(tc, 0, -1);
}
int thread_context_init(thread_context* tc, tauc* t)
{
    tc->t = t;
    int r = pool_init(&tc->permmem);
    if (r) return thread_context_partial_fin(tc, r, 0);
    r = sbuffer_init(&tc->modules, sizeof(llvm_module*) * 8);
    if (r) return thread_context_partial_fin(tc, r, 1);
    r = pool_init(&tc->tempmem);
    if (r) return thread_context_partial_fin(tc, r, 2);
    tc->err_log = error_log_create(&t->mel);
    if (!tc->err_log) return thread_context_partial_fin(tc, r, 2);
    r = parser_init(&tc->p, tc);
    if (r) return thread_context_partial_fin(tc, r, 3);
    r = resolver_init(&tc->r, tc);
    if (r) return thread_context_partial_fin(tc, r, 4);
    r = scc_detector_init(&tc->sccd, &tc->permmem);
    if (r) return thread_context_partial_fin(tc, r, 5);
    r = stack_init(&tc->tempstack, &tc->permmem);
    if (r) return thread_context_partial_fin(tc, r, 6);
    r = list_builder_init(&tc->listb, &tc->tempmem, 64);
    if (r) return thread_context_partial_fin(tc, r, 7);
    r = list_builder_init(&tc->listb2, &tc->tempmem, 64);
    if (r) return thread_context_partial_fin(tc, r, 8);
    tc->llvmb = llvm_backend_new(tc);
    if (!tc->llvmb) return thread_context_partial_fin(tc, -1, 9);
    tc->has_preordered = false;
    return OK;
}
int thread_context_do_job(thread_context* tc, job* j)
{

    if (j->kind == JOB_PARSE) {
        parse_error pe;
        TIME(pe = parser_parse_file(&tc->p, &j->concrete.parse););
        tflush();
        if (pe) tauc_error_occured(tc->t, pe);
        if (pe == PE_FATAL) return ERR;
        return OK;
    }
    if (j->kind == JOB_RESOLVE) {
        int r = OK;
        bool can_link = false;
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

        resolve_error re;
        TIME(re = resolver_resolve(
                 &tc->r, start, end, &startid, &endid, &private_sym_count););
        if (re) tauc_error_occured(tc->t, re);
        if (re == RE_FATAL) r = ERR;
        // don't bother creating objs if we had any error somewhere
        // since we can't create the final exe anyways
        // (this needs to change this later once we can reuse objs)
        if (!re && tauc_success_so_far(tc->t)) {
            llvm_module* mod;
            llvm_error lle = llvm_backend_emit_module(
                tc->llvmb, start, end, startid, endid, private_sym_count, &mod);
            if (lle) tauc_error_occured(tc->t, lle);
            if (lle == LLE_FATAL) r = ERR;
            if (!lle) {
                llvm_module** tgt =
                    sbuffer_append(&tc->modules, sizeof(llvm_module*));
                if (tgt) {
                    *tgt = mod;
                    ureg lh = atomic_ureg_dec(&tc->t->linking_holdups);
                    if (lh == 1) can_link = true;
                }
                else {
                    error_log_report_allocation_failiure(tc->err_log);
                    r = ERR;
                    if (llvm_delete_objs(&mod, &mod + 1)) {
                        // TODO: think about how to handle this
                        assert(false);
                        return ERR;
                    }
                    llvm_free_module(mod);
                }
            }
        }
        if (!j->concrete.resolve.single_store) tfree(j->concrete.resolve.start);
        if (can_link) r = tauc_link(tc->t);
        return r;
    }
    if (j->kind == JOB_FINALIZE) {
        job_queue_stop(&tc->t->jobqueue);
        // DEBUG:
        // print_mdg_node(tc->t->mdg.root_node, 0);
        // puts("");
        int r = mdg_final_sanity_check(&tc->t->mdg, tc);
        if (!r) {
            ureg lh = atomic_ureg_dec(&tc->t->linking_holdups);
            if (lh == 1) {
                TIME(r = tauc_link(tc->t););
            }
        }
        tauc_error_occured(tc->t, r);
        return r;
    }
    assert(false); // unknown job type
    return ERR;
}
#include <utils/debug_utils.h>
void thread_context_run(thread_context* tc)
{
    int r = OK;
    job j;
    while (true) {
        r = job_queue_pop(
            &tc->t->jobqueue, &j, tc->has_preordered,
            atomic_ureg_load(&tc->t->thread_count));
        tc->has_preordered = false;
        if (r == JQ_DONE) {
            r = OK;
            break;
        }
        else if (r == JQ_WAITER_COUNT_REACHED) {
            j.kind = JOB_FINALIZE;
        }
        else if (r != OK) {
            break;
        }
        r = thread_context_do_job(tc, &j);
        if (r) break;
    }
    debug_utils_free_res();
}
int thread_context_preorder_job(thread_context* tc)
{
    if (tc->has_preordered) return OK;
    tc->has_preordered = true;
    return job_queue_preorder_job(&tc->t->jobqueue);
}
