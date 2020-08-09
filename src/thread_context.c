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
        case 10: list_builder_fin(&tc->listb2); // fallthrough
        case 9: list_builder_fin(&tc->listb); // fallthrough
        case 8: stack_fin(&tc->temp_stack); // fallthrough
        case 7: sbuffer_fin(&tc->temp_buffer); // fallthrough
        case 6: sccd_fin(&tc->sccd); // fallthrough
        case 5: resolver_fin(&tc->r); // fallthrough
        case 4: parser_fin(&tc->p); // fallthrough
        case 3: pool_fin(&tc->tempmem); // fallthrough
        case 2: sbuffer_fin(&tc->modules); // fallthrough
        case 1: pool_fin(&tc->permmem);
        case 0: break;
    }
    return r;
}
void thread_context_fin(thread_context* tc)
{
    thread_context_partial_fin(tc, 0, -1);
}
int thread_context_init(thread_context* tc, tauc* t)
{
    tc->t = t;
    tc->err_log = error_log_create(&t->mel);
    if (!tc->err_log) return ERR;
    int r = pool_init(&tc->permmem);
    if (r) return thread_context_partial_fin(tc, r, 0);
    r = sbuffer_init(&tc->modules, sizeof(llvm_module*) * 8);
    if (r) return thread_context_partial_fin(tc, r, 1);
    r = pool_init(&tc->tempmem);
    if (r) return thread_context_partial_fin(tc, r, 2);
    r = parser_init(&tc->p, tc);
    if (r) return thread_context_partial_fin(tc, r, 3);
    r = resolver_init(&tc->r, tc);
    if (r) return thread_context_partial_fin(tc, r, 4);
    r = sccd_init(&tc->sccd, tc);
    if (r) return thread_context_partial_fin(tc, r, 5);
    r = sbuffer_init(&tc->temp_buffer, 64);
    if (r) return thread_context_partial_fin(tc, r, 6);
    r = stack_init(&tc->temp_stack, &tc->permmem);
    if (r) return thread_context_partial_fin(tc, r, 7);
    r = list_builder_init(&tc->listb, &tc->tempmem, 64);
    if (r) return thread_context_partial_fin(tc, r, 8);
    r = list_builder_init(&tc->listb2, &tc->tempmem, 64);
    if (r) return thread_context_partial_fin(tc, r, 9);
    tc->has_preordered = false;
    return OK;
}
int thread_context_do_job(thread_context* tc, job* j)
{

    if (j->kind == JOB_PARSE) {
        parse_error pe;
        TAU_TIME_STAGE_CTX(
            tc->t, { pe = parser_parse_file(&tc->p, &j->concrete.parse); },
            {
                tprintf("parsing ");
                tprintn(
                    j->concrete.parse.file->head.name.start,
                    string_len(j->concrete.parse.file->head.name));
                tputchar(' ');
            });

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
        resolve_error re;
        llvm_module* mod = NULL;
        re = resolver_resolve_and_emit(
            &tc->r, start, end, j->concrete.resolve.partial_res_data, &mod);
        if (re == RE_SUSPENDED) return RE_OK;
        if (re) tauc_error_occured(tc->t, re);
        if (re == RE_FATAL) r = ERR;
        // don't bother creating objs if we had any error somewhere
        // since we can't create the final exe anyways
        // (this needs to change this later once we can reuse objs)
        if (!re && mod) {
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
        if (!j->concrete.resolve.single_store) tfree(j->concrete.resolve.start);
        if (can_link) r = tauc_link(tc->t);
        return r;
    }
    if (j->kind == JOB_FINALIZE) {
        job_queue_stop(&tc->t->jobqueue);
        // DEBUG:
        if (tc->t->emit_ast) {
            print_mdg_node(tc->t->mdg.root_node, 0);
            tputs("");
        }
        int r = mdg_final_sanity_check(&tc->t->mdg, tc);
        if (!r) {
            ureg lh = atomic_ureg_dec(&tc->t->linking_holdups);
            if (lh == 1) {
                r = tauc_link(tc->t);
            }
        }
        else {
            tauc_error_occured(tc->t, r);
        }
        return r;
    }
    if (j->kind == JOB_LOAD_PP) {
        assert(false); // TODO: lazy pp loading. for now it's gonna be eager
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
            atomic_ureg_load(&tc->t->active_thread_count));
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
    ureg atc = atomic_ureg_dec(&tc->t->active_thread_count) - 1;
    job_queue_check_waiters(&tc->t->jobqueue, atc);
}
int thread_context_preorder_job(thread_context* tc)
{
    if (tc->has_preordered) return OK;
    tc->has_preordered = true;
    return job_queue_preorder_job(&tc->t->jobqueue);
}
