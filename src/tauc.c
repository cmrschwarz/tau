#include "tauc.h"
#include "error_log.h"
#include "print_ast.h"
#include "thread_context.h"
#include "utils/allocator.h"
#include "symbol_table.h"
#include "assert.h"

struct tauc TAUC;

static inline int tauc_partial_fin(int r, int i)
{
    switch (i) {
        case -1:
        case 10: mdg_fin(&TAUC.mdg);
        case 9: thread_context_fin(&TAUC.main_thread_context);
        case 8: fin_global_symtab();
        case 7: atomic_ureg_fin(&TAUC.linking_holdups);
        case 6: atomic_ureg_fin(&TAUC.node_ids);
        case 5: atomic_ureg_fin(&TAUC.thread_count);
        case 4: aseglist_fin(&TAUC.worker_threads);
        case 3: job_queue_fin(&TAUC.jobqueue);
        case 2: file_map_fin(&TAUC.filemap);
        case 1: llvm_backend_fin_globals();
        case 0: break;
    }
    if (r) master_error_log_report("memory allocation failed");
    return r;
}
int tauc_init()
{
    int r = llvm_initialize_primitive_information();
    if (r) return tauc_partial_fin(r, 0); // ^ this doesn't need to be fin'd
    r = llvm_backend_init_globals();
    if (r) return tauc_partial_fin(r, 0);
    r = file_map_init(&TAUC.filemap);
    if (r) return tauc_partial_fin(r, 1);
    r = job_queue_init(&TAUC.jobqueue);
    if (r) return tauc_partial_fin(r, 2);
    r = aseglist_init(&TAUC.worker_threads);
    if (r) return tauc_partial_fin(r, 3);
    r = atomic_ureg_init(&TAUC.thread_count, 1);
    if (r) return tauc_partial_fin(r, 4);
    r = atomic_ureg_init(&TAUC.node_ids, 0);
    if (r) return tauc_partial_fin(r, 5);
    // 1 for release generation, one for final sanity check
    r = atomic_ureg_init(&TAUC.linking_holdups, 2);
    if (r) return tauc_partial_fin(r, 6);
    r = init_global_symtab(); // needs node_ids
    if (r) return tauc_partial_fin(r, 7);
    r = thread_context_init(&TAUC.main_thread_context);
    if (r) return tauc_partial_fin(r, 8);
    r = mdg_init(&TAUC.mdg);
    if (r) return tauc_partial_fin(r, 9);
    return OK;
}
int tauc_request_end()
{
    job jb;
    jb.kind = JOB_FINALIZE;
    ureg w, j;
    int r = job_queue_push(&TAUC.jobqueue, &jb, &w, &j);
    if (r != ERR) return OK;
    return ERR;
}
void tauc_fin()
{
    aseglist_iterator it;
    worker_thread* wt;
    aseglist_iterator_begin(&it, &TAUC.worker_threads);

    // tauc_request_end();
    // thread_context_run(&TAUC.main_thread_context);
    job_queue_stop(&TAUC.jobqueue);
    while (true) {
        wt = aseglist_iterator_next(&it);
        if (!wt) break;
        worker_thread_status wts = atomic_ureg_load(&wt->status);
        if (wts != WTS_FAILED) {
            thread_join(&wt->thr);
        }
    }
    mdg_fin(&TAUC.mdg);
    aseglist_iterator_begin(&it, &TAUC.worker_threads);
    while (true) {
        wt = aseglist_iterator_next(&it);
        if (!wt) break;
        thread_context_fin(&wt->tc);
        tfree(wt);
    }

    tauc_partial_fin(0, 9);
}

int tauc_run(int argc, char** argv)
{
    if (argc < 2) return 0;
    for (int i = 1; i < argc; i++) {
        src_file* f = file_map_get_file_from_path(
            &TAUC.filemap, string_from_cstr(argv[i]));
        if (!f) return ERR;
        src_file_require(f, NULL, SRC_RANGE_INVALID, TAUC.mdg.root_node);
    }
    return thread_context_run(&TAUC.main_thread_context);
}

void worker_thread_fn(void* ctx)
{
    puts("added worker thread!");
    worker_thread* wt = (worker_thread*)ctx;
    thread_context_run(&wt->tc);
    atomic_ureg_store(&wt->status, WTS_TERMINATED);
}
int tauc_add_worker_thread()
{
    // TODO: better mem management
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
    r = thread_launch(&wt->thr, worker_thread_fn, wt);
    if (r) {
        // all other initialization failiures are due to memory allocation,
        // which is deemed fatal for the CALLING thread
        // a thread spawn failiure isn't really though, so we make the error
        // appear in the new context, and make the old one continue like we
        // succeeded
        thread_context_fin(&wt->tc);
        error_log_report_critical_failiure(
            &wt->tc.err_log, "failed to spawn additional worker thread");
        atomic_ureg_store(&wt->status, WTS_FAILED);
        return OK; // this is intentional, see above
    }
    return OK;
}

int tauc_add_job(job* j)
{
    ureg waiters, jobs;
    int r = job_queue_push(&TAUC.jobqueue, j, &waiters, &jobs);
    if (r) return r;
    // TODO: tweak spawn condition
    if (jobs > waiters /*+ 1*/) {
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

int tauc_request_parse(
    src_file* f, src_file* requiring_file, src_range requiring_srange)
{
    job j;
    j.kind = JOB_PARSE;
    j.concrete.parse.file = f;
    j.concrete.parse.requiring_file = requiring_file;
    j.concrete.parse.requiring_srange = requiring_srange;
    return tauc_add_job(&j);
}
int tauc_request_resolve_multiple(mdg_node** start, mdg_node** end)
{
    job j;
    j.kind = JOB_RESOLVE;
    j.concrete.resolve.single_store = NULL;
    j.concrete.resolve.start = start;
    j.concrete.resolve.end = end;
    return tauc_add_job(&j);
}
int tauc_request_resolve_single(mdg_node* node)
{
    job j;
    j.kind = JOB_RESOLVE;
    // we can't use start and end here since jobs are copied by value
    j.concrete.resolve.single_store = node;
    return tauc_add_job(&j);
}
int tauc_link()
{
    ureg mod_count = 0;

    aseglist_iterator it;
    aseglist_iterator_begin(&it, &TAUC.worker_threads);
    thread_context* tc = &TAUC.main_thread_context;
    // tauc_request_end();
    // thread_context_run(&TAUC.main_thread_context);
    while (true) {
        mod_count += (sbuffer_get_capacity(&tc->modules) -
                      sbuffer_get_curr_segment_free_space(&tc->modules)) /
                     sizeof(llvm_module*);
        worker_thread* wt = (worker_thread*)aseglist_iterator_next(&it);
        if (!wt) break;
        tc = &wt->tc;
    }
    llvm_module** mods = tmalloc(mod_count * sizeof(llvm_module*));
    llvm_module** i = mods;
    aseglist_iterator_begin(&it, &TAUC.worker_threads);
    tc = &TAUC.main_thread_context;
    while (true) {
        sbi mit;
        sbi_begin(&mit, &tc->modules);
        for (llvm_module** m = sbi_next(&mit, sizeof(llvm_module*)); m;
             m = sbi_next(&mit, sizeof(llvm_module*))) {
            *i = *m;
            i++;
        }
        worker_thread* wt = (worker_thread*)aseglist_iterator_next(&it);
        if (!wt) break;
        tc = &wt->tc;
    }
    assert(i - mods == mod_count);
    int r = llvm_link_modules(mods, i, "hello_tau");
    r |= llvm_delete_objs(mods, i);
    for (llvm_module** m = mods; m != i; m++) {
        llvm_free_module(*m);
    }
    tfree(mods);
    return r;
}