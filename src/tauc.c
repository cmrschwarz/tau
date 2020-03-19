#include "tauc.h"
#include "error_log.h"
#include "print_ast.h"
#include "thread_context.h"
#include "utils/allocator.h"
#include "symbol_table.h"
#include "assert.h"
#include "utils/debug_utils.h"
#include "utils/panic.h"
#if DEBUG
#include "test/unit_tests.h"
#endif
#include <stdlib.h>

static inline int tauc_core_partial_fin(tauc* t, int r, int i)
{
    switch (i) {
        case -1:
        case 12: mdg_fin(&t->mdg); // fallthrough
        case -2: // skip mdg because we freed that earlier when we still had all
                 // threads and their permmem
        case 11: aseglist_fin(&t->module_dtors); // fallthrough
        case 10: aseglist_fin(&t->module_ctors); // fallthrough
        case 9: thread_context_fin(&t->main_thread_context); // fallthrough
        case 8: fin_root_symtab(t->root_symtab); // fallthrough
        case 7: atomic_ureg_fin(&t->linking_holdups); // fallthrough
        case 6: atomic_sreg_fin(&t->error_code); // fallthrough
        case 5: atomic_ureg_fin(&t->node_ids); // fallthrough
        case 4: atomic_ureg_fin(&t->active_thread_count); // fallthrough
        case 3: aseglist_fin(&t->worker_threads); // fallthrough
        case 2: job_queue_fin(&t->jobqueue); // fallthrough
        case 1: llvm_backend_fin_globals(); // fallthrough
        case 0: break;
    }
    if (r) master_error_log_report(&t->mel, "memory allocation failed");
    return r;
}
int tauc_core_init(tauc* t)
{
    int r = llvm_initialize_primitive_information();
    // ^ this doesn't need to be fin'd
    if (r) return tauc_core_partial_fin(t, r, 0);
    r = llvm_backend_init_globals();
    if (r) return tauc_core_partial_fin(t, r, 0);
    r = job_queue_init(&t->jobqueue);
    if (r) return tauc_core_partial_fin(t, r, 1);
    r = aseglist_init(&t->worker_threads);
    if (r) return tauc_core_partial_fin(t, r, 2);
    r = atomic_ureg_init(&t->active_thread_count, 1);
    if (r) return tauc_core_partial_fin(t, r, 3);
    r = atomic_ureg_init(&t->node_ids, 0);
    if (r) return tauc_core_partial_fin(t, r, 4);
    r = atomic_sreg_init(&t->error_code, 0);
    if (r) return tauc_core_partial_fin(t, r, 5);
    // 1 for release generation, one for final sanity check
    r = atomic_ureg_init(&t->linking_holdups, 2);
    if (r) return tauc_core_partial_fin(t, r, 6);
    r = init_root_symtab(&t->root_symtab); // needs node_ids
    if (r) return tauc_core_partial_fin(t, r, 7);
    r = thread_context_init(&t->main_thread_context, t);
    if (r) return tauc_core_partial_fin(t, r, 8);
    r = aseglist_init(&t->module_ctors);
    if (r) return tauc_core_partial_fin(t, r, 9);
    r = aseglist_init(&t->module_dtors);
    if (r) return tauc_core_partial_fin(t, r, 10);
    r = mdg_init(&t->mdg);
    if (r) return tauc_core_partial_fin(t, r, 11);
    t->emit_asm = false;
    t->emit_ll = false;
    t->explicit_exe = false;
    t->emit_ast = false;
    t->emit_exe = true;
    t->trap_on_error = false;
    t->emit_objs = false;
    t->opt_strat = OPT_STRAT_UNSPECIFIED;
    t->debug_symbols = true;
    t->explicit_debug_symbols = false;
    t->verbosity_flags = 0;
    target_platform_get_host(&t->host_target);
    target_platform_set_unknown(&t->target);
    return OK;
}
void tauc_core_fin(tauc* t)
{
    tauc_core_partial_fin(t, 0, -2);
}
void tauc_core_fin_no_run(tauc* t)
{
    tauc_core_partial_fin(t, 0, -1);
}
int complain_trailing_args(tauc* t, error_log* el, char* arg)
{
    char* msg = error_log_cat_strings_3(
        el, "command line option \"", arg,
        "\" must be specified before input files");
    master_error_log_report(&t->mel, msg);
    return ERR;
}
int check_multi_opt_strats(tauc* t, optimization_strategy opt_strat)
{
    if (t->opt_strat != OPT_STRAT_UNSPECIFIED) {
        master_error_log_report(
            &t->mel, "only one optimization strategy may be specified");
        return ERR;
    }
    if (!t->explicit_debug_symbols) {
        t->debug_symbols = false;
    }
    t->opt_strat = opt_strat;
    return OK;
}
int handle_cmd_args(
    tauc* t, error_log* el, int argc, char** argv, bool* files_found)
{
    int r = 0;
#if DEBUG
    bool tests_run = false;
#endif
    for (int i = 1; i < argc; i++) {
        char* arg = argv[i];
        assert(arg);
        if (arg[0] != '-') {
            *files_found = true;
            src_file* f = file_map_get_file_from_path(
                &t->filemap, NULL, string_from_cstr(argv[i]));
            if (!f) {
                tauc_error_occured(t, ERR);
                return ERR;
            }
            r = src_file_require(
                f, t, NULL, SRC_RANGE_INVALID, t->mdg.root_node);
            if (r) return r;
            continue;
        }
        if (*files_found) return complain_trailing_args(t, el, arg);
        if (!strcmp(arg, "-T")) {
            if (i == argc - 1) {
                master_error_log_report(
                    &t->mel, "-T requires a number to follow");
                return ERR;
            }
            char* end = argv[i + 1] + strlen(argv[i + 1]);
            char* num_end;
            long num = strtol(argv[i + 1], &num_end, 10);
            if (num > U16_MAX || num < 0 || num == 0 || num_end != end) {
                char* msg = error_log_cat_strings_3(
                    el, "invalid argument pair \"-t ", argv[i + 1], "\"");
                master_error_log_report(&t->mel, msg);
                return ERR;
            }
            platttform_override_virt_core_count(num);
            i++;
        }
        else if (!strcmp(arg, "--arch")) {
            if (i == argc - 1) {
                master_error_log_report(
                    &t->mel, "--arch requires an arch name to follow");
                return ERR;
            }
            if (t->target.arch != ARCH_UNKNOWN) {
                master_error_log_report(
                    &t->mel, "--arch mustn't be given twice");
                return ERR;
            }
            t->target.arch = parse_arch_kind(argv[i + 1]);
            if (t->target.arch == ARCH_UNKNOWN) {
                char* msg = error_log_cat_strings_3(
                    el, "unknown arch '", argv[i + 1], "'");
                master_error_log_report(&t->mel, msg);
                return ERR;
            }
            i++;
        }
        else if (!strcmp(arg, "--os")) {
            if (i == argc - 1) {
                master_error_log_report(
                    &t->mel, "--os requires an os name to follow");
                return ERR;
            }
            if (t->target.os != OS_UNKNOWN) {
                master_error_log_report(&t->mel, "--os mustn't be given twice");
                return ERR;
            }
            t->target.os = parse_os_kind(argv[i + 1]);
            if (t->target.os == OS_UNKNOWN) {
                char* msg = error_log_cat_strings_3(
                    el, "unknown os '", argv[i + 1], "'");
                master_error_log_report(&t->mel, msg);
                return ERR;
            }
            i++;
        }
        else if (!strcmp(arg, "--object-format")) {
            if (i == argc - 1) {
                master_error_log_report(
                    &t->mel,
                    "--object-format requires an object-format name to follow");
                return ERR;
            }
            if (t->target.object_format != OBJECT_FORMAT_UNKNOWN) {
                master_error_log_report(
                    &t->mel, "--object-format mustn't be given twice");
                return ERR;
            }
            t->target.object_format = parse_object_format_kind(argv[i + 1]);
            if (t->target.object_format == OBJECT_FORMAT_UNKNOWN) {
                char* msg = error_log_cat_strings_3(
                    el, "unknown object-format '", argv[i + 1], "'");
                master_error_log_report(&t->mel, msg);
                return ERR;
            }
            i++;
        }
        else if (!strcmp(arg, "--ast")) {
            t->emit_ast = true;
            if (!t->explicit_exe) t->emit_exe = false;
        }
        else if (!strcmp(arg, "-S")) {
            t->emit_asm = true;
            if (!t->explicit_exe) t->emit_exe = false;
        }
        else if (!strcmp(arg, "-L")) {
            t->emit_ll = true;
            if (!t->explicit_exe) t->emit_exe = false;
        }
        else if (!strcmp(arg, "-O")) {
            t->emit_objs = true;
            if (!t->explicit_exe) t->emit_exe = false;
        }
        else if (!strcmp(arg, "-E")) {
            t->emit_exe = true;
            t->explicit_exe = true;
        }
        else if (!strcmp(arg, "-O0")) {
            if (check_multi_opt_strats(t, OPT_STRAT_O0)) return ERR;
        }
        else if (!strcmp(arg, "-O1")) {
            if (check_multi_opt_strats(t, OPT_STRAT_O1)) return ERR;
        }
        else if (!strcmp(arg, "-O2")) {
            if (check_multi_opt_strats(t, OPT_STRAT_O2)) return ERR;
        }
        else if (!strcmp(arg, "-O3")) {
            if (check_multi_opt_strats(t, OPT_STRAT_O3)) return ERR;
        }
        else if (!strcmp(arg, "-OS")) {
            if (check_multi_opt_strats(t, OPT_STRAT_OS)) return ERR;
        }
        else if (!strcmp(arg, "-D")) {
            t->debug_symbols = true;
            t->explicit_debug_symbols = true;
        }
        else if (!strcmp(arg, "--debugbreak")) {
            t->trap_on_error = true;
        }
#if DEBUG
        else if (!strcmp(arg, "--run-unit-tests")) {
            r = run_unit_tests(argc, argv);
            tests_run = true;
            if (r) return r;
        }
#endif
        else {
            // TODO: rework this to avoid the alloc, its kinda stupid
            char* msg = error_log_cat_strings_3(
                el, "unknown command line option \"", arg, "\"");
            master_error_log_report(&t->mel, msg);
            return ERR;
        }
    }
    t->needs_emit_stage = (t->emit_exe || t->emit_asm || t->emit_ll);
    if (!*files_found) {
#if DEBUG
        if (!tests_run || (t->emit_asm || t->emit_ast || t->explicit_exe)) {
#endif
            master_error_log_report(&t->mel, "no input files");
            r |= ERR;
#if DEBUG
        }
#endif
    }
    target_platform_fill_gaps(&t->target, &t->host_target);
    if (t->opt_strat == OPT_STRAT_UNSPECIFIED) {
        t->opt_strat = OPT_STRAT_O0;
    }
    return OK;
}
int tauc_scaffolding_init(tauc* t)
{
    int r = file_map_init(&t->filemap);
    if (r) return r;
    r = master_error_log_init(&t->mel, &t->filemap);
    if (r) {
        file_map_fin(&t->filemap);
        return r;
    }
    return OK;
}
void tauc_scaffolding_fin(tauc* t)
{
    master_error_log_fin(&t->mel);
    file_map_fin(&t->filemap);
}
int tauc_run_jobs(tauc* t)
{
    thread_context_run(&t->main_thread_context);
    aseglist_iterator it;
    worker_thread* wt;
    aseglist_iterator_begin(&it, &t->worker_threads);

    while (true) {
        wt = aseglist_iterator_next(&it);
        if (!wt) break;
        if (!wt->spawn_failed) {
            thread_join(&wt->thr);
        }
    }
    mdg_fin(&t->mdg);
    aseglist_iterator_begin(&it, &t->worker_threads);
    while (true) {
        wt = aseglist_iterator_next(&it);
        if (!wt) break;
        thread_context_fin(&wt->tc);
        tfree(wt);
    }
    return atomic_sreg_load_flat(&t->error_code);
}

int tauc_run(int argc, char** argv)
{
    tauc t;
    int r;

    r = tauc_scaffolding_init(&t);
    if (r) return r;
    bool files_found = false;
    r = tauc_core_init(&t);
    if (!r) {
        thread_context_preorder_job(&t.main_thread_context);
        r = handle_cmd_args(
            &t, t.main_thread_context.err_log, argc, argv, &files_found);
        if (!r) {
            if (files_found) {
                TAU_TIME_STAGE_CTX(
                    &t, tprintf("total "),
                    {
                        r = tauc_run_jobs(&t);
                        tauc_core_fin(&t);
                    },
                    {
                        tputs("\n");
                        tflush();
                    });
            }
            else {
                tauc_core_fin_no_run(&t);
            }
        }
        else {
            tauc_core_fin_no_run(&t);
        }
    }
    master_error_log_unwind(&t.mel);
    tauc_scaffolding_fin(&t);
    debug_utils_free_res();
    if (!r) {
        if (tauc_success_so_far(&t)) return OK;
        return atomic_sreg_load(&t.error_code);
    }
    return r;
}

void worker_thread_fn(void* ctx)
{
    // tputs("added worker thread!");
    tflush();
    worker_thread* wt = (worker_thread*)ctx;
    thread_context_run(&wt->tc);
}
int tauc_add_worker_thread(tauc* t)
{
    // preorder a job for the new thread
    job_queue_preorder_job(&t->jobqueue);
    // TODO: better mem management
    worker_thread* wt = tmalloc(sizeof(worker_thread));
    if (!wt) return ERR;
    int r = thread_context_init(&wt->tc, t);
    if (r) {
        tfree(wt);
        return r;
    }
    wt->spawn_failed = false;
    r = aseglist_add(&t->worker_threads, wt);
    if (r) {
        thread_context_fin(&wt->tc);
        tfree(wt);
        return r;
    }
    r = thread_launch(&wt->thr, worker_thread_fn, wt);
    if (r) {
        // all other initialization failiures are due to memory
        // allocation, which is deemed fatal for the CALLING thread a
        // thread spawn failiure isn't really though, so we make the
        // error appear in the new context, and make the old one
        // continue like we succeeded
        thread_context_fin(&wt->tc);
        error_log_report_critical_failiure(
            wt->tc.err_log, "failed to spawn additional worker thread");
        wt->spawn_failed = true;
        return OK; // this is intentional, see above
    }
    return OK;
}

int tauc_add_job(tauc* t, job* j, bool prevent_thread_spawn)
{
    ureg waiters, jobs;
    int r = job_queue_push(&t->jobqueue, j, &waiters, &jobs);
    assert(r != JQ_DONE);
    if (r) return r;
    if (jobs > waiters && !prevent_thread_spawn) {
        ureg max_tc = plattform_get_virt_core_count();
        ureg tc = atomic_ureg_load(&t->active_thread_count);
        if (tc < max_tc) {
            tc = atomic_ureg_inc(&t->active_thread_count);
            if (tc < max_tc) {
                return tauc_add_worker_thread(t);
            }
            else {
                atomic_ureg_dec(&t->active_thread_count);
            }
        }
    }
    return OK;
}

int tauc_request_parse(
    tauc* t, src_file* f, src_map* requiring_smap, src_range requiring_srange)
{
    job j;
    j.kind = JOB_PARSE;
    j.concrete.parse.file = f;
    j.concrete.parse.requiring_smap = requiring_smap;
    j.concrete.parse.requiring_srange = requiring_srange;
    return tauc_add_job(t, &j, false);
}
int tauc_request_resolve_multiple(tauc* t, mdg_node** start, mdg_node** end)
{
    job j;
    j.kind = JOB_RESOLVE;
    j.concrete.resolve.single_store = NULL;
    j.concrete.resolve.start = start;
    j.concrete.resolve.end = end;
    return tauc_add_job(t, &j, false);
}
int tauc_request_pp_module(tauc* t, mdg_node* mdg)
{
    // TODO: also load dependencies
    assert(false);
    job j;
    j.kind = JOB_LOAD_PP;
    j.concrete.load_pp.node = mdg;
    return tauc_add_job(t, &j, false);
}
int tauc_request_resolve_single(tauc* t, mdg_node* node)
{
    job j;
    j.kind = JOB_RESOLVE;
    // we can't use start and end here since jobs are copied by value
    j.concrete.resolve.single_store = node;
    return tauc_add_job(t, &j, false);
}
int tauc_request_finalize(tauc* t)
{
    job j;
    j.kind = JOB_FINALIZE;
    return tauc_add_job(t, &j, true);
}
int tauc_link(tauc* t)
{
    ureg mod_count = 0;

    aseglist_iterator it;
    aseglist_iterator_begin(&it, &t->worker_threads);
    thread_context* tc = &t->main_thread_context;
    // tauc_request_end();
    // thread_context_run(&t->main_thread_context);
    while (true) {
        mod_count += sbuffer_get_used_size(&tc->modules) / sizeof(llvm_module*);
        worker_thread* wt = (worker_thread*)aseglist_iterator_next(&it);
        if (!wt) break;
        tc = &wt->tc;
    }
    llvm_module** mods = tmalloc(mod_count * sizeof(llvm_module*));
    llvm_module** i = mods;
    aseglist_iterator_begin(&it, &t->worker_threads);
    tc = &t->main_thread_context;
    while (true) {
        sbuffer_iterator mit = sbuffer_iterator_begin(&tc->modules);
        for (llvm_module** m =
                 sbuffer_iterator_next(&mit, sizeof(llvm_module*));
             m; m = sbuffer_iterator_next(&mit, sizeof(llvm_module*))) {
            *i = *m;
            i++;
        }
        worker_thread* wt = (worker_thread*)aseglist_iterator_next(&it);
        if (!wt) break;
        tc = &wt->tc;
    }
    assert(ptrdiff(i, mods) / sizeof(llvm_module*) == mod_count);
    int r = 0;

    if (t->emit_exe) {
        r = llvm_link_modules(t, mods, i, &t->filemap.rt_src_libs, "a.out");
        if (!t->emit_objs) {
            r |= llvm_delete_objs(mods, i);
        }
    }
    for (llvm_module** m = mods; m != i; m++) {
        llvm_free_module(*m);
    }
    tfree(mods);
    return r;
}
void tauc_error_occured(tauc* t, int ec)
{
    if (t->trap_on_error) debugbreak();
    sreg ov = 0;
    while (ov == 0) {
        if (atomic_sreg_cas(&t->error_code, &ov, (sreg)ec)) break;
    }
}
bool tauc_success_so_far(tauc* t)
{
    return (atomic_sreg_load(&t->error_code) == OK);
}
