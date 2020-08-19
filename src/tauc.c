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

static inline int global_scope_init(scope* gs)
{
    gs->osym.sym.name = "__global_scope__";
    gs->osym.sym.node.kind = MF_MODULE; // this is a lie, but it works :)
    gs->osym.sym.node.flags = AST_NODE_FLAGS_DEFAULT;
    gs->body.pprn = NULL;
    gs->osym.sym.declaring_body = NULL;
    gs->osym.visible_within_body = NULL;
    gs->body.elements = NULL;
    gs->body.srange = SRC_RANGE_INVALID;
    gs->body.parent = NULL;
    gs->body.owning_node = (ast_elem*)gs;
    gs->body.symtab = symbol_table_create(PRIMITIVE_COUNT + 1, 0);
    if (!gs->body.symtab) return ERR;
    for (int i = 0; i < PRIMITIVE_COUNT; i++) {
        if (symbol_table_insert(gs->body.symtab, (symbol*)&PRIMITIVES[i])) {
            symbol_table_destroy(gs->body.symtab);
            return ERR;
        }
        PRIMITIVES[i].sym.declaring_body = &gs->body;
    }
    return OK;
}
static inline void global_scope_fin(scope* gs)
{
    symbol_table_destroy(gs->body.symtab);
}

static inline int tauc_core_partial_fin(tauc* t, int r, int i)
{
    switch (i) {
        case -1:
        case 9: mdg_fin(&t->mdg); // fallthrough
        case -2: // skip mdg because we freed that earlier when we still had all
                 // threads and their permmem
        case 8: list_fin(&t->required_files, false); // fallthrough
        case 7: aseglist_fin(&t->module_dtors); // fallthrough
        case 6: aseglist_fin(&t->module_ctors); // fallthrough
        case 5: thread_context_fin(&t->main_thread_context); // fallthrough
        case 4: global_scope_fin(&t->global_scope); // fallthrough
        case 3: aseglist_fin(&t->worker_threads); // fallthrough
        case 2: job_queue_fin(&t->jobqueue); // fallthrough
        case 1: llvm_backend_fin_globals(); // fallthrough
        case 0: break;
    }
    return r;
}
int tauc_core_init(tauc* t)
{
    int r = llvm_initialize_primitive_information();
    // ^ this doesn't need to be fin'd
    if (r) return tauc_core_partial_fin(t, r, 0);
    r = llvm_backend_init_globals(t);
    if (r) return tauc_core_partial_fin(t, r, 0);
    r = job_queue_init(&t->jobqueue);
    if (r) return tauc_core_partial_fin(t, r, 1);
    r = aseglist_init(&t->worker_threads);
    if (r) return tauc_core_partial_fin(t, r, 2);
    r = global_scope_init(&t->global_scope); // needs node_ids
    if (r) return tauc_core_partial_fin(t, r, 3);
    r = thread_context_init(&t->main_thread_context, t);
    if (r) return tauc_core_partial_fin(t, r, 4);
    r = aseglist_init(&t->module_ctors);
    if (r) return tauc_core_partial_fin(t, r, 5);
    r = aseglist_init(&t->module_dtors);
    if (r) return tauc_core_partial_fin(t, r, 6);
    r = list_init(&t->required_files);
    if (r) return tauc_core_partial_fin(t, r, 7);
    r = mdg_init(&t->mdg);
    if (r) return tauc_core_partial_fin(t, r, 8);

    atomic_ureg_init(&t->active_thread_count, 1);
    atomic_ureg_init(&t->node_ids, 0);
    atomic_sreg_init(&t->error_code, 0);
    // 1 for release generation, one for final sanity check
    atomic_ureg_init(&t->linking_holdups, 2);
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
    t->ok_on_error = false;
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
bool strstart(const char* present, const char* expected)
{
    ureg exp_len = strlen(expected);
    if (strlen(present) < exp_len) return false;
    if (memcmp(present, expected, exp_len)) return false;
    return true;
}
int handle_cmd_args(
    tauc* t, error_log* el, int argc, char** argv, bool* files_found)
{
    int r = 0;
    bool tests_run = false;
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
                f, t, NULL, SRC_RANGE_INVALID, t->mdg.root_node, true);
            if (r) return r;
            r = list_append(
                &t->required_files, &t->main_thread_context.permmem, f);
            if (r) {
                tauc_error_occured(t, ERR);
                return ERR;
            }
            continue;
        }
        if (*files_found) return complain_trailing_args(t, el, arg);
        if (strstart(arg, "-T")) {
            bool joined_arg = false;
            char* num_start;
            if (arg[2] != '\0') {
                joined_arg = true;
                num_start = arg + 2;
            }
            else {
                if (i == argc - 1) {
                    master_error_log_report(
                        &t->mel, "-T requires a number to follow");
                    return ERR;
                }
                num_start = argv[i + 1];
                i++;
            }
            char* arg_end = num_start + strlen(num_start);
            char* num_end = arg_end;
            long num = strtol(num_start, &num_end, 10);
            if (num > U16_MAX || num < 0 || num == 0 || num_end != arg_end) {
                char* msg;
                if (joined_arg) {
                    msg = error_log_cat_strings_3(
                        el, "invalid argument '", arg, "'");
                }
                else {
                    msg = error_log_cat_strings_3(
                        el, "invalid argument '-T ", num_start, "'");
                }
                master_error_log_report(&t->mel, msg);
                return ERR;
            }
            platttform_override_virt_core_count(num);
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
        else if (!strcmp(arg, "--timings")) {
            t->verbosity_flags |= VERBOSITY_FLAGS_TIME_STAGES;
        }
        else if (!strcmp(arg, "--thread-spawns")) {
            t->verbosity_flags |= VERBOSITY_FLAG_THREAD_SPAWNS;
        }
        else if (!strcmp(arg, "--stage-begins")) {
            t->verbosity_flags |= VERBOSITY_FLAGS_STAGE_BEGINS;
        }
        else if (!strcmp(arg, "--pprns")) {
            t->verbosity_flags |= VERBOSITY_FLAGS_PPRNS;
        }
        else if (!strcmp(arg, "--pastes")) {
            t->verbosity_flags |= VERBOSITY_FLAG_PASTES;
        }
        else if (!strcmp(arg, "--liveness")) {
            t->verbosity_flags |= VERBOSITY_FLAGS_LIVENESS;
        }
        else if (!strcmp(arg, "--sccd")) {
            t->verbosity_flags |= VERBOSITY_FLAGS_SCCD;
        }
        else if (!strcmp(arg, "--used-in-pp")) {
            t->verbosity_flags |= VERBOSITY_FLAGS_USED_IN_PP;
        }
        else if (!strcmp(arg, "--files")) {
            t->verbosity_flags |= VERBOSITY_FLAGS_FILES;
        }
        else if (!strcmp(arg, "--ok-on-error")) {
            t->ok_on_error = true;
        }
        else if (!strcmp(arg, "--run-unit-tests")) {
#if DEBUG
            r = run_unit_tests(argc, argv);
            if (r) return r;
#else
            master_error_log_report(
                &t->mel, "unit tests not available in release build");
#endif
            tests_run = true;
        }

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
        if (!tests_run || (t->emit_asm || t->emit_ast || t->explicit_exe)) {
            master_error_log_report(&t->mel, "no input files");
            r |= ERR;
        }
    }
    target_platform_fill_gaps(&t->target, &t->host_target);
    if (t->opt_strat == OPT_STRAT_UNSPECIFIED) {
        t->opt_strat = OPT_STRAT_O0;
    }
    return r;
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
        if (wt->spawned) {
            thread_join(&wt->thr);
        }
    }
    mdg_fin(&t->mdg);
    aseglist_iterator_begin(&it, &t->worker_threads);
    while (true) {
        wt = aseglist_iterator_next(&it);
        if (!wt) break;
        if (wt->initialized) thread_context_fin(&wt->tc);
        tfree(wt);
    }
    return atomic_sreg_load_flat(&t->error_code);
}

int tauc_run(int argc, char** argv)
{
    tauc t;
    if (talloc_init(&t.mel)) return ERR;
    int r;
    timer_start(&t.total_time);
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
                r = tauc_run_jobs(&t);
                tauc_core_fin(&t);
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
    timer_stop(&t.total_time);
    if (t.verbosity_flags & VERBOSITY_FLAGS_TIME_STAGES) {
        timespan ts;
        timer_get_elapsed(&t.total_time, &ts);
        tprintf("total [");
        pretty_print_timespan(&ts);
        tprintf("]\n");
    }
    debug_utils_free_res();
    talloc_fin();
    if (t.ok_on_error) return OK;
    if (!r) {
        if (tauc_success_so_far(&t)) return OK;
        r = atomic_sreg_load(&t.error_code);
    }
    return r;
}

void worker_thread_fn(void* ctx)
{
    worker_thread* wt = (worker_thread*)ctx;
    wt->spawned = true;
    int r = thread_context_init(&wt->tc, wt->tc.t);
    if (r) return;
    wt->initialized = true;
    if (wt->tc.t->verbosity_flags & VERBOSITY_FLAG_THREAD_SPAWNS) {
        if (wt->tc.t->verbosity_flags & VERBOSITY_FLAGS_STAGE_BEGINS) {
            // so the initial job gets put here
            tprintf("started worker thread: ");
        }
        else {
            tputs("started worker thread");
            tflush();
        }
    }
    thread_context_run(&wt->tc);
    // we do this right before the worker thread exits
    // to be on the safe side
    debug_utils_free_res();
}
int tauc_add_worker_thread(tauc* t)
{
    // preorder a job for the new thread
    // TODO: better mem management
    worker_thread* wt = tmalloc(sizeof(worker_thread));
    if (!wt) return ERR;
    wt->tc.t = t;
    wt->spawned = false;
    wt->initialized = false;
    int r = aseglist_add(&t->worker_threads, wt);
    if (r) {
        tfree(wt);
        return r;
    }
    if (wt->tc.t->verbosity_flags & VERBOSITY_FLAG_THREAD_SPAWNS) {
        tput("requesting worker thread");
        if (wt->tc.t->verbosity_flags & VERBOSITY_FLAGS_STAGE_BEGINS) {
            tput(" @");
            pretty_print_timer_elapsed(&wt->tc.t->total_time);
        }
        tputs("");
        tflush();
    }
    wt->tc.has_preordered = false;
    thread_context_preorder_job(&wt->tc);
    r = thread_launch(&wt->thr, worker_thread_fn, wt);
    if (r) {
        master_error_log_report(&t->mel, "failed to spawn worker thread");
        return OK; // our thread might live on? debatable.
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
int tauc_request_resolve_multiple(
    tauc* t, mdg_node** start, mdg_node** end, partial_resolution_data* prd)
{
    job j;
    j.kind = JOB_RESOLVE;
    j.concrete.resolve.single_store = NULL;
    j.concrete.resolve.start = start;
    j.concrete.resolve.end = end;
    j.concrete.resolve.partial_res_data = prd;
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
int tauc_request_resolve_single(
    tauc* t, mdg_node* node, partial_resolution_data* prd)
{
    job j;
    j.kind = JOB_RESOLVE;
    // we can't use start and end here since jobs are copied by value
    j.concrete.resolve.single_store = node;
    j.concrete.resolve.partial_res_data = prd;
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
    worker_thread* wt;
    while (true) {
        mod_count += sbuffer_get_used_size(&tc->modules) / sizeof(llvm_module*);
        do {
            wt = (worker_thread*)aseglist_iterator_next(&it);
        } while (wt && !wt->initialized);
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
        do {
            wt = (worker_thread*)aseglist_iterator_next(&it);
        } while (wt && !wt->initialized);
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
