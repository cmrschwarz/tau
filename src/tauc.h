#ifndef TAUC_TAUC_H
#define TAUC_TAUC_H
#include "file_map.h"
#include "job_queue.h"
#include "thread_context.h"
#include "utils/threading.h"
#include "target_platform.h"
#include "utils/debug_utils.h"

typedef enum optimization_strategy_e {
    OPT_STRAT_UNSPECIFIED,
    OPT_STRAT_O0,
    OPT_STRAT_O1,
    OPT_STRAT_O2,
    OPT_STRAT_O3,
    OPT_STRAT_OS,
} optimization_strategy;

typedef struct tauc_s {
    // these two are still needed for error reporting after
    // the compiler has run
    master_error_log mel;
    file_map filemap;

    thread_context main_thread_context;
    aseglist worker_threads;
    atomic_ureg active_thread_count;
    // number of task that need to be completed before linking can start. the
    // task that changes this to 0 does the linking.
    atomic_ureg linking_holdups;
    module_dependency_graph mdg;
    job_queue jobqueue;
    atomic_sreg error_code;
    atomic_ureg node_ids; // stores the max used id
    symbol_table* root_symtab;
    aseglist module_ctors;
    aseglist module_dtors;
    target_platform host_target;
    target_platform target;
    optimization_strategy opt_strat;
    ureg verbosity_flags;
    bool emit_ll;
    bool emit_asm;
    bool emit_exe;
    bool emit_ast;
    bool emit_objs;
    bool needs_emit_stage;
    bool explicit_exe;
    bool trap_on_error;
    bool debug_symbols;
    bool explicit_debug_symbols;
} tauc;

// THREADSAFE
int tauc_request_parse(
    tauc* t, src_file* f, src_map* requiring_smap, src_range requiring_stmt);
int tauc_request_resolve_single(tauc* t, mdg_node* node);
int tauc_request_resolve_multiple(tauc* t, mdg_node** start, mdg_node** end);
int tauc_request_pp_module(tauc* t, mdg_node* mdg);
int tauc_request_finalize(tauc* t);
bool tauc_success_so_far(tauc* t);
void tauc_error_occured(tauc* t, int ec);
int tauc_link(tauc* t);

// MAIN THREAD ONLY
int tauc_run(int argc, char** argv); // errors are returned by fin instead

// for unit testing only, otherwise let tauc_run call these
int tauc_scaffolding_init(tauc* t);
int tauc_core_init(tauc* t);
void tauc_core_fin(tauc* t);
void tauc_core_fin_no_run(tauc* t);
void tauc_scaffolding_fin(tauc* t);

#define VERBOSITY_FLAGS_TIME_STAGES 1
#define TAU_TIME_STAGE_CTX(t, before, code, after)                             \
    do {                                                                       \
        if ((t)->verbosity_flags & VERBOSITY_FLAGS_TIME_STAGES) {              \
            before;                                                            \
            TIME(code);                                                        \
            after;                                                             \
        }                                                                      \
        else {                                                                 \
            code                                                               \
        }                                                                      \
    } while (false)

#define TAU_TIME_STAGE(t, code) TAU_TIME_STAGE_CTX(t, , code, )

#endif
