#pragma once
#include "file_map.h"
#include "job_queue.h"
#include "thread_context.h"
#include "utils/threading.h"
#include "ptr_map.h"
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

#define VERBOSITY_FLAGS_TIME_STAGES 0x1
#define VERBOSITY_FLAGS_PPRNS 0x2
#define VERBOSITY_FLAGS_LIVENESS 0x4
#define VERBOSITY_FLAGS_SCCD 0x8
#define VERBOSITY_FLAGS_STAGE_BEGINS 0x10
#define VERBOSITY_FLAG_THREAD_SPAWNS 0x20
#define VERBOSITY_FLAG_PASTES 0x40
#define VERBOSITY_FLAGS_USED_IN_PP 0x80
#define VERBOSITY_FLAGS_FILES 0x100
#define VERBOSITY_FLAGS_LINKER_ARGS 0x200

typedef struct tauc_s {
    // these two are still needed for error reporting after
    // the compiler has run
    master_error_log mel;
    file_map filemap;
    global_ptr_map gpm;

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
    scope global_scope;
    aseglist module_ctors;
    aseglist module_dtors;
    target_platform host_target;
    target_platform target;
    optimization_strategy opt_strat;
    ureg verbosity_flags;
    timer total_time;
    list required_files; // used to get 'roots of trust' for the root module
    const char* output_path;
    bool emit_ll;
    bool emit_asm;
    bool emit_exe;
    bool emit_ast;
    bool emit_objs;
    bool needs_emit_stage;
    bool explicit_exe;
    bool trap_on_error;
    bool ok_on_error;
    bool debug_symbols;
    bool explicit_debug_symbols;
} tauc;

// THREADSAFE
int tauc_request_parse(
    tauc* t, src_file* f, src_map* requiring_smap, src_range requiring_stmt);
int tauc_request_resolve_single(
    tauc* t, mdg_node* node, partial_resolution_data* prd);
int tauc_request_resolve_multiple(
    tauc* t, mdg_node** start, mdg_node** end, partial_resolution_data* prd);
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

#define TAU_TIME_STAGE_CTX(t, code, code_before_msg)                           \
    do {                                                                       \
        tauc* tp = t;                                                          \
        if (tp->verbosity_flags & VERBOSITY_FLAGS_STAGE_BEGINS) {              \
            code_before_msg;                                                   \
            tput("@");                                                         \
            pretty_print_timer_elapsed(&tp->total_time);                       \
            tputs("");                                                         \
            tflush();                                                          \
        }                                                                      \
        if ((t)->verbosity_flags & VERBOSITY_FLAGS_TIME_STAGES) {              \
            TIME_MSG_LN(code, code_before_msg);                                \
        }                                                                      \
        else {                                                                 \
            code;                                                              \
        }                                                                      \
    } while (false)

#define TAU_TIME_STAGE(t, code) TAU_TIME_STAGE_CTX(t, , code, )
