#include "tauc.h"
#include "error_log.h"
#include "file_map.h"
#include "print_ast.h"
#include "utils/allocator.h"

struct tauc TAUC;

static inline int tauc_partial_fin(int r, int i)
{
    switch (i) {
        case 4: file_map_fin(&TAUC.file_map);
        case 3: mdg_fin(&TAUC.mdg);
        case 2: pool_fin(&TAUC.permmem);
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
    r = pool_init(&TAUC.permmem);
    if (r) return tauc_partial_fin(r, 1);
    r = mdg_init(&TAUC.mdg);
    if (r) return tauc_partial_fin(r, 2);
    r = file_map_init(&TAUC.file_map);
    if (r) return tauc_partial_fin(r, 3);
    return OK;
}
void tauc_fin()
{
    tauc_partial_fin(4, 0);
}

int tauc_run(int argc, char** argv)
{
    int r = parser_init(
        &TAUC.main_thread_context.stage.s1.p, &TAUC.main_thread_context);
    if (r) return ERR;
    if (argc < 2) return 0;
    for (int i = 1; i < argc; i++) {
        src_file* f = file_map_get_file_from_path(
            &TAUC.file_map, string_from_cstr(argv[i]));
        if (!f) return ERR;
        if (parser_parse_file(&TAUC.main_thread_context.stage.s1.p, f))
            return ERR;
        print_astn((stmt*)&TAUC.main_thread_context.stage.s1.p.root, 0);
    }
    return OK;
}

void thread_context_fin(thread_context* tc)
{
    pool_fin(&tc->stagemem);
    pool_fin(&tc->permmem);
    error_log_fin(&tc->error_log);
}

int thread_context_init(thread_context* tc)
{
    int r = pool_init(&tc->permmem);
    if (r) {
        master_error_log_report("thread setup error: memory allocation failed");
        return r;
    }
    r = pool_init(&tc->stagemem);
    if (r) {
        pool_fin(&tc->permmem);
        master_error_log_report("thread setup error: memory allocation failed");
        return r;
    }
    error_log_init(&tc->error_log, &tc->permmem);
    return OK;
}

void worker_thread_fin(worker_thread* wt)
{
    thread_context_fin(&wt->tc);
}
