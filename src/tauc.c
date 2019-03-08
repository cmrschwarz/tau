#include "tauc.h"
#include "error_log.h"
#include "print_ast.h"
#include "src_file.h"
#include "utils/allocator.h"

struct tauc TAUC;

int tauc_init()
{
    TAUC.worker_threads = NULL;
    int r = thread_context_init(&TAUC.main_thread_context);
    if (r) return ERR;
    r = pool_init(&TAUC.permmem, &TAUC.main_thread_context.tal);
    if (!r) {
        r = mdg_init(&TAUC.mdg, &TAUC.main_thread_context.tal);
        if (r) pool_fin(&TAUC.permmem);
    }
    if (r) {
        thread_context_fin(&TAUC.main_thread_context);
        master_error_log_report("thread setup error: memory allocation failed");
        return ERR;
    }
    return OK;
}

int test_tokenizer(src_file* f)
{
    if (tk_open_file(&TAUC.main_thread_context.stage.s1.p.tk, f)) return ERR;
    token* t;
    do {
        t = tk_consume(&TAUC.main_thread_context.stage.s1.p.tk);
        token_debug_print(f, t);
    } while (t != NULL && t->type != TT_EOF);
    return tk_close_file(&TAUC.main_thread_context.stage.s1.p.tk);
}
int tauc_run(int argc, char** argv)
{
    int r = parser_init(
        &TAUC.main_thread_context.stage.s1.p, &TAUC.main_thread_context);
    if (r) return ERR;
    src_file* f = (src_file*)pool_alloc(&TAUC.permmem, sizeof(src_file));
    if (!f) return -1;
    if (src_file_init(f, &TAUC.main_thread_context, "test/test.tau"))
        return ERR;
    if (parser_parse_file(&TAUC.main_thread_context.stage.s1.p, f)) return ERR;
    print_astn((stmt*)&TAUC.main_thread_context.stage.s1.p.root, 0);
    return OK;
}

void tauc_fin_temps()
{
}

void tauc_fin()
{
    pool_fin(&TAUC.permmem);
    thread_context_fin(&TAUC.main_thread_context);
}

void thread_context_fin(thread_context* tc)
{
    pool_fin(&tc->stagemem);
    pool_fin(&tc->permmem);
    tal_fin(&tc->tal);
    error_log_fin(&tc->error_log);
}

int thread_context_init(thread_context* tc)
{
    int r = tal_init(&tc->tal);
    if (r) {
        master_error_log_report(
            "thread setup error: allocator initialization failed");
        return r;
    }
    r = pool_init(&tc->permmem, &tc->tal);
    if (r) {
        tal_fin(&tc->tal);
        master_error_log_report("thread setup error: memory allocation failed");
        return r;
    }
    r = pool_init(&tc->stagemem, &tc->tal);
    if (r) {
        pool_fin(&tc->permmem);
        tal_fin(&tc->tal);
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
