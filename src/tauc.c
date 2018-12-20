#include "tauc.h"
#include "error_log.h"
#include "utils/allocator.h"

int tauc_init(tauc* tauc, int argc, char** argv){
    tauc->worker_threads = NULL;
    int r = error_log_init(&tauc->main_thread_context.error_log);
    if(r) return r;
    r = tal_init(&tauc->main_thread_context.tal);
    if(r){
        error_log_fin(&tauc->main_thread_context.error_log);
        return r;
    }
    r = pool_init(&tauc->main_thread_context.permmem, &tauc->main_thread_context.tal);
    if(r){
        tal_fin(&tauc->main_thread_context.tal);
        error_log_fin(&tauc->main_thread_context.error_log);
        return r;
    }
    r = pool_init(&tauc->main_thread_context.stagemem, &tauc->main_thread_context.tal);
    if(r){
        pool_fin(&tauc->main_thread_context.permmem);
        tal_fin(&tauc->main_thread_context.tal);
        error_log_fin(&tauc->main_thread_context.error_log);
        return r;
    }
    return 0;
}

void tauc_fin(tauc* tauc){
    pool_fin(&tauc->permmem);
}

int thread_context_init(thread_context* tc, tauc* tauc){
   
}
void thread_context_fin(thread_context* tc){
    pool_fin(&tc->stagemem);
    pool_fin(&tc->permmem);
    tal_fin(&tc->tal);
    error_log_fin(&tc->error_log);
}

int worker_thread_init(worker_thread* wt, tauc* tauc){
    int r = error_log_init(&wt->tc.error_log);
    if(r) return r;
    r = tal_init(&wt->tc.tal);
    if(r){
        error_log_fin(&wt->tc.error_log);
        return r;
    }
    r = pool_init(&wt->tc.permmem, &wt->tc.tal);
    if(r){
        tal_fin(&wt->tc.tal);
        error_log_fin(&wt->tc.error_log);
        return r;
    }
    r = pool_init(&wt->tc.stagemem, &wt->tc.tal);
    if(r){
        pool_fin(&wt->tc.permmem);
        tal_fin(&wt->tc.tal);
        error_log_fin(&wt->tc.error_log);
        return r;
    }
    wt->next = tauc->worker_threads;
    return 0;
}

void worker_thread_fin(worker_thread* wt){
    thread_context_fin(&wt->tc);
}