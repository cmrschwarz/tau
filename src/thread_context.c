#include "thread_context.h"
#include "utils/allocator.h"
#include "utils/pool.h"

int thread_context_init(thread_context* tc){
    int r = tal_init(&tc->tal);
    if(r) return -1;
    r = pool_init(&tc->permmem, &tc->tal);
    if(r){
        tal_fin(&tc->tal);
        return -1;
    }
    r = pool_init(&tc->stagemem, &tc->tal);
    if(r){
        pool_fin(&tc->permmem);
        tal_fin(&tc->tal);
        return -1;
    }
    return 0;
}
void thread_context_fin(thread_context* tc){
    pool_fin(&tc->stagemem);
    pool_fin(&tc->permmem);
    tal_fin(&tc->tal);
}

int worker_thread_init(worker_thread* wt){
    int r = thread_context_init(&wt->tc);
    if (r) return r;
    return 0;
}

void worker_thread_fin(worker_thread* wt){
    thread_context_fin(&wt->tc);
}