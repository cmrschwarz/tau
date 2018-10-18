#include "error_log.h"
#include "thread_context.h"
#include "tauc.h"

#define ALLOCATION_FAILURE_NONE NULL
#define ALLOCATION_FAILURE_FIRST ((void*)UREG_MAX)

void error_log_report(struct thread_context* tc, error* e){
    atomic_bool_store(&TAUC.error_log.error_occured, true);
    e->previous = tc->tel.errors;
    tc->tel.errors = e;
}
void error_log_report_allocation_failiure(struct thread_context* tc){
    if(tc->tel.errors != NULL){
        tc->tel.allocation_failure_point = tc->tel.errors;
    }
    else{
        tc->tel.allocation_failure_point = ALLOCATION_FAILURE_FIRST;
    }
}

int error_log_init(error_log* el){
    int r = mutex_init(&el->mtx);
    if(r) return r;
    atomic_bool_init(&el->error_occured, false); 
    return 0;
}
void error_log_fin(error_log* el){
    atomic_bool_fin(&el->error_occured);
}
int thread_error_log_init(error_log* el, thread_error_log* tel){
    int r = mutex_lock(&el->mtx);
    if(r)return r;
    tel->next = el->thread_error_logs;
    el->thread_error_logs = tel;
    mutex_unlock(&el->mtx);
    tel->errors = NULL;
    tel->next = NULL;
    tel->allocation_failure_point = ALLOCATION_FAILURE_NONE;
    return 0;
}
void thread_error_log_fin(thread_error_log* tel){

}