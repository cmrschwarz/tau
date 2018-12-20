#include "error_log.h"
#include "tauc.h"

#define ALLOCATION_FAILURE_NONE NULL
#define ALLOCATION_FAILURE_FIRST ((void*)UREG_MAX)

static master_error_log MASTER_ERROR_LOG;

int master_error_log_init(){
    int r = atomic_bool_init(&MASTER_ERROR_LOG.error_occured, false); 
    if(r) return -1;
    r = mutex_init(&MASTER_ERROR_LOG.mtx);
    if(r){
        atomic_bool_fin(&MASTER_ERROR_LOG.error_occured);
        return -2;
    }
    MASTER_ERROR_LOG.error_log_error = 0;
    return 0;
}
void master_error_log_fin(){
    mutex_fin(&MASTER_ERROR_LOG.mtx);
    atomic_bool_fin(&MASTER_ERROR_LOG.error_occured);
}

int error_log_init(error_log* el){
    if(el == NULL){
        MASTER_ERROR_LOG.error_log_error = -1;
        return NULL;
    }
    int r = mutex_lock(&MASTER_ERROR_LOG.mtx);
    if(r){
        MASTER_ERROR_LOG.error_log_error = -2;
        return NULL;
    }
    el->next = MASTER_ERROR_LOG.error_logs;
    MASTER_ERROR_LOG.error_logs = el;
    mutex_unlock(&MASTER_ERROR_LOG.mtx);
    el->errors = NULL;
    el->next = NULL;
    el->allocation_failure_point = ALLOCATION_FAILURE_NONE;
    return el;
}
void error_log_fin(error_log* el){
    //THINK
    el->errors = NULL;
    el->allocation_failure_point = ALLOCATION_FAILURE_NONE;
}

void error_log_report(error_log* el, error* e){
    atomic_bool_store(&MASTER_ERROR_LOG.error_occured, true);
    e->previous = el->errors;
    el->errors = e;
}
void error_log_report_allocation_failiure(error_log* tel){
    if(tel->allocation_failure_point)
    atomic_bool_store(&MASTER_ERROR_LOG.error_occured, true);
    if(tel->errors != NULL){
        tel->allocation_failure_point = tel->errors;
    }
    else{
        tel->allocation_failure_point = ALLOCATION_FAILURE_FIRST;
    }
}

int master_error_log_unwind(int r){
    //TODO
    return 0;
}