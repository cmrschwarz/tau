#pragma once

#include "src_map.h"
#include "mdg.h"
#include "utils/c_extensions.h"
#include "utils/error.h"
#include "utils/threading.h"

typedef enum PACK_ENUM job_kind_s {
    JOB_PARSE,
    JOB_RESOLVE,
    JOB_FINALIZE,
    JOB_LOAD_PP,
} job_kind;

typedef struct job_parse_s {
    src_file* file;
    src_map* requiring_smap;
    src_range requiring_srange;
} job_parse;

typedef struct job_resolve_s {
    mdg_node** start;
    mdg_node** end;
    mdg_node* single_store;
    partial_resolution_data* partial_res_data;
} job_resolve;

typedef struct job_load_pp_s {
    mdg_node* node;
} job_load_pp;

typedef struct job_s {
    job_kind kind;
    union {
        job_parse parse;
        job_resolve resolve;
        job_load_pp load_pp;
    } concrete;
} job;

// TODO: have separate job lists per thread so that we can give a
// thread affinity to partial resolutions (-> no private symbol redo in llvm)
// also have a 'beta queue' for resolution of mdgs with resolved but ungenerated
// deps so other jobs are preferred
typedef struct job_queue_s {
    job* buffer;
    job* buffer_end;
    job* head;
    job* tail;
    ureg waiters;
    ureg preorders;
    ureg jobs;
    cond_var has_jobs;
    mutex lock;
} job_queue;

int job_queue_init(job_queue* jq);
void job_queue_fin(job_queue* jq);

#define JQ_DONE STATUS_1
#define JQ_NONE STATUS_2
#define JQ_WAITER_COUNT_REACHED STATUS_2

// these return OK, ERR, or JQ_DONE
int job_queue_push(job_queue* jq, const job* jb, ureg* waiters, ureg* jobs);

// returns OK, ERR or JQ_WAITER_COUNT_REACHED
int job_queue_pop(
    job_queue* jq, job* j, bool has_preordered, ureg break_on_waiter_count);

// returns OK or ERR
int job_queue_preorder_job(job_queue* jq);

// returns OK or JQ_NONE
int job_queue_try_pop(job_queue* jq, job* j);

void job_queue_check_waiters(job_queue* jq, ureg break_on_waiter_count);

void job_queue_stop(job_queue* jq);
