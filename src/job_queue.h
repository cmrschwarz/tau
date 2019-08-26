#ifndef TAUC_JOB_QUEUE_H
#define TAUC_JOB_QUEUE_H

#include "file_map.h"
#include "mdg.h"
#include "utils/c_extensions.h"
#include "utils/error.h"
#include "utils/threading.h"

typedef enum PACK_ENUM job_kind_s {
    JOB_PARSE,
    JOB_RESOLVE,
    JOB_FINALIZE,
} job_kind;

typedef struct job_parse_s {
    src_file* file;
    src_file* requiring_file;
    src_range requiring_srange;
} job_parse;

typedef struct job_resolve {
    mdg_node** start;
    mdg_node** end;
    mdg_node* single_store;
} job_resolve;

typedef struct job_s {
    job_kind kind;
    union {
        job_parse parse;
        job_resolve resolve;
    } concrete;
} job;

typedef struct job_queue_s {
    job* buffer;
    job* buffer_end;
    job* head;
    job* tail;
    ureg waiters;
    ureg jobs;
    cond_var has_jobs;
    mutex lock;
} job_queue;

int job_queue_init(job_queue* jq);
void job_queue_fin(job_queue* jq);

#define JQ_DONE STATUS_1
#define JQ_NONE STATUS_2
// these return OK, ERR, or JQ_DONE
int job_queue_push(job_queue* jq, const job* jb, ureg* waiters, ureg* jobs);
int job_queue_pop(job_queue* jq, job* j);
int job_queue_try_pop(job_queue* jq, job* j);

void job_queue_stop(job_queue* jq);
#endif