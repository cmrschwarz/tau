#pragma once
#include "file_map.h"
#include "mdg.h"
#include "utils/c_extensions.h"
#include "utils/error.h"
#include "utils/threading.h"

typedef enum PACK_ENUM job_type {
    JOB_PARSE,
    JOB_RESOLVE_SINGLE,
    JOB_RESOLVE_MULTIPLE,
    JOB_FINALIZE,
} job_type;

typedef struct job_parse {
    src_file* file;
    src_file* requiring_file;
    src_range requiring_srange;
} job_parse;

typedef struct job_resolve_single {
    mdg_node* node;
} job_resolve_single;

typedef struct job_resolve_multiple {
    mdg_node** start;
    mdg_node** end;
} job_resolve_multiple;

typedef struct job {
    job_type type;
    union concrete {
        job_parse parse;
        job_resolve_single resolve_single;
        job_resolve_multiple resolve_multiple;
    } concrete;
} job;

typedef struct job_queue {
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
