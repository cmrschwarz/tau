#pragma once
#include "file_map.h"
#include "mdg.h"
#include "utils/c_extensions.h"
#include "utils/threading.h"

typedef enum job_queue_result {
    JQR_SUCCESS = 0,
    JQR_SUCCESS_WITH_REINFORCEMENTS_REQUEST,
    JQR_DONE,
    JQR_ERROR,
} job_queue_result;

typedef enum PACK_ENUM job_type {
    JOB_PARSE,
    JOB_RESOLVE,
} job_type;

typedef struct job_parse {
    src_file* file;
} job_parse;

typedef struct job_resolve {
    mdg_node* node;
} job_resolve;

typedef struct job {
    job_type type;
    union concrete {
        job_parse parse;
        job_resolve resolve;
    } concrete;
} job;

typedef struct job_queue {
    job* buffer;
    job* buffer_end;
    job* head;
    job* tail;
    ureg idle_threads_count;
    ureg threads_count;
    cond_var has_jobs;
    mutex lock;
} job_queue;

int job_queue_init(job_queue* jq);
void job_queue_fin(job_queue* jq);

job_queue_result job_queue_request_parse(job_queue* jq, src_file* f);
job_queue_result job_queue_request_resolve(job_queue* jq, mdg_node* node);

job_queue_result job_queue_pop(job_queue* jq, job* j);

void job_queue_inform_thread_added(job_queue* jq);

// debugging utility
void job_queue_force_done(job_queue* jq);