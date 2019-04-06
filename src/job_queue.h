#pragma once
#include "file_map.h"
#include "mdg.h"
#include "utils/c_extensions.h"
#include "utils/threading.h"
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
    mutex lock;
} job_queue;

int job_queue_init(job_queue* jq);
void job_queue_fin(job_queue* jq);

int job_queue_request_parse(job_queue* jq, src_file* f);
int job_queue_request_resolve(job_queue* jq, mdg_node* node);

int job_queue_pop(job_queue* jq, job* j);