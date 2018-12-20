#pragma once
#include "utils/allocator.h"
#include "utils/pool.h"
#include "utils/threading.h"
#include "tokenizer.h"
#include "error_log.h"

typedef struct stage_1_share{
    mutex share_lock;
    sbuffer files;
    sbi unparsed_file;
}stage_1_share;

typedef struct stage_1{
    tokenizer tk;
    stage_1_share* share;
}stage_1;

struct tauc;
typedef struct thread_context{
    struct tauc* tauc;
    thread_allocator tal;
    error_log error_log;
    pool permmem;
    pool stagemem;
    union{
        stage_1 s1;
    } stage;
}thread_context;

typedef struct worker_thread{
    struct worker_thread* next;
    thread_context tc;
    thread thread;
}worker_thread;

typedef struct tauc{
    thread_context main_thread_context;
    worker_thread* worker_threads;
    pool permmem;
    union {
        stage_1_share s1;
    }stage_share;
}tauc;

int tauc_init(tauc* tauc, int argc, char** argv);
void tauc_fin(tauc* tauc);

int worker_thread_init(worker_thread* wt, tauc* tauc);
void worker_thread_fin(worker_thread* wt);
