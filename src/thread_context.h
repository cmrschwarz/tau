#pragma once
#include "utils/allocator.h"
#include "utils/pool.h"
#include "utils/threading.h"
#include "tokenizer.h"
#include "error_log.h"

struct tauc;

typedef struct stage_1_share{
    mutex share_lock;
    sbuffer files;
    sbi unparsed_file;
}stage_1_share;

typedef struct stage_1{
    tokenizer tk;
    stage_1_share* share;
}stage_1;

typedef struct thread_context{
    thread_allocator tal;
    thread_error_log tel;
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

typedef struct{
    thread_allocator* tal;
    thread_context* threads;

    pool permmem;
    union {
        stage_1_share s1;
    }stage_share;
}tauc;

int thread_context_init(thread_context* tc);
void thread_context_fin(thread_context* tc);

int worker_thread_init(worker_thread* tc);
void worker_thread_fin(worker_thread* tc);