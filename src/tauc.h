#pragma once
#include "utils/allocator.h"
#include "utils/pool.h"
#include "utils/threading.h"
#include "tokenizer.h"

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
    struct thread_context* next;
    thread thread;
    thread_allocator tal;
    pool permmem;
    pool stagemem;
    union{
        stage_1 s1;
    } stage;
}thread_context;


typedef struct{
    thread_allocator* tal;
    thread_context* threads;

    pool permmem;
    union {
        stage_1_share s1;
    }stage_share;
}tauc;