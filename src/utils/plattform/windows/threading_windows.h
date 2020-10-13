#pragma once
#include "sane_windows.h"

typedef struct thread_s {
    HANDLE thrd;
    thread_function_ptr thread_fn;
    void* context;
} thread;

typedef CRITICAL_SECTION mutex;
typedef SRWLOCK rwlock;
typedef CONDITION_VARIABLE cond_var;