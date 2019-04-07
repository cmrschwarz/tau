#pragma once
#include <pthread.h>

// use libc's atomics for now
#include "../libc/atomics_libc.h"

typedef struct {
    pthread_t pthread;
    // thread_function_ptr is defined in threading.h before this is included
    thread_function_ptr thread_fn;
    void* context;
} thread;

typedef pthread_mutex_t mutex;
typedef pthread_cond_t cond_var;