#pragma once

#ifndef __USE_UNIX98
#define __USE_UNIX98
#include <pthread.h>
#undef __USE_UNIX98
#else
#include <pthread.h>
#endif

typedef struct thread_s {
    pthread_t pthread;
    thread_function_ptr thread_fn;
    void* context;
} thread;

typedef pthread_mutex_t mutex;
typedef pthread_cond_t cond_var;
typedef pthread_rwlock_t rwlock;

