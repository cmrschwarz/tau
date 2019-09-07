#ifndef TAUC_UTILS_THREADING_H
#define TAUC_UTILS_THREADING_H

#include "plattform.h"
#include "types.h"
#include "atomics.h"

typedef void (*thread_function_ptr)(void* context);
#if OS_LINUX
#include "os/linux/threading_linux.h"
#else
#error no threading backend for configured plattform
#endif

#if __cplusplus
#define THREAD_LOCAL thread_local
#elif !defined(__STDC_NO_THREADS__)
#include <threads.h>
#define THREAD_LOCAL thread_local
#elif CMPLR_GCC || CMPLR_CLANG
#define THREAD_LOCAL __thread
#endif

// Threads
int thread_yield();
int thread_sleep(ureg microsecs);

int thread_launch(thread* t, thread_function_ptr thread_fn, void* context);
int thread_join(thread* t);
int thread_detach(thread* t);

// Mutexes
int mutex_init(mutex* m);
bool mutex_try_lock(mutex* m);
void mutex_lock(mutex* m);
void mutex_unlock(mutex* m);
void mutex_fin(mutex* m);

// Condition variables
int cond_var_init(cond_var* cv);
void cond_var_wait(cond_var* cv, mutex* m);
void cond_var_notify_one(cond_var* cv);
void cond_var_notify_all(cond_var* cv);
void cond_var_fin(cond_var* cv);

#endif