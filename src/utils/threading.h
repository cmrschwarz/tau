#pragma once

#include "plattform.h"
#include "types.h"
#include "atomics.h"

typedef void (*thread_function_ptr)(void* context);
#if HOST_OS_LINUX || HOST_OS_OSX
#include "plattform/linux/threading_linux.h"
#elif HOST_OS_WINDOWS
#include "plattform/windows/threading_windows.h"
#else
#error tauc has no threading backend for configured plattform
#endif

#if __cplusplus
#define THREAD_LOCAL thread_local
#elif CMPLR_GCC || CMPLR_CLANG
#define THREAD_LOCAL __thread
#elif CMPLR_MSVC
#define THREAD_LOCAL __declspec( thread )
#elif !defined(__STDC_NO_THREADS__)
#include <threads.h>
#define THREAD_LOCAL thread_local
#else
#error tauc has no TLS configuration on this plattform
#endif

int thread_yield();
int thread_sleep(ureg microsecs);

ureg thread_id();
int thread_launch(thread* t, thread_function_ptr thread_fn, void* context);
int thread_join(thread* t);
int thread_detach(thread* t);

int mutex_init(mutex* m);
bool mutex_try_lock(mutex* m);
void mutex_lock(mutex* m);
void mutex_unlock(mutex* m);
void mutex_fin(mutex* m);

int rwlock_init(rwlock* rwl);
bool rwlock_try_read(rwlock* rwl);
void rwlock_read(rwlock* rwl);
void rwlock_end_read(rwlock* rwl);
bool rwlock_try_write(rwlock* rwl);
void rwlock_write(rwlock* rwl);
void rwlock_end_write(rwlock* rwl);
void rwlock_fin(rwlock* rwl);

int cond_var_init(cond_var* cv);
void cond_var_wait(cond_var* cv, mutex* m);
void cond_var_notify_one(cond_var* cv);
void cond_var_notify_all(cond_var* cv);
void cond_var_fin(cond_var* cv);
