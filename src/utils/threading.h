#pragma once
#include "plattform.h"
#include "types.h"

typedef void (*thread_function_ptr)(void* context);
#if OS_LINUX
#    include "os/linux/threading_linux.h"
#else
#    error no threading backend for configured plattform
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
int mutex_lock(mutex* m);
void mutex_unlock(mutex* m);
void mutex_fin(mutex* m);

// Atomics
int atomic_ureg_init(atomic_ureg* a, ureg value);
void atomic_ureg_store(atomic_ureg* a, ureg value);
ureg atomic_ureg_load(atomic_ureg* a, ureg value);
void atomic_ureg_fin(atomic_ureg* a);

int atomic_sreg_init(atomic_sreg* a, sreg value);
void atomic_sreg_store(atomic_sreg* a, sreg value);
sreg atomic_sreg_load(atomic_sreg* a, sreg value);
void atomic_sreg_fin(atomic_sreg* a);

int atomic_bool_init(atomic_bool* a, bool value);
void atomic_bool_store(atomic_bool* a, bool value);
bool atomic_bool_load(atomic_bool* a, bool value);
void atomic_bool_fin(atomic_bool* a);