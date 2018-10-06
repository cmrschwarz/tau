#pragma once
#include "plattform.h"
#include "types.h"

typedef void (*thread_function_ptr) (void* context);
#if OS_LINUX
#include "os/linux/threading_linux.h"
#else
#error no threading backend for configured plattform
#endif

//Threads
int thread_yield();
int thread_sleep(ureg microsecs);

int thread_launch(thread* t, thread_function_ptr thread_fn, void* context);
int thread_join(thread* t);
int thread_detach(thread* t);


//Mutexes
int mutex_init(mutex* m);
bool mutex_try_lock(mutex* m);
int mutex_lock(mutex* m);
int mutex_unlock(mutex* m);
void mutex_fin(mutex* m);


//Atomics
void atomic_ureg_init(atomic_ureg* a, ureg value);
void atomic_ureg_store(atomic_ureg* a, ureg value);
ureg atomic_ureg_load(atomic_ureg* a, ureg value);
void atomic_ureg_fin(atomic_ureg* a, ureg value);

void atomic_sreg_init(atomic_ureg* a, sreg value);
void atomic_sreg_store(atomic_ureg* a, sreg value);
sreg atomic_sreg_load(atomic_ureg* a, sreg value);
void atomic_sreg_fin(atomic_ureg* a, sreg value);

void atomic_bool_init(atomic_ureg* a, bool value);
void atomic_bool_store(atomic_ureg* a, bool value);
bool atomic_bool_load(atomic_ureg* a, bool value);
void atomic_bool_fin(atomic_ureg* a, bool value);