#pragma once
#include "plattform.h"
#include "types.h"

typedef void (*thread_function_ptr)(void* context);
#if OS_LINUX
#include "os/linux/threading_linux.h"
#else
#error no threading backend for configured plattform
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

// Atomics
// add,sub,inc and dec return the previously stored value

int atomic_ptr_init(atomic_ptr* a, void* value);
void* atomic_ptr_load(atomic_ptr* a);
void atomic_ptr_store(atomic_ptr* a, void* value);
bool atomic_ptr_cas(atomic_ptr* a, void** oldval, void* newval);
void atomic_ptr_fin(atomic_ptr* a);

int atomic_bool_init(atomic_bool* a, bool value);
bool atomic_bool_load(atomic_bool* a);
void atomic_bool_store(atomic_bool* a, bool value);
bool atomic_bool_cas(atomic_bool* a, bool* oldval, bool newval);
void atomic_bool_fin(atomic_bool* a);

int atomic_ureg_init(atomic_ureg* a, ureg value);
ureg atomic_ureg_load(atomic_ureg* a);
void atomic_ureg_store(atomic_ureg* a, ureg value);
ureg atomic_ureg_inc(atomic_ureg* a);
ureg atomic_ureg_dec(atomic_ureg* a);
ureg atomic_ureg_add(atomic_ureg* a, ureg v);
ureg atomic_ureg_sub(atomic_ureg* a, ureg v);
bool atomic_ureg_cas(atomic_ureg* a, ureg* oldval, ureg newval);
void atomic_ureg_fin(atomic_ureg* a);

int atomic_sreg_init(atomic_sreg* a, sreg value);
sreg atomic_sreg_load(atomic_sreg* a);
void atomic_sreg_store(atomic_sreg* a, sreg value);
sreg atomic_sreg_inc(atomic_sreg* a);
sreg atomic_sreg_dec(atomic_sreg* a);
sreg atomic_sreg_add(atomic_sreg* a, sreg v);
sreg atomic_sreg_sub(atomic_sreg* a, sreg v);
bool atomic_sreg_cas(atomic_sreg* a, sreg* oldval, sreg newval);
void atomic_sreg_fin(atomic_sreg* a);
