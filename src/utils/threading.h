#include "system.h"
#include "types.h"

//Threads
typedef void (*thread_function_ptr) (void* context);

#if OS_LINUX
#include <pthread.h>
typedef struct {
    pthread_t pthread;
    thread_function_ptr thread_fn;
    void* context;
}thread;
#elif OS_WINDOWS
//TODO
#endif

int thread_yield();
int thread_sleep(ureg millis);

int thread_launch(thread* t, thread_function_ptr thread_fn, void* context);
int thread_join(thread* t);
int thread_detach(thread* t);


//Mutexes
#if OS_LINUX
typedef  pthread_mutex_t mutex; 
#else 
//TODO
#endif

int mutex_init(mutex* m);
bool mutex_try_lock(mutex* m);
int mutex_lock(mutex* m);
int mutex_unlock(mutex* m);
void mutex_fin(mutex* m);


//Atomics
#include "stdatomic.h"
typedef _Atomic ureg atomic_ureg;
typedef _Atomic sreg atomic_sreg;
typedef _Atomic bool atomic_bool;

void atomic_ureg_init(atomic_ureg* a, ureg value);
void atomic_ureg_store(atomic_ureg* a, ureg value);
ureg atomic_ureg_load(atomic_ureg* a, ureg value);
void atomic_ureg_fin(atomic_ureg* a, ureg value);

void atomic_sreg_init(atomic_ureg* a, sreg value);
void atomic_sreg_store(atomic_ureg* a, sreg value);
ureg atomic_sreg_load(atomic_ureg* a, sreg value);
void atomic_sreg_fin(atomic_ureg* a, sreg value);

void atomic_bool_init(atomic_ureg* a, bool value);
void atomic_bool_store(atomic_ureg* a, bool value);
ureg atomic_bool_load(atomic_ureg* a, bool value);
void atomic_bool_fin(atomic_ureg* a, bool value);