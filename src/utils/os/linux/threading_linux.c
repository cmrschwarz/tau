#include "../../threading.h"
#if OS_LINUX

#include <stdio.h>
#include <time.h>
#include <unistd.h>
int mutex_init(mutex* m){
    return pthread_mutex_init(m, NULL);
}
bool mutex_try_lock(mutex* m){
    return pthread_mutex_trylock(m) == 0; 
}
int mutex_lock(mutex* m){
    return pthread_mutex_lock(m);
}
int mutex_unlock(mutex* m){
    return pthread_mutex_unlock(m);
}
void mutex_fin(mutex* m){
    pthread_mutex_destroy(m);
}

int thread_yield(){
    return sched_yield();
}
int thread_sleep(ureg microsecs){
    return usleep(microsecs);
}

static void* thread_wrapper(void* ctx){
    thread* t = ctx;
    t->thread_fn(t->context);
    return NULL;
}

int thread_launch(thread* t, thread_function_ptr thread_fn, void* context){
    t->thread_fn = thread_fn;
    t->context = context;
    return pthread_create(&t->pthread, NULL, thread_wrapper, (void*)t);
}
int thread_join(thread* t){
    return pthread_join(t->pthread, NULL);
}
int thread_detach(thread* t){
    return pthread_detach(t->pthread);
}

#endif