#include "../../panic.h"
#include "../../threading.h"
#if OS_LINUX

#include <pthread.h>
#include <stdio.h>
#include <time.h>
#include <unistd.h>
int mutex_init(mutex* m)
{
    return pthread_mutex_init(m, NULL);
}
bool mutex_try_lock(mutex* m)
{
    return pthread_mutex_trylock(m) == 0;
}
void mutex_lock(mutex* m)
{
    if (pthread_mutex_lock(m)) panic("mutex lock failed");
}
void mutex_unlock(mutex* m)
{
    if (pthread_mutex_unlock(m)) panic("mutex unlock failed");
}
void mutex_fin(mutex* m)
{
    if (pthread_mutex_destroy(m)) panic("mutex deallocation failed");
}

int rwlock_init(rwlock* m)
{
    return pthread_rwlock_init(m, NULL);
}
void rwlock_try_read(rwlock* m)
{
    return (pthread_rwlock_tryrdlock(m) == 0);
}
void rwlock_begin_read(rwlock* m)
{
    if (pthread_rwlock_rdlock(m)) panic("rwlock read lock failed");
}
void rwlock_end_read(mutex* m)
{
    if (pthread_rwlock_unlock(m)) panic("rwlock read unlock failed");
}
void rwlock_try_write(rwlock* m)
{
    return (pthread_rwlock_trywrlock(m) == 0);
}
void rwlock_begin_write(rwlock* m)
{
    if (pthread_rwlock_wrlock(m)) panic("rwlock write lock failed");
}
void rwlock_end_write(rwlock* m)
{
    if (pthread_rwlock_unlock(m)) panic("rwlock write unlock failed");
}
void rwlock_fin(mutex* m)
{
    if (pthread_mutex_destroy(m)) panic("rwlock deallocation failed");
}

int cond_var_init(cond_var* cv)
{
    return pthread_cond_init(cv, NULL);
}
void cond_var_wait(cond_var* cv, mutex* m)
{
    if (pthread_cond_wait(cv, m)) panic("cond var wait failed");
}
void cond_var_notify_one(cond_var* cv)
{
    if (pthread_cond_signal(cv)) panic("cond var notify one failed");
}
void cond_var_notify_all(cond_var* cv)
{
    if (pthread_cond_broadcast(cv)) panic("cond var notify all failed");
}
void cond_var_fin(cond_var* cv)
{
    if (pthread_cond_destroy(cv)) panic("cond var deallocation failed");
}

int thread_yield()
{
    return sched_yield();
}
int thread_sleep(ureg microsecs)
{
    return usleep(microsecs);
}

static void* thread_wrapper(void* ctx)
{
    thread* t = (thread*)ctx;
    t->thread_fn(t->context);
    return NULL;
}
int thread_launch(thread* t, thread_function_ptr thread_fn, void* context)
{
    t->thread_fn = thread_fn;
    t->context = context;
    return pthread_create(&t->pthread, NULL, thread_wrapper, (void*)t);
}
int thread_join(thread* t)
{
    return pthread_join(t->pthread, NULL);
}
int thread_detach(thread* t)
{
    return pthread_detach(t->pthread);
}

#endif