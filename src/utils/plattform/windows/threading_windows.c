#include "../../panic.h"
#include "../../threading.h"
#if HOST_OS_WINDOWS
#include "../../error.h"
#include "sane_windows.h"

int mutex_init(mutex* m)
{
    InitializeCriticalSection(m);
    return OK;
}
bool mutex_try_lock(mutex* m)
{
    return TryEnterCriticalSection(m);
}
void mutex_lock(mutex* m)
{
    EnterCriticalSection(m);
}
void mutex_unlock(mutex* m)
{
    LeaveCriticalSection(m);
}
void mutex_fin(mutex* m)
{
    DeleteCriticalSection(m);
}

int rwlock_init(rwlock* rwl)
{
    InitializeSRWLock(rwl);
    return OK;
}
bool rwlock_try_read(rwlock* rwl)
{
    return TryAcquireSRWLockShared(rwl);
}
void rwlock_read(rwlock* rwl)
{
    AcquireSRWLockShared(rwl);
}
void rwlock_end_read(rwlock* rwl)
{
    ReleaseSRWLockShared(rwl);
}
bool rwlock_try_write(rwlock* rwl)
{
    return TryAcquireSRWLockExclusive(rwl);
}
void rwlock_write(rwlock* rwl)
{
    AcquireSRWLockExclusive(rwl);
}
void rwlock_end_write(rwlock* rwl)
{
    ReleaseSRWLockExclusive(rwl);
}
void rwlock_fin(rwlock* rwl)
{
    //nothing to do
}

int cond_var_init(cond_var* cv)
{
    InitializeConditionVariable(cv);
    return OK;
}
void cond_var_wait(cond_var* cv, mutex* m)
{
    if (SleepConditionVariableCS(cv, m, INFINITE)) panic("cond var wait failed");
}
void cond_var_notify_one(cond_var* cv)
{
    WakeConditionVariable(cv);
}
void cond_var_notify_all(cond_var* cv)
{
    WakeAllConditionVariable(cv);
}
void cond_var_fin(cond_var* cv)
{
    //nothing to do
}

int thread_yield()
{
    Sleep(0);
    return OK;
}
int thread_sleep(ureg microsecs)
{
    Sleep((DWORD)(microsecs / 1000));
    return OK;
}
ureg thread_id()
{
    return GetCurrentThreadId();
}
DWORD WINAPI thread_wrapper(void* ctx)
{
    thread* t = (thread*)ctx;
    t->thread_fn(t->context);
    return 0;
}
int thread_launch(thread* t, thread_function_ptr thread_fn, void* context)
{
    t->thread_fn = thread_fn;
    t->context = context;
    t->thrd = CreateThread(NULL, 0, thread_wrapper, context, 0, NULL); 
    return t->thrd ? OK : ERR;
}
int thread_join(thread* t)
{
    DWORD res = WaitForSingleObject(t->thrd, INFINITE);
    if (res != WAIT_OBJECT_0) return ERR;
    return OK;
}
int thread_detach(thread* t)
{
    CloseHandle(t->thrd);
    return OK;
}

#endif
