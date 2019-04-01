#pragma once
#include "pool.h"
#include "threading.h"

// TODO: make this lock free
// TOOD: the thread allocator in here is problematic,
// consider ditching tal alltogether
typedef struct atomic_pool {
    pool pool;
    mutex mtx;
} atomic_pool;

static inline int atomic_pool_init(atomic_pool* p)
{
    int r = mutex_init(&p->mtx);
    if (r) return r;
    r = pool_init(&p->pool);
    if (r) {
        mutex_fin(&p->mtx);
        return r;
    }
    return 0;
}
static inline void atomic_pool_fin(atomic_pool* p)
{
    pool_fin(&p->pool);
    mutex_fin(&p->mtx);
}
static inline void* atomic_pool_alloc(atomic_pool* p, ureg size)
{
    mutex_lock(&p->mtx);
    void* res = pool_alloc(&p->pool, size);
    mutex_unlock(&p->mtx);
    return res;
}
static inline void atomic_pool_clear_unlocked(atomic_pool* p)
{
    pool_clear(&p->pool);
}
static inline void atomic_pool_clear(atomic_pool* p)
{
    mutex_lock(&p->mtx);
    pool_clear(&p->pool);
    mutex_unlock(&p->mtx);
}