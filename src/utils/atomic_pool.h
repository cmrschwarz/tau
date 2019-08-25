#ifndef TAUC_UTILS_ATOMIC_POOL_H
#define TAUC_UTILS_ATOMIC_POOL_H

#include "pool.h"
#include "threading.h"

// TODO: make this lock free
// TOOD: the thread allocator in here is problematic,
// consider ditching tal alltogether
typedef struct atomic_pool_s {
    pool basepool;
    mutex mtx;
} atomic_pool;

static inline int atomic_pool_init(atomic_pool* p)
{
    int r = mutex_init(&p->mtx);
    if (r) return r;
    r = pool_init(&p->basepool);
    if (r) {
        mutex_fin(&p->mtx);
        return r;
    }
    return 0;
}
static inline void atomic_pool_fin(atomic_pool* p)
{
    pool_fin(&p->basepool);
    mutex_fin(&p->mtx);
}
static inline void* atomic_pool_alloc(atomic_pool* p, ureg size)
{
    mutex_lock(&p->mtx);
    void* res = pool_alloc(&p->basepool, size);
    mutex_unlock(&p->mtx);
    return res;
}
static inline void atomic_pool_clear_unlocked(atomic_pool* p)
{
    pool_clear(&p->basepool);
}
static inline void atomic_pool_clear(atomic_pool* p)
{
    mutex_lock(&p->mtx);
    pool_clear(&p->basepool);
    mutex_unlock(&p->mtx);
}

#endif