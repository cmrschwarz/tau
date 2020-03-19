#ifndef TAUC_UTILS_RWSLOCK_H
#define TAUC_UTILS_RWSLOCK_H

#include "threading.h"
typedef atomic_sreg rwslock;

static inline int rwslock_init(rwslock* l)
{
    return atomic_sreg_init(l, 0);
}
static inline void rwslock_fin(rwslock* l)
{
    atomic_sreg_fin(l);
}

static inline void rwslock_read(rwslock* l)
{
    sreg v;
    while (true) {
        v = atomic_sreg_inc(l);
        if (v >= 0) return;
        atomic_sreg_dec(l);
    }
}
static inline void rwslock_end_read(rwslock* l)
{
    atomic_sreg_dec(l);
}
static inline void rwslock_write(rwslock* l)
{

    while (true) {
        sreg v = atomic_sreg_load(l);
        while (v >= 0) {
            if (atomic_sreg_cas(l, &v, v - SREG_MAX)) {
                while (v != -SREG_MAX) v = atomic_sreg_load(l);
                return;
            }
        }
    }
}
static inline void rwslock_end_write(rwslock* l)
{
    atomic_sreg_add(l, SREG_MAX);
}
#endif
