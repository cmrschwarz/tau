#ifndef TAUC_UTILS_ATOMICS_LIBC_H
#define TAUC_UTILS_ATOMICS_LIBC_H

#define USE_LIBC_ATOMICS

#if !defined(__cplusplus)
#include <stdatomic.h>
typedef atomic_size_t atomic_ureg;
typedef atomic_ptrdiff_t atomic_sreg;
typedef atomic_intptr_t atomic_ptr;
#else
#include <atomic>
typedef std::atomic_size_t atomic_ureg;
typedef std::atomic_ptrdiff_t atomic_sreg;
typedef std::atomic_intptr_t atomic_ptr;
typedef std::atomic_bool atomic_bool;
#endif

#endif