#pragma once

#define USE_LIBC_ATOMICS

#if !defined(__cplusplus) && !defined(__STDC_NO_ATOMICS__)
#include <stdatomic.h>
typedef atomic_size_t atomic_ureg;
typedef atomic_ptrdiff_t atomic_sreg;
typedef atomic_intptr_t atomic_ptr;
#else
#include <atomic>
typedef atomic_size_t atomic_ureg;
typedef atomic_ptrdiff_t atomic_sreg;
typedef atomic_intptr_t atomic_ptr;
#endif
