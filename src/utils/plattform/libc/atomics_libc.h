#pragma once
#include "../../plattform.h"
#include "../../types.h"

#define USE_LIBC_ATOMICS

#if CMPLR_CLANG
#   define ATOMIC_PREFIX _Atomic
#elif CMPLR_GCC 
#   define ATOMIC_PREFIX volatile
#else
#   error tauc has no libc atomics implementation on this plattform
#endif

typedef struct atomic_ureg_s {
    ATOMIC_PREFIX ureg val;
} atomic_ureg;
typedef struct atomic_sreg_s {
    ATOMIC_PREFIX sreg val;
} atomic_sreg;
typedef struct atomic_ptr_s {
    void* ATOMIC_PREFIX val;
} atomic_ptr;
// we can't name it atomic_bool since that's used in stdatomic and would cause
// all kinds of problems...
typedef struct atomic_boolean_s {
    ATOMIC_PREFIX bool val;
} atomic_boolean;
