#pragma once
#define USE_LIBC_ATOMICS
#include "../../types.h"
#ifdef __clang__
#define ATOMIC_PREFIX _Atomic
#elif __GNUC__
#define ATOMIC_PREFIX volatile
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
