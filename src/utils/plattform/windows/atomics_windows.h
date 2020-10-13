#pragma once
#include "../../types.h"
typedef struct atomic_ureg_s {
    volatile ureg val;
} atomic_ureg;
typedef struct atomic_sreg_s {
    volatile sreg val;
} atomic_sreg;
typedef struct atomic_ptr_s {
    volatile void* val;
} atomic_ptr;
// we can't name it atomic_bool since that's used in stdatomic and would cause
// all kinds of problems...
typedef struct atomic_boolean_s {
    volatile bool val;
} atomic_boolean;
