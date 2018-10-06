#pragma once

#ifndef __STDC_NO_ATOMICS__
#include <stdatomic.h>
#endif

#define USE_LIBC_ATOMICS

typedef _Atomic ureg atomic_ureg;
typedef _Atomic sreg atomic_sreg;
typedef _Atomic bool atomic_bool;