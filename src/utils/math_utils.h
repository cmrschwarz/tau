#pragma once

#include "plattform.h"
#include "types.h"

// ulog2(0) = 0
// ulog2(1) = 1
// ulog2(2) = 2
// ulog2(3) = 2
// ulog2(4) = 3
static inline ureg ulog2(ureg v)
{
#if HOST_ARCH_X86 && REG_WIDTH_64
    __asm__("bsrq %0, %0" : "=r"(v) : "0"(v));
    return v;
#elif HOST_ARCH_X86 && REG_WIDTH_32
    __asm__("bsrl %0, %0" : "=r"(v) : "0"(v));
    return v;
#else
    ureg res = 0;
    while (v >>= 1) res++;
    return res;
#endif
}

// returns the next power of two greater than the number
// if the number is already a power of two, returns the number itself
// if the number is 0, returns 0
// if the number is greater than UREG_MAX / 2 + 1, the result is undefined
static inline ureg ceil_to_pow2(ureg v)
{
    v--;
    v |= v >> 1;
    v |= v >> 2;
    v |= v >> 4;
    v |= v >> 8;
    v |= v >> 16;
#if REG_WIDTH_64
    v |= v >> 32;
#endif
    v++;
    return v;
}

static inline ureg ptrdiff(void* greater, void* smaller)
{
    return (char*)greater - (char*)smaller;
}
static inline sreg ptrdiffs(void* left, void* right)
{
    return (char*)left - (char*)right;
}
static inline void* ptradd(void* ptr, ureg val)
{
    return (void*)((char*)ptr + val);
}
static inline void* ptradds(void* ptr, sreg val)
{
    return (void*)((char*)ptr + val);
}
static inline void* ptrsub(void* ptr, ureg val)
{
    return (void*)((char*)ptr - val);
}
static inline void* ptrsubs(void* ptr, sreg val)
{
    return (void*)((char*)ptr - val);
}
static inline bool ptreq(void* a, void* b)
{
    return a == b;
}
static inline void ptrswap(void** a, void** b)
{
    void* a_tmp = *a;
    *a = *b;
    *b = a_tmp;
}
static inline void ptrswapt(void** a, void** b, void** c)
{
    *c = *a;
    *a = *b;
    *b = *c;
}
// macro useful for compile time evaluation
#define CEIL_TO_MULT_OF_POW2(val, pow2) (((val + pow2 - 1) / pow2) * pow2)

static inline ureg ceil_to_mult_of_pow_two(ureg val, ureg pow2)
{
    return CEIL_TO_MULT_OF_POW2(val, pow2);
}
