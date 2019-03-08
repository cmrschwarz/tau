#pragma once
#include "plattform.h"
#include "types.h"

static inline ureg ulog2(ureg v)
{
#if ARCH_X86 && REG_WIDTH_64
    __asm__("bsrq %0, %0" : "=r"(v) : "0"(v));
    return v;
#elif ARCH_X86 && REG_WIDTH_32
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
#if ARCH_X86
    return (ureg)(1) << ulog2(v);
#else
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
#endif
    return v;
}

static inline ureg ptrdiff(void* greater, void* smaller)
{
    return (ureg)greater - (ureg)smaller;
}
static inline sreg ptrdiffs(void* left, void* right)
{
    return (sreg)left - (sreg)right;
}
static inline void* ptradd(void* ptr, ureg val)
{
    return (void*)((ureg)ptr + val);
}
static inline void* ptradds(void* ptr, sreg val)
{
    return (void*)((ureg)ptr + val);
}
static inline void* ptrsub(void* ptr, ureg val)
{
    return (void*)((ureg)ptr - val);
}
static inline void* ptrsubs(void* ptr, sreg val)
{
    return (void*)((ureg)ptr - val);
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

static inline ureg align_size(ureg siz, ureg alignment)
{
    ureg mod = siz % alignment;
    if (mod) siz += alignment - mod;
    return siz;
}