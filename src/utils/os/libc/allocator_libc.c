#include "../../allocator.h"
#ifdef USE_LIBC_ALLOCATOR

#include "../../math_utils.h"

#include <memory.h>
#define PRINT_ALLOCS 0
#if DEBUG
#include "../../panic.h"
#include "../../threading.h"
#include <stdio.h>
static atomic_sreg allocations;
#endif
static inline void count_alloc(void* v)
{
#if DEBUG
    atomic_sreg_inc(&allocations);
#if PRINT_ALLOCS
    printf("alloc: %zx\n", v);
#endif
#endif
}
static inline void count_free(void* v)
{
#if DEBUG
    atomic_sreg_dec(&allocations);
#if PRINT_ALLOCS
    printf("free: %zx\n", v);
#endif
#endif
}
int talloc_init()
{
#if DEBUG
    return atomic_sreg_init(&allocations, 0);
#else
    return 0;
#endif
}

void talloc_fin()
{
#if DEBUG
    sreg a = atomic_sreg_load(&allocations);
    if (a) {
        printf("MEMORY LEAK! (allocs - frees) = %zd\n", a);
        panic("Memory leak!");
    }
#endif
}

void* tmalloc(ureg size)
{
    void* r = malloc(size);
    if (r) count_alloc(r);
    return r;
}

void* tmallocz(ureg size)
{
    void* m = tmalloc(size);
    if (m != NULL) memset(m, 0, size);
    return m;
}

void* trealloc(void* old, ureg used_size, ureg new_size)
{
    return realloc(old, new_size);
}

void tfree(void* mem)
{
    count_free(mem);
    free(mem);
}

#endif
