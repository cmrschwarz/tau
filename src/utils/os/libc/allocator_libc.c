#include "../../allocator.h"
#ifdef USE_LIBC_ALLOCATOR

#include "../../math_utils.h"

#include <memory.h>
#if DEBUG
#include "../../panic.h"
#include "../../threading.h"
#include <stdio.h>
static atomic_sreg allocations;
#endif
static inline void count_alloc()
{
#if DEBUG
    atomic_sreg_inc(&allocations);
#endif
}
static inline void count_free()
{
#if DEBUG
    atomic_sreg_dec(&allocations);
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
    //  printf("alloc %zx\n", r);
    // fflush(stdout);
    count_alloc();
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
    count_free();
    // printf("free %zx\n", mem);
    // fflush(stdout);
    free(mem);
}

#endif
