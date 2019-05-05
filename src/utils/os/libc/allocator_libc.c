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
    count_alloc();
    return r;
}

void* tmallocz(ureg size)
{
    void* m = malloc(size);
    if (m) memset(m, 0, size);
    count_alloc();
    return m;
}

void* trealloc(void* old, ureg used_size, ureg new_size)
{
    return realloc(old, new_size);
}

void tfree(void* mem)
{
    free(mem);
    count_free();
}

#endif
