#include "../../allocator.h"
#ifdef USE_LIBC_ALLOCATOR

#include "../../math_utils.h"

#include <memory.h>
#if DEBUG
#define PRINT_ALLOCS 0
#define DISABLE_LEAKCHECK 0
#include "../../panic.h"
#include "../../threading.h"
#include "utils/debug_utils.h"
#include <stdio.h>
static atomic_sreg allocations;
#endif

#include "../../../error_log.h"
static master_error_log* mel = NULL;

static inline void count_alloc(void* v, ureg size)
{
#if DEBUG
    atomic_sreg_inc(&allocations);
#if PRINT_ALLOCS
    tprintf("alloc: %zx [%zu B]\n", v, size);
#endif
#endif
}
static inline void count_free(void* v)
{
#if DEBUG
    atomic_sreg_dec(&allocations);
#if PRINT_ALLOCS
    tprintf("free: %zx\n", v);
#endif
#endif
}
bool talloc_initialized()
{
    return mel != NULL;
}
int talloc_init(master_error_log* el)
{
#if DEBUG
    atomic_sreg_init(&allocations, 0);
#endif
    mel = el;
    return OK;
}

void talloc_fin()
{
    assert(mel);
    mel = NULL;
#if DEBUG && !DISABLE_LEAKCHECK
    sreg a = atomic_sreg_load(&allocations);
    if (a) {
        fprintf(stderr, "MEMORY LEAK! (allocs - frees) = %zd\n", a);
        panic("Memory leak!");
    }
#endif
}

void* tmalloc(ureg size)
{
    void* r = malloc(size);
    if (r) {
        count_alloc(r, size);
    }
    else {
        assert(mel);
        aseglist_iterator it;
        aseglist_iterator_begin(&it, &mel->error_logs);
        ureg tid = thread_id();
        for (error_log* el = aseglist_iterator_next(&it); el != NULL;
             el = aseglist_iterator_next(&it)) {
            if (el->tid == tid) {
                error_log_report_allocation_failiure(el);
                return NULL;
            }
        }
        master_error_log_report(mel, "allocation failure");
        return NULL;
    }
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
    assert(mem != NULL);
    count_free(mem);
    free(mem);
}

#endif
