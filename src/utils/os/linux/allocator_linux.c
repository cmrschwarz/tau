#include "../../plattform.h"
#if OS_LINUX
#include "../../../error_log.h"
#include "../../allocator.h"
#include "../../math_utils.h"
#include "../../panic.h"
#include "inttypes.h"
#include <linux/mman.h>
#include <memory.h>
#include <signal.h>
#include <sys/mman.h>
// this is a really basic allocator using mmap
// this will eventually be replaced by allocator_linux_caching
// but for now, it's good enough

int allocator_init()
{
    return OK;
}

void allocator_fin()
{
}

static inline void bumpallocdelta(thread_allocator* tal)
{
#if DEBUG
    tal->alloc_delta++;
#endif
}

static inline void decallocdelta(thread_allocator* tal)
{
#if DEBUG
    tal->alloc_delta--;
#endif
}

int tal_init(thread_allocator* tal)
{
#if DEBUG
    tal->alloc_delta = 0;
#endif
    return OK;
}

void tal_fin(thread_allocator* tal)
{
#if DEBUG
    if (tal->alloc_delta != 0) {
        printf(
            "debug: memory leaks detected: allocs - frees = %" PRId64 "\n",
            tal->alloc_delta);
    }
#endif
}

int tal_alloc(thread_allocator* tal, ureg size, memblock* b)
{
    bumpallocdelta(tal);
    b->start = mmap(
        NULL, size, PROT_READ | PROT_WRITE,
        MAP_PRIVATE | MAP_ANONYMOUS | MAP_UNINITIALIZED, -1, 0);
    if (b->start == MAP_FAILED) return ERR;
    b->end = ptradd(b->start, size);
    return OK;
}
int tal_allocz(thread_allocator* tal, ureg size, memblock* b)
{
    bumpallocdelta(tal);
    // without the MAP_UNITIALIZED flag memory will be zero
    b->start = mmap(
        NULL, size, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
    if (b->start == MAP_FAILED) return ERR;
    b->end = ptradd(b->start, size);
    return OK;
}

int tal_realloc(
    thread_allocator* tal, ureg used_size, ureg new_size, memblock* b)
{
    memblock mbnew;
    if (tal_alloc(tal, new_size, &mbnew)) return ERR;
    memcpy(mbnew.start, b->start, used_size);
    tal_free(tal, b);
    *b = mbnew;
    return OK;
}

void tal_free(thread_allocator* tal, memblock* b)
{
    decallocdelta(tal);
    int res = munmap(b->start, ptrdiff(b->end, b->start));
    if (res) panic("fatal: memory deallocation failure");
}

// TODO: implement properly
void* tal_gpmalloc(thread_allocator* tal, ureg size)
{
    // no bumpallocdelta here because of the threading guarantees
    // TODO: implement free counting
    return malloc(size);
}
void allocator_gpfree(void* mem)
{
    free(mem);
}

void* tal_tlmalloc(thread_allocator* tal, ureg size)
{
    bumpallocdelta(tal);
    return malloc(size);
}
void tal_tlfree(thread_allocator* tal, void* mem)
{
    decallocdelta(tal);
    free(mem);
}

#endif