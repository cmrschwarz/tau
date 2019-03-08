#pragma once
#include "threading.h"
#include "types.h"

typedef struct thread_allocator thread_allocator;

typedef struct {
    void* start;
    void* end;
} memblock;

int allocator_init();
void allocator_fin();

int tal_init(thread_allocator* tal);
void tal_fin(thread_allocator* tal);

// general purpose allocation like malloc.
// can be freed using allocator_gpfree on any thread
void* tal_gpmalloc(thread_allocator* tal, ureg size);
void allocator_gpfree(void* mem);

// general purpose thread_local allocation
// must be freed using tal_tlfree on the same thread with the same tal
void* tal_tlmalloc(thread_allocator* tal, ureg size);
void tal_tlfree(thread_allocator* tal, void* mem);

// used for large allocations. size must be multiple of PAGE_SIZE
// memblock returned is potentially bigger than requested and can be fully used
// must be freed with tal_free on the same thread using the same tal
int tal_alloc(thread_allocator* tal, ureg size, memblock* b);

// like alloc, but guarantees that the memory is zeroed
int tal_allocz(thread_allocator* tal, ureg size, memblock* b);

int tal_realloc(
    thread_allocator* tal, ureg used_size, ureg new_size, memblock* b);
void tal_free(thread_allocator* tal, memblock* b);

#if OS_LINUX
#include "os/linux/allocator_linux.h"
#elif USE_LIBC
#include "os/libc/allocator_libc.h"
#else
#error no allocator present for the configured plattform
#endif