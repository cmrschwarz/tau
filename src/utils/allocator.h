#pragma once
#include "threading.h"
#include "types.h"

/*
typedef int (*alloc_fn)(void* al, ureg size, memblock* b);
typedef int (*allocv_fn)(void* al, ureg size, memblock* b);
typedef int (*realloc_fn)(void* al, ureg used_size, ureg size, memblock* b);
typedef int (*free_fn)(void* al, memblock* b);

typedef struct {
    void* allocator;
    alloc_fn alloc;
    allocv_fn allocv;
    realloc_fn realloc;
    free_fn free;
}allocator;
*/

typedef struct thread_allocator thread_allocator;

typedef struct {
    void* start;
    void* end;
} memblock;

int allocator_init();
void allocator_fin();

int tal_init(thread_allocator* tal);
void tal_fin(thread_allocator* tal);

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