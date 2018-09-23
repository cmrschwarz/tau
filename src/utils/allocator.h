#include "types.h"
#include <pthread.h>
typedef struct{
    memblock* next;
    void* start;
    ureg size;
}memblock;

typedef struct {
    arena* next;   
    memblock* blocks;
}arena;

typedef struct{
    thread_allocator* next;
    arena* arenas;
    memblock* free_list;
}thread_allocator;

typedef struct {
    thread_allocator* threads;
    pthread_mutex_t lock;
}allocator;



extern allocator ALLOCATOR;

int tal_init(thread_allocator* tal);
void tal_fin(thread_allocator* tal);
int arena_init(arena* a, thread_allocator* tal);
void arena_fin(arena* a, thread_allocator* tal);
void* arena_alloc(arena* a, ureg size);
void arena_clear(arena* a);
