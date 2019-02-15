#pragma once
#include "allocator.h"
#include "types.h"

typedef struct pool_segment {
    struct pool_segment* next;
    u8* head;
    u8* end;
} pool_segment;

typedef struct pool {
    struct pool* next;
    thread_allocator* tal;
    pool_segment* segments;
} pool;

int pool_init(pool* p, thread_allocator* tal);
void pool_fin(pool* p);
void* pool_alloc(pool* p, ureg size);
void pool_clear(pool* p);