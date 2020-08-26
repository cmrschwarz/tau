#pragma once

#include "allocator.h"
#include "types.h"

typedef struct pool_segment_s {
    struct pool_segment_s* next;
    struct pool_segment_s* prev;
    u8* head;
    u8* end;
} pool_segment;

typedef struct pool_s {
    pool_segment* head_segment;
} pool;

int pool_init(pool* p);
void pool_init_dummy(pool* p);
void pool_fin(pool* p);
void* pool_alloc(pool* p, ureg size);
void pool_undo_last_alloc(pool* p, ureg size);
void pool_clear(pool* p);

void pool_steal_all(pool* p, pool* donor);
void pool_steal_used(pool* p, pool* donor);
