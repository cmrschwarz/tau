#pragma once

struct allocator_linux_caching_block;

typedef struct thread_allocator {
    struct thread_allocator* next;
    struct allocator_linux_caching_block* free_list;
} thread_allocator;
