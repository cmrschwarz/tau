#pragma once
#include "../../threading.h"
typedef struct thread_allocator {
    ureg alloc_count; // allocation count for debugging
} thread_allocator;
