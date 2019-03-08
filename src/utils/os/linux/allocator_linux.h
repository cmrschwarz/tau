#pragma once
#include "../../plattform.h"
#include "../../threading.h"
typedef struct thread_allocator {
#if DEBUG
    sreg alloc_delta;
#else
    char _; // to avoid size 0
#endif
} thread_allocator;
