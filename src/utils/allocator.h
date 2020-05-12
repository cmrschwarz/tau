#pragma once

#include "threading.h"
#include "types.h"

int talloc_init();
void talloc_fin();

void* tmalloc(ureg size);
void* trealloc(void* mem, ureg size_used, ureg size_new);
void* tmallocz(ureg size);
void tfree(void* mem);

#if HOST_OS_LINUX
#define USE_LIBC_ALLOCATOR
#endif

