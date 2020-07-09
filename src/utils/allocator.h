#pragma once

#include "threading.h"
#include "types.h"

typedef struct master_error_log_s master_error_log;
int talloc_init(master_error_log* mel);
void talloc_fin();
bool talloc_initialized();

void* tmalloc(ureg size);
void* trealloc(void* mem, ureg size_used, ureg size_new);
void* tmallocz(ureg size);
void tfree(void* mem);

#if HOST_OS_LINUX
#define USE_LIBC_ALLOCATOR
#endif
