#include "../../allocator.h"
#ifdef USE_LIBC_ALLOCATOR

#include "../../math_utils.h"
#include <memory.h>

int talloc_init()
{
    return 0;
}

void talloc_fin()
{
}

void* tmalloc(ureg size)
{
    return malloc(size);
}

void* tmallocz(ureg size)
{
    void* m = malloc(size);
    if (m) memset(m, 0, size);
    return m;
}

void* trealloc(void* old, ureg used_size, ureg new_size)
{
    return realloc(old, new_size);
}

void tfree(void* mem)
{
    free(mem);
}

#endif