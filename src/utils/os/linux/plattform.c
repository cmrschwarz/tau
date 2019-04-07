#include "../../plattform.h"
#include <sys/unistd.h>
#if OS_LINUX
static ureg _page_size = 0;
static ureg _cache_line_size = 0;

ureg plattform_get_page_size()
{
    if (!_page_size) _page_size = sysconf(_SC_PAGESIZE);
    return _page_size;
}

ureg plattform_get_cache_line_size()
{
    if (!_cache_line_size)
        _cache_line_size = sysconf(_SC_LEVEL1_DCACHE_LINESIZE);
    return _cache_line_size;
}
#endif