#include "../../plattform.h"
#if OS_LINUX
#include <stdio.h>
#include <unistd.h>
static ureg _page_size = 0;
static ureg _cache_line_size = 0;
static ureg _virt_core_count = 0;

ureg plattform_get_page_size()
{
    if (!_page_size) _page_size = sysconf(_SC_PAGESIZE);
    return _page_size;
}

ureg plattform_get_cache_line_size()
{
    if (!_cache_line_size) {
#ifdef _SC_LEVEL1_DCACHE_LINESIZE
        _cache_line_size = sysconf(_SC_LEVEL1_DCACHE_LINESIZE);
#else
        FILE* p = 0;
        p = fopen(
            "/sys/devices/system/cpu/cpu0/cache/index0/coherency_line_size",
            "r");
        if (p) {
            fscanf(p, "%zu", &_cache_line_size);
            fclose(p);
        }
        else {
            _cache_line_size = 32; // fallback
        }
#endif
    }

    return _cache_line_size;
}
ureg plattform_get_virt_core_count()
{
    if (!_virt_core_count) _virt_core_count = sysconf(_SC_NPROCESSORS_ONLN);
    return _virt_core_count;
}
#endif