#include "../../plattform.h"
#if HOST_OS_WINDOWS
#include "sane_windows.h"
#include "../../panic.h"
static bool sys_info_aquired = false;
static bool proc_info_aquired = false;
static SYSTEM_INFO system_info;
static SYSTEM_LOGICAL_PROCESSOR_INFORMATION_EX  proc_info;
ureg plattform_get_page_size()
{
    if (!sys_info_aquired) GetSystemInfo(&system_info);
    return system_info.dwPageSize;
}

ureg plattform_get_cache_line_size()
{
    if (!proc_info_aquired) {
        DWORD size = sizeof(SYSTEM_LOGICAL_PROCESSOR_INFORMATION_EX);
        bool res = GetLogicalProcessorInformationEx(RelationProcessorCore, &proc_info, &size);
        if (!res) panic("failed to query cache line size");
    }
    return proc_info.Cache.LineSize;
}
ureg plattform_get_virt_core_count()
{
    if (!sys_info_aquired) GetSystemInfo(&system_info);
    return system_info.dwNumberOfProcessors;
}
void platttform_override_virt_core_count(ureg count)
{
    if (!sys_info_aquired) GetSystemInfo(&system_info);
    system_info.dwNumberOfProcessors = (DWORD)count;
}

bool is_stderr_tty(){
    return false; //TODO: proper implementation?
}
#endif
