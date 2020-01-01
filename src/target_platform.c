#include "target_platform.h"
#include "utils/string.h"
#include <assert.h>
#include "utils/plattform.h"

arch_kind parse_arch_kind(char* str)
{
    if (cstr_eq(str, "x86")) return ARCH_X86;
    if (cstr_eq(str, "amd64")) return ARCH_X86_64;
    assert(false); // TODO
    return ARCH_UNKNOWN;
}
os_kind parse_os_kind(char* str)
{
    if (cstr_eq(str, "linux")) return OS_LINUX;
    if (cstr_eq(str, "windows")) return OS_WIN32;
    assert(false); // TODO
    return OS_UNKNOWN;
}
object_format_kind parse_object_format_kind(char* str)
{
    if (cstr_eq(str, "elf")) return OBJECT_FORMAT_ELF;
    if (cstr_eq(str, "coff")) return OBJECT_FORMAT_COFF;
    assert(false); // TODO
    return OBJECT_FORMAT_UNKNOWN;
}

// TODO: implement this properly :)
void target_platform_get_host(target_platform* tp)
{
#if HOST_OS_LINUX
    tp->os = OS_LINUX;
    tp->object_format = OBJECT_FORMAT_ELF;
#elif OS_WINDOWS
    tp->os = OS_WIN32;
    tp->object_format = OBJECT_FORMAT_COFF;
#else
// TODO
#error unsupported HOST OS
#endif

#if HOST_ARCH_X86 && REG_WIDTH_64
    tp->arch = ARCH_X86_64;
#elif HOST_ARCH_X86 && REG_WIDTH_32
    tp->arch = ARCH_X86;
#else
// TODO
#error unsupported HOST OS
#endif
}

void target_platform_set_unknown(target_platform* tp)
{
    tp->arch = ARCH_UNKNOWN;
    tp->object_format = OBJECT_FORMAT_UNKNOWN;
    tp->os = OS_UNKNOWN;
}

void target_platform_fill_gaps(target_platform* tgt, target_platform* src)
{
    if (tgt->arch == ARCH_UNKNOWN) tgt->arch = src->arch;
    if (tgt->object_format == OBJECT_FORMAT_UNKNOWN) {
        tgt->object_format = src->object_format;
    }
    if (tgt->os == OS_UNKNOWN) tgt->os = src->os;
}
