#ifndef TAUC_UTILS_PLATTFORM_H
#define TAUC_UTILS_PLATTFORM_H

#include "types.h"

#define OS_POSIX 1
#define OS_LINUX 1
#define OS_OSX 0
#define OS_WINDOWS 0

#define USE_LIBC 1

#define ARCH_X86 1
#define ARCH_ARM 0

#define CMPLR_GCC 1
#define CMPLR_MSVC 0
#define CMPLR_CLANG 0

ureg plattform_get_page_size();
ureg plattform_get_cache_line_size();
ureg plattform_get_virt_core_count();

#if CMAKE_NO_DEBUG
#define DEBUG 0
#else
#define DEBUG 1
#endif

#endif