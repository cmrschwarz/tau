#pragma once
#include "types.h"

#define OS_POSIX 1
#define OS_LINUX 1
#define OS_OSX 0
#define OS_WINDOWS 0

#define USE_LIBC 0

#define ARCH_X86 1
#define ARCH_ARM 0

#define CMPLR_GCC 1
#define CMPLR_MSVC 0
#define CMPLR_CLANG 0

// these could point to a (caching)function returning this at runtime
#define PAGE_SIZE 4096
#define CACHE_LINE_SIZE 64

ureg plattform_get_page_size();
ureg plattform_get_cache_line_size();

#if CMAKE_NO_DEBUG
#define DEBUG 0
#else
#define DEBUG 1
#endif
