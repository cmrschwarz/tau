#pragma once
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

// could point to a (caching)function returning this at runtime
#define PAGE_SIZE 4096

#define DEBUG 1