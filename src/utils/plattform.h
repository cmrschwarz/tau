#pragma once

#include "types.h"

// OS: deduced automatically
#define HOST_OS_LINUX 0
#define HOST_OS_OSX 0
#define HOST_OS_WINDOWS 0

// compiler: deduced automatically
#define CMPLR_GCC 0
#define CMPLR_MSVC 0
#define CMPLR_CLANG 0

// architecture: we only support x86 for now
#define HOST_ARCH_X86 1
#define HOST_ARCH_ARM 0

// debug mode: deduced automatically
#define DEBUG 0
#define RELEASE 0

ureg plattform_get_page_size();
ureg plattform_get_cache_line_size();
ureg plattform_get_virt_core_count();
void platttform_override_virt_core_count(ureg count);
bool is_stderr_tty();
int delete_file(const char* path);

// deduce debug mode
#undef DEBUG
#undef RELEASE
#if TAUC_DEBUG
#define DEBUG 1
#define RELEASE 0
#else
#define DEBUG 0
#define RELEASE 1
#endif

// deduce OS
#if defined(linux) || defined(__linux) || defined(__linux__)
#undef HOST_OS_LINUX
#define HOST_OS_LINUX 1
#elif defined(__APPLE__) && defined(__MACH__)
#undef HOST_OS_OSX
#define HOST_OS_OSX 1
#elif defined(_WIN64) || defined(_WIN32)
#undef HOST_OS_WINDOWS
#define HOST_OS_WINDOWS 1
#else
#error "Unknown OS"
#endif

// deduce compiler
#if defined(__clang__)
#undef CMPLR_CLANG
#define CMPLR_CLANG 1
#elif defined(__GNUC__) || defined(__GNUG__)
#undef CMPLR_GCC
#define CMPLR_GCC 1
#elif defined(_MSC_VER)
#undef CMPLR_MSVC
#define CMPLR_MSVC 1
#else
#error "Unknown Compiler"
#endif
