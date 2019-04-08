#pragma once
#include "plattform.h"

#if CMPLR_GCC || CMPLR_CLANG
#define PACK_ENUM __attribute__((__packed__))
#else
#define PACK_ENUM
#endif

// prevents emtpy transation unit warnings from ISO compliant compilers
#define POSSIBLY_EMPTY_TL typedef int _dummy_content_for_iso_compilers;

// clang-format off
#define ANONYMOUS_UNION_START union {
#define ANONYMOUS_UNION_END };
// clang-format on