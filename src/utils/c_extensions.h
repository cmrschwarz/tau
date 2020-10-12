#pragma once

#include "plattform.h"

#if CMPLR_GCC || CMPLR_CLANG
#define PACK_ENUM __attribute__((__packed__))
#define PACK_STRUCT __attribute__((__packed__))
#else
// TODO: msvc?
#define PACK_ENUM
#define PACK_STRUCT
#endif

// prevents emtpy transation unit warnings from ISO compliant compilers
#define POSSIBLY_EMPTY_TL typedef int _dummy_content_for_iso_compilers;

#define STRINGIFY(s) #s
#define CONCAT_RAW(a, b) a##b
#define CONCAT(a, b) CONCAT_RAW(a, b)
#define STR_CAT(a, b) a b
#define STR_CAT_3(a, b, c) a b c
