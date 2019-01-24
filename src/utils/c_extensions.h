#pragma once
#include "utils/plattform.h"

#if CMPLR_GCC || CMPLR_CLANG 
#    define PACK_ENUM  __attribute__ ((__packed__))
#else
#   if __cplusplus
#       define PACK_ENUM : unsigned char //!!Fails for more than 256 elements
#   else
#       define PACK_ENUM 
#   endif
#endif
