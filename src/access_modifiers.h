#pragma once
#include "utils/c_extensions.h"
// !symbol_table's data layout depends on this enums order
typedef enum PACK_ENUM access_modifier {
    AM_UNSPECIFIED = 0,
    // identical to unspecified inside structs
    AM_PACKAGE_PRIVATE = 0,
    // identical to unspecified inside modules
    AM_SCOPE_LOCAL = 0,
    AM_PRIVATE = 1,
    AM_PROTECTED = 2,
    AM_PUBLIC = 3,
    AM_ENUM_ELEMENT_COUNT = 4,
} access_modifier;
