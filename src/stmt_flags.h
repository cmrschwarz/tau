#pragma once
#include "utils/c_extensions.h"
typedef u16 stmt_flags;
#define STMT_FLAGS_DEFAULT (u16)0

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

void stmt_flags_set_access_mod(stmt_flags* f, access_modifier m);
access_modifier stmt_flags_get_access_mod(stmt_flags f);

void stmt_flags_set_const(stmt_flags* f);
bool stmt_flags_get_const(stmt_flags f);

void stmt_flags_set_sealed(stmt_flags* f);
bool stmt_flags_get_sealed(stmt_flags f);

void stmt_flags_set_virtual(stmt_flags* f);
bool stmt_flags_get_virtual(stmt_flags f);

void stmt_flags_set_static(stmt_flags* f);
bool stmt_flags_get_static(stmt_flags f);

void stmt_flags_set_compound_decl(stmt_flags* f);
bool stmt_flags_get_compound_decl(stmt_flags f);

void stmt_flags_set_resolved(stmt_flags* f);
bool stmt_flags_get_resolved(stmt_flags f);

void stmt_flags_set_from_pp(stmt_flags* f);
bool stmt_flags_get_from_pp(stmt_flags f);

void stmt_flags_set_used_in_pp(stmt_flags* f);
bool stmt_flags_get_used_in_pp(stmt_flags f);

void stmt_flags_set_parse_error(stmt_flags* f);
bool stmt_flags_get_parse_error(stmt_flags f);
void stmt_flags_set_redeclared(stmt_flags* f);
bool stmt_flags_get_redeclared(stmt_flags f);
