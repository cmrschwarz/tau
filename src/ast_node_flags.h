#pragma once
#include "utils/c_extensions.h"
typedef u16 ast_node_flags;
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

void ast_node_flags_set_access_mod(ast_node_flags* f, access_modifier m);
access_modifier ast_node_flags_get_access_mod(ast_node_flags f);

void ast_node_flags_set_const(ast_node_flags* f);
bool ast_node_flags_get_const(ast_node_flags f);

void ast_node_flags_set_sealed(ast_node_flags* f);
bool ast_node_flags_get_sealed(ast_node_flags f);

void ast_node_flags_set_virtual(ast_node_flags* f);
bool ast_node_flags_get_virtual(ast_node_flags f);

void ast_node_flags_set_static(ast_node_flags* f);
bool ast_node_flags_get_static(ast_node_flags f);

void ast_node_flags_set_compound_decl(ast_node_flags* f);
bool ast_node_flags_get_compound_decl(ast_node_flags f);

void ast_node_flags_set_resolved(ast_node_flags* f);
bool ast_node_flags_get_resolved(ast_node_flags f);

void ast_node_flags_set_resolving(ast_node_flags* f);
bool ast_node_flags_get_resolving(ast_node_flags f);

void ast_node_flags_set_defined_in_pp(ast_node_flags* f);
bool ast_node_flags_get_defined_in_pp(ast_node_flags f);

void ast_node_flags_set_used_in_pp(ast_node_flags* f);
bool ast_node_flags_get_used_in_pp(ast_node_flags f);

void ast_node_flags_set_parse_error(ast_node_flags* f);
bool ast_node_flags_get_parse_error(ast_node_flags f);
void ast_node_flags_set_redeclared(ast_node_flags* f);
bool ast_node_flags_get_redeclared(ast_node_flags f);
