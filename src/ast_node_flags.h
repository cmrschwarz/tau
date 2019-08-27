#ifndef TAUC_AST_NODE_FLAGS_H
#define TAUC_AST_NODE_FLAGS_H

#include "utils/c_extensions.h"
typedef u16 ast_node_flags;
#define AST_NODE_FLAGS_DEFAULT (u16)0

// !symbol_table's data layout depends on this enums order
typedef enum PACK_ENUM access_modifier_e {
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

typedef enum ast_node_flag_values_e {
    ASTF_RESOLVING = 0x1, // this stays set even once resolved
    ASTF_RESOLVED = 0x2,
    ASTF_STATIC = 0x4,
    ASTF_VIRTUAL = 0x8,
    ASTF_SEALED = 0x10,
    ASTF_CONST = 0x20,
    // ASTF_ACCESS_MODIFIER = 0xB0,
    ASTF_PARSE_ERROR = 0x100,
    ASTF_REDECLARATION = 0x200,
    ASTF_COMPUND_DECL = 0x400,
    // never needed simultaneous with compund decl, so we share the bit
    ASTF_RELATIVE_IMPORT = 0x400,
    ASTF_DEFINED_IN_PP = 0x800,
} ast_node_flag_values;

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

void ast_node_flags_set_relative_import(ast_node_flags* f);
bool ast_node_flags_get_relative_import(ast_node_flags f);

void ast_node_flags_set_resolved(ast_node_flags* f);
bool ast_node_flags_get_resolved(ast_node_flags f);

// these just use the reverse resolving/resolved flags, since all nodes to emit
// were previously resolved
void ast_node_flags_set_id_adjusted(ast_node_flags* f);
bool ast_node_flags_get_id_adjusted(ast_node_flags f);

void ast_node_flags_set_emitted(ast_node_flags* f);
bool ast_node_flags_get_emitted(ast_node_flags f);

void ast_node_flags_set_resolving(ast_node_flags* f);
void ast_node_flags_clear_resolving(ast_node_flags* f);
bool ast_node_flags_get_resolving(ast_node_flags f);

void ast_node_flags_set_defined_in_pp(ast_node_flags* f);
bool ast_node_flags_get_defined_in_pp(ast_node_flags f);

void ast_node_flags_set_used_in_pp(ast_node_flags* f);
bool ast_node_flags_get_used_in_pp(ast_node_flags f);

void ast_node_flags_set_parse_error(ast_node_flags* f);
bool ast_node_flags_get_parse_error(ast_node_flags f);
void ast_node_flags_set_redeclared(ast_node_flags* f);
bool ast_node_flags_get_redeclared(ast_node_flags f);

#endif