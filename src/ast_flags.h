#ifndef TAUC_AST_NODE_FLAGS_H
#define TAUC_AST_NODE_FLAGS_H

#include "utils/c_extensions.h"
typedef u16 ast_flags;
#define AST_NODE_FLAGS_DEFAULT (u16)0

// !symbol_table's data layout depends on this enums order
typedef enum PACK_ENUM access_modifier_e {
    AM_DEFAULT = 0, // osc
    AM_INTERNAL = 1, // module
    AM_PRIVATE = 2, // current scope (module / struct)
    AM_PROTECTED = 3, // current scope + scopes with using ...;
    AM_PUBLIC = 4, // everybody
    AM_ENUM_ELEMENT_COUNT = 5,
} access_modifier;

#define ASTF_DECLARED_OFFSET 0
#define ASTF_RESOLVING_OFFSET 1
#define ASTF_RESOLVED_OFFSET 2
#define ASTF_STATIC_OFFSET 3
#define ASTF_VIRTUAL_OFFSET 4
#define ASTF_SEALED_OFFSET 5
#define ASTF_CONST_OFFSET 6
#define ASTF_ERROR_OFFSET 7

// the following three are node specific and never overlap
#define ASTF_RELATIVE_IMPORT_OFFSET 8
#define ASTF_COMPUND_DECL_OFFSET 8
#define ASTF_PASTING_PP_EXPR_OFFSET 8

#define ASTF_OVERLOADED_IN_PP_OFFSET 9
#define ASTF_USED_IN_PP_OFFSET 10
// we have space for 3 more here
#define ASTF_ACCESS_MODIFIER_OFFSET 13
#define ASTF_ACCESS_MODIFIER_MASK (0x7 << ASTF_ACCESS_MODIFIER_OFFSET)

typedef enum ast_flags_values_e {
    // this stays set even once resolved
    ASTF_DECLARED = 1 << ASTF_DECLARED_OFFSET,
    ASTF_RESOLVING = 1 << ASTF_RESOLVING_OFFSET,
    ASTF_RESOLVED = 1 << ASTF_RESOLVED_OFFSET,
    ASTF_STATIC = 1 << ASTF_STATIC_OFFSET,
    ASTF_VIRTUAL = 1 << ASTF_VIRTUAL_OFFSET,
    ASTF_SEALED = 1 << ASTF_SEALED_OFFSET,
    ASTF_CONST = 1 << ASTF_CONST_OFFSET,
    ASTF_ERROR = 1 << ASTF_ERROR_OFFSET,
    ASTF_RELATIVE_IMPORT = 1 << ASTF_RELATIVE_IMPORT_OFFSET,
    ASTF_COMPUND_DECL = 1 << ASTF_COMPUND_DECL_OFFSET,
    ASTF_PASTING_PP_EXPR = 1 << ASTF_PASTING_PP_EXPR_OFFSET,
    ASTF_OVERLOADED_IN_PP = 1 << ASTF_OVERLOADED_IN_PP_OFFSET,
    ASTF_USED_IN_PP = 1 << ASTF_USED_IN_PP_OFFSET,
    ASTF_ACCESS_MODIFIER = ASTF_ACCESS_MODIFIER_MASK, // ! multiple bits
} ast_flags_values;

void ast_flags_set_access_mod(ast_flags* f, access_modifier m);
access_modifier ast_flags_get_access_mod(ast_flags f);

void ast_flags_set_const(ast_flags* f);
bool ast_flags_get_const(ast_flags f);

void ast_flags_set_sealed(ast_flags* f);
bool ast_flags_get_sealed(ast_flags f);

void ast_flags_set_virtual(ast_flags* f);
bool ast_flags_get_virtual(ast_flags f);

void ast_flags_set_static(ast_flags* f);
bool ast_flags_get_static(ast_flags f);

void ast_flags_set_compound_decl(ast_flags* f);
bool ast_flags_get_compound_decl(ast_flags f);

void ast_flags_set_relative_import(ast_flags* f);
bool ast_flags_get_relative_import(ast_flags f);

void ast_flags_set_pasting_pp_expr(ast_flags* f);
bool ast_flags_get_pasting_pp_expr(ast_flags f);

void ast_flags_set_declared(ast_flags* f);
bool ast_flags_get_declared(ast_flags f);

void ast_flags_set_resolved(ast_flags* f);
bool ast_flags_get_resolved(ast_flags f);

void ast_flags_set_resolving(ast_flags* f);
void ast_flags_clear_resolving(ast_flags* f);
bool ast_flags_get_resolving(ast_flags f);

void ast_flags_set_overloaded_in_pp(ast_flags* f);
bool ast_flags_get_overloaded_in_pp(ast_flags f);

void ast_flags_set_used_in_pp(ast_flags* f);
bool ast_flags_get_used_in_pp(ast_flags f);

void ast_flags_set_error(ast_flags* f);
bool ast_flags_get_error(ast_flags f);

#endif
