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

// shared bit since applied to different nodes
#define ASTF_SEALED_OFFSET 5
#define ASTF_PP_STMT_END_UNREACHABLE 5

// shared bit since applied to different nodes
#define ASTF_PP_EXPR_RES_USED 6
#define ASTF_IMPORT_GROUP_MODULE_USED 6
#define ASTF_CONST_OFFSET 6

#define ASTF_ERROR_OFFSET 7

// shared bit since applied to different nodes
#define ASTF_RELATIVE_IMPORT_OFFSET 8
#define ASTF_EXTERN_FUNC_OFFSET 8
#define ASTF_COMPUND_DECL_OFFSET 8
#define ASTF_PASTING_PP_EXPR_OFFSET 8

#define ASTF_OVERLOADED_IN_PP_OFFSET 9
#define ASTF_USED_IN_PP_OFFSET 10

#define ASTF_COMPTIME_KNOWN 11

#define ASTF_MEMBER_FUNC_OFFSET 12

#define ASTF_ACCESS_MODIFIER_OFFSET 13
#define ASTF_ACCESS_MODIFIER_MASK (0x7 << ASTF_ACCESS_MODIFIER_OFFSET)

void ast_flags_set_access_mod(ast_flags* f, access_modifier m);
access_modifier ast_flags_get_access_mod(ast_flags f);

void ast_flags_set_const(ast_flags* f);
bool ast_flags_get_const(ast_flags f);

void ast_flags_set_pp_expr_res_used(ast_flags* f);
bool ast_flags_get_pp_expr_res_used(ast_flags f);

void ast_flags_set_extern_func(ast_flags* f);
bool ast_flags_get_extern_func(ast_flags f);

void ast_flags_set_import_group_module_used(ast_flags* f);
bool ast_flags_get_import_group_module_used(ast_flags f);

void ast_flags_set_sealed(ast_flags* f);
bool ast_flags_get_sealed(ast_flags f);

void ast_flags_set_pp_stmt_end_unreachabale(ast_flags* f);
bool ast_flags_get_pp_stmt_end_unreachabale(ast_flags f);

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

void ast_flags_set_comptime_known(ast_flags* f);
bool ast_flags_get_comptime_known(ast_flags f);

// used for expr_calls and the funcs themselves
void ast_flags_set_member_func(ast_flags* f);
bool ast_flags_get_member_func(ast_flags f);

#endif
