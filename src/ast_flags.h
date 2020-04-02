#ifndef TAUC_AST_NODE_FLAGS_H
#define TAUC_AST_NODE_FLAGS_H

#include "utils/c_extensions.h"
typedef u16 ast_flags;
#define AST_NODE_FLAGS_DEFAULT (u16)0

// !symbol_table's data layout depends on this enums order
typedef enum PACK_ENUM access_modifier_e {
    AM_LOCAL, // module frame (default inside structs and mf's)
    AM_INTERNAL, // module
    AM_PUBLIC, // everybody
    AM_PROTECTED, // current scope + scopes with use ...;
    AM_PRIVATE, // current scope (module / struct)
    AM_ENUM_ELEMENT_COUNT,
    AM_UNKNOWN = AM_ENUM_ELEMENT_COUNT,
} access_modifier;

#define ASTF_DECLARED_OFFSET 0
#define ASTF_RESOLVING_OFFSET 1
#define ASTF_RESOLVED_OFFSET 2

// shared bit since applied to different nodes
#define ASTF_STATIC_OFFSET 3 // on sym_var
#define ASTF_PP_STMT_END_UNREACHABLE 3 // on stmt_paste_evaluation

#define ASTF_COMPTIME_OFFSET 4 // on sym_var

// free slot: 5

// shared bit since applied to different nodes
#define ASTF_CONST_OFFSET 6 // on sym_var
#define ASTF_PP_EXPR_RES_USED 6 // on expr_pp
#define ASTF_IMPORT_GROUP_MODULE_USED 6 // om sym_import_group ({(),..} or not)

#define ASTF_ERROR_OFFSET 7 // TODO: implement poisoning

// shared bit since applied to different nodes
#define ASTF_COMPUND_DECL_OFFSET 8 // on sym_var
#define ASTF_TYPE_OPERATOR_OFFSET 8 // on op_binary / op_unary
#define ASTF_RELATIVE_IMPORT_OFFSET 8 // on sym_import_module (when not ::xx)
#define ASTF_EXTERN_FUNC_OFFSET 8 // on sc_func

#define ASTF_OVERLOADED_IN_PP_OFFSET 9 // TODO: implement use xor change rule
#define ASTF_USED_IN_PP_OFFSET 10

// free slot: 11

#define ASTF_INSTANCE_MEMBER_OFFSET 12 // on sym_var

#define ASTF_ACCESS_MODIFIER_OFFSET 13 // on sym_var
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

void ast_flags_set_comptime(ast_flags* f);
bool ast_flags_get_comptime(ast_flags f);

void ast_flags_set_pp_stmt_end_unreachabale(ast_flags* f);
bool ast_flags_get_pp_stmt_end_unreachabale(ast_flags f);

void ast_flags_set_virtual(ast_flags* f);
bool ast_flags_get_virtual(ast_flags f);

void ast_flags_set_static(ast_flags* f);
bool ast_flags_get_static(ast_flags f);

void ast_flags_set_type_operator(ast_flags* f);
bool ast_flags_get_type_operator(ast_flags f);

void ast_flags_set_compound_decl(ast_flags* f);
bool ast_flags_get_compound_decl(ast_flags f);

void ast_flags_set_relative_import(ast_flags* f);
bool ast_flags_get_relative_import(ast_flags f);

void ast_flags_set_declared(ast_flags* f);
bool ast_flags_get_declared(ast_flags f);
void ast_flags_clear_declared(ast_flags* f);

void ast_flags_set_resolved(ast_flags* f);
bool ast_flags_get_resolved(ast_flags f);
void ast_flags_clear_resolved(ast_flags* f);

void ast_flags_set_resolving(ast_flags* f);
void ast_flags_clear_resolving(ast_flags* f);
bool ast_flags_get_resolving(ast_flags f);

void ast_flags_set_overloaded_in_pp(ast_flags* f);
bool ast_flags_get_overloaded_in_pp(ast_flags f);

void ast_flags_set_used_in_pp(ast_flags* f);
bool ast_flags_get_used_in_pp(ast_flags f);

void ast_flags_set_error(ast_flags* f);
bool ast_flags_get_error(ast_flags f);

// used for expr_calls, funcs and vars
void ast_flags_set_instance_member(ast_flags* f);
bool ast_flags_get_instance_member(ast_flags f);

#endif
