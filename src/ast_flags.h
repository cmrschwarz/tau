#pragma once

#include "utils/c_extensions.h"
typedef struct ast_node_s ast_node;
#define AST_NODE_FLAGS_DEFAULT (u16)0

// !symbol_table's data layout depends on this enums order
typedef enum PACK_ENUM access_modifier_e {
    AM_LOCAL, // module frame (default inside structs and mf's)
    AM_INTERNAL, // module
    AM_PUBLIC, // everybody
    AM_PROTECTED, // current scope + scopes with use ...;
    AM_PRIVATE, // current scope (module / struct)
    AM_NONE,
    AM_ENUM_ELEMENT_COUNT = AM_NONE,
} access_modifier;

typedef enum PACK_ENUM dtor_kind_e {
    DTOR_KIND_POD,
    DTOR_KIND_KNOWN_DEAD,
    DTOR_KIND_STATIC,
    DTOR_KIND_DYNAMIC,
} dtor_kind;

// after the resolver we repurpose these bits
#define ASTF_DECLARED_OFFSET 0 // on any ast node, during resolvion
#define ASTF_RESOLVING_OFFSET 1 // on any ast node, during resolvion
#define ASTF_DTOR_KIND_OFFSET 0 // on sym var (when in func), set during prp
#define ASTF_DTOR_KIND_MASK (0x3 << ASTF_ACCESS_MODIFIER_OFFSET)

#define ASTF_RESOLVED_OFFSET 2

// shared bit since applied to different nodes
#define ASTF_STATIC_OFFSET 3 // on sym_var
#define ASTF_PP_STMT_END_UNREACHABLE 3 // on stmt_paste_evaluation

#define ASTF_COMPTIME_OFFSET 4 // basically on any symbol

#define ASTF_IMPLICIT_OFFSET 5 // on ops / funcs
#define ASTF_EXPLICIT_OFFSET 5 // on variables

// shared bit since applied to different nodes
#define ASTF_CONST_OFFSET 6 // on vars and funcs
#define ASTF_PP_EXPR_RES_USED 6 // on expr_pp
#define ASTF_IMPORT_GROUP_MODULE_USED 6 // om sym_import_group ({(),..} or not)

#define ASTF_POISONED_OFFSET 7 // TODO: implement poisoning

// shared bit since applied to different nodes
#define ASTF_COMPUND_DECL_OFFSET 8 // on sym_var
#define ASTF_TYPE_OPERATOR_OFFSET 8 // on op_binary / op_unary
#define ASTF_RELATIVE_IMPORT_OFFSET 8 // on sym_import_module (when not ::xx)
#define ASTF_EXTERN_FUNC_OFFSET 8 // on funcs

// on funcs and global vars. needed for interupted resolution
#define ASTF_EMITTED_FOR_PP 9

#define ASTF_USED_IN_PP_OFFSET 10

#define ASTF_FUNC_IS_OP_OFFSET 11 // on funcs --> ops

#define ASTF_INSTANCE_MEMBER_OFFSET 12 // on funcs and vars and expr_calls

// needs 3 bits (13 - 15)
#define ASTF_ACCESS_MODIFIER_OFFSET 13 // on any symbol
#define ASTF_ACCESS_MODIFIER_MASK (0x7 << ASTF_ACCESS_MODIFIER_OFFSET)

void ast_node_set_default_flags(ast_node* n);

void ast_node_set_access_mod(ast_node* n, access_modifier m);
access_modifier ast_node_get_access_mod(ast_node* n);

void ast_node_set_dtor_kind(ast_node* n, dtor_kind dk);
dtor_kind ast_node_get_dtor_kind(ast_node* n);

void ast_node_set_const(ast_node* n);
bool ast_node_get_const(ast_node* n);

void ast_node_set_pp_expr_res_used(ast_node* n);
bool ast_node_get_pp_expr_res_used(ast_node* n);

void ast_node_set_extern_func(ast_node* n);
bool ast_node_get_extern_func(ast_node* n);

void ast_node_set_import_group_module_used(ast_node* n);
bool ast_node_get_import_group_module_used(ast_node* n);

void ast_node_set_comptime(ast_node* n);
bool ast_node_get_comptime(ast_node* n);

void ast_node_set_implicit(ast_node* n);
bool ast_node_get_implicit(ast_node* n);

void ast_node_set_explicit(ast_node* n);
bool ast_node_get_explicit(ast_node* n);

void ast_node_set_not_required(ast_node* n);
bool ast_node_get_not_required(ast_node* n);

void ast_node_set_func_is_op(ast_node* n);
bool ast_node_get_func_is_op(ast_node* n);

void ast_node_set_pp_stmt_end_unreachabale(ast_node* n);
bool ast_node_get_pp_stmt_end_unreachabale(ast_node* n);

void ast_node_set_static(ast_node* n);
bool ast_node_get_static(ast_node* n);

void ast_node_set_type_operator(ast_node* n);
bool ast_node_get_type_operator(ast_node* n);

void ast_node_set_compound_decl(ast_node* n);
bool ast_node_get_compound_decl(ast_node* n);

void ast_node_set_relative_import(ast_node* n);
bool ast_node_get_relative_import(ast_node* n);
void ast_flags_clear_relative_import(ast_node* n);

void ast_node_set_declared(ast_node* n);
bool ast_node_get_declared(ast_node* n);
void ast_flags_clear_declared(ast_node* n);

void ast_node_set_resolved(ast_node* n);
bool ast_node_get_resolved(ast_node* n);
void ast_flags_clear_resolved(ast_node* n);

void ast_node_set_resolving(ast_node* n);
void ast_flags_clear_resolving(ast_node* n);
bool ast_node_get_resolving(ast_node* n);

void ast_node_set_overloaded_in_pp(ast_node* n);
bool ast_node_get_overloaded_in_pp(ast_node* n);

void ast_node_set_used_in_pp(ast_node* n);
bool ast_node_get_used_in_pp(ast_node* n);

void ast_node_set_poisoned(ast_node* n);
bool ast_node_get_poisoned(ast_node* n);

void ast_node_set_instance_member(ast_node* n);
bool ast_node_get_instance_member(ast_node* n);

bool ast_node_get_emitted_for_pp(ast_node* n);
void ast_node_set_emitted_for_pp(ast_node* n);
