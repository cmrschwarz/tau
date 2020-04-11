#include "ast_flags.h"

static inline void bitmask_set_bit(u16* data, ureg offs)
{
    *data = *data | (1 << offs);
}
static inline void bitmask_clear_bit(u16* data, ureg offs)
{
    *data = *data & ~((u16)1 << offs);
}
static inline bool bitmask_get_bit(u16 data, ureg offs)
{
    return data & (1 << offs);
}
static inline void bitmask_set_range(u16* data, ureg offs, u16 value)
{
    *data = *data | (value << offs);
}
static inline u16 bitmask_get_range(u16 data, ureg offs, ureg mask)
{
    return (data & mask) >> offs;
}

void ast_flags_set_access_mod(ast_flags* f, access_modifier m)
{
    bitmask_set_range(f, ASTF_ACCESS_MODIFIER_OFFSET, m);
}
access_modifier ast_flags_get_access_mod(ast_flags f)
{
    return (access_modifier)(bitmask_get_range(
        f, ASTF_ACCESS_MODIFIER_OFFSET, ASTF_ACCESS_MODIFIER_MASK));
}

void ast_flags_set_const(ast_flags* f)
{
    bitmask_set_bit(f, ASTF_CONST_OFFSET);
}
bool ast_flags_get_const(ast_flags f)
{
    return bitmask_get_bit(f, ASTF_CONST_OFFSET);
}

void ast_flags_set_pp_expr_res_used(ast_flags* f)
{
    bitmask_set_bit(f, ASTF_PP_EXPR_RES_USED);
}
bool ast_flags_get_pp_expr_res_used(ast_flags f)
{
    return bitmask_get_bit(f, ASTF_PP_EXPR_RES_USED);
}

void ast_flags_set_extern_func(ast_flags* f)
{
    bitmask_set_bit(f, ASTF_EXTERN_FUNC_OFFSET);
}
bool ast_flags_get_extern_func(ast_flags f)
{
    return bitmask_get_bit(f, ASTF_EXTERN_FUNC_OFFSET);
}
void ast_flags_set_import_group_module_used(ast_flags* f)
{
    bitmask_set_bit(f, ASTF_IMPORT_GROUP_MODULE_USED);
}
bool ast_flags_get_import_group_module_used(ast_flags f)
{
    return bitmask_get_bit(f, ASTF_IMPORT_GROUP_MODULE_USED);
}

void ast_flags_set_pp_stmt_end_unreachabale(ast_flags* f)
{
    bitmask_set_bit(f, ASTF_PP_STMT_END_UNREACHABLE);
}
bool ast_flags_get_pp_stmt_end_unreachabale(ast_flags f)
{
    return bitmask_get_bit(f, ASTF_PP_STMT_END_UNREACHABLE);
}

void ast_flags_set_comptime(ast_flags* f)
{
    bitmask_set_bit(f, ASTF_COMPTIME_OFFSET);
}
bool ast_flags_get_comptime(ast_flags f)
{
    return bitmask_get_bit(f, ASTF_COMPTIME_OFFSET);
}

void ast_flags_set_static(ast_flags* f)
{
    bitmask_set_bit(f, ASTF_STATIC_OFFSET);
}
bool ast_flags_get_static(ast_flags f)
{
    return bitmask_get_bit(f, ASTF_STATIC_OFFSET);
}

void ast_flags_set_implicit(ast_flags* f)
{
    bitmask_set_bit(f, ASTF_IMPLICIT_OFFSET);
}
bool ast_flags_get_implicit(ast_flags f)
{
    return bitmask_get_bit(f, ASTF_IMPLICIT_OFFSET);
}

void ast_flags_set_explicit(ast_flags* f)
{
    bitmask_set_bit(f, ASTF_EXPLICIT_OFFSET);
}
bool ast_flags_get_explicit(ast_flags f)
{
    return bitmask_get_bit(f, ASTF_EXPLICIT_OFFSET);
}

void ast_flags_set_func_is_op(ast_flags* f)
{
    bitmask_set_bit(f, ASTF_FUNC_IS_OP_OFFSET);
}
bool ast_flags_get_func_is_op(ast_flags f)
{
    return bitmask_get_bit(f, ASTF_FUNC_IS_OP_OFFSET);
}

void ast_flags_set_type_operator(ast_flags* f)
{
    bitmask_set_bit(f, ASTF_TYPE_OPERATOR_OFFSET);
}
bool ast_flags_get_type_operator(ast_flags f)
{
    return bitmask_get_bit(f, ASTF_TYPE_OPERATOR_OFFSET);
}

void ast_flags_set_compound_decl(ast_flags* f)
{
    bitmask_set_bit(f, ASTF_COMPUND_DECL_OFFSET);
}
bool ast_flags_get_compound_decl(ast_flags f)
{
    return bitmask_get_bit(f, ASTF_COMPUND_DECL_OFFSET);
}

void ast_flags_set_declared(ast_flags* f)
{
    bitmask_set_bit(f, ASTF_DECLARED_OFFSET);
}
bool ast_flags_get_declared(ast_flags f)
{
    return bitmask_get_bit(f, ASTF_DECLARED_OFFSET);
}
void ast_flags_clear_declared(ast_flags* f)
{
    bitmask_clear_bit(f, ASTF_DECLARED_OFFSET);
}

void ast_flags_set_relative_import(ast_flags* f)
{
    bitmask_set_bit(f, ASTF_RELATIVE_IMPORT_OFFSET);
}
bool ast_flags_get_relative_import(ast_flags f)
{
    return bitmask_get_bit(f, ASTF_RELATIVE_IMPORT_OFFSET);
}

void ast_flags_set_error(ast_flags* f)
{
    bitmask_set_bit(f, ASTF_ERROR_OFFSET);
}
bool ast_flags_get_error(ast_flags f)
{
    return bitmask_get_bit(f, ASTF_ERROR_OFFSET);
}

void ast_flags_set_resolved(ast_flags* f)
{
    bitmask_set_bit(f, ASTF_RESOLVED_OFFSET);
}
bool ast_flags_get_resolved(ast_flags f)
{
    return bitmask_get_bit(f, ASTF_RESOLVED_OFFSET);
}
void ast_flags_clear_resolved(ast_flags* f)
{
    bitmask_clear_bit(f, ASTF_RESOLVED_OFFSET);
}

void ast_flags_set_resolving(ast_flags* f)
{
    bitmask_set_bit(f, ASTF_RESOLVING_OFFSET);
}
void ast_flags_clear_resolving(ast_flags* f)
{
    bitmask_clear_bit(f, ASTF_RESOLVING_OFFSET);
}
bool ast_flags_get_resolving(ast_flags f)
{
    return bitmask_get_bit(f, ASTF_RESOLVING_OFFSET);
}

void ast_flags_set_overloaded_in_pp(ast_flags* f)
{
    bitmask_set_bit(f, ASTF_OVERLOADED_IN_PP_OFFSET);
}
bool ast_flags_get_overloaded_in_pp(ast_flags f)
{
    return bitmask_get_bit(f, ASTF_OVERLOADED_IN_PP_OFFSET);
}
void ast_flags_set_used_in_pp(ast_flags* f)
{
    bitmask_set_bit(f, ASTF_USED_IN_PP_OFFSET);
}
bool ast_flags_get_used_in_pp(ast_flags f)
{
    return bitmask_get_bit(f, ASTF_USED_IN_PP_OFFSET);
}
void ast_flags_set_instance_member(ast_flags* f)
{
    bitmask_set_bit(f, ASTF_INSTANCE_MEMBER_OFFSET);
}
bool ast_flags_get_instance_member(ast_flags f)
{
    return bitmask_get_bit(f, ASTF_INSTANCE_MEMBER_OFFSET);
}
