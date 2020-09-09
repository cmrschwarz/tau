#include "ast_flags.h"
#include "ast.h"
static inline void u16_set_bit(u16* data, ureg offs)
{
    *data = *data | (1 << offs);
}
static inline void u16_clear_bit(u16* data, ureg offs)
{
    *data = *data & ~((u16)1 << offs);
}
static inline bool u16_get_bit(u16 data, ureg offs)
{
    return data & (1 << offs);
}
static inline void u16_set_range(u16* data, ureg offs, u16 mask, u16 value)
{
    *data = (*data & ~mask) | (value << offs);
}
static inline u16 u16_get_range(u16 data, ureg offs, u16 mask)
{
    return (data & mask) >> offs;
}

void ast_node_set_default_flags(ast_node* n)
{
    n->flags = AST_NODE_FLAGS_DEFAULT;
    n->emitted_for_pp = false;
}

void ast_node_set_poisoned(ast_node* n)
{
    u16_set_bit(&n->flags, ASTF_POISONED_OFFSET);
}
bool ast_node_get_poisoned(ast_node* n)
{
    return u16_get_bit(n->flags, ASTF_POISONED_OFFSET);
}

void ast_node_set_instance_member(ast_node* n)
{
    u16_set_bit(&n->flags, ASTF_INSTANCE_MEMBER_OFFSET);
}
bool ast_node_get_instance_member(ast_node* n)
{
    return u16_get_bit(n->flags, ASTF_INSTANCE_MEMBER_OFFSET);
}

void ast_node_set_const(ast_node* n)
{
    u16_set_bit(&n->flags, ASTF_CONST_OFFSET);
}
bool ast_node_get_const(ast_node* n)
{
    return u16_get_bit(n->flags, ASTF_CONST_OFFSET);
}

void ast_node_set_pp_expr_res_used(ast_node* n)
{
    u16_set_bit(&n->flags, ASTF_PP_EXPR_RES_USED);
}
bool ast_node_get_pp_expr_res_used(ast_node* n)
{
    return u16_get_bit(n->flags, ASTF_PP_EXPR_RES_USED);
}

void ast_node_set_pp_expr_contains_paste_eval(ast_node* n)
{
    u16_set_bit(&n->flags, ASTF_PP_EXPR_CONTAINS_PASTE_EVAL);
}
bool ast_node_get_pp_expr_contains_paste_eval(ast_node* n)
{
    return u16_get_bit(n->flags, ASTF_PP_EXPR_CONTAINS_PASTE_EVAL);
}
void ast_node_set_extern_func(ast_node* n)
{
    u16_set_bit(&n->flags, ASTF_EXTERN_FUNC_OFFSET);
}
bool ast_node_get_extern_func(ast_node* n)
{
    return u16_get_bit(n->flags, ASTF_EXTERN_FUNC_OFFSET);
}

void ast_node_set_import_group_module_used(ast_node* n)
{
    u16_set_bit(&n->flags, ASTF_IMPORT_GROUP_MODULE_USED);
}
bool ast_node_get_import_group_module_used(ast_node* n)
{
    return u16_get_bit(n->flags, ASTF_IMPORT_GROUP_MODULE_USED);
}

void ast_node_set_pp_stmt_end_unreachabale(ast_node* n)
{
    u16_set_bit(&n->flags, ASTF_PP_STMT_END_UNREACHABLE);
}
bool ast_node_get_pp_stmt_end_unreachabale(ast_node* n)
{
    return u16_get_bit(n->flags, ASTF_PP_STMT_END_UNREACHABLE);
}

void ast_node_set_comptime(ast_node* n)
{
    u16_set_bit(&n->flags, ASTF_COMPTIME_OFFSET);
}
bool ast_node_get_comptime(ast_node* n)
{
    return u16_get_bit(n->flags, ASTF_COMPTIME_OFFSET);
}

void ast_node_set_static(ast_node* n)
{
    u16_set_bit(&n->flags, ASTF_STATIC_OFFSET);
}
bool ast_node_get_static(ast_node* n)
{
    return u16_get_bit(n->flags, ASTF_STATIC_OFFSET);
}

void ast_node_set_implicit(ast_node* n)
{
    u16_set_bit(&n->flags, ASTF_IMPLICIT_OFFSET);
}
bool ast_node_get_implicit(ast_node* n)
{
    return u16_get_bit(n->flags, ASTF_IMPLICIT_OFFSET);
}

void ast_node_set_func_is_op(ast_node* n)
{
    u16_set_bit(&n->flags, ASTF_FUNC_IS_OP_OFFSET);
}
bool ast_node_get_func_is_op(ast_node* n)
{
    return u16_get_bit(n->flags, ASTF_FUNC_IS_OP_OFFSET);
}

void ast_node_set_type_operator(ast_node* n)
{
    u16_set_bit(&n->flags, ASTF_TYPE_OPERATOR_OFFSET);
}
bool ast_node_get_type_operator(ast_node* n)
{
    return u16_get_bit(n->flags, ASTF_TYPE_OPERATOR_OFFSET);
}

void ast_node_set_compound_decl(ast_node* n)
{
    u16_set_bit(&n->flags, ASTF_COMPUND_DECL_OFFSET);
}
bool ast_node_get_compound_decl(ast_node* n)
{
    return u16_get_bit(n->flags, ASTF_COMPUND_DECL_OFFSET);
}

void ast_node_set_relative_import(ast_node* n)
{
    u16_set_bit(&n->flags, ASTF_RELATIVE_IMPORT_OFFSET);
}
bool ast_node_get_relative_import(ast_node* n)
{
    return u16_get_bit(n->flags, ASTF_RELATIVE_IMPORT_OFFSET);
}
void ast_node_clear_relative_import(ast_node* n)
{
    u16_clear_bit(&n->flags, ASTF_RELATIVE_IMPORT_OFFSET);
}

ast_node_status ast_node_get_status(ast_node* n)
{
    return (ast_node_status)u16_get_range(
        n->flags, ASTF_STATUS_OFFSET, ASTF_STATUS_MASK);
}
void ast_node_set_status(ast_node* n, ast_node_status s)
{
    u16_set_range(&n->flags, ASTF_STATUS_OFFSET, ASTF_STATUS_MASK, (u16)s);
}

void ast_node_set_declared(ast_node* n)
{
    assert(ast_node_get_status(n) == NODE_STATUS_PARSED);
    ast_node_set_status(n, NODE_STATUS_DECLARED);
}
bool ast_node_get_declared(ast_node* n)
{
    return ast_node_get_status(n) >= NODE_STATUS_DECLARED;
}
void ast_node_set_resolving(ast_node* n)
{
    assert(ast_node_get_status(n) <= NODE_STATUS_DECLARED);
    ast_node_set_status(n, NODE_STATUS_RESOLVING);
}
bool ast_node_get_resolving(ast_node* n)
{
    return ast_node_get_status(n) >= NODE_STATUS_RESOLVING;
}
void ast_node_clear_resolving(ast_node* n)
{
    assert(ast_node_get_status(n) == NODE_STATUS_RESOLVING);
    ast_node_set_status(n, NODE_STATUS_DECLARED);
}

void ast_node_set_trait_resolved(ast_node* n)
{
    assert(ast_node_get_status(n) <= NODE_STATUS_RESOLVING);
    ast_node_set_status(n, NODE_STATUS_TRAIT_RESOLVED);
}
bool ast_node_get_trait_resolved(ast_node* n)
{
    return ast_node_get_status(n) >= NODE_STATUS_TRAIT_RESOLVED;
}

void ast_node_set_resolved(ast_node* n)
{
    assert(ast_node_get_status(n) <= NODE_STATUS_RESOLVED);
    ast_node_set_status(n, NODE_STATUS_RESOLVED);
}
bool ast_node_get_resolved(ast_node* n)
{
    return ast_node_get_status(n) >= NODE_STATUS_RESOLVED;
}

void ast_node_set_emitted_for_pp(ast_node* n)
{
    assert(ast_elem_is_symbol((ast_elem*)n) || n->kind == EXPR_PP);
    assert(!n->emitted_for_pp);
    n->emitted_for_pp = true;
}
bool ast_node_get_emitted_for_pp(ast_node* n)
{
    assert(ast_elem_is_symbol((ast_elem*)n) || n->kind == EXPR_PP);
    return n->emitted_for_pp;
}

bool ast_node_get_contains_error(ast_node* n)
{
    return u16_get_bit(n->flags, ASTF_CONTAINS_ERRORS);
}
void ast_node_set_contains_error(ast_node* n)
{
    u16_set_bit(&n->flags, ASTF_CONTAINS_ERRORS);
}
void ast_node_set_used_in_pp(ast_node* n)
{
    u16_set_bit(&n->flags, ASTF_USED_IN_PP_OFFSET);
}
bool ast_node_get_used_in_pp(ast_node* n)
{
    return u16_get_bit(n->flags, ASTF_USED_IN_PP_OFFSET);
}

void ast_node_set_access_mod(ast_node* n, access_modifier m)
{
    u16_set_range(
        &n->flags, ASTF_ACCESS_MODIFIER_OFFSET, ASTF_ACCESS_MODIFIER_MASK, m);
}
access_modifier ast_node_get_access_mod(ast_node* n)
{
    return (access_modifier)(u16_get_range(
        n->flags, ASTF_ACCESS_MODIFIER_OFFSET, ASTF_ACCESS_MODIFIER_MASK));
}

void ast_node_set_dtor_kind(ast_node* n, dtor_kind dk)
{
    u16_set_range(&n->flags, ASTF_DTOR_KIND_OFFSET, ASTF_DTOR_KIND_MASK, dk);
}

dtor_kind ast_node_get_dtor_kind(ast_node* n)
{
    return (dtor_kind)(
        u16_get_range(n->flags, ASTF_DTOR_KIND_OFFSET, ASTF_DTOR_KIND_MASK));
}