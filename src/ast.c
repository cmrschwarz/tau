#include "ast.h"
#include "utils/panic.h"

bool is_unary_op_postfix(op_type t)
{
    switch (t) {
        case OP_POST_INCREMENT:
        case OP_POST_DECREMENT: return true;
        default: return false;
    }
}
void get_expr_bounds(expr* n, ureg* start, ureg* end)
{
    switch (n->type) {
        case EXPR_LOOP: {
            if (start) *start = src_range_get_start(n->srange);
            if (end) get_expr_bounds(n, NULL, end);
            return;
        }
        case EXPR_OP_BINARY: {
            if (start) get_expr_bounds(((expr_op_binary*)n)->lhs, start, NULL);
            if (end) get_expr_bounds(((expr_op_binary*)n)->rhs, NULL, end);
            return;
        }
        case EXPR_OP_UNARY: {
            expr_op_unary* u = (expr_op_unary*)n;
            if (!is_unary_op_postfix(u->expr.op_type)) {
                if (start) {
                    src_range_large r;
                    src_range_unpack(u->expr.srange, &r);
                    *start = r.start;
                }
                if (end) get_expr_bounds(u->child, NULL, end);
                return;
            }
        } // fallthrough for postfix unary op
        default: {
            src_range_large r;
            src_range_unpack(((expr*)n)->srange, &r);
            if (start) *start = r.start;
            if (end) *end = r.end;
            return;
        }
    }
}
void stmt_get_highlight_bounds(stmt* stmt, ureg* start, ureg* end)
{
    switch (stmt->type) {
        case SCS_MODULE:
        case SCF_EXTEND:
        case SC_STRUCT:
        case SC_TRAIT:
        case SCF_FUNC:
        case SCS_MODULE_GENERIC:
        case SCF_EXTEND_GENERIC:
        case SC_STRUCT_GENERIC:
        case SC_TRAIT_GENERIC:
        case SYM_VAR_DECL:
        case SCF_FUNC_GENERIC: {
            src_range dr = ((symbol*)stmt)->decl_range;
            src_range_large srl;
            src_range_unpack(dr, &srl);
            if (start) *start = srl.start;
            if (end) *end = srl.end;
            return;
        }
        case STMT_EXPRESSION: {
            get_expr_bounds(((stmt_expr*)stmt)->expr, start, end);
            return;
        }
        default: {
            panic("unexpected astnt");
            return;
        }
    }
}
#define AM_OFFSET 6
#define AM_MASK (3 << AM_OFFSET)
#define CONST_OFFSET 4
#define CONST_MASK (1 << CONST_OFFSET)
#define SEALED_OFFSET 3
#define SEALED_MASK (1 << SEALED_OFFSET)
#define VIRTUAL_OFFSET 2
#define VIRTUAL_MASK (1 << VIRTUAL_OFFSET)
#define STATIC_OFFSET 1
#define STATIC_MASK (1 << STATIC_OFFSET)
#define EXTEND_OFFSET 0
#define EXTEND_MASK (1 << STATIC_OFFSET)

void stmt_flags_set_access_mod(stmt_flags* f, access_modifier m)
{
    *f = *f | (m << AM_OFFSET);
}
access_modifier stmt_flags_get_access_mod(stmt_flags f)
{
    return (access_modifier)((f & AM_MASK) >> AM_OFFSET);
}

void stmt_flags_set_const(stmt_flags* f, bool cnst)
{
    *f = *f | (cnst << CONST_OFFSET);
}
bool stmt_flags_get_const(stmt_flags f)
{
    return (f & CONST_MASK) >> CONST_OFFSET;
}

void stmt_flags_set_sealed(stmt_flags* f, bool sld)
{
    *f = *f | (sld << SEALED_OFFSET);
}
bool stmt_flags_get_sealed(stmt_flags f)
{
    return (f & SEALED_MASK) >> SEALED_OFFSET;
}

void stmt_flags_set_virtual(stmt_flags* f, bool virt)
{
    *f = *f | (virt << VIRTUAL_OFFSET);
}
bool stmt_flags_get_virtual(stmt_flags f)
{
    return (f & VIRTUAL_MASK) >> VIRTUAL_OFFSET;
}

void stmt_flags_set_static(stmt_flags* f, bool stat)
{
    *f = *f | (stat << STATIC_OFFSET);
}
bool stmt_flags_get_static(stmt_flags f)
{
    return (f & STATIC_MASK) >> STATIC_OFFSET;
}

void stmt_flags_set_module_extension(stmt_flags* f, bool ext)
{
    *f = *f | (ext << EXTEND_OFFSET);
}
bool stmt_flags_get_module_extension(stmt_flags f)
{
    return (f & EXTEND_MASK) >> EXTEND_OFFSET;
}