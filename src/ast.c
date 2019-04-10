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
        case SC_MODULE:
        case SC_EXTEND:
        case SC_STRUCT:
        case SC_TRAIT:
        case SC_FUNC:
        case SC_MODULE_GENERIC:
        case SC_EXTEND_GENERIC:
        case SC_STRUCT_GENERIC:
        case SC_TRAIT_GENERIC:
        case SYM_VAR:
        case SC_FUNC_GENERIC: {
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
#define SEALED_OFFSET 3
#define VIRTUAL_OFFSET 2
#define STATIC_OFFSET 1
#define EXTEND_OFFSET 0

static inline void bitmask_set_bit(u8* data, ureg offs)
{
    *data = *data | 1 << offs;
}
static inline void bitmask_clear_bit(u8* data, ureg offs)
{
    *data = *data & ~((u8)1 << offs);
}
static inline bool bitmask_get_bit(u8 data, ureg offs)
{
    return (data >> offs) & 0x1;
}
static inline void bitmask_set_range(u8* data, ureg offs, u8 value)
{
    *data = *data | (value << offs);
}
static inline void bitmask_clear_range(u8* data, ureg mask)
{
    *data = *data & ~mask;
}
static inline u8 bitmask_get_range(u8 data, ureg offs, ureg mask)
{
    return (data >> offs) & mask;
}

void stmt_flags_set_access_mod(stmt_flags* f, access_modifier m)
{
    bitmask_set_range(f, AM_OFFSET, m);
}
access_modifier stmt_flags_get_access_mod(stmt_flags f)
{
    return (access_modifier)(bitmask_get_range(f, AM_OFFSET, AM_MASK));
}

void stmt_flags_set_const(stmt_flags* f)
{
    bitmask_set_bit(f, CONST_OFFSET);
}
bool stmt_flags_get_const(stmt_flags f)
{
    return bitmask_get_bit(f, CONST_OFFSET);
}

void stmt_flags_set_sealed(stmt_flags* f)
{
    bitmask_set_bit(f, SEALED_OFFSET);
}
bool stmt_flags_get_sealed(stmt_flags f)
{
    return bitmask_get_bit(f, SEALED_OFFSET);
}

void stmt_flags_set_virtual(stmt_flags* f)
{
    bitmask_set_bit(f, VIRTUAL_OFFSET);
}
bool stmt_flags_get_virtual(stmt_flags f)
{
    return bitmask_get_bit(f, VIRTUAL_OFFSET);
}

void stmt_flags_set_static(stmt_flags* f)
{
    bitmask_set_bit(f, STATIC_OFFSET);
}
bool stmt_flags_get_static(stmt_flags f)
{
    return bitmask_get_bit(f, STATIC_OFFSET);
}

void stmt_flags_set_module_extension(stmt_flags* f)
{
    bitmask_set_bit(f, EXTEND_OFFSET);
}
bool stmt_flags_get_module_extension(stmt_flags f)
{
    return bitmask_get_bit(f, EXTEND_OFFSET);
}

#define REDECL_OFFSET 7

void err_flags_set_redeclared(err_flags* f)
{
    bitmask_set_bit(f, REDECL_OFFSET);
}
bool err_flags_get_redeclared(err_flags f)
{
    return bitmask_get_bit(f, REDECL_OFFSET);
}
