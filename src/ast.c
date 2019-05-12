#include "ast.h"
#include "utils/panic.h"

src_file* open_scope_get_file(open_scope* s)
{
    src_range_large srl;
    src_range_unpack(s->scope.symbol.stmt.srange, &srl);
    return srl.file;
}
bool ast_node_is_open_scope(ast_node* s)
{
    switch (*s) {
        case OSC_MODULE:
        case OSC_EXTEND:
        case OSC_EXTEND_GENERIC:
        case OSC_MODULE_GENERIC: return true;
        default: return false;
    }
}
bool ast_node_is_expr(ast_node* s)
{
    return (*s > STMT_LAST_STMT_ID);
}
src_range ast_node_get_src_range(ast_node* s)
{
    if (ast_node_is_expr(s)) return ((expr*)s)->srange;
    return ((stmt*)s)->srange;
}
bool body_is_braced(body* b)
{
    if (b->children && !b->children->next) {
        return (b->srange != b->children->srange);
    }
    return true;
}
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
#define AM_OFFSET 6
#define AM_MASK (3 << AM_OFFSET)
#define CONST_OFFSET 4
#define SEALED_OFFSET 3
#define VIRTUAL_OFFSET 2
#define STATIC_OFFSET 1
#define COMPUND_DECL_OFFSET 0

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

void stmt_flags_set_compound_decl(stmt_flags* f)
{
    bitmask_set_bit(f, COMPUND_DECL_OFFSET);
}
bool stmt_flags_get_compound_decl(stmt_flags f)
{
    return bitmask_get_bit(f, COMPUND_DECL_OFFSET);
}

#define REDECL_OFFSET 7
#define PE_OFFSET 6
void err_flags_set_parse_error(err_flags* f)
{
    bitmask_set_bit(f, PE_OFFSET);
}
bool err_flags_get_parse_error(err_flags f)
{
    return bitmask_get_bit(f, PE_OFFSET);
}
void err_flags_set_redeclared(err_flags* f)
{
    bitmask_set_bit(f, REDECL_OFFSET);
}
bool err_flags_get_redeclared(err_flags f)
{
    return bitmask_get_bit(f, REDECL_OFFSET);
}
