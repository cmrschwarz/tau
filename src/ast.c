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
    return *s <= OSC_LAST_OSC_ID;
}
bool ast_node_is_scope(ast_node* s)
{
    return *s <= SC_LAST_SC_ID;
}
bool ast_node_is_symbol(ast_node* s)
{
    return *s <= SYM_LAST_SYM_ID;
}
bool ast_node_is_stmt(ast_node* s)
{
    return (*s < STMT_LAST_STMT_ID);
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
    switch (n->kind) {
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
