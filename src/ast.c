#include "ast.h"
#include "utils/panic.h"
#include "utils/c_extensions.h"

#define mk_prim(prim_kind, prim_name)                                          \
    [prim_kind] = {                                                            \
        .sym = {.node = {.kind = PRIMITIVE,                                    \
                         .pt_kind = prim_kind,                                 \
                         .flags = ASTF_RESOLVED,                               \
                         .srange = SRC_RANGE_INVALID},                         \
                .name = prim_name,                                             \
                .next = NULL},                                                 \
    }
primitive PRIMITIVES[] = {
    mk_prim(PT_VOID, "void"),     mk_prim(PT_INT, "int"),
    mk_prim(PT_UINT, "uint"),     mk_prim(PT_FLOAT, "float"),
    mk_prim(PT_STRING, "string"), mk_prim(PT_BINARY_STRING, "bstring"),
};
ureg PRIMITIVE_COUNT = sizeof(PRIMITIVES) / sizeof(primitive);

src_file* open_scope_get_file(open_scope* s)
{
    return src_range_get_file(s->scp.sym.node.srange);
}
src_file* ast_node_get_file(ast_node* n, symbol_table* st)
{
    src_file* f = src_range_get_file(n->srange);
    if (f) return f;
    f = symbol_table_get_file(st);
    assert(f);
    return f;
}
char* ast_elem_get_label(ast_elem* n, bool* lbl)
{
    char* name;
    switch (n->kind) {
        case EXPR_BLOCK: {
            name = ((expr_block*)n)->name;
            if (lbl) *lbl = true;
        } break;
        case EXPR_MATCH: {
            name = ((expr_match*)n)->name;
            if (lbl) *lbl = true;
        } break;
        default: {
            name = NULL;
            if (lbl) *lbl = false;
        } break;
    }
    return name;
}
bool ast_elem_is_open_scope(ast_elem* s)
{
    return s->kind <= OSC_LAST_OSC_ID;
}
bool ast_elem_is_scope(ast_elem* s)
{
    return s->kind <= SC_LAST_SC_ID;
}
bool ast_elem_is_symbol(ast_elem* s)
{
    return s->kind <= SYM_LAST_SYM_ID;
}
bool ast_elem_is_stmt(ast_elem* s)
{
    return (s->kind < STMT_LAST_STMT_ID);
}
bool ast_elem_is_expr(ast_elem* s)
{
    return (s->kind > STMT_LAST_STMT_ID);
}
bool ast_body_is_braced(ast_body* b)
{
    if (b->elements[0] && !b->elements[1]) {
        return (b->srange != b->elements[0]->srange);
    }
    return true;
}
bool is_unary_op_postfix(operator_kind t)
{
    switch (t) {
        case OP_POST_INCREMENT:
        case OP_POST_DECREMENT: return true;
        default: return false;
    }
}
void ast_node_get_bounds(ast_node* n, ureg* start, ureg* end)
{
    switch (n->kind) {
        case EXPR_LOOP: {
            if (start) *start = src_range_get_start(n->srange);
            if (end) ast_node_get_bounds(n, NULL, end);
            return;
        }
        case EXPR_OP_BINARY: {
            if (start)
                ast_node_get_bounds(((expr_op_binary*)n)->lhs, start, NULL);
            if (end) ast_node_get_bounds(((expr_op_binary*)n)->rhs, NULL, end);
            return;
        }
        case EXPR_OP_UNARY: {
            expr_op_unary* u = (expr_op_unary*)n;
            if (!is_unary_op_postfix(u->node.op_kind)) {
                if (start) {
                    src_range_large r;
                    src_range_unpack(u->node.srange, &r);
                    *start = r.start;
                }
                if (end) ast_node_get_bounds(u->child, NULL, end);
                return;
            }
        } // fallthrough for postfix unary op
        default: {
            src_range_large r;
            src_range_unpack(n->srange, &r);
            if (start) *start = r.start;
            if (end) *end = r.end;
            return;
        }
    }
}

int ast_type_node_get_mod_count(ast_type_node atn)
{
    int b = 0;
    while (b < ATM_BYTES && atn.mods[b] != 0) b++;
    int f = 0;
    while (atn.mods[b] != 0) {
        f++;
        atn.mods[b] >>= ATM_BITS;
    }
    return b * ATM_PER_BYTE + f;
}
ast_type_mod ast_type_node_get_mod_n(ast_type_node atn, int n)
{
    assert(n < ATM_MAX_COUNT);
    int offs = (n % ATM_PER_BYTE) * ATM_BITS;
    return atn.mods[n / ATM_PER_BYTE] >> offs;
}
void ast_type_node_set_mod_n(ast_type_node atn, ast_type_mod mod, int n)
{
    assert(n < ATM_MAX_COUNT);
    int b = n / ATM_PER_BYTE;
    int offs = (n % ATM_PER_BYTE) * ATM_BITS;
    atn.mods[b] = (atn.mods[b] & (ATM_MASK << offs)) | (mod << offs);
}

char* op_to_str(operator_kind t)
{
    switch (t) {
        case OP_ASSIGN: return "=";
        case OP_ADD: return "+";
        case OP_ADD_ASSIGN: return "+=";
        case OP_SUB: return "-";
        case OP_SUB_ASSIGN: return "-=";
        case OP_MUL: return "*";
        case OP_MUL_ASSIGN: return "*=";
        case OP_DIV: return "/";
        case OP_DIV_ASSIGN: return "/=";
        case OP_MOD: return "%";
        case OP_MOD_ASSIGN: return "%=";
        case OP_LSHIFT: return "<<";
        case OP_LSHIFT_ASSIGN: return "<<=";
        case OP_RSHIFT: return ">>";
        case OP_RSHIFT_ASSIGN: return ">>=";
        case OP_LESS_THAN: return "<";
        case OP_LESS_THAN_OR_EQUAL: return "<=";
        case OP_GREATER_THAN: return ">";
        case OP_GREATER_THAN_OR_EQUAL: return ">=";
        case OP_EQUAL: return "==";
        case OP_UNEQAL: return "!=";
        case OP_AND: return "&&";
        case OP_BITWISE_AND: return "&";
        case OP_BITWISE_AND_ASSIGN: return "&=";
        case OP_OR: return "||";
        case OP_BITWISE_OR: return "|";
        case OP_BITWISE_OR_ASSIGN: return "|=";
        case OP_XOR: return "^^";
        case OP_BITWISE_XOR: return "^";
        case OP_BITWISE_XOR_ASSIGN: return "^=";
        case OP_BITWISE_NOT_ASSIGN: return "~=";

        case OP_DEREF: return "*";
        case OP_POINTER_OF: return "%";
        case OP_REF_OF: return "&";
        case OP_RREF_OF: return "$";
        case OP_CLOSURE_BY_VALUE: return "^";
        case OP_NOT: return "!";
        case OP_BITWISE_NOT: return "~";
        case OP_UNARY_PLUS: return "+";
        case OP_UNARY_MINUS: return "-";
        case OP_PRE_INCREMENT: return "++";
        case OP_PRE_DECREMENT: return "--";
        case OP_POST_INCREMENT: return "++";
        case OP_POST_DECREMENT: return "--";
        case OP_CONST: return "const ";
        case OP_MEMBER_ACCESS: return ".";
        case OP_PP: return "#";
        default: return "<Unknown Operator>";
    }
    return 0;
}
