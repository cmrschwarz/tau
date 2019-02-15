#include "print_ast.h"
#include "error_log.h"
#include "stdio.h"
#include "utils/math_utils.h"

void pc(char c)
{
    putchar(c);
}
void p(char* c)
{
    fputs(c, stdout);
    fflush(stdout);
}
void pu(char* c)
{
    if (c == NULL) {
        c = "unknown";
    }
    fputs(c, stdout);
    fflush(stdout);
}
void print_indent(ureg indent)
{
    for (ureg i = 0; i < indent; i++) {
        p(MASTER_ERROR_LOG.tab_spaces);
    }
}
void print_astn_body_braced(ast_node* body, ureg indent_old)
{
    p("{\n");
    while (body != NULL) {
        print_astn(body, indent_old + 1);
        body = body->next;
    }
    print_indent(indent_old);
    p("}\n");
}
void print_astn_body(ast_node* body, ureg indent)
{
    while (body != NULL) {
        print_astn(body, indent);
        body = body->next;
    }
}
void print_astn_params_decl(astn_param_decl* d)
{
    while (d != NULL) {
        pu(d->nastn.name);
        pc(':');
        if (d->type != NULL) {
            pc(' ');
            print_expr(d->type);
            if (d->default_value != NULL) pc(' ');
        }
        if (d->default_value != NULL) {
            p("= ");
            print_expr(d->default_value);
        }
        d = (astn_param_decl*)d->nastn.astn.next;
        if (d) p(", ");
    }
}
void print_astn(ast_node* astn, ureg indent)
{
    print_indent(indent);
    switch (astn->type) {
    case ASTNT_MODULE: {
        astn_module* m = (astn_module*)astn;
        p("module ");
        if (m->nastn.name != NULL) {
            p(m->nastn.name);
            pc(' ');
        }
        print_astn_body_braced(m->body, indent);
    } break;
    case ASTNT_EXPRESSION: {
        astn_expr* e = (astn_expr*)astn;
        print_expr(e->expr);
        p(";\n");
    } break;
    case ASTNT_FUNCTION: {
        astn_function* f = (astn_function*)astn;
        p("func ");
        pu(f->nastn.name);
        p("(");
        print_astn_params_decl(f->params);
        pc(')');
        print_astn_body_braced(f->body, indent);
    } break;
    case ASTNT_GENERIC_FUNCTION: {
        astn_generic_function* f = (astn_generic_function*)astn;
        p("func ");
        pu(f->nastn.name);
        p("[");
        print_astn_params_decl(f->generic_params);
        pc(']');
        p("(");
        print_astn_params_decl(f->params);
        pc(')');
        print_astn_body_braced(f->body, indent);
    } break;
    case ASTNT_VAR_DECL: {
        astn_var_decl* d = (astn_var_decl*)astn;
        if (astn_flags_get_const(d->nastn.astn.flags)) p("const ");
        pu(d->nastn.name);
        pc(':');
        if (d->type != NULL) {
            pc(' ');
            print_expr(d->type);
            if (d->value != NULL) pc(' ');
        }
        if (d->value != NULL) {
            print_expr(d->value);
        }
        p(";\n");
    } break;
    default: {
        p("<Unkown Statement>;\n");
    }
    }
}

void print_expr_list(expr_node_list* enl)
{
    expr_node** start = ((expr_node**)enl->end_ptr) + 1;
    expr_node** end = *enl->end_ptr;
    while (start != end) {
        print_expr(*start);
        start++;
        if (start != end) p(", ");
    }
}
void print_expr(expr_node* en)
{
    switch (en->type) {
    case ENT_IDENTIFIER:
    case ENT_NUMBER: pu(((en_str_value*)en)->value); break;
    case ENT_BINARY_LITERAL:
        pc('\'');
        pu(((en_str_value*)en)->value);
        pc('\'');
        break;
    case ENT_STRING_LITERAL:
        pc('"');
        pu(((en_str_value*)en)->value);
        pc('"');
        break;
    case ENT_OP_BINARY: {
        en_op_binary* b = (en_op_binary*)en;
        print_expr(b->lhs);
        pc(' ');
        p(op_to_str(en->op_type));
        pc(' ');
        print_expr(b->rhs);
        break;
    }
    case ENT_OP_UNARY: {
        en_op_unary* u = (en_op_unary*)en;
        if (is_unary_op_postfix(en->op_type)) {
            print_expr(u->child);
            p(op_to_str(en->op_type));
        }
        else {
            p(op_to_str(en->op_type));
            print_expr(u->child);
        }
        break;
    }
    case ENT_ARRAY: {
        pc('{');
        print_expr_list(&((en_array*)en)->elements);
        pc('}');
    } break;
    case ENT_TUPLE: {
        pc('[');
        print_expr_list(&((en_tuple*)en)->elements);
        pc(']');
    } break;
    case ENT_OP_CALL: {
        en_call* c = (en_call*)en;
        print_expr(c->lhs);
        pc('(');
        print_expr_list(&c->args);
        pc(')');
    } break;
    case ENT_OP_ACCESS: {
        en_access* acc = (en_access*)en;
        print_expr(acc->lhs);
        pc('[');
        print_expr_list(&acc->args);
        pc(']');
    } break;
    case ENT_OP_PARENTHESES: {
        en_parentheses* pr = (en_parentheses*)en;
        pc('(');
        print_expr(pr->child);
        pc(')');
    } break;
    default: {
        p("<unknown expr>");
    } break;
    }
}

char* op_to_str(expr_node_type t)
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
    case OP_VALUE_OF: return "^";
    case OP_NOT: return "!";
    case OP_BITWISE_NOT: return "~";
    case OP_UNARY_PLUS: return "+";
    case OP_UNARY_MINUS: return "-";
    case OP_PRE_INCREMENT: return "++";
    case OP_PRE_DECREMENT: return "--";
    case OP_POST_INCREMENT: return "++";
    case OP_POST_DECREMENT: return "--";
    case OP_CONST: return "const ";
    case OP_SCOPE_ACCESS: return "::";
    case OP_MEMBER_ACCESS: return ".";
    default: return NULL;
    }
    return 0;
}

bool is_unary_op_postfix(expr_node_type t)
{
    switch (t) {
    case OP_POST_INCREMENT:
    case OP_POST_DECREMENT: return true;
    default: return false;
    }
}