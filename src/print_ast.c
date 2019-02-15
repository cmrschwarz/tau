#include "print_ast.h"
#include "error_log.h"
#include "stdio.h"
#include "utils/math_utils.h"

void pc(char c)
{
    putchar(c);
    fflush(stdout);
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
void ps(char* c)
{
    pu(c);
    pc(' ');
    fflush(stdout);
}
void print_indent(ureg indent)
{
    for (ureg i = 0; i < indent; i++) {
        p(MASTER_ERROR_LOG.tab_spaces);
    }
}
void print_astn_body_braced(stmt* body, ureg indent_old)
{
    p("{\n");
    while (body != NULL) {
        print_astn(body, indent_old + 1);
        body = body->next;
    }
    print_indent(indent_old);
    p("}\n");
}
void print_astn_body(stmt* body, ureg indent)
{
    while (body != NULL) {
        print_astn(body, indent);
        body = body->next;
    }
}
void print_astn_params_decl(stmt_param_decl* d)
{
    while (d != NULL) {
        pu(d->nstmt.name);
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
        d = (stmt_param_decl*)d->nstmt.stmt.next;
        if (d) p(", ");
    }
}
void print_astn(stmt* astn, ureg indent)
{
    print_indent(indent);
    switch (astn->type) {
        case ASTNT_EXPRESSION: {
            stmt_expr* e = (stmt_expr*)astn;
            print_expr(e->expr);
            p(";\n");
        } break;
        case ASTNT_FUNCTION: {
            stmt_function* f = (stmt_function*)astn;
            p("func ");
            pu(f->nstmt.name);
            p("(");
            print_astn_params_decl(f->params);
            pc(')');
            print_astn_body_braced(f->body, indent);
        } break;
        case ASTNT_GENERIC_FUNCTION: {
            stmt_generic_function* f = (stmt_generic_function*)astn;
            p("func ");
            pu(f->nstmt.name);
            p("[");
            print_astn_params_decl(f->generic_params);
            pc(']');
            p("(");
            print_astn_params_decl(f->params);
            pc(')');
            print_astn_body_braced(f->body, indent);
        } break;
        case ASTNT_STRUCT: {
            stmt_struct* s = (stmt_struct*)astn;
            p("struct ");
            if (s->nstmt.name) ps(s->nstmt.name);
            print_astn_body_braced(s->body, indent);
        } break;
        case ASTNT_GENERIC_STRUCT: {
            stmt_generic_struct* s = (stmt_generic_struct*)astn;
            p("struct ");
            if (s->nstmt.name) ps(s->nstmt.name);
            p("[");
            print_astn_params_decl(s->generic_params);
            pc(']');
            print_astn_body_braced(s->body, indent);
        } break;
        case ASTNT_TRAIT: {
            stmt_trait* t = (stmt_trait*)astn;
            p("trait ");
            if (t->nstmt.name) ps(t->nstmt.name);
            print_astn_body_braced(t->body, indent);
        } break;
        case ASTNT_GENERIC_TRAIT: {
            stmt_generic_trait* t = (stmt_generic_trait*)astn;
            p("trait ");
            if (t->nstmt.name) ps(t->nstmt.name);
            p("[");
            print_astn_params_decl(t->generic_params);
            pc(']');
            print_astn_body_braced(t->body, indent);
        } break;
        case ASTNT_MODULE: {
            stmt_module* m = (stmt_module*)astn;
            p("module ");
            if (m->nstmt.name) ps(m->nstmt.name);
            print_astn_body_braced(m->body, indent);
        } break;
        case ASTNT_GENERIC_MODULE: {
            stmt_generic_module* m = (stmt_generic_module*)astn;
            p("module ");
            if (m->nstmt.name) ps(m->nstmt.name);
            p("[");
            print_astn_params_decl(m->generic_params);
            pc(']');
            print_astn_body_braced(m->body, indent);
        } break;
        case ASTNT_EXTEND: {
            stmt_extend* e = (stmt_extend*)astn;
            p("extend ");
            if (e->nstmt.name) ps(e->nstmt.name);
            print_astn_body_braced(e->body, indent);
        } break;
        case ASTNT_GENERIC_EXTEND: {
            stmt_generic_extend* e = (stmt_generic_extend*)astn;
            p("extend ");
            if (e->nstmt.name) ps(e->nstmt.name);
            p("[");
            print_astn_params_decl(e->generic_params);
            pc(']');
            print_astn_body_braced(e->body, indent);
        } break;
        case ASTNT_VAR_DECL: {
            stmt_var_decl* d = (stmt_var_decl*)astn;
            if (stmt_flags_get_const(d->nstmt.stmt.flags)) p("const ");
            pu(d->nstmt.name);
            pc(':');
            if (d->type != NULL) {
                pc(' ');
                print_expr(d->type);
                if (d->value != NULL) pc(' ');
            }
            if (d->value != NULL) {
                p("= ");
                print_expr(d->value);
            }
            p(";\n");
        } break;
        default: {
            p("<Unkown Statement>;\n");
        }
    }
}

void print_expr_list(expr_list* enl)
{
    astn** start = ((astn**)enl->end_ptr) + 1;
    astn** end = *enl->end_ptr;
    while (start != end) {
        print_expr(*start);
        start++;
        if (start != end) p(", ");
    }
}
void print_expr(astn* ex)
{
    switch (*(astnt*)ex) {
        case ENT_IDENTIFIER:
        case ENT_NUMBER: pu(((expr_str_value*)ex)->value); break;
        case ENT_BINARY_LITERAL:
            pc('\'');
            pu(((expr_str_value*)ex)->value);
            pc('\'');
            break;
        case ENT_STRING_LITERAL:
            pc('"');
            pu(((expr_str_value*)ex)->value);
            pc('"');
            break;
        case ENT_OP_BINARY: {
            expr_op_binary* b = (expr_op_binary*)ex;
            print_expr(b->lhs);
            pc(' ');
            p(op_to_str(b->ex.op_type));
            pc(' ');
            print_expr(b->rhs);
            break;
        }
        case ENT_OP_UNARY: {
            expr_op_unary* u = (expr_op_unary*)ex;
            if (is_unary_op_postfix(u->ex.op_type)) {
                print_expr(u->child);
                p(op_to_str(u->ex.op_type));
            }
            else {
                p(op_to_str(u->ex.op_type));
                print_expr(u->child);
            }
            break;
        }
        case ENT_ARRAY: {
            pc('{');
            print_expr_list(&((expr_array*)ex)->elements);
            pc('}');
        } break;
        case ENT_TUPLE: {
            pc('[');
            print_expr_list(&((expr_tuple*)ex)->elements);
            pc(']');
        } break;
        case ENT_OP_CALL: {
            expr_call* c = (expr_call*)ex;
            print_expr(c->lhs);
            pc('(');
            print_expr_list(&c->args);
            pc(')');
        } break;
        case ENT_OP_ACCESS: {
            expr_access* acc = (expr_access*)ex;
            print_expr(acc->lhs);
            pc('[');
            print_expr_list(&acc->args);
            pc(']');
        } break;
        case ENT_OP_PARENTHESES: {
            expr_parentheses* pr = (expr_parentheses*)ex;
            pc('(');
            print_expr(pr->child);
            pc(')');
        } break;
        default: {
            p("<unknown expr>");
        } break;
    }
}

char* op_to_str(op_type t)
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
        case OP_SCOPE_ACCESS: return "::";
        case OP_MEMBER_ACCESS: return ".";
        default: return NULL;
    }
    return 0;
}