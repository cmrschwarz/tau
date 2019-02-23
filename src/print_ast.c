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
void pinn(char* c)
{
    if (c) p(c);
}
void print_indent(ureg indent)
{
    for (ureg i = 0; i < indent; i++) {
        p(MASTER_ERROR_LOG.tab_spaces);
    }
}

void print_body_braced(body* body, ureg indent_old)
{
    stmt* head = body->children;
    p("{\n");
    while (head) {
        print_astn(head, indent_old + 1);
        head = head->next;
    }
    print_indent(indent_old);
    p("}");
}
void print_body_braced_nl(body* body, ureg indent_old)
{
    print_body_braced(body, indent_old);
    pc('\n');
}
void print_body(body* body, ureg indent)
{
    stmt* head = body->children;
    while (head) {
        print_astn(head, indent);
        head = head->next;
    }
}
void print_astn_params_decl(stmt_param_decl* d, ureg indent)
{
    while (d != NULL) {
        pu(d->symbol.name);
        pc(':');
        if (d->type != NULL) {
            pc(' ');
            print_expr(d->type, indent);
            if (d->default_value != NULL) pc(' ');
        }
        if (d->default_value != NULL) {
            p("= ");
            print_expr(d->default_value, indent);
        }
        d = (stmt_param_decl*)d->symbol.stmt.next;
        if (d) p(", ");
    }
}
void print_astn(stmt* astn, ureg indent)
{
    print_indent(indent);
    switch (astn->type) {
        case ASTNT_FILE: {
            print_body(&((stmt_module*)astn)->scope_full.scope.body, indent);
        } break;
        case ASTNT_EXPRESSION: {
            stmt_expr* e = (stmt_expr*)astn;
            print_expr(e->expr, indent);
            p(";\n");
        } break;
        case ASTNT_FUNCTION: {
            stmt_func* f = (stmt_func*)astn;
            p("func ");
            pu(f->scope_full.scope.symbol.name);
            p("(");
            print_astn_params_decl(f->params, indent);
            pc(')');
            print_body_braced_nl(&f->scope_full.scope.body, indent);
        } break;
        case ASTNT_FUNC_GENERIC: {
            stmt_func_generic* f = (stmt_func_generic*)astn;
            p("func ");
            pu(f->scope_full.scope.symbol.name);
            p("[");
            print_astn_params_decl(f->generic_params, indent);
            pc(']');
            p("(");
            print_astn_params_decl(f->params, indent);
            pc(')');
            print_body_braced_nl(&f->scope_full.scope.body, indent);
        } break;
        case ASTNT_STRUCT: {
            stmt_struct* s = (stmt_struct*)astn;
            p("struct ");
            pinn(s->scope.symbol.name);
            print_body_braced_nl(&s->scope.body, indent);
        } break;
        case ASTNT_STRUCT_GENERIC: {
            stmt_struct_generic* s = (stmt_struct_generic*)astn;
            p("struct ");
            pinn(s->scope.symbol.name);
            p("[");
            print_astn_params_decl(s->generic_params, indent);
            pc(']');
            print_body_braced_nl(&s->scope.body, indent);
        } break;
        case ASTNT_TRAIT: {
            stmt_trait* t = (stmt_trait*)astn;
            p("trait ");
            pinn(t->scope.symbol.name);
            print_body_braced_nl(&t->scope.body, indent);
        } break;
        case ASTNT_TRAIT_GENERIC: {
            stmt_trait_generic* t = (stmt_trait_generic*)astn;
            p("trait ");
            pinn(t->scope.symbol.name);
            p("[");
            print_astn_params_decl(t->generic_params, indent);
            pc(']');
            print_body_braced_nl(&t->scope.body, indent);
        } break;
        case ASTNT_MODULE: {
            stmt_module* m = (stmt_module*)astn;
            p("module ");
            pinn(m->scope_full.scope.symbol.name);
            print_body_braced_nl(&m->scope_full.scope.body, indent);
        } break;
        case ASTNT_MODULE_GENERIC: {
            stmt_module_generic* m = (stmt_module_generic*)astn;
            p("module ");
            pinn(m->scope_full.scope.symbol.name);
            p("[");
            print_astn_params_decl(m->generic_params, indent);
            pc(']');
            print_body_braced_nl(&m->scope_full.scope.body, indent);
        } break;
        case ASTNT_EXTEND: {
            stmt_extend* e = (stmt_extend*)astn;
            p("extend ");
            pinn(e->scope_full.scope.symbol.name);
            print_body_braced_nl(&e->scope_full.scope.body, indent);
        } break;
        case ASTNT_EXTEND_GENERIC: {
            stmt_extend_generic* e = (stmt_extend_generic*)astn;
            p("extend ");
            pinn(e->scope_full.scope.symbol.name);
            p("[");
            print_astn_params_decl(e->generic_params, indent);
            pc(']');
            print_body_braced_nl(&e->scope_full.scope.body, indent);
        } break;
        case ASTNT_VAR_DECL: {
            stmt_var_decl* d = (stmt_var_decl*)astn;
            if (stmt_flags_get_const(d->symbol.stmt.flags)) p("const ");
            pu(d->symbol.name);
            if (d->type != NULL) {
                p(": ");
                print_expr(d->type, indent);
                if (d->value != NULL) pc(' ');
            }
            else {
                p(" :");
            }
            if (d->value != NULL) {
                p("= ");
                print_expr(d->value, indent);
            }
            p(";\n");
        } break;
        case ASTNT_LABEL: {
            stmt_label* l = (stmt_label*)astn;
            p("label ");
            p(l->symbol.name);
            p(";\n");
        } break;
        default: {
            p("<Unkown Statement>;\n");
        }
    }
}

void print_expr_list(expr** el, ureg indent)
{
    while (*el) {
        print_expr(*el, indent);
        el++;
        if (*el) p(", ");
    }
}
void print_expr(expr* ex, ureg indent)
{
    switch (ex->type) {
        case ENT_IDENTIFIER:
        case ENT_NUMBER: pu(((expr_str_value*)ex)->value); break;
        case ENT_BLOCK: {
            expr_block* b = (expr_block*)ex;
            print_body_braced(&b->body, indent);
        } break;
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
            print_expr(b->lhs, indent);
            pc(' ');
            p(op_to_str(b->expr.op_type));
            pc(' ');
            print_expr(b->rhs, indent);
            break;
        }
        case ASTNT_GIVE: {
            stmt_give* g = (stmt_give*)ex;
            p("give ");
            if (g->target.name) ps(g->target.name);
            if (g->value != NULL) print_expr(g->value, indent);
        } break;
        case ASTNT_RETURN: {
            stmt_return* r = (stmt_return*)ex;
            p("return");
            if (r->value != NULL) {
                pc(' ');
                print_expr(r->value, indent);
            }
        } break;
        case ASTNT_GOTO: {
            stmt_goto* g = (stmt_goto*)ex;
            p("goto ");
            p(g->target.name);
        } break;
        case ENT_OP_UNARY: {
            expr_op_unary* u = (expr_op_unary*)ex;
            if (is_unary_op_postfix(u->expr.op_type)) {
                print_expr(u->child, indent);
                p(op_to_str(u->expr.op_type));
            }
            else {
                p(op_to_str(u->expr.op_type));
                print_expr(u->child, indent);
            }
            break;
        }
        case ENT_ARRAY: {
            pc('{');
            print_expr_list(((expr_array*)ex)->elements, indent);
            pc('}');
        } break;
        case ENT_TUPLE: {
            pc('[');
            print_expr_list(((expr_tuple*)ex)->elements, indent);
            pc(']');
        } break;
        case ENT_OP_CALL: {
            expr_call* c = (expr_call*)ex;
            print_expr(c->lhs, indent);
            pc('(');
            print_expr_list(c->args, indent);
            pc(')');
        } break;
        case ENT_OP_ACCESS: {
            expr_access* acc = (expr_access*)ex;
            print_expr(acc->lhs, indent);
            pc('[');
            print_expr_list(acc->args, indent);
            pc(']');
        } break;
        case ENT_OP_PARENTHESES: {
            expr_parentheses* pr = (expr_parentheses*)ex;
            pc('(');
            print_expr(pr->child, indent);
            pc(')');
        } break;
        case ENT_LOOP: {
            expr_loop* l = (expr_loop*)ex;
            if (l->expr_named.name != NULL) {
                p("label ");
                ps(l->expr_named.name);
            }
            p("loop ");
            print_expr(l->body, indent);
        } break;
        case ENT_WHILE: {
            expr_while* w = (expr_while*)ex;
            if (w->expr_named.name != NULL) {
                p("label ");
                ps(w->expr_named.name);
            }
            p("while ");
            print_expr(w->condition, indent);
            pc(' ');
            print_expr(w->while_body, indent);
            pc('\n');
            print_indent(indent);
            p("finally ");
            print_expr(w->finally_body, indent);
        } break;
        case ENT_IF: {
            expr_if* i = (expr_if*)ex;
            p("if ");
            print_expr(i->condition, indent);
            pc(' ');
            print_expr(i->if_body, indent);
            pc('\n');
            print_indent(indent);
            p("else ");
            print_expr(i->else_body, indent);
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