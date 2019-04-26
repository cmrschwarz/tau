#include "print_ast.h"
#include "error_log.h"
#include "file_map.h"
#include "parser.h"
#include "stdio.h"
#include "utils/math_utils.h"

void pc(char c)
{
    putchar(c);
    fflush(stdout);
}
void p(const char* c)
{
    fputs(c, stdout);
    fflush(stdout);
}
void pu(const char* c)
{
    if (c == NULL) {
        c = "unknown";
    }
    fputs(c, stdout);
    fflush(stdout);
}
void ps(const char* c)
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
void print_requires(require* r, ureg indent)
{
    while (*(void**)r != NULL) {
        print_indent(indent);
        ps("require");
        src_file_print_path(r->file, false);
        p(";\n");
        r++;
    }
}
void print_body_elements(body* body, ureg indent)
{
    stmt* head = body->children;
    while (head) {
        print_indent(indent);
        print_astn(head, indent);
        if (!stmt_allowed_to_drop_semicolon(head)) pc(';');
        pc('\n');
        head = head->next;
    }
}
void print_open_scope_body(open_scope* osc, ureg indent)
{
    p("{\n");
    indent++;
    print_requires(osc->requires, indent);
    print_body_elements(&osc->scope.body, indent);
    indent--;
    print_indent(indent);
    p("}");
}
void print_body_braced(body* body, ureg indent)
{
    p("{\n");
    print_body_elements(body, indent + 1);
    print_indent(indent);
    p("}");
}
void print_body(body* body, ureg indent)
{
    if (!body_is_braced(body) && body->children && !body->children->next) {
        print_astn(body->children, indent);
    }
    else {
        print_body_braced(body, indent);
    }
}
void print_sym_params(sym_param* d, ureg indent)
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
        d = (sym_param*)d->symbol.stmt.next;
        if (d) p(", ");
    }
}
void print_expr_list(expr** el, ureg indent)
{
    if (!el) return;
    while (*el) {
        print_expr(*el, indent);
        el++;
        if (*el) p(", ");
    }
}
void print_compound_decl_list(expr** el, ureg indent)
{
    while (*el) {
        if ((**el).type == SYM_VAR_UNINITIALIZED) {
            sym_var* d = (sym_var*)*el;
            if (stmt_flags_get_const(d->symbol.stmt.flags)) p("const ");
            pu(d->symbol.name);
            if (d->type != NULL) {
                p(": ");
                print_expr(d->type, indent);
            }
        }
        else if ((**el).type == EXPR_TUPLE) {
            expr_tuple* t = (expr_tuple*)(*el);
            pc('(');
            print_compound_decl_list(t->elements, indent);
            if (t->elements && !t->elements[1]) {
                pc(',');
            }
            pc(')');
        }
        else {
            print_expr(*el, indent);
        }
        el++;
        if (*el) p(", ");
    }
}
void print_astn_nl(stmt* astn, ureg indent)
{
    print_astn(astn, indent);
    pc('\n');
}
void print_astn(stmt* astn, ureg indent)
{
    switch (astn->type) {
        case STMT_EXPRESSION: {
            stmt_expr* e = (stmt_expr*)astn;
            print_expr(e->expr, indent);
        } break;
        case STMT_COMPOUND_ASSIGN: {
            stmt_compound_assignment* ca = (stmt_compound_assignment*)astn;
            pc('(');
            bool colon = stmt_flags_get_compound_decl(ca->stmt.flags);
            if (colon) {
                print_compound_decl_list(ca->elements, indent);
            }
            else {
                print_expr_list(ca->elements, indent);
            }
            if (ca->elements && !ca->elements[1]) {
                pc(',');
            }
            p(") ");
            if (colon) pc(':');
            p("= ");
            print_expr(ca->value, indent);
        } break;
        case SC_FUNC: {
            sc_func* f = (sc_func*)astn;
            p("func ");
            pu(f->scope.symbol.name);
            p("(");
            print_sym_params(f->params, indent);
            pc(')');
            print_body_braced(&f->scope.body, indent);
        } break;
        case SC_FUNC_GENERIC: {
            sc_func_generic* f = (sc_func_generic*)astn;
            p("func ");
            pu(f->scope.symbol.name);
            p("[");
            print_sym_params(f->generic_params, indent);
            pc(']');
            p("(");
            print_sym_params(f->params, indent);
            pc(')');
            print_body_braced(&f->scope.body, indent);
        } break;
        case SC_STRUCT: {
            sc_struct* s = (sc_struct*)astn;
            p("struct ");
            pinn(s->scope.symbol.name);
            print_body_braced(&s->scope.body, indent);
        } break;
        case SC_STRUCT_GENERIC: {
            sc_struct_generic* s = (sc_struct_generic*)astn;
            p("struct ");
            pinn(s->scope.symbol.name);
            p("[");
            print_sym_params(s->generic_params, indent);
            pc(']');
            print_body_braced(&s->scope.body, indent);
        } break;
        case SC_TRAIT: {
            sc_trait* t = (sc_trait*)astn;
            p("trait ");
            pinn(t->scope.symbol.name);
            print_body_braced(&t->scope.body, indent);
        } break;
        case SC_TRAIT_GENERIC: {
            sc_trait_generic* t = (sc_trait_generic*)astn;
            p("trait ");
            pinn(t->scope.symbol.name);
            p("[");
            print_sym_params(t->generic_params, indent);
            pc(']');
            print_body_braced(&t->scope.body, indent);
        } break;
        case OSC_MODULE: {
            osc_module* m = (osc_module*)astn;
            p("module ");
            pinn(m->oscope.scope.symbol.name);
            print_open_scope_body(&m->oscope, indent);
        } break;
        case OSC_MODULE_GENERIC: {
            osc_module_generic* m = (osc_module_generic*)astn;
            p("module ");
            pinn(m->oscope.scope.symbol.name);
            p("[");
            print_sym_params(m->generic_params, indent);
            pc(']');
            print_open_scope_body(&m->oscope, indent);
        } break;
        case OSC_EXTEND: {
            osc_extend* e = (osc_extend*)astn;
            p("extend ");
            pinn(e->oscope.scope.symbol.name);
            print_open_scope_body(&e->oscope, indent);
        } break;
        case OSC_EXTEND_GENERIC: {
            osc_extend_generic* e = (osc_extend_generic*)astn;
            p("extend ");
            pinn(e->oscope.scope.symbol.name);
            p("[");
            print_sym_params(e->generic_params, indent);
            pc(']');
            print_open_scope_body(&e->oscope, indent);
        } break;
        case SYM_VAR: {
            sym_var* d = (sym_var*)astn;
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
        } break;
        case SYM_NAMED_USING: {
            sym_named_using* nu = (sym_named_using*)astn;
            if (stmt_flags_get_const(nu->symbol.stmt.flags)) p("const ");
            p("using ");
            p(nu->symbol.name);
            p(" = ");
            print_expr(nu->target, indent);
        } break;
        case STMT_USING: {
            stmt_using* u = (stmt_using*)astn;
            if (stmt_flags_get_const(u->stmt.flags)) p("const ");
            p("using ");
            print_expr(u->target, indent);
        } break;
        case SYM_LABEL: {
            sym_label* l = (sym_label*)astn;
            p("label ");
            p(l->symbol.name);
        } break;
        case STMT_GIVE: {
            stmt_give* g = (stmt_give*)astn;
            if (g->target.name != NULL) {
                ps("break");
                ps(g->target.name);
                ps("give");
                print_expr(g->value, indent);
            }
            else {
                p("give ");
                print_expr(g->value, indent);
            }
        } break;
        case STMT_BREAK: {
            stmt_break* b = (stmt_break*)astn;
            p("break");
            if (b->target.name) {
                pc(' ');
                p(b->target.name);
            }
        } break;
        case STMT_RETURN: {
            stmt_return* r = (stmt_return*)astn;
            p("return");
            if (r->value != NULL) {
                pc(' ');
                print_expr(r->value, indent);
            }
        } break;
        case STMT_GOTO: {
            stmt_goto* g = (stmt_goto*)astn;
            p("goto ");
            p(g->target.name);
        } break;
        default: {
            p("<Unkown Statement>;");
        }
    }
}
void print_expr(expr* ex, ureg indent)
{
    switch (ex->type) {
        case EXPR_IDENTIFIER:
        case EXPR_NUMBER: pu(((expr_str_value*)ex)->value); break;
        case EXPR_BLOCK: {
            expr_block* b = (expr_block*)ex;
            print_body_braced(&b->body, indent);
        } break;
        case SYM_VAR_UNINITIALIZED: {
            sym_var* d = (sym_var*)ex;
            if (stmt_flags_get_const(d->symbol.stmt.flags)) p("const ");
            pu(d->symbol.name);
            pc(':');
            if (d->type != NULL) {
                pc(' ');
                print_expr(d->type, indent);
            }
        } break;
        case EXPR_BINARY_LITERAL:
            pc('\'');
            pu(((expr_str_value*)ex)->value);
            pc('\'');
            break;
        case EXPR_STRING_LITERAL:
            pc('"');
            pu(((expr_str_value*)ex)->value);
            pc('"');
            break;
        case EXPR_OP_BINARY: {
            expr_op_binary* b = (expr_op_binary*)ex;
            print_expr(b->lhs, indent);
            pc(' ');
            p(op_to_str(b->expr.op_type));
            pc(' ');
            print_expr(b->rhs, indent);
            break;
        }
        case EXPR_OP_UNARY: {
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
        case EXPR_ARRAY: {
            pc('[');
            print_expr_list(((expr_array*)ex)->elements, indent);
            pc(']');
        } break;
        case EXPR_TUPLE: {
            expr** elements = ((expr_tuple*)ex)->elements;
            pc('(');
            print_expr_list(elements, indent);
            if (elements && !elements[1]) {
                pc(',');
            }
            pc(')');
        } break;
        case EXPR_OP_CALL: {
            expr_call* c = (expr_call*)ex;
            print_expr(c->lhs, indent);
            pc('(');
            print_expr_list(c->args, indent);
            pc(')');
        } break;
        case EXPR_OP_ACCESS: {
            expr_access* acc = (expr_access*)ex;
            print_expr(acc->lhs, indent);
            pc('[');
            print_expr_list(acc->args, indent);
            pc(']');
        } break;
        case EXPR_OP_PARENTHESES: {
            expr_parentheses* pr = (expr_parentheses*)ex;
            pc('(');
            print_expr(pr->child, indent);
            pc(')');
        } break;
        case EXPR_LOOP: {
            expr_loop* l = (expr_loop*)ex;
            if (l->expr_named.name != NULL) {
                p("label ");
                ps(l->expr_named.name);
            }
            p("loop ");
            print_body(&l->body, indent);
        } break;
        case EXPR_WHILE: {
            expr_while* w = (expr_while*)ex;
            if (w->expr_named.name != NULL) {
                p("label ");
                ps(w->expr_named.name);
            }
            p("while ");
            print_expr(w->condition, indent);
            pc(' ');
            print_body(&w->while_body, indent);
            if (w->finally_body.children) {
                pc('\n');
                print_indent(indent);
                p("finally ");
                print_body(&w->finally_body, indent);
            }
        } break;
        case EXPR_DO: {
            expr_do* ed = (expr_do*)ex;
            p("do ");
            print_expr(ed->expr_body, indent);
            if (ed->tail_stmt) {
                print_astn(ed->tail_stmt, indent);
            }
        } break;
        case EXPR_DO_WHILE: {
            expr_do_while* dw = (expr_do_while*)ex;
            if (dw->expr_named.name != NULL) {
                p("label ");
                ps(dw->expr_named.name);
            }
            p("do ");
            print_body(&dw->do_body, indent);
            p(" while ");
            print_expr(dw->condition, indent);
            if (dw->finally_body.children) {
                pc('\n');
                print_indent(indent);
                p("finally ");
                print_body(&dw->finally_body, indent);
            }
        } break;
        case EXPR_MATCH: {
            expr_match* m = (expr_match*)ex;
            if (m->expr_named.name != NULL) {
                p("label ");
                ps(m->expr_named.name);
            }
            p("match ");
            print_expr(m->match_expr, indent);
            p(" {\n");
            match_arm** ma = m->match_arms;
            indent++;
            while (*ma) {
                print_indent(indent);
                print_expr((**ma).condition, indent);
                p(" => ");
                print_body(&(**ma).body, indent);
                pc('\n');
                ma++;
            }
            indent--;
            print_indent(indent);
            pc('}');
        } break;
        case EXPR_IF: {
            expr_if* i = (expr_if*)ex;
            p("if ");
            print_expr(i->condition, indent);
            pc(' ');
            print_body(&i->if_body, indent);
            if (i->else_body.children) {
                if (body_is_braced(&i->if_body)) {
                    pc('\n');
                    print_indent(indent);
                }
                else {
                    pc(' ');
                }
                p("else ");
                print_body(&i->else_body, indent);
            }
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
