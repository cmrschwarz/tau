#include "print_ast.h"
#include "error_log.h"
#include "file_map.h"
#include "mdg.h"
#include "parser.h"
#include "stdio.h"
#include "tauc.h"
#include "utils/math_utils.h"
#include <assert.h>

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
void print_requires(file_require* r, ureg indent)
{
    while (*(void**)r != NULL) {
        print_indent(indent);
        p("require \"");
        src_file_print_path(r->file, false);
        p("\";\n");
        r++;
    }
}
void print_body_elements(body* body, ureg indent)
{
    for (ast_node** n = body->elements; *n; n++) {
        print_indent(indent);
        print_ast_node(*n, indent);
        if (!ast_node_may_drop_semicolon(*n)) pc(';');
        pc('\n');
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
    if (!body_is_braced(body) && body->elements[0] && !body->elements[1]) {
        print_ast_node(body->elements[0], indent);
    }
    else {
        print_body_braced(body, indent);
    }
}
void print_namable_braced_body(body* body, char* name, ureg indent)
{
    if (name) {
        p(name);
        pc('@');
    }
    print_body_braced(body, indent);
}
void print_sym_params(sym_param* d, ureg indent)
{
    while (d != NULL) {
        pu(d->symbol.name);
        pc(':');
        if (d->type != NULL) {
            pc(' ');
            print_ast_node(d->type, indent);
            if (d->default_value != NULL) pc(' ');
        }
        if (d->default_value != NULL) {
            p("= ");
            print_ast_node(d->default_value, indent);
        }
        d = (sym_param*)d->symbol.next;
        if (d) p(", ");
    }
}
void print_expr_list(ast_node** el, ureg count, ureg indent)
{
    if (!el) return;
    for (ureg i = 0; i < count; i++) {
        print_ast_node(*el, indent);
        el++;
        if (i < count - 1) p(", ");
    }
}
void print_compound_decl_list(ast_node** el, ureg elem_count, ureg indent)
{
    for (ureg i = 0; i < elem_count; i++) {
        if ((**el).kind == SYM_VAR) {
            sym_var* v = (sym_var*)*el;
            if (ast_node_flags_get_const(v->symbol.node.flags)) p("const ");
            pu(v->symbol.name);
            if (v->type != NULL) {
                p(": ");
                print_ast_node(v->type, indent);
            }
        }
        else if ((**el).kind == EXPR_TUPLE) {
            expr_tuple* t = (expr_tuple*)(*el);
            pc('(');
            print_compound_decl_list(t->elements, t->elem_count, indent);
            if (t->elements && !t->elements[1]) {
                pc(',');
            }
            pc(')');
        }
        else {
            print_ast_node(*el, indent);
        }
        el++;
        if (*el) p(", ");
    }
}
void print_ast_node_nl(ast_node* n, ureg indent)
{
    print_ast_node(n, indent);
    pc('\n');
}
void print_mdg_node_until(mdg_node* m, mdg_node* stop)
{
    if (m->parent != stop) {
        print_mdg_node_until(m->parent, stop);
        pc('.');
    }
    p(m->name);
}
void print_module_import(module_import* mi, mdg_node* parent, ureg indent)
{
    if (mi->name != NULL) {
        p(mi->name);
        p(" = ");
    }
    if (mi->tgt != parent) {
        print_mdg_node_until(mi->tgt, parent);
        if (mi->selected_symbols || mi->nested_imports) pc('.');
    }
    if (mi->selected_symbols && *(void**)mi->selected_symbols == NULL) {
        pc('*');
    }
    else if (mi->selected_symbols) {
        p("(");
        symbol_import* si = mi->selected_symbols;
        while (true) {
            if (si->alias) {
                p(si->alias);
                p(" = ");
            }
            p(si->symbol_name);
            si++;
            if (!*(void**)si) break;
            p(", ");
        }
        pc(')');
    }
    else if (mi->nested_imports) {
        p("{");
        module_import* si = mi->nested_imports;
        while (true) {
            print_module_import(si, mi->tgt, indent);
            si++;
            if (!*(void**)si) break;
            p(", ");
        }
        pc('}');
    }
}
void print_expr_in_parens(ast_node* ex, ureg indent)
{
    pc('(');
    print_ast_node(ex, indent);
    pc(')');
}
char* get_expr_name(ast_node* n)
{
    switch (n->kind) {
        case EXPR_BLOCK: return ((expr_block*)n)->name;
        case EXPR_MACRO: return ((expr_macro*)n)->name;
        case EXPR_MATCH: return ((expr_match*)n)->name;
        default: assert(false);
    }
}
void print_ast_elem_name(ast_elem* n)
{
    if (ast_elem_is_symbol(n)) {
        pu(((symbol*)n)->name);
    }
    else if (n->kind == PRIMITIVE) {
        pu(PRIMITIVES[((ast_node*)n)->primitive_kind].name);
    }
    else {
        pu("<unknown node>");
    }
}
void print_ast_node(ast_node* n, ureg indent)
{
    // TODO: print access modifiers
    switch (n->kind) {
        case STMT_IMPORT: {
            stmt_import* si = (stmt_import*)n;
            p("import ");
            print_module_import(&si->module_import, TAUC.mdg.root_node, indent);
        } break;
        case STMT_COMPOUND_ASSIGN: {
            stmt_compound_assignment* ca = (stmt_compound_assignment*)n;
            pc('(');
            bool colon = ast_node_flags_get_compound_decl(ca->node.flags);
            if (colon) {
                print_compound_decl_list(ca->elements, ca->elem_count, indent);
            }
            else {
                print_expr_list(ca->elements, ca->elem_count, indent);
            }
            if (ca->elements && !ca->elements[1]) {
                pc(',');
            }
            p(") ");
            if (colon) pc(':');
            p("= ");
            print_ast_node(ca->value, indent);
        } break;
        case SC_FUNC: {
            sc_func* f = (sc_func*)n;
            p("func ");
            pu(f->scope.symbol.name);
            p("(");
            print_sym_params(f->params, indent);
            pc(')');
            if (f->return_type) {
                p(" -> ");
                print_ast_node(f->return_type, indent + 1);
                pc(' ');
            }
            print_body_braced(&f->scope.body, indent);
        } break;
        case SC_FUNC_GENERIC: {
            sc_func_generic* f = (sc_func_generic*)n;
            p("func ");
            pu(f->scope.symbol.name);
            p("[");
            print_sym_params(f->generic_params, indent);
            pc(']');
            p("(");
            print_sym_params(f->params, indent);
            pc(')');
            if (f->return_type) {
                p(" -> ");
                print_ast_node(f->return_type, indent + 1);
                pc(' ');
            }
            print_body_braced(&f->scope.body, indent);
        } break;
        case SC_STRUCT: {
            sc_struct* s = (sc_struct*)n;
            p("struct ");
            pinn(s->scope.symbol.name);
            print_body_braced(&s->scope.body, indent);
        } break;
        case SC_STRUCT_GENERIC: {
            sc_struct_generic* s = (sc_struct_generic*)n;
            p("struct ");
            pinn(s->scope.symbol.name);
            p("[");
            print_sym_params(s->generic_params, indent);
            pc(']');
            print_body_braced(&s->scope.body, indent);
        } break;
        case SC_TRAIT: {
            sc_trait* t = (sc_trait*)n;
            p("trait ");
            pinn(t->scope.symbol.name);
            print_body_braced(&t->scope.body, indent);
        } break;
        case SC_TRAIT_GENERIC: {
            sc_trait_generic* t = (sc_trait_generic*)n;
            p("trait ");
            pinn(t->scope.symbol.name);
            p("[");
            print_sym_params(t->generic_params, indent);
            pc(']');
            print_body_braced(&t->scope.body, indent);
        } break;
        case OSC_MODULE: {
            osc_module* m = (osc_module*)n;
            p("module ");
            pinn(m->oscope.scope.symbol.name);
            print_open_scope_body(&m->oscope, indent);
        } break;
        case OSC_MODULE_GENERIC: {
            osc_module_generic* m = (osc_module_generic*)n;
            p("module ");
            pinn(m->oscope.scope.symbol.name);
            p("[");
            print_sym_params(m->generic_params, indent);
            pc(']');
            print_open_scope_body(&m->oscope, indent);
        } break;
        case OSC_EXTEND: {
            osc_extend* e = (osc_extend*)n;
            p("extend ");
            pinn(e->oscope.scope.symbol.name);
            print_open_scope_body(&e->oscope, indent);
        } break;
        case OSC_EXTEND_GENERIC: {
            osc_extend_generic* e = (osc_extend_generic*)n;
            p("extend ");
            pinn(e->oscope.scope.symbol.name);
            p("[");
            print_sym_params(e->generic_params, indent);
            pc(']');
            print_open_scope_body(&e->oscope, indent);
        } break;
        case EXPR_PP: {
            pc('#');
            print_ast_node(((expr_pp*)n)->pp_expr, indent);
        } break;
        case SYM_NAMED_USING: {
            sym_named_using* nu = (sym_named_using*)n;
            if (ast_node_flags_get_const(nu->symbol.node.flags)) p("const ");
            p("using ");
            p(nu->symbol.name);
            p(" = ");
            print_ast_node(nu->target, indent);
        } break;
        case STMT_USING: {
            stmt_using* u = (stmt_using*)n;
            if (ast_node_flags_get_const(u->node.flags)) p("const ");
            p("using ");
            print_ast_node(u->target, indent);
        } break;
        case EXPR_IDENTIFIER: {
            expr_identifier* i = (expr_identifier*)n;
            if (ast_node_flags_get_resolved(n->flags)) {
                print_ast_elem_name((ast_elem*)i->value.symbol);
            }
            else {
                pu(i->value.str);
            }
        } break;
        case EXPR_LITERAL: {
            expr_literal* l = (expr_literal*)n;
            switch (n->primitive_kind) {
                case PT_BINARY_STRING:
                    pc('\'');
                    pu(l->value.str);
                    pc('\'');
                    break;
                case PT_STRING:
                    pc('"');
                    pu(l->value.str);
                    pc('"');
                    break;
                case PT_INT: pu(l->value.str); break;
                default: assert(false); break;
            }
        } break;
        case EXPR_BLOCK: {
            expr_block* b = (expr_block*)n;
            if (b->name) {
                p(b->name);
                pc('@');
            }
            print_body_braced(&b->body, indent);
        } break;
        case SYM_VAR: {
            sym_var* v = (sym_var*)n;
            if (ast_node_flags_get_const(v->symbol.node.flags)) p("const ");
            pu(v->symbol.name);
            if (v->type != NULL) {
                p(": ");
                print_ast_node(v->type, indent);
            }
        } break;
        case SYM_VAR_INITIALIZED: {
            sym_var_initialized* v = (sym_var_initialized*)n;
            if (ast_node_flags_get_const(v->var.symbol.node.flags)) p("const ");
            pu(v->var.symbol.name);
            if (v->var.type != NULL) {
                p(": ");
                print_ast_node(v->var.type, indent);
                if (v->initial_value != NULL) pc(' ');
            }
            else {
                p(" :");
            }
            if (v->initial_value != NULL) {
                p("= ");
                print_ast_node(v->initial_value, indent);
            }
        } break;
        case EXPR_OP_BINARY: {
            expr_op_binary* b = (expr_op_binary*)n;
            print_ast_node(b->lhs, indent);
            pc(' ');
            p(op_to_str(b->node.operator_kind));
            pc(' ');
            print_ast_node(b->rhs, indent);
            break;
        }
        case EXPR_OP_UNARY: {
            expr_op_unary* u = (expr_op_unary*)n;
            if (is_unary_op_postfix(u->node.operator_kind)) {
                print_ast_node(u->child, indent);
                p(op_to_str(u->node.operator_kind));
            }
            else {
                p(op_to_str(u->node.operator_kind));
                print_ast_node(u->child, indent);
            }
            break;
        }
        case EXPR_ARRAY: {
            expr_array* a = (expr_array*)n;
            pc('[');
            print_expr_list(a->elements, a->elem_count, indent);
            pc(']');
        } break;
        case EXPR_TUPLE: {
            expr_tuple* t = (expr_tuple*)n;
            pc('(');
            print_expr_list(t->elements, t->elem_count, indent);
            if (t->elem_count == 1) {
                pc(',');
            }
            pc(')');
        } break;
        case EXPR_OP_CALL: {
            expr_call* c = (expr_call*)n;
            print_ast_node(c->lhs, indent);
            pc('(');
            print_expr_list(c->args, c->arg_count, indent);
            pc(')');
        } break;
        case EXPR_OP_ACCESS: {
            expr_access* acc = (expr_access*)n;
            print_ast_node(acc->lhs, indent);
            pc('[');
            print_expr_list(acc->args, acc->arg_count, indent);
            pc(']');
        } break;
        case EXPR_OP_PARENTHESES: {
            expr_parentheses* pr = (expr_parentheses*)n;
            print_expr_in_parens(pr->child, indent);
        } break;
        case EXPR_BREAK: {
            expr_break* b = (expr_break*)n;
            p("break");
            if (b->target) {
                pc(' ');
                pc('@');
                p(get_expr_name(b->target));
            }
            if (b->value) {
                pc(' ');
                print_ast_node(b->value, indent);
            }
        } break;
        case EXPR_CONTINUE: {
            expr_continue* c = (expr_continue*)n;
            p("continue");
            if (c->target) {
                pc(' ');
                pc('@');
                p(get_expr_name(c->target));
            }
        } break;
        case EXPR_RETURN: {
            expr_return* r = (expr_return*)n;
            p("return");
            if (r->value != NULL) {
                pc(' ');
                print_ast_node(r->value, indent);
            }
        } break;
        case EXPR_LOOP: {
            expr_loop* l = (expr_loop*)n;
            p("loop ");
            print_namable_braced_body(&l->body, l->name, indent);
        } break;
        case EXPR_MATCH: {
            expr_match* m = (expr_match*)n;
            p("match ");
            print_expr_in_parens(m->match_expr, indent);
            pc(' ');
            if (m->name != NULL) {
                p(m->name);
                pc('@');
            }
            p("{\n");
            match_arm** ma = (match_arm**)m->body.elements;
            indent++;
            while (*ma) {
                print_indent(indent);
                print_ast_node((**ma).condition, indent);
                p(" => ");
                print_ast_node((**ma).value, indent);
                if (!ast_node_may_drop_semicolon((**ma).value)) pc(';');
                pc('\n');
                ma++;
            }
            indent--;
            print_indent(indent);
            pc('}');
        } break;
        case EXPR_IF: {
            expr_if* i = (expr_if*)n;
            p("if ");
            print_expr_in_parens(i->condition, indent);
            pc(' ');
            print_ast_node(i->if_body, indent);
            if (i->else_body) {
                if (ast_node_may_drop_semicolon(i->if_body)) {
                    p("\n");
                    print_indent(indent);
                }
                else {
                    pc(' ');
                }
                p("else ");
                print_ast_node(i->else_body, indent);
            }
        } break;
        default: {
            p("<unknown expression>");
        } break;
    }
}

void print_mdg_node(mdg_node* mdg, ureg indent)
{
    print_indent(indent);
    p("module ");
    p(mdg->name);
    p("{\n");

    aseglist_iterator it;
    aseglist_iterator_begin(&it, &mdg->open_scopes);
    for (open_scope* osc = aseglist_iterator_next(&it); osc != NULL;
         osc = aseglist_iterator_next(&it)) {
        print_requires(osc->requires, indent + 1);
    }
    aseglist_iterator_begin(&it, &mdg->open_scopes);
    for (open_scope* osc = aseglist_iterator_next(&it); osc != NULL;
         osc = aseglist_iterator_next(&it)) {
        print_body_elements(&osc->scope.body, indent + 1);
    }
    print_indent(indent);
    p("}");
}
