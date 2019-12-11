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
        p("    ");
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
void print_body_elements(ast_body* body, mdg_node* cmdg, ureg indent)
{
    for (ast_node** n = body->elements; *n; n++) {
        print_indent(indent);
        print_ast_node(*n, cmdg, indent);
        if (!ast_node_may_drop_semicolon(*n)) pc(';');
        pc('\n');
    }
}
void print_open_scope_body(open_scope* osc, mdg_node* cmdg, ureg indent)
{
    p("{\n");
    indent++;
    print_requires(osc->requires, indent);
    print_body_elements(&osc->sc.body, cmdg, indent);
    indent--;
    print_indent(indent);
    p("}");
}
void print_body_braced(ast_body* body, mdg_node* cmdg, ureg indent)
{
    p("{\n");
    print_body_elements(body, cmdg, indent + 1);
    print_indent(indent);
    p("}");
}
void print_body(ast_body* body, mdg_node* cmdg, ureg indent)
{
    if (!ast_body_is_braced(body) && body->elements[0] && !body->elements[1]) {
        print_ast_node(body->elements[0], cmdg, indent);
    }
    else {
        print_body_braced(body, cmdg, indent);
    }
}
void print_namable_braced_body(
    ast_body* body, char* name, mdg_node* cmdg, ureg indent)
{
    if (name) {
        pc('@');
        p(name);
    }
    print_body_braced(body, cmdg, indent);
}
void print_sym_params(
    sym_param* params, ureg param_count, mdg_node* cmdg, ureg indent)
{
    for (ureg i = 0; i < param_count; i++) {
        pu(params[i].sym.name);
        pc(':');
        if (params[i].type != NULL) {
            pc(' ');
            print_ast_node(params[i].type, cmdg, indent);
            if (params[i].default_value != NULL) pc(' ');
        }
        if (params[i].default_value != NULL) {
            p("= ");
            print_ast_node(params[i].default_value, cmdg, indent);
        }
        if (i < param_count - 1) p(", ");
    }
}
void print_expr_list(ast_node** el, ureg count, mdg_node* cmdg, ureg indent)
{
    if (!el) return;
    for (ureg i = 0; i < count; i++) {
        print_ast_node(*el, cmdg, indent);
        el++;
        if (i < count - 1) p(", ");
    }
}
void print_compound_decl_list(
    ast_node** el, ureg elem_count, mdg_node* cmdg, ureg indent)
{
    for (ureg i = 0; i < elem_count; i++) {
        if ((**el).kind == SYM_VAR) {
            sym_var* v = (sym_var*)*el;
            if (ast_flags_get_const(v->sym.node.flags)) p("const ");
            pu(v->sym.name);
            if (v->type != NULL) {
                p(": ");
                print_ast_node(v->type, cmdg, indent);
            }
        }
        else if ((**el).kind == EXPR_TUPLE) {
            expr_tuple* t = (expr_tuple*)(*el);
            pc('(');
            print_compound_decl_list(t->elements, t->elem_count, cmdg, indent);
            if (t->elements && !t->elements[1]) {
                pc(',');
            }
            pc(')');
        }
        else {
            print_ast_node(*el, cmdg, indent);
        }
        el++;
        if (*el) p(", ");
    }
}
void print_ast_node_nl(ast_node* n, mdg_node* cmdg, ureg indent)
{
    print_ast_node(n, cmdg, indent);
    pc('\n');
}
bool print_mdg_node_until(mdg_node* m, mdg_node* stop)
{
    if (m->parent == stop) return false;
    if (print_mdg_node_until(m->parent, stop)) p("::");
    p(m->name);
    return true;
}
void print_expr_in_parens(ast_node* ex, mdg_node* cmdg, ureg indent)
{
    pc('(');
    print_ast_node(ex, cmdg, indent);
    pc(')');
}
char* get_expr_name(ast_node* n)
{
    switch (n->kind) {
        case EXPR_BLOCK: return ((expr_block*)n)->name;
        case EXPR_MACRO: return ((expr_macro_call*)n)->name;
        case EXPR_MATCH: return ((expr_match*)n)->name;
        case EXPR_LOOP: return ((expr_loop*)n)->name;
        default: assert(false); return NULL;
    }
}
void print_ast_elem_name(ast_elem* n)
{
    if (ast_elem_is_symbol(n)) {
        pu(((symbol*)n)->name);
    }
    else if (n->kind == PRIMITIVE) {
        pu(PRIMITIVES[((ast_node*)n)->pt_kind].sym.name);
    }
    else {
        pu("<unknown node>");
    }
}
void print_import_group(
    sym_import_group* g, mdg_node* block_parents_parent, ureg indent)
{
    // TODO: improve this mess, we can't even preserve import order right now
    if (print_mdg_node_until(g->parent_mdgn, block_parents_parent)) p("::");
    symbol** c;
    symbol** cend;
    if (ast_flags_get_resolved(g->sym.node.flags)) {
        c = (symbol**)(g->children.symtab + 1);
        cend = c + g->children.symtab->decl_count;
        while (c != cend && !*c) c++; // skip initial blanks
    }
    else {
        c = &g->children.symbols;
        cend = NULL;
    }
    bool syms = ((**c).node.kind == SYM_IMPORT_SYMBOL);
    p(syms ? "(" : "{\n");
    while (true) {
        if (c == cend) break;
        if (*c == NULL) {
            if (!cend) break;
            c++;
            continue;
        }
        if (!syms) print_indent(indent + 1);
        if ((**c).node.kind == SYM_IMPORT_GROUP) {
            print_import_group(
                (sym_import_group*)*c, g->parent_mdgn->parent, indent + 1);
        }
        else if ((**c).node.kind == SYM_IMPORT_MODULE) {
            print_mdg_node_until(
                ((sym_import_module*)*c)->target, g->parent_mdgn->parent);
        }
        else if ((**c).node.kind == SYM_IMPORT_SYMBOL) {
            // TODO: remove if aboce by fixing unnamed group containing parent
            // st
            assert((**c).node.kind == SYM_IMPORT_SYMBOL);
            sym_import_symbol* sym = (sym_import_symbol*)*c;
            // equals is fine here since we alloc only once
            if (sym->sym.name != sym->target.name) {
                p(sym->sym.name);
                p(" = ");
            }
            p(sym->target.name);
        }
        else {
            c++;
            continue;
        }
        if (cend) {
            c++;
            if (c != cend) p(", ");
        }
        else {
            c = &(**c).next;
            if (*c) p(", ");
        }
        if (!syms) pc('\n');
    }
    if (!syms) {
        print_indent(indent);
        pc('}');
    }
    else {
        pc(')');
    }
}
void print_ast_node_modifiers(ast_flags flags)
{
    access_modifier am = ast_flags_get_access_mod(flags);
    switch (am) {
        case AM_PRIVATE: p(token_strings[TK_KW_PRIVATE]); break;
        case AM_PROTECTED: p(token_strings[TK_KW_PROTECTED]); break;
        case AM_PUBLIC: p(token_strings[TK_KW_PUBLIC]); break;
        default: break;
    };
    if (am) pc(' ');
    if (ast_flags_get_virtual(flags)) {
        p(token_strings[TK_KW_VIRTUAL]);
        pc(' ');
    }
    if (ast_flags_get_const(flags)) {
        p(token_strings[TK_KW_CONST]);
        pc(' ');
    }
    if (ast_flags_get_sealed(flags)) {
        p(token_strings[TK_KW_SEALED]);
        pc(' ');
    }
}
void print_ast_node(ast_node* n, mdg_node* cmdg, ureg indent)
{
    // TODO: print access modifiers
    switch (n->kind) {
        case SYM_IMPORT_MODULE: {
            p("import ");
            sym_import_module* im = (sym_import_module*)n;
            if (ast_flags_get_relative_import(n->flags)) {
                p("self::");
                print_mdg_node_until(im->target, cmdg->parent);
            }
            else {
                print_mdg_node_until(im->target, NULL);
            }
        } break;
        case SYM_IMPORT_GROUP: {
            p("import ");
            if (ast_flags_get_relative_import(n->flags)) {
                p("self::");
                print_import_group((sym_import_group*)n, cmdg->parent, indent);
            }
            else {
                print_import_group((sym_import_group*)n, NULL, indent);
            }
        } break;
        case STMT_COMPOUND_ASSIGN: {
            stmt_compound_assignment* ca = (stmt_compound_assignment*)n;
            pc('(');
            bool colon = ast_flags_get_compound_decl(ca->node.flags);
            if (colon) {
                print_compound_decl_list(
                    ca->elements, ca->elem_count, cmdg, indent);
            }
            else {
                print_expr_list(ca->elements, ca->elem_count, cmdg, indent);
            }
            if (ca->elements && !ca->elements[1]) {
                pc(',');
            }
            p(") ");
            if (colon) pc(':');
            p("= ");
            print_ast_node(ca->value, cmdg, indent);
        } break;
        case SC_FUNC: {
            sc_func* f = (sc_func*)n;
            print_ast_node_modifiers(n->flags);
            p("func ");
            pu(f->sc.sym.name);
            p("(");
            print_sym_params(f->params, f->param_count, cmdg, indent);
            pc(')');
            if (f->return_type) {
                p(" -> ");
                print_ast_node(f->return_type, cmdg, indent + 1);
                pc(' ');
            }
            print_body_braced(&f->sc.body, cmdg, indent);
        } break;
        case SC_FUNC_GENERIC: {
            sc_func_generic* f = (sc_func_generic*)n;
            p("func ");
            pu(f->sc.sym.name);
            p("[");
            print_sym_params(
                f->generic_params, f->generic_param_count, cmdg, indent);
            pc(']');
            p("(");
            print_sym_params(f->params, f->param_count, cmdg, indent);
            pc(')');
            if (f->return_type) {
                p(" -> ");
                print_ast_node(f->return_type, cmdg, indent + 1);
                pc(' ');
            }
            print_body_braced(&f->sc.body, cmdg, indent);
        } break;
        case SC_STRUCT: {
            sc_struct* s = (sc_struct*)n;
            p("struct ");
            pinn(s->sc.sym.name);
            print_body_braced(&s->sc.body, cmdg, indent);
        } break;
        case SC_STRUCT_GENERIC: {
            sc_struct_generic* s = (sc_struct_generic*)n;
            p("struct ");
            pinn(s->sc.sym.name);
            p("[");
            print_sym_params(
                s->generic_params, s->generic_param_count, cmdg, indent);
            pc(']');
            print_body_braced(&s->sc.body, cmdg, indent);
        } break;
        case SC_TRAIT: {
            sc_trait* t = (sc_trait*)n;
            p("trait ");
            pinn(t->sc.sym.name);
            print_body_braced(&t->sc.body, cmdg, indent);
        } break;
        case SC_TRAIT_GENERIC: {
            sc_trait_generic* t = (sc_trait_generic*)n;
            p("trait ");
            pinn(t->sc.sym.name);
            p("[");
            print_sym_params(
                t->generic_params, t->generic_param_count, cmdg, indent);
            pc(']');
            print_body_braced(&t->sc.body, cmdg, indent);
        } break;
        case OSC_MODULE:
        case OSC_EXTEND: {
            osc_module* m = (osc_module*)n;
            p(n->kind == OSC_EXTEND ? "extend " : "module ");
            pinn(m->oscope.sc.sym.name);
            print_open_scope_body(&m->oscope, cmdg, indent);
        } break;
        case OSC_MODULE_GENERIC:
        case OSC_EXTEND_GENERIC: {
            osc_module_generic* m = (osc_module_generic*)n;
            p(n->kind == OSC_EXTEND ? "extend " : "module ");
            pinn(m->oscope.sc.sym.name);
            p("[");
            print_sym_params(
                m->generic_params, m->generic_param_count, cmdg, indent);
            pc(']');
            print_open_scope_body(&m->oscope, cmdg, indent);
        } break;
        case EXPR_PP: {
            pc('#');
            print_ast_node(((expr_pp*)n)->pp_expr, cmdg, indent);
        } break;
        case EXPR_PASTE_STR: {
            p("paste(");
            print_ast_node(((expr_paste_str*)n)->value, cmdg, indent);
            pc(')');
        } break;
        case SYM_NAMED_USING: {
            sym_named_using* nu = (sym_named_using*)n;
            print_ast_node_modifiers(nu->sym.node.flags);
            p("using ");
            p(nu->sym.name);
            p(" = ");
            print_ast_node(nu->target, cmdg, indent);
        } break;
        case STMT_USING: {
            stmt_using* u = (stmt_using*)n;
            print_ast_node_modifiers(u->node.flags);
            p("using ");
            print_ast_node(u->target, cmdg, indent);
        } break;
        case EXPR_IDENTIFIER: {
            expr_identifier* i = (expr_identifier*)n;
            if (ast_flags_get_resolved(n->flags)) {
                print_ast_elem_name((ast_elem*)i->value.sym);
            }
            else {
                pu(i->value.str);
            }
        } break;
        case EXPR_LITERAL: {
            expr_literal* l = (expr_literal*)n;
            switch (n->pt_kind) {
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
            print_namable_braced_body(&b->body, b->name, cmdg, indent);
        } break;
        case SYM_VAR: {
            sym_var* v = (sym_var*)n;
            print_ast_node_modifiers(v->sym.node.flags);
            pu(v->sym.name);
            if (v->type != NULL) {
                p(": ");
                print_ast_node(v->type, cmdg, indent);
            }
        } break;
        case SYM_VAR_INITIALIZED: {
            sym_var_initialized* v = (sym_var_initialized*)n;
            print_ast_node_modifiers(v->var.sym.node.flags);
            pu(v->var.sym.name);
            if (v->var.type != NULL) {
                p(": ");
                print_ast_node(v->var.type, cmdg, indent);
                if (v->initial_value != NULL) pc(' ');
            }
            else {
                p(" :");
            }
            if (v->initial_value != NULL) {
                p("= ");
                print_ast_node(v->initial_value, cmdg, indent);
            }
        } break;
        case EXPR_OP_BINARY: {
            expr_op_binary* b = (expr_op_binary*)n;
            print_ast_node(b->lhs, cmdg, indent);
            pc(' ');
            p(op_to_str(b->node.op_kind));
            pc(' ');
            print_ast_node(b->rhs, cmdg, indent);
            break;
        }
        case EXPR_OP_UNARY: {
            expr_op_unary* u = (expr_op_unary*)n;
            if (is_unary_op_postfix(u->node.op_kind)) {
                print_ast_node(u->child, cmdg, indent);
                p(op_to_str(u->node.op_kind));
            }
            else {
                p(op_to_str(u->node.op_kind));
                print_ast_node(u->child, cmdg, indent);
            }
            break;
        }
        case EXPR_ARRAY: {
            expr_array* a = (expr_array*)n;
            pc('{');
            print_expr_list(a->elements, a->elem_count, cmdg, indent);
            pc('}');
        } break;
        case ARRAY_DECL: {
            array_decl* a = (array_decl*)n;
            pc('[');
            print_ast_node(a->length_spec, cmdg, indent);
            pc(']');
            print_ast_node(a->base_type, cmdg, indent);
        } break;
        case EXPR_TUPLE: {
            expr_tuple* t = (expr_tuple*)n;
            pc('(');
            print_expr_list(t->elements, t->elem_count, cmdg, indent);
            if (t->elem_count == 1) {
                pc(',');
            }
            pc(')');
        } break;
        case EXPR_CALL: {
            expr_call* c = (expr_call*)n;
            print_ast_node(c->lhs, cmdg, indent);
            pc('(');
            print_expr_list(c->args, c->arg_count, cmdg, indent);
            pc(')');
        } break;
        case EXPR_ACCESS: {
            expr_access* acc = (expr_access*)n;
            print_ast_node(acc->lhs, cmdg, indent);
            pc('[');
            print_expr_list(acc->args, acc->arg_count, cmdg, indent);
            pc(']');
        } break;
        case EXPR_PARENTHESES: {
            expr_parentheses* pr = (expr_parentheses*)n;
            print_expr_in_parens(pr->child, cmdg, indent);
        } break;
        case EXPR_BREAK: {
            expr_break* b = (expr_break*)n;
            p("break");
            if (b->target) {
                char* n = get_expr_name(b->target);
                if (n) {
                    pc(' ');
                    pc('@');
                    p(n);
                }
            }
            if (b->value) {
                pc(' ');
                print_ast_node(b->value, cmdg, indent);
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
            expr_break* r = (expr_break*)n;
            p("return");
            if (r->value != NULL) {
                pc(' ');
                print_ast_node(r->value, cmdg, indent);
            }
        } break;
        case EXPR_LOOP: {
            expr_loop* l = (expr_loop*)n;
            p("loop ");
            print_namable_braced_body(&l->body, l->name, cmdg, indent);
        } break;
        case EXPR_MATCH: {
            expr_match* m = (expr_match*)n;
            p("match ");
            print_expr_in_parens(m->match_expr, cmdg, indent);
            pc(' ');
            if (m->name != NULL) {
                pc('@');
                p(m->name);
            }
            p("{\n");
            match_arm** ma = (match_arm**)m->body.elements;
            indent++;
            while (*ma) {
                print_indent(indent);
                print_ast_node((**ma).condition, cmdg, indent);
                p(" => ");
                print_ast_node((**ma).value, cmdg, indent);
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
            print_expr_in_parens(i->condition, cmdg, indent);
            pc(' ');
            print_ast_node(i->if_body, cmdg, indent);
            if (i->else_body) {
                if (ast_node_may_drop_semicolon(i->if_body)) {
                    p("\n");
                    print_indent(indent);
                }
                else {
                    pc(' ');
                }
                p("else ");
                print_ast_node(i->else_body, cmdg, indent);
            }
        } break;
        case EXPR_MEMBER_ACCESS:
        case EXPR_SCOPE_ACCESS: {
            expr_scope_access* esa = (expr_scope_access*)n;
            print_ast_node(esa->lhs, cmdg, indent);
            p(n->kind == EXPR_MEMBER_ACCESS ? "." : "::");
            if (!ast_flags_get_resolved(n->flags)) {
                pu(esa->target.name);
            }
            else {
                pu(esa->target.sym->name);
            }
        } break;
        case EXPR_PASTE_EVALUATION: {
            print_ast_node(((expr_paste_evaluation*)n)->expr, cmdg, indent);
            break;
        }
        case STMT_PASTE_EVALUATION: {
            print_body(&((stmt_paste_evaluation*)n)->body, cmdg, indent);
            break;
        }
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
        print_body_elements(&osc->sc.body, mdg, indent + 1);
    }
    print_indent(indent);
    p("}");
}
