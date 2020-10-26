#include "print_ast.h"
#include "error_log.h"
#include "file_map.h"
#include "mdg.h"
#include "parser.h"
#include "stdio.h"
#include "tauc.h"
#include "utils/math_utils.h"
#include "utils/debug_utils.h"
#include "utils/math_utils.h"
#include "utils/panic.h"
#include "utils/int_string_conversions.h"
#include <assert.h>

int p_raw(const char* str, sbuffer* buff)
{
    if (buff) {
        ureg len = strlen(str);
        char* s = sbuffer_append(buff, len);
        if (!s) return ERR;
        memcpy(s, str, len);
    }
    else {
        tput(str);
    }
    return OK;
}
int pc_raw(char c, sbuffer* buff)
{
    if (buff) {
        char* cp = sbuffer_append(buff, 1);
        if (!cp) return ERR;
        *cp = c;
    }
    else {
        tputchar(c);
    }
    return OK;
}
int pu_raw(const char* str, sbuffer* buff)
{
    if (str == NULL) str = "unknown";
    return p_raw(str, buff);
}
int pinn_raw(const char* str, sbuffer* buff)
{
    if (str == NULL) return OK;
    return p_raw(str, buff);
}
int p_ureg(ureg i, sbuffer* buff)
{
    ureg len = ureg_get_decimal_digits_count(i);
    char* s;
    if (buff) {
        s = sbuffer_append(buff, len);
        if (!s) return ERR;
    }
    else {
        s = alloca(len + 1);
    }
    ureg res = ureg_to_decimal_string(i, s);
    assert(res == len);
    UNUSED(res);
    if (!buff) tput(s);
    return OK;
}
int p_sreg(sreg i, sbuffer* buff)
{
    ureg len = sreg_get_decimal_digits_count(i);
    char* s;
    if (buff) {
        s = sbuffer_append(buff, len);
        if (!s) return ERR;
    }
    else {
        s = alloca(len + 1);
    }
    ureg res = sreg_to_decimal_string(i, s);
    assert(res == len);
    UNUSED(res);
    if (!buff) tput(s);
    return OK;
}
#define p(str)                                                                 \
    do {                                                                       \
        if (p_raw(str, buff)) return ERR;                                      \
    } while (false)

#define pc(c)                                                                  \
    do {                                                                       \
        if (pc_raw(c, buff)) return ERR;                                       \
    } while (false)

#define pu(str)                                                                \
    do {                                                                       \
        if (pu_raw(str, buff)) return ERR;                                     \
    } while (false)

#define pinn(str)                                                              \
    do {                                                                       \
        if (pinn_raw(str, buff)) return ERR;                                   \
    } while (false)

#define attempt(exp)                                                           \
    do {                                                                       \
        if (exp) return ERR;                                                   \
    } while (false)

int print_indent(ureg indent, sbuffer* buff)
{
    for (ureg i = 0; i < indent; i++) {
        p("    ");
    }
    return OK;
}
int print_requires(file_require* r, ureg indent, sbuffer* buff)
{
    while (*(void**)r != NULL) {
        if (print_indent(indent, buff)) return ERR;
        p("require \"");
        file_map_head_print_path(r->fmh, false);
        p("\";\n");
        r++;
    }
    return OK;
}
int print_body_elements(
    ast_body* body, ast_body* ctx, ureg indent, sbuffer* buff)
{
    for (ast_node** n = body->elements; *n; n++) {
        attempt(print_indent(indent, buff));
        attempt(print_ast_node(*n, ctx, PM_FULL, indent, buff));
        if (!ast_node_may_drop_semicolon(*n)) pc(';');
        pc('\n');
    }
    return OK;
}
int print_module_frame_body(
    module_frame* mf, ast_body* ctx, ureg indent, sbuffer* buff)
{
    p("{\n");
    indent++;
    attempt(print_requires(mf->requires, indent, buff));
    attempt(print_body_elements(&mf->body, ctx, indent, buff));
    indent--;
    attempt(print_indent(indent, buff));
    p("}");
    return OK;
}
int print_body_braced(ast_body* body, ast_body* ctx, ureg indent, sbuffer* buff)
{
    p("{\n");
    attempt(print_body_elements(body, ctx, indent + 1, buff));
    attempt(print_indent(indent, buff));
    p("}");
    return OK;
}
int print_body(ast_body* body, ast_body* ctx, ureg indent, sbuffer* buff)
{
    if (!ast_body_is_braced(body) && body->elements[0] && !body->elements[1]) {
        attempt(print_ast_node(body->elements[0], ctx, PM_FULL, indent, buff));
    }
    else {
        attempt(print_body_braced(body, ctx, indent, buff));
    }
    return OK;
}
int print_namable_braced_body(
    ast_body* body, char* name, ast_body* ctx, ureg indent, sbuffer* buff)
{
    if (name) {
        pc('@');
        p(name);
    }
    attempt(print_body_braced(body, ctx, indent, buff));
    return OK;
}
int print_generic_args(
    sym_param_generic_inst* args, ureg arg_count, ast_body* ctx, ureg indent,
    sbuffer* buff)
{
    for (ureg i = 0; i < arg_count; i++) {
        attempt(print_ast_elem(args[i].value, ctx, PM_TYPE, indent, buff));
        if (i < arg_count - 1) p(", ");
    }
    return OK;
}
int print_sym_params(
    sym_param* params, ureg param_count, ast_body* ctx, ureg indent,
    sbuffer* buff)
{
    for (ureg i = 0; i < param_count; i++) {
        pu(params[i].sym.name);
        pc(':');
        if (params[i].type != NULL) {
            pc(' ');
            attempt(print_ast_node(params[i].type, ctx, PM_FULL, indent, buff));
            if (params[i].default_value != NULL) pc(' ');
        }
        if (params[i].default_value != NULL) {
            p("= ");
            attempt(print_ast_node(
                params[i].default_value, ctx, PM_FULL, indent, buff));
        }
        if (i < param_count - 1) p(", ");
    }
    return OK;
}
int print_expr_list(
    ast_node** el, ureg count, ast_body* ctx, ureg indent, sbuffer* buff)
{
    if (!el) return OK;
    for (ureg i = 0; i < count; i++) {
        attempt(print_ast_node(*el, ctx, PM_FULL, indent, buff));
        el++;
        if (i < count - 1) p(", ");
    }
    return OK;
}
int print_compound_decl_list(
    ast_node** el, ureg elem_count, ast_body* ctx, ureg indent, sbuffer* buff)
{
    for (ureg i = 0; i < elem_count; i++) {
        if ((**el).kind == SYM_VAR) {
            sym_var* v = (sym_var*)*el;
            if (ast_node_get_const(*el)) p("const ");
            pu(v->osym.sym.name);
            if (v->type != NULL) {
                p(": ");
                attempt(print_ast_node(v->type, ctx, PM_FULL, indent, buff));
            }
        }
        else if ((**el).kind == EXPR_TUPLE) {
            expr_tuple* t = (expr_tuple*)(*el);
            pc('(');
            attempt(print_compound_decl_list(
                t->elements, t->elem_count, ctx, indent, buff));
            if (t->elements && !t->elements[1]) {
                pc(',');
            }
            pc(')');
        }
        else {
            attempt(print_ast_node(*el, ctx, PM_FULL, indent, buff));
        }
        el++;
        if (*el) p(", ");
    }
    return OK;
}
int print_ast_node_nl(
    ast_node* n, ast_body* ctx, print_mode pm, ureg indent, sbuffer* buff)
{
    attempt(print_ast_node(n, ctx, pm, indent, buff));
    pc('\n');
    return OK;
}
// skipping zero means printing including the start
int print_mdg_node_until(
    mdg_node* start, mdg_node* m, ureg skip_levels, ureg* lvl_done,
    sbuffer* buff)
{
    if (m == start) {
        if (skip_levels == 0) p(m->name);
        if (lvl_done) *lvl_done = 1;
        return OK;
    }
    ureg lvl;
    attempt(print_mdg_node_until(start, m->parent, skip_levels, &lvl, buff));
    if (lvl > skip_levels) p("::");
    if (lvl >= skip_levels) p(m->name);
    if (lvl_done) *lvl_done = lvl + 1;
    return OK;
}
int print_expr_in_parens(
    ast_node* ex, ast_body* ctx, print_mode pm, ureg indent, sbuffer* buff)
{
    pc('(');
    attempt(print_ast_node(ex, ctx, pm, indent, buff));
    pc(')');
    return OK;
}
char* get_expr_name(ast_node* n)
{
    switch (n->kind) {
        case EXPR_BLOCK: return ((expr_block*)n)->ebb.name;
        case EXPR_MACRO: return ((expr_macro_call*)n)->name;
        case EXPR_MATCH: return ((expr_match*)n)->name;
        case EXPR_LOOP: return ((expr_loop*)n)->ebb.name;
        default: assert(false); return NULL;
    }
}
int print_ast_elem_name(ast_elem* n, sbuffer* buff)
{
    if (ast_elem_is_symbol(n)) {
        pu(((symbol*)n)->name);
    }
    else if (n->kind == SYM_PRIMITIVE) {
        pu(PRIMITIVES[((ast_node*)n)->pt_kind].sym.name);
    }
    else {
        pu("<unknown node>");
    }
    return OK;
}
int print_import_symbol(ast_node* n, sbuffer* buff)
{
    sym_import_symbol* is = (sym_import_symbol*)n;
    char* tgt_name = is->target_body ? is->target.name : is->target.name;
    // doing a pointer comparison is actually the right thing to do
    // here: in the common case we reuse the string, and if the
    // user gave the thing the same name it had a new string will
    // have been allocated which gives the correct behavior this
    // will some day break when we do string interning :D
    if (is->osym.sym.name != tgt_name) {
        p(is->osym.sym.name);
        p(" = ");
    }
    p(tgt_name);
    return OK;
}
int print_import_module(ast_node* n, mdg_node* group_parent, sbuffer* buff)
{
    if (!group_parent) p("import ");
    sym_import_module* im = (sym_import_module*)n;
    if (im->osym.sym.name != im->im_data.imported_module->name) {
        p(im->osym.sym.name);
        p(" = ");
    }
    int r;
    if (ast_node_get_relative_import(n)) {
        assert(
            !group_parent || group_parent == im->im_data.cmw.requiring_module);
        p("self::");
        r = print_mdg_node_until(
            im->im_data.cmw.requiring_module, im->im_data.imported_module, 1,
            NULL, buff);
        if (r) return r;
    }
    else {
        r = print_mdg_node_until(
            group_parent, im->im_data.imported_module, group_parent ? 1 : 2,
            NULL, buff);
        if (r) return r;
    }
    if (!group_parent) p(";");
    return OK;
}
int print_import_group(ast_node* node, bool child, ureg indent, sbuffer* buff)
{
    if (!child) p("import ");
    import_group_data* ig_data;
    import_module_data* im_data;
    const char* name;
    mdg_node* group_parent;
    import_group_get_data(node, &ig_data, &im_data, &name, &group_parent);
    if (name) {
        p(name);
        p(" = ");
    }
    if (ast_node_get_relative_import(node)) {
        assert(!child);
        p("self::");
    }
    ureg lvl;
    attempt(print_mdg_node_until(
        ig_data->relative_to, group_parent, 1, &lvl, buff));
    if (lvl >= 2) {
        p("::");
    }
    list_it it;
    list_it_begin(&it, &ig_data->children_ordered);
    ast_node* n;
    bool first = true;
    p(im_data ? "(" : "{");
    while ((n = list_it_next(&it, &ig_data->children_ordered))) {
        if (!first) p(", ");
        first = false;
        if (ast_elem_is_import_group((ast_elem*)n)) {
            attempt(print_import_group(n, true, indent + 1, buff));
        }
        else if (n->kind == SYM_IMPORT_SYMBOL) {
            attempt(print_import_symbol(n, buff));
        }
        else {
            assert(n->kind == SYM_IMPORT_MODULE);
            attempt(print_import_module(n, group_parent, buff));
        }
    }
    p(im_data ? ")" : "}");
    if (!child) p(";");
    return OK;
}
int print_ast_node_modifiers(ast_elem* e, sbuffer* buff)
{
    assert(ast_elem_is_node(e));
    ast_node* n = (ast_node*)e;
    access_modifier am = ast_node_get_access_mod(n);
    switch (am) {
        // TODO: properly check for the default in this context
        case AM_LOCAL: break;
        case AM_INTERNAL: p(token_strings[TK_KW_INTERNAL]); break;
        case AM_PRIVATE: p(token_strings[TK_KW_PRIVATE]); break;
        case AM_PROTECTED: p(token_strings[TK_KW_PROTECTED]); break;
        case AM_PUBLIC: p(token_strings[TK_KW_PUBLIC]); break;
        default: break;
    };
    if (am) pc(' ');
    if (ast_node_get_const(n)) {
        p(token_strings[TK_KW_CONST]);
        pc(' ');
    }
    return OK;
}
// TODO: we can fully get rid of ctx by using importing_module in
// import_module
int print_ast_elem(
    ast_elem* n, ast_body* ctx, print_mode pm, ureg indent, sbuffer* buff)
{
    // TODO: print access modifiers
    switch (n->kind) {
        case SYM_IMPORT_MODULE: {
            attempt(print_import_module((ast_node*)n, NULL, buff));
        } break;
        case SYM_NAMED_MOD_IMPORT_GROUP:
        case SYM_NAMED_SYM_IMPORT_GROUP:
        case ASTN_ANONYMOUS_MOD_IMPORT_GROUP:
        case ASTN_ANONYMOUS_SYM_IMPORT_GROUP: {
            attempt(print_import_group((ast_node*)n, false, indent, buff));
        } break;
        case STMT_COMPOUND_ASSIGN: {
            stmt_compound_assignment* ca = (stmt_compound_assignment*)n;
            pc('(');
            bool colon = ast_node_get_compound_decl(&ca->node);
            if (colon) {
                attempt(print_compound_decl_list(
                    ca->elements, ca->elem_count, ctx, indent, buff));
            }
            else {
                attempt(print_expr_list(
                    ca->elements, ca->elem_count, ctx, indent, buff));
            }
            if (ca->elements && !ca->elements[1]) {
                pc(',');
            }
            p(") ");
            if (colon) pc(':');
            p("= ");
            attempt(print_ast_node(ca->value, ctx, pm, indent, buff));
        } break;
        case SC_FUNC:
        case SC_FUNC_GENERIC: {
            sc_func_base* fnb = (sc_func_base*)n;
            sc_func_generic* fng =
                (n->kind == SC_FUNC) ? NULL : (sc_func_generic*)n;

            attempt(print_ast_node_modifiers(n, buff));
            p("func ");
            pu(fnb->sc.osym.sym.name);
            if (fng) {
                p("[");
                attempt(print_sym_params(
                    fng->generic_params, fng->generic_param_count, ctx, indent,
                    buff));
                pc(']');
            }
            p("(");
            attempt(print_sym_params(
                fnb->params, fnb->param_count, ctx, indent, buff));
            pc(')');
            if (fnb->return_type) {
                p(" -> ");
                attempt(print_ast_node(
                    fnb->return_type, ctx, pm, indent + 1, buff));
                pc(' ');
            }
            if (pm == PM_FULL) {
                attempt(print_body_braced(&fnb->sc.body, ctx, indent, buff));
            }
        } break;
        case SC_STRUCT: {
            sc_struct* s = (sc_struct*)n;
            if (pm != PM_TYPE) p("struct ");
            pinn(s->sb.sc.osym.sym.name);
            if (pm == PM_FULL) {
                attempt(print_body_braced(&s->sb.sc.body, ctx, indent, buff));
            }
        } break;
        case SC_STRUCT_GENERIC: {
            sc_struct_generic* s = (sc_struct_generic*)n;
            if (pm != PM_TYPE) p("struct ");
            pinn(s->sb.sc.osym.sym.name);
            p("[");
            attempt(print_sym_params(
                s->generic_params, s->generic_param_count, ctx, indent, buff));
            pc(']');
            if (pm == PM_FULL) {
                attempt(print_body_braced(&s->sb.sc.body, ctx, indent, buff));
            }
        } break;
        case SC_STRUCT_GENERIC_INST: {
            sc_struct_generic_inst* s = (sc_struct_generic_inst*)n;
            if (pm != PM_TYPE) p("struct ");
            pinn(s->st.sb.sc.osym.sym.name);
            p("[");
            attempt(print_generic_args(
                s->generic_args, s->generic_arg_count, ctx, indent, buff));
            pc(']');
            if (pm == PM_FULL) {
                attempt(
                    print_body_braced(&s->st.sb.sc.body, ctx, indent, buff));
            }
        } break;
        case SC_TRAIT: {
            sc_trait* t = (sc_trait*)n;
            if (pm != PM_TYPE) p("trait ");
            pinn(t->sb.sc.osym.sym.name);
            if (pm == PM_FULL) {
                attempt(print_body_braced(&t->sb.sc.body, ctx, indent, buff));
            }
        } break;
        case SC_TRAIT_GENERIC: {
            sc_trait_generic* t = (sc_trait_generic*)n;
            if (pm != PM_TYPE) p("trait ");
            pinn(t->sb.sc.osym.sym.name);
            p("[");
            attempt(print_sym_params(
                t->generic_params, t->generic_param_count, ctx, indent, buff));
            pc(']');
            if (pm) {
                attempt(print_body_braced(&t->sb.sc.body, ctx, indent, buff));
            }
        } break;
        case MF_MODULE:
        case MF_EXTEND: {
            module_frame* mf = (module_frame*)n;
            if (pm != PM_TYPE) p(n->kind == MF_EXTEND ? "extend " : "module ");
            p(module_frame_get_module(mf)->name);
            if (pm == PM_FULL) {
                attempt(print_module_frame_body(mf, ctx, indent, buff));
            }
        } break;
        case MF_MODULE_GENERIC:
        case MF_EXTEND_GENERIC: {
            module_frame_generic* mf = (module_frame_generic*)n;
            p(n->kind == MF_EXTEND_GENERIC ? "extend " : "module ");
            p(module_frame_get_module(&mf->frame)->name);
            p("[");
            attempt(print_sym_params(
                mf->generic_params, mf->generic_param_count, ctx, indent,
                buff));
            pc(']');
            if (pm == PM_FULL) {
                attempt(print_module_frame_body(&mf->frame, ctx, indent, buff));
            }
        } break;
        case EXPR_PP: {
            pc('#');
            attempt(
                print_ast_node(((expr_pp*)n)->pp_expr, ctx, pm, indent, buff));
        } break;
        case EXPR_PASTE_STR: {
            p("paste(");
            attempt(print_ast_node(
                ((expr_paste_str*)n)->value, ctx, pm, indent, buff));
            pc(')');
        } break;
        case SYM_NAMED_USE: {
            sym_named_use* nu = (sym_named_use*)n;
            attempt(print_ast_node_modifiers(n, buff));
            p("using ");
            p(nu->osym.sym.name);
            p(" = ");
            attempt(print_ast_node(nu->target, ctx, pm, indent, buff));
        } break;
        case STMT_USE: {
            stmt_use* u = (stmt_use*)n;
            attempt(print_ast_node_modifiers(n, buff));
            p("using ");
            attempt(print_ast_node(u->target, ctx, pm, indent, buff));
        } break;
        case EXPR_IDENTIFIER: {
            expr_identifier* i = (expr_identifier*)n;
            if (ast_node_get_resolved((ast_node*)n) &&
                !ast_node_get_poisoned((ast_node*)n)) {
                attempt(print_ast_elem_name((ast_elem*)i->value.sym, buff));
            }
            else {
                pu(i->value.str);
            }
        } break;
        case EXPR_LITERAL: {
            expr_literal* l = (expr_literal*)n;
            switch (l->node.pt_kind) {
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
                case PT_FLUID_INT: pu(l->value.str); break;
                case PT_UINT: attempt(p_ureg(l->value.val_ureg, buff)); break;
                case PT_INT: attempt(p_sreg(l->value.val_ureg, buff)); break;
                default: assert(false); break;
            }
        } break;
        case EXPR_BLOCK: {
            expr_block* b = (expr_block*)n;
            if (pm) {
                attempt(print_namable_braced_body(
                    &b->ebb.body, b->ebb.name, ctx, indent, buff));
            }
            else {
                p("{...}");
            }
        } break;
        case SYM_VAR: {
            sym_var* v = (sym_var*)n;
            attempt(print_ast_node_modifiers(n, buff));
            pu(v->osym.sym.name);
            if (v->type != NULL) {
                p(": ");
                attempt(print_ast_node(v->type, ctx, pm, indent, buff));
            }
        } break;
        case SYM_VAR_INITIALIZED: {
            sym_var_initialized* v = (sym_var_initialized*)n;
            attempt(print_ast_node_modifiers(n, buff));
            pu(v->var.osym.sym.name);
            if (v->var.type != NULL) {
                p(": ");
                attempt(print_ast_node(v->var.type, ctx, pm, indent, buff));
                if (v->initial_value != NULL) pc(' ');
            }
            else {
                p(" :");
            }
            if (v->initial_value != NULL) {
                p("= ");
                attempt(
                    print_ast_node(v->initial_value, ctx, pm, indent, buff));
            }
        } break;
        case EXPR_OP_BINARY: {
            expr_op_binary* b = (expr_op_binary*)n;
            attempt(print_ast_node(b->lhs, ctx, pm, indent, buff));
            pc(' ');
            p(op_to_str(b->node.op_kind));
            pc(' ');
            attempt(print_ast_node(b->rhs, ctx, pm, indent, buff));
            break;
        }
        case EXPR_CAST: {
            expr_cast* ec = (expr_cast*)n;
            pc('(');
            attempt(print_ast_node(ec->value, ctx, pm, indent, buff));
            p(" as ");
            attempt(print_ast_node(ec->target_type, ctx, pm, indent, buff));
            pc(')');
            break;
        }
        case EXPR_OP_UNARY: {
            expr_op_unary* u = (expr_op_unary*)n;
            if (is_unary_op_postfix(u->node.op_kind)) {
                attempt(print_ast_node(u->child, ctx, pm, indent, buff));
                p(op_to_str(u->node.op_kind));
            }
            else {
                p(op_to_str(u->node.op_kind));
                attempt(print_ast_node(u->child, ctx, pm, indent, buff));
            }
            break;
        }
        case EXPR_ARRAY: {
            expr_array* a = (expr_array*)n;
            pc('{');
            attempt(
                print_expr_list(a->elements, a->elem_count, ctx, indent, buff));
            pc('}');
        } break;
        case EXPR_ARRAY_TYPE: {
            expr_array_type* ate = (expr_array_type*)n;
            pc('[');
            attempt(print_ast_node(ate->length_spec, ctx, pm, indent, buff));
            pc(']');
            attempt(print_ast_node(
                ate->slice_type.base_type, ctx, pm, indent, buff));
        } break;
        case EXPR_TUPLE: {
            expr_tuple* t = (expr_tuple*)n;
            pc('(');
            attempt(
                print_expr_list(t->elements, t->elem_count, ctx, indent, buff));
            if (t->elem_count == 1) {
                pc(',');
            }
            pc(')');
        } break;
        case EXPR_CALL: {
            expr_call* c = (expr_call*)n;
            attempt(print_ast_node(c->lhs, ctx, pm, indent, buff));
            pc('(');
            attempt(print_expr_list(c->args, c->arg_count, ctx, indent, buff));
            pc(')');
        } break;
        case EXPR_ACCESS: {
            expr_access* acc = (expr_access*)n;
            attempt(print_ast_node(acc->lhs, ctx, pm, indent, buff));
            pc('[');
            attempt(
                print_expr_list(acc->args, acc->arg_count, ctx, indent, buff));
            pc(']');
        } break;
        case EXPR_PARENTHESES: {
            expr_parentheses* pr = (expr_parentheses*)n;
            attempt(print_expr_in_parens(pr->child, ctx, pm, indent, buff));
        } break;
        case EXPR_BREAK: {
            expr_break* b = (expr_break*)n;
            p("break");
            const char* name = NULL;
            if (ast_node_get_resolved((ast_node*)n)) {
                name = b->target.ebb->name;
            }
            else {
                name = b->target.label;
            }
            if (name) {
                pc(' ');
                pc('@');
                p(name);
            }
            if (b->value) {
                pc(' ');
                attempt(print_ast_node(b->value, ctx, pm, indent, buff));
            }
        } break;
        case EXPR_CONTINUE: {
            expr_continue* c = (expr_continue*)n;
            p("continue");
            const char* name = NULL;
            if (ast_node_get_resolved((ast_node*)n)) {
                name = c->target.ebb->name;
            }
            else {
                name = c->target.label;
            }
            if (name) {
                pc(' ');
                pc('@');
                p(name);
            }
        } break;
        case EXPR_RETURN: {
            expr_break* r = (expr_break*)n;
            p("return");
            if (r->value != NULL) {
                pc(' ');
                attempt(print_ast_node(r->value, ctx, pm, indent, buff));
            }
        } break;
        case EXPR_LOOP: {
            expr_loop* l = (expr_loop*)n;
            p("loop ");
            attempt(print_namable_braced_body(
                &l->ebb.body, l->ebb.name, ctx, indent, buff));
        } break;
        case EXPR_MATCH: {
            expr_match* m = (expr_match*)n;
            p("match ");
            attempt(print_expr_in_parens(m->match_expr, ctx, pm, indent, buff));
            pc(' ');
            if (m->name != NULL) {
                pc('@');
                p(m->name);
            }
            p("{\n");
            match_arm** ma = (match_arm**)m->body.elements;
            indent++;
            while (*ma) {
                attempt(print_indent(indent, buff));
                attempt(
                    print_ast_node((**ma).condition, ctx, pm, indent, buff));
                p(" => ");
                attempt(print_ast_node((**ma).value, ctx, pm, indent, buff));
                if (!ast_node_may_drop_semicolon((**ma).value)) pc(';');
                pc('\n');
                ma++;
            }
            indent--;
            print_indent(indent, buff);
            pc('}');
        } break;
        case EXPR_IF: {
            expr_if* i = (expr_if*)n;
            p("if ");
            attempt(print_expr_in_parens(i->condition, ctx, pm, indent, buff));
            pc(' ');
            attempt(print_ast_node(i->if_body, ctx, pm, indent, buff));
            if (i->else_body) {
                if (ast_node_may_drop_semicolon(i->if_body)) {
                    p("\n");
                    print_indent(indent, buff);
                }
                else {
                    pc(' ');
                }
                p("else ");
                attempt(print_ast_node(i->else_body, ctx, pm, indent, buff));
            }
        } break;
        case EXPR_MEMBER_ACCESS:
        case EXPR_SCOPE_ACCESS: {
            expr_scope_access* esa = (expr_scope_access*)n;
            attempt(print_ast_node(esa->lhs, ctx, pm, indent, buff));
            p(n->kind == EXPR_MEMBER_ACCESS ? "." : "::");
            if (!ast_node_get_resolved((ast_node*)n)) {
                pu(esa->target.name);
            }
            else {
                pu(esa->target.sym->name);
            }
        } break;
        case EXPR_PASTE_EVALUATION: {
            p("<pasted>(");
            attempt(print_ast_node(
                ((paste_evaluation*)n)->expr, ctx, pm, indent, buff));
            p(")");
        } break;
        case STMT_PASTE_EVALUATION: {
            p("<pasted>");
            attempt(print_body_braced(
                &((paste_evaluation*)n)->body, ctx, indent, buff));
        } break;
        case SYM_PRIMITIVE: {
            assert(pm == PM_TYPE);
            p(((symbol*)n)->name);
        } break;
        case TYPE_POINTER: {
            assert(pm == PM_TYPE);
            type_pointer* tp = (type_pointer*)n;
            p("*");
            if (tp->tb.is_const) p("const ");
            return print_ast_elem(tp->base_type, ctx, pm, indent, buff);
        } break;
        case TYPE_SLICE: {
            assert(pm == PM_TYPE);
            type_slice* ts = (type_slice*)n;
            p("[]");
            if (ts->tb.is_const) p("const ");
            return print_ast_elem(ts->ctype_members, ctx, pm, indent, buff);
        } break;
        case TYPE_ARRAY: {
            assert(pm == PM_TYPE);
            type_array* ta = (type_array*)n;
            p("[");
            attempt(p_ureg(ta->length, buff));
            p("]");
            return print_ast_elem(
                ta->slice_type.ctype_members, ctx, pm, indent, buff);
        } break;
        case ELEM_DUMMY: {
            p("<dummy>");
        } break;
        default: {
            p("<unknown expression>");
        } break;
    }
    return OK;
}

int print_mdg_node(mdg_node* mdg, ureg indent, sbuffer* buff)
{
    attempt(print_indent(indent, buff));
    p("module ");
    p(mdg->name);
    p("{\n");

    aseglist_iterator it;
    aseglist_iterator_begin(&it, &mdg->module_frames);
    for (module_frame* mf = aseglist_iterator_next(&it); mf != NULL;
         mf = aseglist_iterator_next(&it)) {
        attempt(print_requires(mf->requires, indent + 1, buff));
    }
    aseglist_iterator_begin(&it, &mdg->module_frames);
    for (module_frame* mf = aseglist_iterator_next(&it); mf != NULL;
         mf = aseglist_iterator_next(&it)) {
        attempt(print_body_elements(&mf->body, &mf->body, indent + 1, buff));
    }
    attempt(print_indent(indent, buff));
    p("}");
    return OK;
}
char* ast_elem_to_string(
    thread_context* tc, pool* mem, ast_elem* n, ast_body* context,
    print_mode pm, ureg* str_len)
{
    sbuffer* buff = &tc->temp_buffer;
    ureg size_prev = sbuffer_get_used_size(buff);
    sbuffer_iterator it = sbuffer_iterator_begin_at_end(buff);
    if (print_ast_elem(n, context, pm, 0, buff)) {
        sbuffer_set_end(buff, &it);
        return NULL;
    }
    ureg size_after = sbuffer_get_used_size(buff);
    ureg size = size_after - size_prev;
    char* res = pool_alloc(mem, size + 1);
    if (!res) return NULL;
    sbuffer_memcpy(res, it, size);
    res[size] = '\0';
    if (str_len) *str_len = size + 1;
    return res;
}

int print_ast_node(
    ast_node* n, ast_body* ctx, print_mode pm, ureg indent, sbuffer* buff)
{
    return print_ast_elem((ast_elem*)n, ctx, pm, indent, buff);
}
