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
#include <assert.h>

void pc(char c)
{
    tputchar(c);
}
void p(const char* c)
{
    tput(c);
}
void pu(const char* c)
{
    if (c == NULL) {
        c = "unknown";
    }
    tput(c);
}
void ps(const char* c)
{
    pu(c);
    pc(' ');
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
        file_map_head_print_path(r->fmh, false);
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
void print_module_frame_body(module_frame* mf, mdg_node* cmdg, ureg indent)
{
    p("{\n");
    indent++;
    print_requires(mf->requires, indent);
    print_body_elements(&mf->body, cmdg, indent);
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
            if (ast_node_get_const(*el)) p("const ");
            pu(v->osym.sym.name);
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
// skipping zero means printing including the start
ureg print_mdg_node_until(mdg_node* start, mdg_node* m, ureg skip_levels)
{
    if (m == start) {
        if (skip_levels == 0) p(m->name);
        return 1;
    }
    ureg lvl = print_mdg_node_until(start, m->parent, skip_levels);
    if (lvl > skip_levels) p("::");
    if (lvl >= skip_levels) p(m->name);
    return lvl + 1;
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
        case EXPR_BLOCK: return ((expr_block*)n)->ebb.name;
        case EXPR_MACRO: return ((expr_macro_call*)n)->name;
        case EXPR_MATCH: return ((expr_match*)n)->name;
        case EXPR_LOOP: return ((expr_loop*)n)->ebb.name;
        default: assert(false); return NULL;
    }
}
void print_ast_elem_name(ast_elem* n)
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
}
void print_import_symbol(ast_node* n)
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
}
void print_import_module(ast_node* n, mdg_node* group_parent)
{
    if (!group_parent) p("import ");
    sym_import_module* im = (sym_import_module*)n;
    if (im->osym.sym.name != im->im_data.imported_module->name) {
        p(im->osym.sym.name);
        p(" = ");
    }
    if (ast_node_get_relative_import(n)) {
        assert(!group_parent || group_parent == im->im_data.importing_module);
        p("self::");
        print_mdg_node_until(
            im->im_data.importing_module, im->im_data.imported_module, 1);
    }
    else {
        print_mdg_node_until(
            group_parent, im->im_data.imported_module, group_parent ? 1 : 2);
    }
    if (!group_parent) p(";");
}
void print_import_group(ast_node* node, bool child, ureg indent)
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
    if (print_mdg_node_until(ig_data->relative_to, group_parent, 1) >= 2) {
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
            print_import_group(n, true, indent + 1);
        }
        else if (n->kind == SYM_IMPORT_SYMBOL) {
            print_import_symbol(n);
        }
        else {
            assert(n->kind == SYM_IMPORT_MODULE);
            print_import_module(n, group_parent);
        }
    }
    p(im_data ? ")" : "}");
    if (!child) p(";");
}
void print_ast_node_modifiers(ast_node* n)
{
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
}
// TODO: we can fully get rid of cmdg by using importing_module in
// import_module
void print_ast_node(ast_node* n, mdg_node* cmdg, ureg indent)
{
    // TODO: print access modifiers
    switch (n->kind) {
        case SYM_IMPORT_MODULE: {
            print_import_module(n, NULL);
        } break;
        case SYM_NAMED_MOD_IMPORT_GROUP:
        case SYM_NAMED_SYM_IMPORT_GROUP:
        case ASTN_ANONYMOUS_MOD_IMPORT_GROUP:
        case ASTN_ANONYMOUS_SYM_IMPORT_GROUP: {
            print_import_group(n, false, indent);
        } break;
        case STMT_COMPOUND_ASSIGN: {
            stmt_compound_assignment* ca = (stmt_compound_assignment*)n;
            pc('(');
            bool colon = ast_node_get_compound_decl(&ca->node);
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
        case SC_FUNC:
        case SC_FUNC_GENERIC: {
            sc_func_base* fnb = (sc_func_base*)n;
            sc_func_generic* fng =
                (n->kind == SC_FUNC) ? NULL : (sc_func_generic*)n;

            print_ast_node_modifiers(n);
            p("func ");
            pu(fnb->sc.osym.sym.name);
            if (fng) {
                p("[");
                print_sym_params(
                    fng->generic_params, fng->generic_param_count, cmdg,
                    indent);
                pc(']');
            }
            p("(");
            print_sym_params(fnb->params, fnb->param_count, cmdg, indent);
            pc(')');
            if (fnb->return_type) {
                p(" -> ");
                print_ast_node(fnb->return_type, cmdg, indent + 1);
                pc(' ');
            }
            print_body_braced(&fnb->sc.body, cmdg, indent);
        } break;
        case SC_STRUCT: {
            sc_struct* s = (sc_struct*)n;
            p("struct ");
            pinn(s->sb.sc.osym.sym.name);
            print_body_braced(&s->sb.sc.body, cmdg, indent);
        } break;
        case SC_STRUCT_GENERIC: {
            sc_struct_generic* s = (sc_struct_generic*)n;
            p("struct ");
            pinn(s->sb.sc.osym.sym.name);
            p("[");
            print_sym_params(
                s->generic_params, s->generic_param_count, cmdg, indent);
            pc(']');
            print_body_braced(&s->sb.sc.body, cmdg, indent);
        } break;
        case SC_TRAIT: {
            sc_trait* t = (sc_trait*)n;
            p("trait ");
            pinn(t->sb.sc.osym.sym.name);
            print_body_braced(&t->sb.sc.body, cmdg, indent);
        } break;
        case SC_TRAIT_GENERIC: {
            sc_trait_generic* t = (sc_trait_generic*)n;
            p("trait ");
            pinn(t->sb.sc.osym.sym.name);
            p("[");
            print_sym_params(
                t->generic_params, t->generic_param_count, cmdg, indent);
            pc(']');
            print_body_braced(&t->sb.sc.body, cmdg, indent);
        } break;
        case MF_MODULE:
        case MF_EXTEND: {
            module_frame* mf = (module_frame*)n;
            p(n->kind == MF_EXTEND ? "extend " : "module ");
            // pinn(mf->oscope.sc.osym.sym.name); //TODO: get name here?
            pinn("?");
            print_module_frame_body(mf, cmdg, indent);
        } break;
        case MF_MODULE_GENERIC:
        case MF_EXTEND_GENERIC: {
            module_frame_generic* mf = (module_frame_generic*)n;
            p(n->kind == MF_EXTEND_GENERIC ? "extend " : "module ");
            // pinn(mf->frame.oscope.sc.osym.sym.name); //TODO: get name
            // here?
            pinn("?");
            p("[");
            print_sym_params(
                mf->generic_params, mf->generic_param_count, cmdg, indent);
            pc(']');
            print_module_frame_body(&mf->frame, cmdg, indent);
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
        case SYM_NAMED_USE: {
            sym_named_use* nu = (sym_named_use*)n;
            print_ast_node_modifiers(n);
            p("using ");
            p(nu->osym.sym.name);
            p(" = ");
            print_ast_node(nu->target, cmdg, indent);
        } break;
        case STMT_USE: {
            stmt_use* u = (stmt_use*)n;
            print_ast_node_modifiers(n);
            p("using ");
            print_ast_node(u->target, cmdg, indent);
        } break;
        case EXPR_IDENTIFIER: {
            expr_identifier* i = (expr_identifier*)n;
            if (ast_node_get_resolved(n) && !ast_node_get_poisoned(n)) {
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
                case PT_FLUID_INT: pu(l->value.str); break;
                case PT_UINT: tprintf("%zu", l->value.val_ureg); break;
                case PT_INT: tprintf("%zi", l->value.val_ureg); break;
                default: assert(false); break;
            }
        } break;
        case EXPR_BLOCK: {
            expr_block* b = (expr_block*)n;
            print_namable_braced_body(&b->ebb.body, b->ebb.name, cmdg, indent);
        } break;
        case SYM_VAR: {
            sym_var* v = (sym_var*)n;
            print_ast_node_modifiers(n);
            pu(v->osym.sym.name);
            if (v->type != NULL) {
                p(": ");
                print_ast_node(v->type, cmdg, indent);
            }
        } break;
        case SYM_VAR_INITIALIZED: {
            sym_var_initialized* v = (sym_var_initialized*)n;
            print_ast_node_modifiers(n);
            pu(v->var.osym.sym.name);
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
        case EXPR_CAST: {
            expr_cast* ec = (expr_cast*)n;
            pc('(');
            print_ast_node(ec->value, cmdg, indent);
            p(" as ");
            print_ast_node(ec->target_type, cmdg, indent);
            pc(')');
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
        case EXPR_ARRAY_TYPE: {
            expr_array_type* ate = (expr_array_type*)n;
            pc('[');
            print_ast_node(ate->length_spec, cmdg, indent);
            pc(']');
            print_ast_node(ate->slice_type.base_type, cmdg, indent);
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
            const char* name = NULL;
            if (ast_node_get_resolved(n)) {
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
                print_ast_node(b->value, cmdg, indent);
            }
        } break;
        case EXPR_CONTINUE: {
            expr_continue* c = (expr_continue*)n;
            p("continue");
            const char* name = NULL;
            if (ast_node_get_resolved(n)) {
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
                print_ast_node(r->value, cmdg, indent);
            }
        } break;
        case EXPR_LOOP: {
            expr_loop* l = (expr_loop*)n;
            p("loop ");
            print_namable_braced_body(&l->ebb.body, l->ebb.name, cmdg, indent);
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
            if (!ast_node_get_resolved(n)) {
                pu(esa->target.name);
            }
            else {
                pu(esa->target.sym->name);
            }
        } break;
        case EXPR_PASTE_EVALUATION: {
            p("<pasted>(");
            print_ast_node(((paste_evaluation*)n)->expr, cmdg, indent);
            p(")");
        } break;
        case STMT_PASTE_EVALUATION: {
            p("<pasted>");
            print_body_braced(&((paste_evaluation*)n)->body, cmdg, indent);
        } break;
        case ELEM_DUMMY: {
            p("<dummy>");
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
    aseglist_iterator_begin(&it, &mdg->module_frames);
    for (module_frame* mf = aseglist_iterator_next(&it); mf != NULL;
         mf = aseglist_iterator_next(&it)) {
        print_requires(mf->requires, indent + 1);
    }
    aseglist_iterator_begin(&it, &mdg->module_frames);
    for (module_frame* mf = aseglist_iterator_next(&it); mf != NULL;
         mf = aseglist_iterator_next(&it)) {
        print_body_elements(&mf->body, mdg, indent + 1);
    }
    print_indent(indent);
    p("}");
}

int ctype_to_string_raw(sbuffer* buff, ast_body* ctx, ast_elem* ctype)
{
    // TODO: check if the type is shadowed and return the fully qualified name
    // in that case
    switch (ctype->kind) {
        case SYM_PRIMITIVE:
        case SC_TRAIT:
        case SC_STRUCT: {
            char* n = ((symbol*)ctype)->name;
            ureg len = strlen(n);
            char* t = sbuffer_append(buff, len);
            if (!t) return ERR;
            memcpy(t, n, len);
        } break;
        case TYPE_POINTER: {
            char* v = sbuffer_append(buff, 1);
            if (!v) return ERR;
            *v = '*';
            return ctype_to_string_raw(
                buff, ctx, ((type_pointer*)ctype)->base_type);
        } break;
        case TYPE_SLICE: {
            char* v = sbuffer_append(buff, 2);
            if (!v) return ERR;
            *v = '[';
            v++;
            *v = ']';
            return ctype_to_string_raw(
                buff, ctx, ((type_slice*)ctype)->ctype_members);
        } break;
        case TYPE_ARRAY: {
            type_array* ta = (type_array*)ctype;
            ureg len;
            if (ta->length < 10) {
                len = 1;
            }
            else {
                len = floor_doble_to_ureg(log10(ta->length)) + 1;
            }
            len += 2;
            char* v = sbuffer_append(buff, len);
            if (!v) return ERR;
            ureg l = snprintf(v, len, "[%zu]", ta->length);
            UNUSED(l);
            assert(l == len);
            return ctype_to_string_raw(buff, ctx, ta->slice_type.ctype_members);
        } break;
        default: assert("false"); panic("compiler error");
    }
    return OK;
}
char* ctype_to_string(
    thread_context* tc, pool* mem, ast_body* context, ast_elem* ctype,
    ureg* str_len)
{
    sbuffer* buff = &tc->temp_buffer;
    ureg size_prev = sbuffer_get_used_size(buff);
    sbuffer_iterator it = sbuffer_iterator_begin_at_end(buff);
    if (ctype_to_string_raw(buff, context, ctype)) {
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
