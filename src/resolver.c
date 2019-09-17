#include "tauc.h"
#include "resolver.h"
#include "thread_context.h"
#include "utils/error.h"
#include "error_log.h"
#include "utils/panic.h"
#include "utils/zero.h"
#include "utils/debug_utils.h"

resolve_error
resolve_param(resolver* r, sym_param* p, symbol_table* st, ast_elem** ctype);
resolve_error resolve_body(resolver* r, ast_body* b);
static resolve_error resolve_ast_node(
    resolver* r, ast_node* n, symbol_table* st, ast_elem** value,
    ast_elem** ctype);
resolve_error resolve_func(resolver* r, sc_func* fn, symbol_table* parent_st);
resolve_error resolve_expr_body(
    resolver* r, ast_node* expr, ast_body* b, symbol_table* parent_st,
    ast_elem** value, ast_elem** ctype);
static resolve_error add_simple_body_decls(
    resolver* r, symbol_table* parent_st, ast_body* b, bool public_st);
resolve_error resolve_expr_scope_access(
    resolver* r, expr_scope_access* esa, symbol_table* st,
    access_modifier* access, ast_elem** value, ast_elem** ctype);
resolve_error get_resolved_symbol_symtab(
    resolver* r, symbol* s, access_modifier* access, symbol_table** tgt_st);

// must be a macro so value and ctype become lazily evaluated
#define RETURN_RESOLVED(pvalue, pctype, value, ctype)                          \
    do {                                                                       \
        if (pvalue) *pvalue = (ast_elem*)(value);                              \
        if (pctype) *pctype = (ast_elem*)(ctype);                              \
        return RE_OK;                                                          \
    } while (false)

static resolve_error
report_unknown_symbol(resolver* r, ast_node* n, symbol_table* st)
{
    src_range_large srl;
    if (n->kind == EXPR_SCOPE_ACCESS || n->kind == EXPR_MEMBER_ACCESS) {
        src_range_unpack(((expr_scope_access*)n)->target_srange, &srl);
    }
    else {
        src_range_unpack(n->srange, &srl);
    }
    error_log_report_annotated(
        &r->tc->err_log, ES_RESOLVER, false, "unknown symbol",
        ast_node_get_file(n, st), srl.start, srl.end,
        "use of an undefined symbol");
    return RE_UNKNOWN_SYMBOL;
}
static resolve_error report_redeclaration_error_raw(
    thread_context* tc, symbol* redecl, src_file* redecl_file, symbol* prev,
    src_file* prev_file)
{
    error_log_report_annotated_twice(
        &tc->err_log, ES_RESOLVER, false, "symbol redeclaration", redecl_file,
        src_range_get_start(redecl->node.srange),
        src_range_get_end(redecl->node.srange),
        "a symbol of this name is already defined in this "
        "scope",
        prev_file, src_range_get_start(prev->node.srange),
        src_range_get_end(prev->node.srange), "previous definition here");
    return RE_SYMBOL_REDECLARATION;
}
static resolve_error report_redeclaration_error(
    resolver* r, symbol* redecl, symbol* prev, symbol_table* st)
{
    return report_redeclaration_error_raw(
        r->tc, redecl, ast_node_get_file((ast_node*)redecl, st), prev,
        ast_node_get_file((ast_node*)prev, st));
}
static resolve_error
add_symbol(resolver* r, symbol_table* st, symbol_table* sst, symbol* sym)
{
    sym->declaring_st = st;
    symbol_table* tgtst =
        (sst && ast_flags_get_access_mod(sym->node.flags) != AM_UNSPECIFIED)
            ? sst
            : st;
    symbol** conflict;
    conflict = symbol_table_insert(tgtst, sym);
    if (conflict) {
        return report_redeclaration_error(r, sym, *conflict, tgtst);
    }
    return RE_OK;
}
resolve_error add_sym_import_module_decl(
    thread_context* tc, src_file* f, symbol_table* st, sym_import_module* im,
    mdg_node* stop, mdg_node* start, sym_import_parent** tgt_parent)
{
    symbol** tgt;
    symbol* next;
    symbol* children;
    sym_import_parent* p;
    if (start->parent != stop) {
        resolve_error re =
            add_sym_import_module_decl(tc, f, st, im, stop, start->parent, &p);
        if (re) return re;
        tgt = &p->children.symbols;
        next = p->children.symbols;
        children = NULL;
    }
    else {
        tgt = symbol_table_find_insert_position(st, start->name);
        if (*tgt) {
            if ((*tgt)->node.kind == SYM_IMPORT_PARENT) {
                if (tgt_parent) {
                    *tgt_parent = (sym_import_parent*)*tgt;
                    return RE_OK;
                }
                p = (sym_import_parent*)*tgt;
            }
            else if (tgt_parent && (*tgt)->node.kind == SYM_IMPORT_MODULE) {
                p = NULL;
                (**tgt).name = "";
                next = (**tgt).next;
                (**tgt).next = NULL;
                children = *tgt;
            }
            else {
                if (!f) f = ast_node_get_file((ast_node*)im, st);
                return report_redeclaration_error_raw(
                    tc, (symbol*)im, f, *tgt, f);
            }
        }
        else {
            p = NULL;
            next = NULL;
            children = NULL;
        }
    }
    // PERF: this is O(n^2) over imports with the same prefix TODO: fix
    if (p) {
        symbol* c = p->children.symbols;
        while (c) {
            if (strcmp(c->name, start->name) != 0) {
                c = c->next;
                continue;
            }
            if (c->node.kind == SYM_IMPORT_MODULE) {
                if (tgt_parent) {
                    // becomes the end of our new parent's children
                    next = c->next;
                    c->next = NULL;
                    children = c;
                    break;
                }
                else {
                    // TODO: maybe only warn about import redecl
                    if (!f) f = ast_node_get_file((ast_node*)im, st);
                    return report_redeclaration_error_raw(
                        tc, (symbol*)im, f, *tgt, f);
                }
            }
            else {
                assert((**tgt).node.kind == SYM_IMPORT_PARENT);
                if (tgt_parent) {
                    *tgt_parent = (sym_import_parent*)c;
                    return RE_OK;
                }
                p = (sym_import_parent*)*tgt;
                im->sym.name = "";
                c = p->children.symbols;
                tgt = &p->children.symbols;
                next = p->children.symbols;
                continue;
            }
        }
    }
    if (tgt_parent) {
        sym_import_parent* p =
            pool_alloc(&tc->permmem, sizeof(sym_import_parent));
        if (!p) return RE_FATAL;
        p->sym.node.kind = SYM_IMPORT_PARENT;
        p->sym.node.flags = AST_NODE_FLAGS_DEFAULT;
        p->sym.name = start->name;
        p->sym.next = next;
        p->children.symbols = children;
        *tgt = (symbol*)p;
        *tgt_parent = p;
    }
    else {
        *tgt = (symbol*)im;
        im->sym.next = next;
    }
    return RE_OK;
}
resolve_error add_import_group_decls(
    thread_context* tc, mdg_node* curr_mdg_node, src_file* f,
    sym_import_group* ig, symbol_table* st)
{
    symbol* next = ig->children.symbols;
    symbol* s;
    resolve_error re;
    while (next != NULL) {
        s = next;
        next = s->next;
        if (s->node.kind == SYM_IMPORT_GROUP) {
            sym_import_group* nig = (sym_import_group*)s;
            if (!nig->sym.name) {
                re = add_import_group_decls(tc, curr_mdg_node, f, nig, st);
                if (re) return re;
                continue;
            }
        }
        else if (s->node.kind == SYM_IMPORT_SYMBOL) {
            symbol** cf = symbol_table_insert(st, s);
            if (cf) {
                if (!f) f = ast_node_get_file((ast_node*)ig, st);
                return report_redeclaration_error_raw(tc, s, f, *cf, f);
            }
        }
        else {
            assert(s->node.kind == SYM_IMPORT_MODULE);
            sym_import_module* im = (sym_import_module*)s;
            if (ast_flags_get_relative_import(s->node.flags)) {
                re = add_sym_import_module_decl(
                    tc, f, st, im, curr_mdg_node, im->target, NULL);
            }
            else {
                re = add_sym_import_module_decl(
                    tc, f, st, im, TAUC.mdg.root_node, im->target, NULL);
            }
            if (re) return re;
        }
    }
    return RE_OK;
}
static resolve_error add_ast_node_decls(
    resolver* r, symbol_table* st, symbol_table* sst, ast_node* n,
    bool public_st)
{
    if (n == NULL) return RE_OK;
    resolve_error re;
    switch (n->kind) {
        case OSC_MODULE:
        case OSC_EXTEND: {
            // these guys are handled from their mdg node, not from
            // where they appear in source
            return RE_OK;
        }
        case SC_STRUCT:
        case SC_TRAIT: {
            re = add_symbol(r, st, sst, (symbol*)n);
            if (public_st &&
                ast_flags_get_access_mod(n->flags) >= AM_PROTECTED) {
                ((sc_struct*)n)->id = r->public_sym_count++;
            }
            else {
                ((sc_struct*)n)->id = r->private_sym_count++;
            }
            if (re) return re;
            public_st =
                public_st && ast_flags_get_access_mod(n->flags) >= AM_PROTECTED;
            return add_simple_body_decls(r, st, &((scope*)n)->body, public_st);
        }

        case SC_FUNC: {
            sc_func* fn = (sc_func*)n;
            if (public_st &&
                ast_flags_get_access_mod(n->flags) >= AM_PROTECTED) {
                fn->id = r->public_sym_count++;
            }
            else {
                fn->id = r->private_sym_count++;
            }
            symbol_table* tgtst =
                (sst && ast_flags_get_access_mod(n->flags) != AM_UNSPECIFIED)
                    ? sst
                    : st;
            symbol** conflict;
            symbol* sym = (symbol*)n;
            sym->declaring_st = st;
            conflict = symbol_table_insert(tgtst, sym);
            if (conflict) {
                sym_func_overloaded* sfo;
                if ((**conflict).node.kind == SC_FUNC) {
                    sfo = (sym_func_overloaded*)pool_alloc(
                        &r->tc->permmem, sizeof(sym_func_overloaded));
                    if (!sfo) return RE_FATAL;
                    sfo->sym.node.kind = SYM_FUNC_OVERLOADED;
                    sfo->sym.node.flags = AST_NODE_FLAGS_DEFAULT;
                    sfo->sym.node.srange = SRC_RANGE_INVALID;
                    sfo->sym.next = (**conflict).next;
                    sfo->sym.name = (**conflict).name;
                    sfo->sym.declaring_st = tgtst;
                    sfo->funcs = (sc_func*)*conflict;
                    (**conflict).next = sym;
                    sym->next = NULL;
                    *conflict = (symbol*)sfo;
                }
                else if ((**conflict).node.kind == SYM_FUNC_OVERLOADED) {
                    sfo = (sym_func_overloaded*)conflict;
                    sym->next = (symbol*)sfo->funcs;
                    sfo->funcs = (sc_func*)n;
                }
                else {
                    return report_redeclaration_error(r, sym, *conflict, tgtst);
                }
            }
            // we only do the parameters here because the declaration and use
            // func body vars is strongly ordered
            ast_body* b = &fn->scp.body;
            if (b->symtab == NULL) {
                b->symtab = st;
            }
            else {
                b->symtab->parent = st;
                for (ureg i = 0; i < fn->param_count; i++) {
                    re =
                        add_symbol(r, b->symtab, NULL, (symbol*)&fn->params[i]);
                    if (re) return re;
                }
            }

            return RE_OK;
        }
        case SYM_IMPORT_GROUP: {
            sym_import_group* ig = (sym_import_group*)n;
            if (ig->sym.name) {
                return add_symbol(r, st, sst, (symbol*)ig);
            }
            symbol_table* pst = st;
            while (pst->owning_node->kind != ELEM_MDG_NODE) {
                pst = pst->parent;
                assert(pst);
            }
            return add_import_group_decls(
                r->tc, (mdg_node*)pst->owning_node, NULL, ig, st);
        }
        case SYM_IMPORT_MODULE: {
            sym_import_module* im = (sym_import_module*)n;
            mdg_node* stop;
            if (ast_flags_get_relative_import(n->flags)) {
                symbol_table* pst = st;
                while (pst->owning_node->kind != ELEM_MDG_NODE) {
                    pst = pst->parent;
                    assert(pst);
                }
                stop = (mdg_node*)pst->owning_node;
            }
            else {
                stop = TAUC.mdg.root_node;
            }
            return add_sym_import_module_decl(
                r->tc, NULL, st, im, stop, im->target, NULL);
        }
        case STMT_USING:
        case STMT_COMPOUND_ASSIGN:
            // TODO
            return RE_OK;

        case SYM_VAR_INITIALIZED: {
            re = add_ast_node_decls(
                r, st, sst, ((sym_var_initialized*)n)->initial_value,
                public_st);
            if (re) return re;
            // fallthrough
        }
        case SYM_VAR: {
            if (public_st &&
                ast_flags_get_access_mod(n->flags) >= AM_PROTECTED) {
                ((sym_var*)n)->var_id = r->public_sym_count++;
            }
            else {
                ((sym_var*)n)->var_id = r->private_sym_count++;
            }
            return add_symbol(r, st, sst, (symbol*)n);
        }
        case SYM_NAMED_USING: {
            re = add_symbol(r, st, sst, (symbol*)n);
            if (re) return re;
            return add_ast_node_decls(
                r, st, sst, ((sym_named_using*)n)->target, public_st);
        }

        case EXPR_RETURN:
        case EXPR_BREAK:
            return add_ast_node_decls(
                r, st, sst, ((expr_break*)n)->value, public_st);

        case EXPR_BLOCK:
            return add_simple_body_decls(r, st, &((expr_block*)n)->body, false);

        case EXPR_IF: {
            expr_if* ei = (expr_if*)n;
            re = add_ast_node_decls(r, st, sst, ei->condition, false);
            if (re) return re;
            re = add_ast_node_decls(r, st, sst, ei->if_body, false);
            if (re) return re;
            return add_ast_node_decls(r, st, sst, ei->else_body, false);
        }

        case EXPR_LOOP:
            return add_simple_body_decls(r, st, &((expr_loop*)n)->body, false);

        case EXPR_MACRO: {
            expr_macro* em = (expr_macro*)n;
            re = add_simple_body_decls(r, st, &em->body, false);
            if (re) return re;
            return add_ast_node_decls(r, st, sst, (ast_node*)em->next, false);
        }

        case EXPR_PP: {
            symbol_table* sstpp = sst ? sst->pp_symtab : NULL;
            return add_ast_node_decls(
                r, st->pp_symtab, sstpp, ((expr_pp*)n)->pp_expr, false);
        }
        case EXPR_MATCH: {
            expr_match* em = (expr_match*)n;
            re = add_ast_node_decls(r, st, sst, em->match_expr, false);
            if (re) return re;
            for (match_arm** ma = (match_arm**)em->body.elements; *ma != NULL;
                 ma++) {
                re = add_ast_node_decls(r, st, sst, (**ma).condition, false);
                if (re) return re;
                re = add_ast_node_decls(r, st, sst, (**ma).value, false);
                if (re) return re;
            }
            return RE_OK;
        }
        case EXPR_OP_BINARY: {
            re = add_ast_node_decls(
                r, st, sst, ((expr_op_binary*)n)->lhs, false);
            if (re) return re;
            return add_ast_node_decls(
                r, st, sst, ((expr_op_binary*)n)->rhs, false);
        }
        case EXPR_ACCESS: {
            expr_access* a = (expr_access*)n;
            re = add_ast_node_decls(r, st, sst, a->lhs, false);
            if (re) return re;
            for (ureg i = 0; i < a->arg_count; i++) {
                re = add_ast_node_decls(r, st, sst, a->args[i], false);
                if (re) return re;
            }
            // we could make this tail recursive by moving lhs down here
            // but this way we traverse as declared ->better cache usage
            return RE_OK;
        }
        case EXPR_CALL: {
            expr_call* c = (expr_call*)n;
            re = add_ast_node_decls(r, st, sst, c->lhs, false);
            if (re) return re;
            for (ureg i = 0; i < c->arg_count; i++) {
                re = add_ast_node_decls(r, st, sst, c->args[i], false);
                if (re) return re;
            }
            return RE_OK;
        }
        case EXPR_SCOPE_ACCESS:
        case EXPR_MEMBER_ACCESS: {
            return add_ast_node_decls(
                r, st, sst, ((expr_scope_access*)n)->lhs, false);
        }
        case EXPR_PARENTHESES: {
            return add_ast_node_decls(
                r, st, sst, ((expr_parentheses*)n)->child, false);
        }
        case EXPR_OP_UNARY: {
            return add_ast_node_decls(
                r, st, sst, ((expr_op_unary*)n)->child, false);
        }
        default:
            return RE_OK; // TODO
            assert(false); // unknown node_kind
    }
    assert(false);
}

static resolve_error add_body_decls(
    resolver* r, symbol_table* parent_st, symbol_table* shared_st, ast_body* b,
    bool public_st)
{
    if (b->symtab == NULL) {
        b->symtab = parent_st;
    }
    else {
        b->symtab->parent = parent_st;
    }
    for (ast_node** n = b->elements; *n; n++) {
        resolve_error re =
            add_ast_node_decls(r, b->symtab, shared_st, *n, public_st);
        if (re) return re;
    }
    return RE_OK;
}
static resolve_error add_simple_body_decls(
    resolver* r, symbol_table* parent_st, ast_body* b, bool public_st)
{
    return add_body_decls(r, parent_st, NULL, b, public_st);
}
static inline resolve_error mark_mdg_nodes_resolved(resolver* r)
{
    for (mdg_node** i = r->start; i != r->end; i++) {
        resolve_error re = mdg_node_resolved(*i, r->tc);
        if (re) {
            return re;
        }
    }
    return RE_OK;
}
static inline void print_debug_info(resolver* r)
{
    tprintf("resolving {");
    mdg_node** i = r->start;
    i = r->start;
    while (i + 1 != r->end) {
        tprintf("%s, ", (**i).name);
        i++;
    }
    tprintf("%s} ", (**i).name);
}

bool ctypes_unifiable(ast_elem* a, ast_elem* b)
{
    // immediately return true e.g. for primitives
    if (a == b) return true;
    return false; // TODO
    /*
    switch (a->kind) {
        case TYPE_MODIFIERS:
    }
     */
}
resolve_error operator_func_applicable(
    resolver* r, symbol_table* op_st, ast_elem* lhs, ast_elem* rhs, sc_func* f,
    bool* applicable, ast_elem** ctype)
{
    // ensure func has exactly 2 parameters
    // [varargs not allowed for operators]
    if (f->param_count != 2) {
        *applicable = false;
        return RE_OK;
    }
    ast_elem* param_ctype;
    resolve_error re = resolve_param(r, &f->params[0], op_st, &param_ctype);
    if (re) return re;
    if (!ctypes_unifiable(lhs, param_ctype)) {
        *applicable = false;
        return RE_OK;
    }
    re = resolve_param(r, &f->params[0], op_st, &param_ctype);
    if (re) return re;
    if (!ctypes_unifiable(rhs, param_ctype)) {
        *applicable = false;
        return RE_OK;
    }
    if (ctype) *ctype = f->return_ctype;
    *applicable = true;
    return RE_OK;
}
resolve_error func_applicable(
    resolver* r, symbol_table* fn_st, ast_elem** call_arg_types, ureg arg_count,
    sc_func* func, bool* applicable, ast_elem** ctype)
{
    // works cause varags are not in the lang yet
    if (func->param_count != arg_count) return false;
    for (ureg i = 0; i < arg_count; i++) {
        ast_elem* ctype;
        resolve_error re = resolve_param(r, &func->params[i], fn_st, &ctype);
        if (re) return re;
        if (!ctypes_unifiable(ctype, call_arg_types[i])) {
            *applicable = false;
            return RE_OK;
        }
    }
    *applicable = true;
    return resolve_ast_node(r, func->return_type, fn_st, ctype, NULL);
}
resolve_error resolve_func_call(
    resolver* r, char* func_name, expr_call* c, symbol_table* func_st,
    symbol_table* args_st, ast_elem** ctype)
{
    ast_elem** call_arg_types =
        sbuffer_append(&r->call_types, c->arg_count * sizeof(ast_elem*));
    resolve_error re = RE_OK;
    for (ureg i = 0; i < c->arg_count; i++) {
        re = resolve_ast_node(r, c->args[i], args_st, NULL, &call_arg_types[i]);
        if (re) return re;
    }
    symbol_table* lt = func_st;

    while (lt) {
        symbol_table* fn_st;
        symbol** s = symbol_table_lookup_with_decl(
            lt, AM_UNSPECIFIED, func_name, &fn_st);
        if (!s) {
            // we use args_st here because thats the scope that the call is in
            re = report_unknown_symbol(r, c->lhs, args_st);
            break;
        }
        bool applicable;
        if ((**s).node.kind == SYM_FUNC_OVERLOADED) {
            sym_func_overloaded* sfo = (sym_func_overloaded*)s;
            sc_func* f = sfo->funcs;
            while (f) {
                re = func_applicable(
                    r, fn_st, call_arg_types, c->arg_count, f, &applicable,
                    ctype);
                if (applicable) c->target = f;
                if (re || applicable) break;
                f = (sc_func*)f->scp.sym.next;
            }
            if (re || applicable) break;
        }
        else if ((**s).node.kind == SC_FUNC) {
            re = func_applicable(
                r, fn_st, call_arg_types, c->arg_count, (sc_func*)(*s),
                &applicable, ctype);
            if (applicable) c->target = (sc_func*)(*s);
            if (re || applicable) break;
        }
        else {
            assert(false);
        }
        lt = lt->parent;
    }
    sbuffer_remove_at_end(&r->call_types, c->arg_count * sizeof(ast_elem*));
    return re;
}
resolve_error
resolve_call(resolver* r, expr_call* c, symbol_table* st, ast_elem** ctype)
{

    if (c->lhs->kind == EXPR_IDENTIFIER) {
        return resolve_func_call(
            r, ((expr_identifier*)c->lhs)->value.str, c, st, st, ctype);
    }
    if (c->lhs->kind == EXPR_SCOPE_ACCESS) {
        expr_scope_access* esa = (expr_scope_access*)c->lhs;
        ast_elem* esa_lhs;
        symbol_table* lhs_st;
        // TODO: fix access modifier restrictions
        access_modifier am = AM_UNSPECIFIED;
        resolve_error re = resolve_ast_node(r, esa->lhs, st, &esa_lhs, NULL);
        if (re) return re;
        assert(ast_elem_is_symbol(esa_lhs));
        re = get_resolved_symbol_symtab(r, (symbol*)esa_lhs, &am, &lhs_st);
        if (re) return re;
        return resolve_func_call(r, esa->target.name, c, lhs_st, st, ctype);
    }
    assert(false); // TODO
    return RE_OK;
}
resolve_error choose_unary_operator_overload(
    resolver* r, expr_op_unary* ou, symbol_table* st, ast_elem** ctype)
{
    ast_elem* child_type;
    resolve_error re = resolve_ast_node(r, ou->child, st, NULL, &child_type);
    if (re) return re;
    if (child_type->kind == PRIMITIVE) {
        ou->op = child_type;
        if (ctype) *ctype = child_type;
        return RE_OK;
    }
    assert(false); // TODO
    return RE_FATAL;
}
resolve_error choose_binary_operator_overload(
    resolver* r, expr_op_binary* ob, symbol_table* st, ast_elem** ctype)
{
    ast_elem *lhs_ctype, *rhs_ctype;
    resolve_error re = resolve_ast_node(r, ob->lhs, st, NULL, &lhs_ctype);
    if (re) return re;
    re = resolve_ast_node(r, ob->rhs, st, NULL, &rhs_ctype);
    if (re) return re;
    if (lhs_ctype->kind == PRIMITIVE && rhs_ctype->kind == PRIMITIVE) {
        // TODO: proper inbuild operator resolution
        // maybe create a cat_prim_kind and switch over cat'ed lhs and rhs
        ob->op = lhs_ctype;
        if (ctype) *ctype = lhs_ctype;
        return RE_OK;
    }
    symbol_table* lt = st;
    while (lt) {
        bool applicable;
        symbol_table* op_st;
        symbol** s = symbol_table_lookup_with_decl(
            lt, AM_UNSPECIFIED, op_to_str(ob->node.op_kind), &op_st);
        if (!s) return report_unknown_symbol(r, (ast_node*)ob, lt);
        if ((**s).node.kind == SYM_FUNC_OVERLOADED) {
            sym_func_overloaded* sfo = (sym_func_overloaded*)s;
            sc_func* f = sfo->funcs;
            while (f) {
                re = operator_func_applicable(
                    r, op_st, lhs_ctype, rhs_ctype, f, &applicable, ctype);
                if (re) return re;
                if (applicable) return RE_OK;
                f = (sc_func*)f->scp.sym.next;
            }
        }
        else if ((**s).node.kind == SC_FUNC) {
            re = operator_func_applicable(
                r, op_st, lhs_ctype, rhs_ctype, (sc_func*)*s, &applicable,
                ctype);
            if (re) return re;
            if (applicable) return RE_OK;
        }
        lt = lt->parent;
    }
    return report_unknown_symbol(r, (ast_node*)ob, st);
}
ast_elem* get_resolved_symbol_ctype(symbol* s)
{
    switch (s->node.kind) {
        case SYM_VAR:
        case SYM_VAR_INITIALIZED: return ((sym_var*)s)->ctype; break;
        case SYM_NAMED_USING: assert(false); return NULL; // TODO
        case PRIMITIVE: assert(false); return NULL; // would be ctype "Type"
        default: return (ast_elem*)s;
    }
}
ast_elem** get_break_target_ctype(ast_node* n)
{
    switch (n->kind) {
        case EXPR_BLOCK: return &((expr_block*)n)->ctype;
        case EXPR_IF: return &((expr_if*)n)->ctype;
        case EXPR_LOOP: return &((expr_loop*)n)->ctype;
        default: assert(false);
    }
}
resolve_error get_resolved_symbol_symtab(
    resolver* r, symbol* s, access_modifier* access, symbol_table** tgt_st)
{
    if (ast_elem_is_scope((ast_elem*)s)) {
        // TODO: handle access change here
        *tgt_st = ((scope*)s)->body.symtab;
        return RE_OK;
    }
    *access = AM_PUBLIC;
    if (s->node.kind == SYM_IMPORT_SYMBOL) {
        return get_resolved_symbol_symtab(
            r, ((sym_import_symbol*)s)->target.sym, access, tgt_st);
    }
    if (s->node.kind == SYM_IMPORT_GROUP) {
        *tgt_st = ((sym_import_group*)s)->children.symtab;
    }
    else if (s->node.kind == SYM_IMPORT_PARENT) {
        *tgt_st = ((sym_import_parent*)s)->children.symtab;
    }
    else if (s->node.kind == SYM_IMPORT_MODULE) {
        *tgt_st = ((sym_import_module*)s)->target->symtab;
    }
    else {
        assert(false); // TODO: error
    }
    return RE_OK;
}
resolve_error
resolve_import_parent(resolver* r, sym_import_parent* ip, symbol_table* st)
{
    ureg children_count = 0;
    ureg use = 0;
    symbol* s = ip->children.symbols;
    assert(s); // if there's no child we would not created a parent
    do {
        // meaning is empty string
        if (*s->name == '\0') {
            assert(use == 0); // we would have complained earlier about a redecl
            use = 1;
        }
        else {
            children_count++;
        }
        s = s->next;
    } while (s);
    assert(children_count > 0);
    symbol_table* pst;
    if (symbol_table_init(&pst, children_count, use, true, (ast_elem*)ip)) {
        return RE_FATAL;
    }
    symbol* next = ip->children.symbols;
    do {
        s = next;
        next = next->next;
        if (*s->name != '\0') {
            symbol** res = symbol_table_insert(pst, s);
            // we checked for collisions during insert into the linked list
            if (!res) assert(!res);
        }
        else {
            symbol_table_insert_using(
                pst, AM_PUBLIC, (ast_node*)s,
                ((sym_import_module*)s)->target->symtab);
        }
    } while (next);
    ip->children.symtab = pst;
    ast_flags_set_resolved(&ip->sym.node.flags);
    return RE_OK;
}
resolve_error
resolve_param(resolver* r, sym_param* p, symbol_table* st, ast_elem** ctype)
{
    ast_flags_set_resolving(&p->sym.node.flags);
    resolve_error re;
    if (p->type) {
        re = resolve_ast_node(r, p->type, st, &p->ctype, NULL);
        if (re) return re;
        if (p->default_value) {
            ast_elem* val;
            re = resolve_ast_node(
                r, (ast_node*)p->default_value, st, NULL, &val);
            if (re) return re;
            if (!ctypes_unifiable(p->ctype, val)) {
                assert(false); // TODO: error
            }
        }
    }
    else {
        re = resolve_ast_node(r, p->default_value, st, NULL, &p->ctype);
        if (re) return re;
    }
    if (ctype) *ctype = p->ctype;
    ast_flags_set_resolved(&p->sym.node.flags);
    return PE_OK;
}
resolve_error resolve_expr_scope_access(
    resolver* r, expr_scope_access* esa, symbol_table* st,
    access_modifier* access, ast_elem** value, ast_elem** ctype)
{
    resolve_error re;
    symbol_table* lhs_st;
    ast_elem* lhs_val;
    re = resolve_ast_node(r, esa->lhs, st, &lhs_val, NULL);
    if (re) return re;
    assert(lhs_val != NULL && ast_elem_is_symbol(lhs_val)); // TODO: log error
    re = get_resolved_symbol_symtab(r, (symbol*)lhs_val, access, &lhs_st);
    if (re) return re;
    symbol_table* rhs_decl_st;
    symbol** s = symbol_table_lookup_with_decl(
        lhs_st, *access, esa->target.name, &rhs_decl_st);
    if (!*s) {
        return report_unknown_symbol(r, (ast_node*)esa, st);
    }
    ast_elem* rhs_val;

    resolve_ast_node(r, (ast_node*)*s, rhs_decl_st, &rhs_val, ctype);
    assert(rhs_val != NULL && ast_elem_is_symbol(rhs_val)); // TODO: log error
    esa->target.sym = (symbol*)lhs_val;
    if (value) *value = rhs_val;
    ast_flags_set_resolved(&esa->node.flags);
    return RE_OK;
}
access_modifier check_member_access(symbol_table* st, scope* tgt)
{
    return AM_UNSPECIFIED; // TODO
}
resolve_error resolve_expr_member_accesss(
    resolver* r, expr_member_access* ema, symbol_table* st,
    access_modifier* access, ast_elem** value, ast_elem** ctype)
{
    ast_elem* lhs_type;
    resolve_ast_node(r, ema->lhs, st, NULL, &lhs_type);
    if (lhs_type->kind != SC_STRUCT) { // TODO: pointers
        // TODO: errror
        assert(false);
        return RE_FATAL;
    }
    scope* sc = (scope*)lhs_type;
    // TODO: try to make this check more efficient
    access_modifier acc = check_member_access(st, sc);
    symbol_table* decl_st;
    symbol** rhs = symbol_table_lookup_limited_with_decl(
        sc->body.symtab, acc, sc->body.symtab->parent, ema->target.name,
        &decl_st);
    if (!*rhs) return report_unknown_symbol(r, (ast_node*)ema, sc->body.symtab);
    ema->target.sym = *rhs;
    resolve_ast_node(r, (ast_node*)*rhs, decl_st, value, ctype);
    ast_flags_set_resolved(&ema->node.flags);
    return RE_OK;
}
// the symbol table is not the one that contains the symbol, but the one
// where it was declared and where the type name loopup should start
static inline resolve_error resolve_ast_node_raw(
    resolver* r, ast_node* n, symbol_table* st, ast_elem** value,
    ast_elem** ctype)
{
    if (!n) {
        RETURN_RESOLVED(value, ctype, VOID_ELEM, VOID_ELEM);
    }
    if (ast_elem_is_open_scope((ast_elem*)n)) {
        if (ctype) *ctype = (ast_elem*)n;
        return RE_OK;
    }
    // PERF: find a way to avoid checking in sub exprs
    bool resolved = ast_flags_get_resolved(n->flags);
    if (resolved) {
        if (!ctype && !value) return RE_OK;
    }
    else {
        if (ast_flags_get_resolving(n->flags)) {
            r->type_loop_start = n;
            return RE_TYPE_LOOP;
        }
        ast_flags_set_resolving(&n->flags);
    }
    resolve_error re;
    switch (n->kind) {
        case PRIMITIVE:
            assert(!ctype);
            RETURN_RESOLVED(value, ctype, &PRIMITIVES[n->pt_kind], NULL);
        case EXPR_LITERAL:
            RETURN_RESOLVED(value, ctype, n, &PRIMITIVES[n->pt_kind]);

        case EXPR_IDENTIFIER: {
            expr_identifier* e = (expr_identifier*)n;
            if (resolved) {
                RETURN_RESOLVED(
                    value, ctype, e->value.sym,
                    get_resolved_symbol_ctype(e->value.sym));
            }
            symbol_table* sym_st;
            symbol** s = symbol_table_lookup_with_decl(
                st, AM_UNSPECIFIED, e->value.str, &sym_st);
            if (!s) return report_unknown_symbol(r, n, st);
            re = resolve_ast_node(r, (ast_node*)*s, sym_st, value, ctype);
            if (re) return re;
            e->value.sym = *s;
            ast_flags_set_resolved(&n->flags);
            return RE_OK;
        }
        case EXPR_CALL: {
            expr_call* c = (expr_call*)n;
            if (resolved)
                RETURN_RESOLVED(value, ctype, c, c->target->return_ctype);
            return resolve_call(r, c, st, ctype);
        }
        case EXPR_CONTINUE:
        case EXPR_OP_UNARY: {
            expr_op_unary* ou = (expr_op_unary*)n;
            if (resolved) {
                if (ou->op->kind == PRIMITIVE) {
                    *ctype =
                        (ast_elem*)&PRIMITIVES[((ast_node*)ou->op)->pt_kind];
                }
                else {
                    *ctype = ((sc_func*)ou->op)->return_ctype;
                }
                return RE_OK;
            }
            re = choose_unary_operator_overload(r, ou, st, ctype);
            if (re) return re;
            ast_flags_set_resolved(&n->flags);
            return RE_OK;
        }
        case EXPR_PARENTHESES: {
            return resolve_ast_node(
                r, ((expr_parentheses*)n)->child, st, value, ctype);
        }
        case EXPR_OP_BINARY: {
            expr_op_binary* ob = (expr_op_binary*)n;
            if (resolved) {
                if (ob->op->kind == PRIMITIVE) {
                    *ctype =
                        (ast_elem*)&PRIMITIVES[((ast_node*)ob->op)->pt_kind];
                }
                else {
                    *ctype = ((sc_func*)ob->op)->return_ctype;
                }
                return RE_OK;
            }
            re = choose_binary_operator_overload(r, ob, st, ctype);
            if (re) return re;
            ast_flags_set_resolved(&n->flags);
            return RE_OK;
        }
        case EXPR_MEMBER_ACCESS: {
            expr_member_access* ema = (expr_member_access*)n;
            if (resolved) {
                RETURN_RESOLVED(
                    value, ctype, n,
                    get_resolved_symbol_ctype(ema->target.sym));
            }
            access_modifier am = AM_UNSPECIFIED;
            return resolve_expr_member_accesss(r, ema, st, &am, value, ctype);
        }
        case EXPR_SCOPE_ACCESS: {
            expr_scope_access* esa = (expr_scope_access*)n;
            if (resolved) {
                RETURN_RESOLVED(
                    value, ctype, n,
                    get_resolved_symbol_ctype(esa->target.sym));
            }
            access_modifier access = AM_UNSPECIFIED;
            return resolve_expr_scope_access(r, esa, st, &access, NULL, ctype);
        }
        case SC_STRUCT:
        case SC_STRUCT_GENERIC:
        case SC_TRAIT:
        case SC_TRAIT_GENERIC: {
            if (ctype) *ctype = (ast_elem*)n;
            if (resolved) return RE_OK;
            re = resolve_body(r, &((scope*)n)->body);
            if (re) return re;
            ast_flags_set_resolved(&n->flags);
            return RE_OK;
        }

        case SC_FUNC:
        case SC_FUNC_GENERIC: {
            // TODO: ctype should actually be some kind of func ptr
            if (ctype) *ctype = (ast_elem*)n;
            if (resolved) return RE_OK;
            return resolve_func(r, (sc_func*)n, st);
        }
        case SYM_IMPORT_PARENT: {
            // TODO: fix the ctype
            assert(!value && !ctype);
            if (resolved) return PE_OK;
            return resolve_import_parent(r, (sym_import_parent*)n, st);
        }
        case SYM_IMPORT_GROUP: {
            assert(!value && !ctype);
            if (!resolved) ast_flags_set_resolved(&n->flags);
            return RE_OK;
        }
        case SYM_IMPORT_MODULE: {
            assert(!ctype);
            if (!resolved) ast_flags_set_resolved(&n->flags);
            RETURN_RESOLVED(
                value, ctype,
                ((sym_import_module*)n)->target->symtab->owning_node, NULL);
            return RE_OK;
        }
        case SYM_IMPORT_SYMBOL: {
            sym_import_symbol* is = (sym_import_symbol*)n;
            if (resolved) {
                RETURN_RESOLVED(
                    value, ctype, is->target.sym,
                    get_resolved_symbol_ctype(is->target.sym));
            }
            symbol_table* sym_st;
            symbol** s = symbol_table_lookup_with_decl(
                st, AM_PROTECTED, is->target.name, &sym_st);
            if (!s) return report_unknown_symbol(r, n, st);
            is->target.sym = *s;
            re = resolve_ast_node(r, (ast_node*)*s, sym_st, value, ctype);
            if (re) return re;
            ast_flags_set_resolved(&n->flags);
            return RE_OK;
        }
        case STMT_USING:
        case SYM_NAMED_USING:
        case STMT_COMPOUND_ASSIGN: {
            // TODO
            return RE_OK;
        }
        case SYM_VAR: {
            sym_var* v = (sym_var*)n;
            if (resolved) RETURN_RESOLVED(value, ctype, v, v->ctype);
            symbol* prev_sym_decl = r->curr_symbol_decl;
            r->curr_symbol_decl = (symbol*)v;
            ast_elem* type;
            re = resolve_ast_node(r, v->type, st, &type, NULL);
            r->curr_symbol_decl = prev_sym_decl;
            if (re) return re;
            v->ctype = type;
            ast_flags_set_resolved(&n->flags);
            RETURN_RESOLVED(value, ctype, v, v->ctype);
        }
        case SYM_VAR_INITIALIZED: {
            sym_var_initialized* vi = (sym_var_initialized*)n;
            if (resolved) {
                RETURN_RESOLVED(value, ctype, vi, vi->var.ctype);
            }
            symbol* prev_sym_decl = r->curr_symbol_decl;
            r->curr_symbol_decl = (symbol*)vi;
            if (vi->var.type) {
                re =
                    resolve_ast_node(r, vi->var.type, st, &vi->var.ctype, NULL);
                if (!re) {
                    ast_elem* val_type;
                    re = resolve_ast_node(
                        r, vi->initial_value, st, NULL, &val_type);
                    if (!re) {
                        if (!ctypes_unifiable(vi->var.ctype, val_type)) {
                            error_log_report_annotated_twice(
                                &r->tc->err_log, ES_RESOLVER, false,
                                "type missmatch in variable declaration",
                                ast_node_get_file(vi->var.type, st),
                                src_range_get_start(vi->var.type->srange),
                                src_range_get_end(vi->var.type->srange),
                                "declared type here",
                                ast_node_get_file(vi->initial_value, st),
                                src_range_get_start(vi->initial_value->srange),
                                src_range_get_end(vi->initial_value->srange),
                                "doesn' t match type of the initial value");
                            re = RE_TYPE_MISSMATCH;
                        }
                    }
                }
            }
            else {
                re = resolve_ast_node(
                    r, vi->initial_value, st, NULL, &vi->var.ctype);
            }
            r->curr_symbol_decl = prev_sym_decl;
            if (re) return re;
            ast_flags_set_resolved(&n->flags);
            RETURN_RESOLVED(value, ctype, vi, vi->var.ctype);
        }

        case EXPR_RETURN:
        case EXPR_BREAK: {
            if (resolved) RETURN_RESOLVED(value, ctype, NULL, UNREACHABLE_ELEM);
            expr_break* b = (expr_break*)n;
            re = resolve_ast_node(r, b->value, st, NULL, &b->value_ctype);
            if (re) return re;
            ast_flags_set_resolved(&n->flags);
            ast_elem* tgt_type;
            if (n->kind == EXPR_BREAK) {
                ast_elem** tgtt = get_break_target_ctype(b->target);
                if (ast_flags_get_resolved(b->target->flags)) {
                    tgt_type = *tgtt;
                }
                else {
                    ast_flags_set_resolved(&b->target->flags);
                    *tgtt = b->value_ctype;
                    RETURN_RESOLVED(value, ctype, NULL, UNREACHABLE_ELEM);
                }
            }
            else {
                // if we return from a block, this block's result becomes
                // unreachable
                if (r->curr_expr_block_owner) {
                    ast_elem** tgtt =
                        get_break_target_ctype(r->curr_expr_block_owner);
                    if (ast_flags_get_resolved(
                            r->curr_expr_block_owner->flags)) {
                        if (*tgtt != UNREACHABLE_ELEM) {
                            assert(false); // TODO: error, unreachable not
                                           // unifiable
                        }
                    }
                    else {
                        ast_flags_set_resolved(
                            &r->curr_expr_block_owner->flags);
                        *tgtt = UNREACHABLE_ELEM;
                    }
                }
                if (b->target->kind == SC_FUNC) {
                    // must already be resolved since parenting function
                    tgt_type = ((sc_func*)b->target)->return_ctype;
                }
                else {
                    assert(b->target->kind == SC_FUNC_GENERIC);
                    tgt_type = ((sc_func_generic*)b->target)->return_ctype;
                }
            }
            if (!ctypes_unifiable(b->value_ctype, tgt_type)) {
                ureg vstart, vend;
                ast_node_get_bounds(b->value, &vstart, &vend);
                error_log_report_annotated_twice(
                    &r->tc->err_log, ES_RESOLVER, false, "type missmatch",
                    ast_node_get_file((ast_node*)b, st), vstart, vend,
                    "the type returned from here doesn't match the target "
                    "scope's",
                    // TODO: st is kinda wrong here
                    ast_node_get_file((ast_node*)b->target, st),
                    src_range_get_start(b->target->srange),
                    src_range_get_end(b->target->srange), "target scope here");
                return RE_TYPE_MISSMATCH;
            }
            RETURN_RESOLVED(value, ctype, NULL, UNREACHABLE_ELEM);
        }
        case EXPR_BLOCK: {
            expr_block* b = (expr_block*)n;
            if (!resolved) {
                re = resolve_expr_body(
                    r, (ast_node*)b, &b->body, st, value, &b->ctype);
                if (re) return re;
            }
            RETURN_RESOLVED(value, ctype, value, b->ctype);
        }

        case EXPR_IF: {
            expr_if* ei = (expr_if*)n;
            if (resolved) RETURN_RESOLVED(value, ctype, n, ei->ctype);
            ast_elem *ctype_if, *ctype_else;
            resolve_error re =
                resolve_ast_node(r, ei->condition, st, NULL, NULL);
            if (re) return re;
            ast_node* old_expr_block_owner = r->curr_expr_block_owner;
            r->curr_expr_block_owner = NULL;
            re = resolve_ast_node(r, ei->if_body, st, NULL, &ctype_if);
            if (!re) {
                re = resolve_ast_node(r, ei->else_body, st, NULL, &ctype_else);
            }
            r->curr_expr_block_owner = old_expr_block_owner;
            if (re) return re;
            if (ctype_if == UNREACHABLE_ELEM) {
                ei->ctype = ctype_else;
            }
            else if (ctype_else == UNREACHABLE_ELEM) {
                ei->ctype = ctype_if;
            }
            else if (!ctypes_unifiable(ctype_if, ctype_else)) {
                error_log_report_annotated(
                    &r->tc->err_log, ES_RESOLVER, false, "type missmatch",
                    ast_node_get_file(n, st), src_range_get_start(n->srange),
                    src_range_get_end(n->srange),
                    "if body and else body evaluate to differently typed "
                    "values");
                return RE_TYPE_MISSMATCH;
            }
            else {
                ei->ctype = ctype_if;
            }
            RETURN_RESOLVED(value, ctype, ei, ei->ctype);
        }

        case EXPR_LOOP: {
            expr_loop* l = (expr_loop*)n;
            if (resolved) RETURN_RESOLVED(value, ctype, n, l->ctype);
            return resolve_expr_body(r, n, &l->body, st, value, ctype);
        }
        case EXPR_MACRO: {
            // TODO ctype
            if (ctype) *ctype = NULL;
            if (resolved) return RE_OK;
            expr_macro* em = (expr_macro*)n;
            re = add_simple_body_decls(r, st, &em->body, false);
            if (re) return re;
            return resolve_ast_node(r, (ast_node*)em->next, st, NULL, NULL);
        }

        case EXPR_PP: {
            // TODO
            if (ctype) *ctype = NULL;
            return RE_OK;
        }
        case EXPR_MATCH: {
            // TODO ctype
            if (ctype) *ctype = NULL;
            if (resolved) return RE_OK;
            expr_match* em = (expr_match*)n;
            re = resolve_ast_node(r, em->match_expr, st, NULL, NULL);
            if (re) return re;
            for (match_arm** ma = (match_arm**)em->body.elements; *ma != NULL;
                 ma++) {
                re = resolve_ast_node(r, (**ma).condition, st, NULL, NULL);
                if (re) return re;
                re = resolve_ast_node(r, (**ma).value, st, NULL, NULL);
                if (re) return re;
            }
            return RE_OK;
        }
        case SYM_PARAM: {
            assert(!value);
            sym_param* p = (sym_param*)n;
            if (resolved) RETURN_RESOLVED(value, ctype, NULL, p->ctype);
            return resolve_param(r, p, st, ctype);
        }
        default: assert(false); return RE_UNKNOWN_SYMBOL;
    }
}
static inline void report_type_loop(resolver* r)
{
    ast_node* n = (ast_node*)stack_pop(&r->error_stack);
    symbol_table* st = (symbol_table*)stack_pop(&r->error_stack);
    assert(n && st);
    ureg stack_s = stack_size(&r->error_stack);
    if (stack_peek_nth(&r->error_stack, stack_s - 2) != n) {
        r->retracing_type_loop = true;
        stack_clear(&r->error_stack);
        resolve_ast_node(r, n, st, NULL, NULL);
    }
    src_range_large srl;
    src_range_unpack(n->srange, &srl);
    if (!srl.file) srl.file = ast_node_get_file(n, st);
    ureg annot_count = stack_size(&r->error_stack) / 2;
    annot_count--; // the starting type is on the stack twice
    error* e = error_log_create_error(
        &r->tc->err_log, ES_RESOLVER, false, "type inference cycle", srl.file,
        srl.start, srl.end, "type definition depends on itself", annot_count);
    for (ureg i = 0; i < annot_count; i++) {
        n = (ast_node*)stack_pop(&r->error_stack);
        if (!n) break;
        st = (symbol_table*)stack_pop(&r->error_stack);
        src_range_unpack(n->srange, &srl);
        if (!srl.file) srl.file = ast_node_get_file(n, st);
        error_add_annotation(e, srl.file, srl.start, srl.end, "");
    }
    stack_pop(&r->error_stack);
    stack_pop(&r->error_stack);
    error_log_report(&r->tc->err_log, e);
}
static resolve_error resolve_ast_node(
    resolver* r, ast_node* n, symbol_table* st, ast_elem** value,
    ast_elem** ctype)
{
    resolve_error re = resolve_ast_node_raw(r, n, st, value, ctype);
    if (re == RE_TYPE_LOOP) {
        if (!r->allow_type_loops) {
            stack_push(&r->error_stack, st);
            stack_push(&r->error_stack, n);
        }
        if (r->type_loop_start == n) {
            if (!ast_flags_get_resolving(n->flags)) {
                ast_flags_clear_resolving(&n->flags);
                report_type_loop(r);
                return RE_ERROR;
            }
        }
        ast_flags_clear_resolving(&n->flags);
    }
    return re;
}
resolve_error resolve_expr_body(
    resolver* r, ast_node* expr, ast_body* b, symbol_table* parent_st,
    ast_elem** value, ast_elem** ctype)
{
    symbol* curr_sym = r->curr_symbol_decl;
    r->curr_symbol_decl = NULL;
    ast_node* parent_expr_block_owner = r->curr_expr_block_owner;
    r->curr_expr_block_owner = expr;
    bool parent_allows_type_loops = r->allow_type_loops;
    if (!r->retracing_type_loop) r->allow_type_loops = true;
    resolve_error re;
    bool second_pass = false;
    bool parenting_type_loop = false;
    ast_elem* stmt_ctype = NULL;
    ast_elem** stmt_ctype_ptr = &stmt_ctype;
    for (ast_node** n = b->elements; *n != NULL; n++) {
        re = resolve_ast_node(r, *n, b->symtab, NULL, stmt_ctype_ptr);
        if (stmt_ctype_ptr && stmt_ctype == UNREACHABLE_ELEM) {
            stmt_ctype_ptr = NULL;
        }
        if (!re) continue;
        if (re == RE_TYPE_LOOP) {
            if (r->type_loop_start == (ast_node*)curr_sym) {
                ast_flags_set_resolving(&curr_sym->node.flags);
                if (r->retracing_type_loop) {
                    stack_clear(&r->error_stack);
                }
                second_pass = true;
            }
            else if (!r->retracing_type_loop) {
                parenting_type_loop = true;
            }
            else {
                break;
            }
            re = RE_OK;
            continue;
        }
        break;
    }
    r->allow_type_loops = parent_allows_type_loops;
    r->curr_expr_block_owner = parent_expr_block_owner;
    r->curr_symbol_decl = curr_sym;
    if (re) return re;
    if (parenting_type_loop) {
        return RE_TYPE_LOOP;
    }
    if (second_pass) {
        for (ast_node** n = b->elements; *n != NULL; n++) {
            re = resolve_ast_node(r, *n, b->symtab, NULL, stmt_ctype_ptr);
            // TODO: if we have a type loop error,
            // make it originate from a(?) break statement
            if (re) return re;
            if (stmt_ctype_ptr && stmt_ctype == UNREACHABLE_ELEM) {
                stmt_ctype_ptr = NULL;
            }
        }
    }
    // means no break occured
    if (!ast_flags_get_resolved(expr->flags)) {
        if (stmt_ctype_ptr) { // the end is reachable -> void
            ast_flags_set_resolved(&expr->flags);
            RETURN_RESOLVED(value, ctype, VOID_ELEM, VOID_ELEM);
        }
        else { // -> unreachable
            ast_flags_set_resolved(&expr->flags);
            RETURN_RESOLVED(value, ctype, NULL, UNREACHABLE_ELEM);
        }
    }
    return RE_OK;
}
// TODO: make sure we return!
resolve_error resolve_func(resolver* r, sc_func* fn, symbol_table* parent_st)
{
    ast_body* b = &fn->scp.body;
    symbol_table* st = b->symtab;
    resolve_error re;
    for (ureg i = 0; i < fn->param_count; i++) {
        re = resolve_param(r, &fn->params[i], st, NULL);
        if (re) return re;
    }
    re = resolve_ast_node(r, fn->return_type, st, &fn->return_ctype, NULL);
    if (re) return re;
    ast_node** n = b->elements;
    ast_elem* stmt_ctype;
    ast_elem** stmt_ctype_ptr = &stmt_ctype;
    while (*n) {
        re = add_ast_node_decls(r, st, NULL, *n, false);
        if (re) return re;
        re = resolve_ast_node(r, *n, st, NULL, stmt_ctype_ptr);
        if (re) return re;
        if (stmt_ctype_ptr && stmt_ctype == UNREACHABLE_ELEM) {
            stmt_ctype_ptr = NULL;
        }
        n++;
    }
    if (stmt_ctype_ptr && fn->return_ctype != VOID_ELEM) {
        ureg brace_end = src_range_get_end(fn->scp.body.srange);
        src_file* f = ast_node_get_file((ast_node*)fn, parent_st);
        error_log_report_annotated_thrice(
            &r->tc->err_log, ES_RESOLVER, false,
            "reachable end of non void function", f, brace_end - 1, brace_end,
            "missing return statement (or unreachable) ", f,
            src_range_get_start(fn->return_type->srange),
            src_range_get_end(fn->return_type->srange),
            "function returns non void type", f,
            src_range_get_start(fn->scp.sym.node.srange),
            src_range_get_end(fn->scp.sym.node.srange), NULL);
        return RE_TYPE_MISSMATCH;
    }
    ast_flags_set_resolved(&fn->scp.sym.node.flags);
    return RE_OK;
}
resolve_error resolve_body(resolver* r, ast_body* b)
{
    resolve_error re;
    for (ast_node** n = b->elements; *n != NULL; n++) {
        re = resolve_ast_node(r, *n, b->symtab, NULL, NULL);
        if (re) return re;
    }
    return RE_OK;
}
void adjust_node_ids(ureg sym_offset, ast_node* n)
{
    switch (n->kind) {
        case SC_FUNC: {
            if (ast_flags_get_access_mod(n->flags) < AM_PROTECTED) return;
            sc_func* fn = (sc_func*)n;
            fn->id += sym_offset;
            return;
        }
        case SYM_VAR:
        case SYM_VAR_INITIALIZED: {
            if (ast_flags_get_access_mod(n->flags) < AM_PROTECTED) return;
            ((sym_var*)n)->var_id += sym_offset;
            return;
        }
        case SC_STRUCT: {
            if (ast_flags_get_access_mod(n->flags) < AM_PROTECTED) return;
            ((sc_struct*)n)->id += sym_offset;
            // fallthrough
        }
        default: return;
    }
}
void adjust_body_ids(ureg sym_offset, ast_body* b)
{
    for (ast_node** i = b->elements; *i; i++) {
        adjust_node_ids(sym_offset, *i);
    }
}
void adjust_ids(ureg sym_offset, mdg_node** start, mdg_node** end)
{
    while (start != end) {
        aseglist_iterator it;
        aseglist_iterator_begin(&it, &(**start).open_scopes);
        for (open_scope* osc = aseglist_iterator_next(&it); osc;
             osc = aseglist_iterator_next(&it)) {
            adjust_body_ids(sym_offset, &osc->scp.body);
        }
        start++;
    }
}
resolve_error resolver_resolve(
    resolver* r, mdg_node** start, mdg_node** end, ureg* startid, ureg* endid,
    ureg* private_sym_count)
{
    r->public_sym_count = 0;
    r->private_sym_count = UREGH_MAX;
    r->start = start;
    r->end = end;
    resolve_error re;
    print_debug_info(r);
    bool contains_root = false;
    for (mdg_node** i = start; i != end; i++) {
        if (*i == TAUC.mdg.root_node) {
            if (tauc_request_finalize()) return RE_FATAL;
            contains_root = true;
        }
        int r = symbol_table_init(
            &(**i).symtab, atomic_ureg_load(&(**i).decl_count),
            atomic_ureg_load(&(**i).using_count), true, (ast_elem*)*i);
        if (r) return RE_FATAL;
        if (!(**i).symtab) return RE_FATAL;
        (**i).symtab->parent = GLOBAL_SYMTAB;
    }
    if (!contains_root) atomic_ureg_inc(&TAUC.linking_holdups);
    for (mdg_node** i = start; i != end; i++) {
        aseglist_iterator asi;
        aseglist_iterator_begin(&asi, &(**i).open_scopes);
        for (open_scope* osc = aseglist_iterator_next(&asi); osc != NULL;
             osc = aseglist_iterator_next(&asi)) {
            re = add_body_decls(
                r, (**i).symtab, (**i).symtab, &osc->scp.body, true);
            if (re) return re;
        }
    }
    for (mdg_node** i = start; i != end; i++) {
        r->curr_mdg = *i;
        aseglist_iterator asi;
        aseglist_iterator_begin(&asi, &(**i).open_scopes);
        for (open_scope* osc = aseglist_iterator_next(&asi); osc != NULL;
             osc = aseglist_iterator_next(&asi)) {
            r->curr_osc = osc;
            re = resolve_body(r, &osc->scp.body);
            if (re) return re;
        }
    }

    if (re) return re;
    *startid = atomic_ureg_add(&TAUC.node_ids, r->public_sym_count);
    *endid = *startid + r->public_sym_count;
    *private_sym_count = r->private_sym_count - UREGH_MAX;
    adjust_ids(*startid, start, end);
    return mark_mdg_nodes_resolved(r);
}
void resolver_fin(resolver* r)
{
    sbuffer_fin(&r->call_types);
    stack_fin(&r->error_stack);
}
int resolver_init(resolver* r, thread_context* tc)
{
    r->tc = tc;
    r->allow_type_loops = false;
    r->curr_symbol_decl = NULL;
    r->type_loop_start = NULL;
    r->curr_expr_block_owner = NULL;
    if (stack_init(&r->error_stack, &r->tc->tempmem)) return ERR;
    if (sbuffer_init(&r->call_types, sizeof(ast_node*) * 32)) {
        stack_fin(&r->error_stack);
        return ERR;
    }
    return OK;
}
ast_elem* get_resolved_ast_node_ctype(ast_node* n)
{
    ast_elem* ctype;
    resolve_ast_node_raw(NULL, n, NULL, NULL, &ctype);
    return ctype;
}
