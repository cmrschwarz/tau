#include "tauc.h"
#include "resolver.h"
#include "thread_context.h"
#include "utils/error.h"
#include "error_log.h"
#include "utils/panic.h"
#include "utils/zero.h"
#include "utils/debug_utils.h"

static resolve_error add_body_decls(
    resolver* r, symbol_table* parent_st, symbol_table* shared_st, ureg ppl,
    ast_body* b, bool public_st);

resolve_error
resolve_param(resolver* r, sym_param* p, ureg ppl, ast_elem** ctype);
resolve_error resolve_body(resolver* r, ast_body* b, ureg ppl);
static resolve_error resolve_ast_node(
    resolver* r, ast_node* n, symbol_table* st, ureg ppl, ast_elem** value,
    ast_elem** ctype);
resolve_error resolve_func(resolver* r, sc_func* fn, ureg ppl);
resolve_error resolve_expr_body(
    resolver* r, ast_node* expr, ast_body* b, ureg ppl, ast_elem** value,
    ast_elem** ctype);
resolve_error resolve_expr_scope_access(
    resolver* r, expr_scope_access* esa, symbol_table* st, ureg ppl,
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
        r->tc->err_log, ES_RESOLVER, false, "unknown symbol",
        ast_node_get_file(n, st), srl.start, srl.end,
        "use of an undefined symbol");
    return RE_UNKNOWN_SYMBOL;
}
static resolve_error report_redeclaration_error(
    resolver* r, symbol* redecl, symbol* prev, symbol_table* st)
{
    error_log_report_annotated_twice(
        r->tc->err_log, ES_RESOLVER, false, "symbol redeclaration",
        ast_node_get_file((ast_node*)redecl, st),
        src_range_get_start(redecl->node.srange),
        src_range_get_end(redecl->node.srange),
        "a symbol of this name is already defined in this "
        "scope",
        ast_node_get_file((ast_node*)prev, st),
        src_range_get_start(prev->node.srange),
        src_range_get_end(prev->node.srange), "previous definition here");
    return RE_SYMBOL_REDECLARATION;
}
static resolve_error
add_symbol(resolver* r, symbol_table* st, symbol_table* sst, symbol* sym)
{
    sym->declaring_st = st;
    symbol_table* tgtst =
        (sst && ast_flags_get_access_mod(sym->node.flags) != AM_DEFAULT) ? sst
                                                                         : st;
    symbol** conflict;
    conflict = symbol_table_insert(tgtst, sym);
    if (conflict) {
        return report_redeclaration_error(r, sym, *conflict, tgtst);
    }
    return RE_OK;
}
resolve_error add_sym_import_module_decl(
    resolver* r, symbol_table* st, sym_import_module* im, mdg_node* stop,
    mdg_node* start, sym_import_parent** tgt_parent)
{
    im->sym.declaring_st = st;
    symbol** tgt;
    symbol* next;
    symbol* children;
    sym_import_parent* p;
    if (start->parent != stop) {
        resolve_error re =
            add_sym_import_module_decl(r, st, im, stop, start->parent, &p);
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
                return report_redeclaration_error(r, (symbol*)im, *tgt, st);
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
                    return report_redeclaration_error(r, (symbol*)im, *tgt, st);
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
            pool_alloc(&r->tc->permmem, sizeof(sym_import_parent));
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
    resolver* r, mdg_node* curr_mdg_node, sym_import_group* ig,
    symbol_table* st)
{
    symbol* next;
    if (!ig->sym.name) {
        next = ig->children.symbols;
        ig->children.symtab = st;
    }
    else {
        st = ig->children.symtab;
        next = *(symbol**)(st + 1);
        *(symbol**)(st + 1) = NULL; // zero the symtab again
    }
    symbol* s;
    resolve_error re;
    while (next != NULL) {
        s = next;
        next = s->next;
        if (s->node.kind == SYM_IMPORT_GROUP) {
            sym_import_group* nig = (sym_import_group*)s;
            re = add_import_group_decls(r, curr_mdg_node, nig, st);
            if (re) return re;
            continue;
        }
        else if (s->node.kind == SYM_IMPORT_SYMBOL) {
            symbol** cf = symbol_table_insert(st, s);
            s->declaring_st = st;
            ((sym_import_symbol*)s)->target_st = ig->parent_mdgn->symtab;
            if (cf) {
                return report_redeclaration_error(r, s, *cf, st);
            }
        }
        else {
            assert(s->node.kind == SYM_IMPORT_MODULE);
            sym_import_module* im = (sym_import_module*)s;
            if (ast_flags_get_relative_import(s->node.flags)) {
                re = add_sym_import_module_decl(
                    r, st, im, curr_mdg_node, im->target, NULL);
            }
            else {
                re = add_sym_import_module_decl(
                    r, st, im, r->tc->t->mdg.root_node, im->target, NULL);
            }
            if (re) return re;
        }
    }
    return RE_OK;
}
static inline void
set_parent_symtabs(symbol_table** tgt, symbol_table* parent_st)
{

    if (*tgt == NULL) {
        *tgt = parent_st;
    }
    else if (parent_st->parent == NULL) {
        (*tgt)->parent = parent_st;
    }
    else {
        // go to ppl 0
        while (parent_st->parent->owning_node == parent_st->owning_node) {
            parent_st = parent_st->parent;
        }
        assert((*tgt)->parent == NULL);
        (*tgt)->parent = parent_st;
    }
}
static inline ureg ast_node_claim_id(resolver* r, ast_node* n, bool public_st)
{
    if (public_st && ast_flags_get_access_mod(n->flags) >= AM_PROTECTED) {
        r->public_sym_count++;
    }
    else {
        r->private_sym_count++;
    }
    return r->id_space++;
}
static resolve_error add_ast_node_decls(
    resolver* r, symbol_table* st, symbol_table* sst, ureg ppl, ast_node* n,
    bool public_st)
{
    if (n == NULL) return RE_OK;
    resolve_error re;
    switch (n->kind) {
        case STMT_USING: {
            pp_resolve_node* pprn =
                sbuffer_append(&r->pp_resolve_nodes, sizeof(pp_resolve_node));
            if (!pprn) return RE_FATAL;
            pprn->declaring_st = st;
            pprn->node = n;
            pprn->ppl = ppl;
            return add_ast_node_decls(
                r, st, sst, ppl, ((stmt_using*)n)->target, public_st);
        }
        case EXPR_PP: {
            bool cp = r->contains_paste;
            r->contains_paste = false;
            re = add_ast_node_decls(
                r, st, sst, ppl, ((expr_pp*)n)->pp_expr, false);
            if (re) return re;
            if (r->contains_paste) {
                ast_flags_set_pasting_pp_expr(&n->flags);
                ppl = 0;
            }
            if (ppl == 0) {
                pp_resolve_node* pprn = sbuffer_append(
                    &r->pp_resolve_nodes, sizeof(pp_resolve_node));
                if (!pprn) return RE_FATAL;
                pprn->declaring_st = st;
                pprn->node = n;
                pprn->ppl = ppl;
            }
            r->contains_paste = cp;
        }
        case EXPR_LITERAL:
        case EXPR_IDENTIFIER: return RE_OK;
        case OSC_MODULE:
        case OSC_EXTEND: {
            // these guys are handled from their mdg node, not from
            // where they appear in source
            return RE_OK;
        }
        case SC_STRUCT:
        case SC_TRAIT: {
            sc_struct* str = (sc_struct*)n;
            re = add_symbol(r, st, sst, (symbol*)n);
            bool members_public_st =
                public_st && ast_flags_get_access_mod(n->flags) >= AM_PROTECTED;
            str->id = 0; // so members can inc this and we get a member id
            re = add_body_decls(
                r, st, NULL, ppl, &str->sc.body, members_public_st);
            if (re) return re;
            str->id = ast_node_claim_id(r, n, public_st);
            return RE_OK;
        }
        case SC_MACRO:
        case SC_FUNC: {
            sc_macro* m = (n->kind == SC_MACRO) ? (sc_macro*)n : NULL;
            sc_func* fn = (n->kind == SC_FUNC) ? (sc_func*)n : NULL;
            if (fn) {
                fn->id = ast_node_claim_id(r, n, public_st);
            }
            symbol_table* tgtst =
                (sst && ast_flags_get_access_mod(n->flags) != AM_DEFAULT) ? sst
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
                    sfo->overloads = (scope*)*conflict;
                    (**conflict).next = sym;
                    sym->next = NULL;
                    *conflict = (symbol*)sfo;
                }
                else if ((**conflict).node.kind == SYM_FUNC_OVERLOADED) {
                    sfo = (sym_func_overloaded*)conflict;
                    sym->next = (symbol*)sfo->overloads;
                    sfo->overloads = (scope*)n;
                }
                else {
                    return report_redeclaration_error(r, sym, *conflict, tgtst);
                }
            }
            // we only do the parameters here because the declaration and use
            // func body vars is strongly ordered
            ast_body* b = &((scope*)n)->body;
            ureg param_count = fn ? fn->param_count : m->param_count;
            sym_param* params = fn ? fn->params : m->params;
            if (b->symtab) {
                for (ureg i = 0; i < param_count; i++) {
                    re = add_symbol(r, b->symtab, NULL, (symbol*)&params[i]);
                    if (re) return re;
                }
            }
            set_parent_symtabs(&b->symtab, st);
            return RE_OK;
        }
        case SYM_IMPORT_GROUP: {
            sym_import_group* ig = (sym_import_group*)n;
            ig->sym.declaring_st = st;
            if (ig->sym.name) {
                re = add_symbol(r, st, sst, (symbol*)ig);
                if (re) return re;
            }
            symbol_table* pst = st;
            while (pst->owning_node->kind != ELEM_MDG_NODE) {
                pst = pst->parent;
                assert(pst);
            }
            return add_import_group_decls(
                r, (mdg_node*)pst->owning_node, ig, st);
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
                stop = r->tc->t->mdg.root_node;
            }
            return add_sym_import_module_decl(
                r, st, im, stop, im->target, NULL);
        }
        case STMT_COMPOUND_ASSIGN:
            // TODO
            assert(false);
            return RE_OK;

        case SYM_VAR_INITIALIZED: {
            re = add_ast_node_decls(
                r, st, sst, ppl, ((sym_var_initialized*)n)->initial_value,
                public_st);
            if (re) return re;
            // fallthrough
        }
        case SYM_VAR: {
            re = add_symbol(r, st, sst, (symbol*)n);
            if (re) return re;
            sym_var* v = (sym_var*)n;
            ast_node_kind owning_kind = v->sym.declaring_st->owning_node->kind;
            if (!ast_flags_get_static(n->flags) &&
                (owning_kind == SC_STRUCT || owning_kind == SC_TRAIT)) {
                assert(owning_kind == SC_STRUCT); // TODO: generics, traits, etc
                v->var_id =
                    ((sc_struct*)v->sym.declaring_st->owning_node)->id++;
            }
            else {
                v->var_id = ast_node_claim_id(r, n, public_st);
            }
            return RE_OK;
        }
        case SYM_NAMED_USING: {
            re = add_symbol(r, st, sst, (symbol*)n);
            if (re) return re;
            return add_ast_node_decls(
                r, st, sst, ppl, ((sym_named_using*)n)->target, public_st);
        }

        case EXPR_RETURN:
        case EXPR_BREAK:
            return add_ast_node_decls(
                r, st, sst, ppl, ((expr_break*)n)->value, public_st);

        case EXPR_BLOCK:
            return add_body_decls(
                r, st, NULL, ppl, &((expr_block*)n)->body, false);

        case EXPR_IF: {
            expr_if* ei = (expr_if*)n;
            re = add_ast_node_decls(r, st, sst, ppl, ei->condition, false);
            if (re) return re;
            re = add_ast_node_decls(r, st, sst, ppl, ei->if_body, false);
            if (re) return re;
            return add_ast_node_decls(r, st, sst, ppl, ei->else_body, false);
        }

        case EXPR_LOOP:
            return add_body_decls(
                r, st, NULL, ppl, &((expr_loop*)n)->body, false);

        case EXPR_MACRO_CALL: {
            expr_macro_call* emc = (expr_macro_call*)n;
            re = add_body_decls(r, st, NULL, ppl, &emc->body, false);
            return re;
        }
        case EXPR_MATCH: {
            expr_match* em = (expr_match*)n;
            re = add_ast_node_decls(r, st, sst, ppl, em->match_expr, false);
            if (re) return re;
            for (match_arm** ma = (match_arm**)em->body.elements; *ma != NULL;
                 ma++) {
                re = add_ast_node_decls(
                    r, st, sst, ppl, (**ma).condition, false);
                if (re) return re;
                re = add_ast_node_decls(r, st, sst, ppl, (**ma).value, false);
                if (re) return re;
            }
            return RE_OK;
        }
        case EXPR_OP_BINARY: {
            re = add_ast_node_decls(
                r, st, sst, ppl, ((expr_op_binary*)n)->lhs, false);
            if (re) return re;
            return add_ast_node_decls(
                r, st, sst, ppl, ((expr_op_binary*)n)->rhs, false);
        }
        case EXPR_ACCESS: {
            expr_access* a = (expr_access*)n;
            re = add_ast_node_decls(r, st, sst, ppl, a->lhs, false);
            if (re) return re;
            for (ureg i = 0; i < a->arg_count; i++) {
                re = add_ast_node_decls(r, st, sst, ppl, a->args[i], false);
                if (re) return re;
            }
            // we could make this tail recursive by moving lhs down here
            // but this way we traverse as declared ->better cache usage
            return RE_OK;
        }
        case EXPR_CALL: {
            expr_call* c = (expr_call*)n;
            re = add_ast_node_decls(r, st, sst, ppl, c->lhs, false);
            if (re) return re;
            for (ureg i = 0; i < c->arg_count; i++) {
                re = add_ast_node_decls(r, st, sst, ppl, c->args[i], false);
                if (re) return re;
            }
            return RE_OK;
        }
        case EXPR_SCOPE_ACCESS:
        case EXPR_MEMBER_ACCESS: {
            return add_ast_node_decls(
                r, st, sst, ppl, ((expr_scope_access*)n)->lhs, false);
        }
        case EXPR_PARENTHESES: {
            return add_ast_node_decls(
                r, st, sst, ppl, ((expr_parentheses*)n)->child, false);
        }
        case EXPR_OP_UNARY: {
            return add_ast_node_decls(
                r, st, sst, ppl, ((expr_op_unary*)n)->child, false);
        }
        default:
            assert(false); // unknown node_kind
            return RE_OK;
    }
    assert(false);
}

static resolve_error add_body_decls(
    resolver* r, symbol_table* parent_st, symbol_table* shared_st, ureg ppl,
    ast_body* b, bool public_st)
{
    set_parent_symtabs(&b->symtab, parent_st);
    for (ast_node** n = b->elements; *n; n++) {
        resolve_error re =
            add_ast_node_decls(r, b->symtab, shared_st, ppl, *n, public_st);
        if (re) return re;
    }
    return RE_OK;
}
static inline void print_debug_info(resolver* r)
{
    tprintf("resolving {");
    mdg_node** i = r->mdgs_begin;
    for (; i + 1 != r->mdgs_end; i++) {
        tprintf("%s, ", (**i).name);
    }
    tprintf("%s} ", (**i).name);
}

bool ctypes_unifiable(ast_elem* a, ast_elem* b)
{
    // immediately return true e.g. for primitives
    if (a == b) return true;
    if (a->kind == TYPE_POINTER && b->kind == TYPE_POINTER) {
        return ctypes_unifiable(
            ((type_pointer*)a)->base, ((type_pointer*)b)->base);
    }
    return false; // TODO
    /*
    switch (a->kind) {
        case TYPE_MODIFIERS:
    }
     */
}
resolve_error operator_func_applicable(
    resolver* r, sc_func* f, ureg op_ppl, ast_elem* lhs, ast_elem* rhs,
    bool* applicable, ast_elem** ctype)
{
    // ensure func has exactly 2 parameters
    // [varargs not allowed for operators]
    if (f->param_count != 2) {
        *applicable = false;
        return RE_OK;
    }
    ast_elem* param_ctype;
    resolve_error re = resolve_param(r, &f->params[0], op_ppl, &param_ctype);
    if (re) return re;
    if (!ctypes_unifiable(lhs, param_ctype)) {
        *applicable = false;
        return RE_OK;
    }
    re = resolve_param(r, &f->params[0], op_ppl, &param_ctype);
    if (re) return re;
    if (!ctypes_unifiable(rhs, param_ctype)) {
        *applicable = false;
        return RE_OK;
    }
    if (ctype) *ctype = f->return_ctype;
    *applicable = true;
    return RE_OK;
}
resolve_error overload_applicable(
    resolver* r, ast_elem** call_arg_types, ureg arg_count, scope* overload,
    ureg ppl, bool* applicable, ast_elem** ctype)
{
    sc_macro* m =
        (overload->sym.node.kind == SC_MACRO) ? (sc_macro*)overload : NULL;
    sc_func* fn = !m ? (sc_func*)overload : NULL;
    ureg param_count = m ? m->param_count : fn->param_count;
    sym_param* params = m ? m->params : fn->params;
    // works cause varags are not in the lang yet
    if (param_count != arg_count) return false;
    for (ureg i = 0; i < arg_count; i++) {
        ast_elem* ctype;
        resolve_error re = resolve_param(r, &params[i], ppl, &ctype);
        if (re) return re;
        if (!ctypes_unifiable(ctype, call_arg_types[i])) {
            *applicable = false;
            return RE_OK;
        }
    }
    *applicable = true;
    if (fn) {
        return resolve_ast_node(
            r, fn->return_type, overload->sym.declaring_st, ppl,
            &fn->return_ctype, NULL);
        if (ctype) *ctype = fn->return_ctype;
    }
    else {
        // TODO: allow non void macros
        *ctype = VOID_ELEM;
        return RE_OK;
    }
}
static inline resolve_error resolve_macro_call(
    resolver* r, expr_macro_call* emc, symbol_table* st, ast_elem** value,
    ast_elem** ctype)
{
    assert(false); // TODO
}
static inline resolve_error resolve_no_block_macro_call(
    resolver* r, expr_call* nbmc, symbol_table* st, sc_macro* m,
    ast_elem** ctype)
{
    assert(false); // TODO
}
// we need a seperate func_st for cases like foo::bar()
resolve_error resolve_func_call(
    resolver* r, expr_call* c, symbol_table* st, ureg ppl, char* func_name,
    symbol_table* func_st, ast_elem** ctype)
{
    ast_elem** call_arg_types =
        sbuffer_append(&r->call_types, c->arg_count * sizeof(ast_elem*));
    resolve_error re = RE_OK;
    for (ureg i = 0; i < c->arg_count; i++) {
        re = resolve_ast_node(r, c->args[i], st, ppl, NULL, &call_arg_types[i]);
        if (re) return re;
    }
    symbol_table* lt = func_st;
    scope* tgt;
    while (lt) {
        ureg fn_ppl;
        symbol** s =
            symbol_table_lookup(lt, ppl, AM_DEFAULT, func_name, &fn_ppl);
        if (!s) {
            // we use args_st here because thats the scope that the call is in
            re = report_unknown_symbol(r, c->lhs, st);
            break;
        }
        bool applicable;

        if ((**s).node.kind == SYM_FUNC_OVERLOADED) {
            sym_func_overloaded* sfo = (sym_func_overloaded*)*s;
            scope* o = sfo->overloads;
            while (o) {
                re = overload_applicable(
                    r, call_arg_types, c->arg_count, o, ppl, &applicable,
                    ctype);
                if (applicable) tgt = o;
                if (re || applicable) break;
                o = (scope*)o->sym.next;
            }
            if (re || applicable) break;
        }
        else if ((**s).node.kind == SC_FUNC) {
            re = overload_applicable(
                r, call_arg_types, c->arg_count, (scope*)(*s), fn_ppl,
                &applicable, ctype);
            if (applicable) tgt = (scope*)(*s);
            if (re || applicable) break;
        }
        else {
            assert(false);
        }
        lt = lt->parent;
    }
    sbuffer_remove_back(&r->call_types, c->arg_count * sizeof(ast_elem*));
    if (!re) {
        if (tgt->sym.node.kind == SC_MACRO) {
            c->node.kind = EXPR_NO_BLOCK_MACRO_CALL;
            return resolve_no_block_macro_call(r, c, st, (sc_macro*)tgt, ctype);
        }
        else {
            c->target.fn = (sc_func*)tgt;
            ast_flags_set_resolved(&c->node.flags);
        }
    }
    return re;
}
resolve_error resolve_call(
    resolver* r, expr_call* c, symbol_table* st, ureg ppl, ast_elem** ctype)
{

    if (c->lhs->kind == EXPR_IDENTIFIER) {
        return resolve_func_call(
            r, c, st, ppl, ((expr_identifier*)c->lhs)->value.str, st, ctype);
    }
    if (c->lhs->kind == EXPR_SCOPE_ACCESS) {
        expr_scope_access* esa = (expr_scope_access*)c->lhs;
        ast_elem* esa_lhs;
        symbol_table* lhs_st;
        // TODO: fix access modifier restrictions
        access_modifier am = AM_DEFAULT;
        resolve_error re =
            resolve_ast_node(r, esa->lhs, st, ppl, &esa_lhs, NULL);
        if (re) return re;
        assert(ast_elem_is_symbol(esa_lhs));
        re = get_resolved_symbol_symtab(r, (symbol*)esa_lhs, &am, &lhs_st);
        if (re) return re;
        return resolve_func_call(
            r, c, st, ppl, esa->target.name, lhs_st, ctype);
    }
    assert(false); // TODO
    return RE_OK;
}
resolve_error create_pointer_to(
    resolver* r, ast_elem* child_type, bool rvalue, ast_elem** tgt)
{
    type_pointer* tp =
        (type_pointer*)pool_alloc(&r->tc->permmem, sizeof(type_pointer));
    if (!tp) return RE_FATAL;
    tp->base = child_type;
    tp->kind = TYPE_POINTER;
    tp->rvalue = rvalue;
    *tgt = (ast_elem*)tp;
    return RE_OK;
}
resolve_error choose_unary_operator_overload(
    resolver* r, expr_op_unary* ou, symbol_table* st, ureg ppl,
    ast_elem** value, ast_elem** ctype)
{
    ast_elem *child_type, *child_value;
    resolve_error re =
        resolve_ast_node(r, ou->child, st, ppl, &child_value, &child_type);
    if (re) return re;

    if (child_type == TYPE_ELEM) {
        if (ou->node.op_kind == OP_DEREF) {
            re = create_pointer_to(r, child_value, false, &child_type);
            if (re) return re;
            ou->op = child_type;
            RETURN_RESOLVED(value, ctype, child_type, TYPE_ELEM);
        }
        else {
            assert(false);
            // TODO: error
            return RE_FATAL;
        }
    }
    else {
        if (ou->node.op_kind == OP_ADDRESS_OF) {
            if (child_type->kind == TYPE_POINTER) {
                // TODO: error
                assert(((type_pointer*)child_type)->rvalue == false);
            }
            re = create_pointer_to(r, child_type, true, &child_type);
            if (re) return re;
            ou->op = child_type;
            RETURN_RESOLVED(value, ctype, ou, child_type);
        }
        else if (ou->node.op_kind == OP_DEREF) {
            assert(child_type->kind == TYPE_POINTER); // TODO: error
            type_pointer* tp = (type_pointer*)child_type;
            ou->op = tp->base;
            RETURN_RESOLVED(value, ctype, ou, tp->base);
            return RE_FATAL;
        }
        else if (child_type->kind != PRIMITIVE) {
            assert(false); // TODO
            return RE_FATAL;
        }
        else {
            ou->op = child_type;
            RETURN_RESOLVED(value, ctype, ou, child_type);
        }
    }
}
bool is_lvalue(ast_node* expr)
{
    if (expr->kind == EXPR_PARENTHESES) {
        return is_lvalue(((expr_parentheses*)expr)->child);
    }
    if (expr->kind == EXPR_IDENTIFIER) return true;
    if (expr->kind == EXPR_OP_UNARY) {
        expr_op_unary* ou = (expr_op_unary*)expr;
        return (ou->node.op_kind == OP_DEREF);
    }
    if (expr->kind == EXPR_MEMBER_ACCESS) return true;
    return false;
}
resolve_error choose_binary_operator_overload(
    resolver* r, expr_op_binary* ob, symbol_table* st, ureg ppl,
    ast_elem** value, ast_elem** ctype)
{
    ast_elem *lhs_ctype, *rhs_ctype;
    resolve_error re = resolve_ast_node(r, ob->lhs, st, ppl, NULL, &lhs_ctype);
    if (re) return re;
    re = resolve_ast_node(r, ob->rhs, st, ppl, NULL, &rhs_ctype);
    if (re) return re;
    if (lhs_ctype->kind == PRIMITIVE && rhs_ctype->kind == PRIMITIVE) {
        if (ob->node.op_kind == OP_ASSIGN) {
            assert(is_lvalue(ob->lhs)); // TODO: error
        }
        // TODO: proper inbuild operator resolution
        // maybe create a cat_prim_kind and switch over cat'ed lhs and rhs
        ob->op = lhs_ctype;
        if (ctype) *ctype = lhs_ctype;
        return RE_OK;
    }
    symbol_table* lt = st;
    while (lt) {
        bool applicable;
        ureg op_ppl;
        symbol** s = symbol_table_lookup(
            lt, ppl, AM_DEFAULT, op_to_str(ob->node.op_kind), &op_ppl);
        if (!s) return report_unknown_symbol(r, (ast_node*)ob, lt);
        if ((**s).node.kind == SYM_FUNC_OVERLOADED) {
            sym_func_overloaded* sfo = (sym_func_overloaded*)s;
            assert(sfo->overloads->sym.node.kind == SC_FUNC); // prevent macros
            sc_func* f = (sc_func*)sfo->overloads;
            while (f) {
                re = operator_func_applicable(
                    r, f, op_ppl, lhs_ctype, rhs_ctype, &applicable, ctype);
                if (re) return re;
                if (applicable) return RE_OK;
                f = (sc_func*)f->sc.sym.next;
            }
        }
        else if ((**s).node.kind == SC_FUNC) {
            re = operator_func_applicable(
                r, (sc_func*)*s, op_ppl, lhs_ctype, rhs_ctype, &applicable,
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
        default: assert(false); return NULL;
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
    assert(s); // if there's no child we would not have created a parent
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
    pst->parent = NULL;
    symbol* next = ip->children.symbols;
    do {
        s = next;
        next = next->next;
        if (*s->name != '\0') {
            symbol** res = symbol_table_insert(pst, s);
            // we checked for collisions during insert into the linked list
            // the check is to prevent unused var warnings in release builds
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
resolve_param(resolver* r, sym_param* p, ureg ppl, ast_elem** ctype)
{
    ast_flags_set_resolving(&p->sym.node.flags);
    resolve_error re;
    if (p->type) {
        re = resolve_ast_node(
            r, p->type, p->sym.declaring_st, ppl, &p->ctype, NULL);
        if (re) return re;
        if (p->default_value) {
            ast_elem* val;
            re = resolve_ast_node(
                r, (ast_node*)p->default_value, p->sym.declaring_st, ppl, NULL,
                &val);
            if (re) return re;
            if (!ctypes_unifiable(p->ctype, val)) {
                assert(false); // TODO: error
            }
        }
    }
    else {
        re = resolve_ast_node(
            r, p->default_value, p->sym.declaring_st, ppl, NULL, &p->ctype);
        if (re) return re;
    }
    if (ctype) *ctype = p->ctype;
    ast_flags_set_resolved(&p->sym.node.flags);
    return PE_OK;
}
resolve_error resolve_expr_scope_access(
    resolver* r, expr_scope_access* esa, symbol_table* st, ureg ppl,
    access_modifier* access, ast_elem** value, ast_elem** ctype)
{
    resolve_error re;
    symbol_table* lhs_st;
    ast_elem* lhs_val;
    re = resolve_ast_node(r, esa->lhs, st, ppl, &lhs_val, NULL);
    if (re) return re;
    assert(lhs_val != NULL && ast_elem_is_symbol(lhs_val)); // TODO: log error
    re = get_resolved_symbol_symtab(r, (symbol*)lhs_val, access, &lhs_st);
    if (re) return re;
    ureg rhs_ppl;
    symbol** s =
        symbol_table_lookup(lhs_st, ppl, *access, esa->target.name, &rhs_ppl);
    if (!*s) {
        return report_unknown_symbol(r, (ast_node*)esa, st);
    }
    ast_elem* rhs_val;

    resolve_ast_node(
        r, (ast_node*)*s, (**s).declaring_st, rhs_ppl, &rhs_val, ctype);
    assert(rhs_val != NULL && ast_elem_is_symbol(rhs_val)); // TODO: log error
    esa->target.sym = (symbol*)lhs_val;
    if (value) *value = rhs_val;
    ast_flags_set_resolved(&esa->node.flags);
    return RE_OK;
}
access_modifier check_member_access(symbol_table* st, scope* tgt)
{
    return AM_DEFAULT; // TODO
}
resolve_error resolve_expr_member_accesss(
    resolver* r, expr_member_access* ema, symbol_table* st, ureg ppl,
    access_modifier* access, ast_elem** value, ast_elem** ctype)
{
    ast_elem* lhs_type;
    resolve_error re = resolve_ast_node(r, ema->lhs, st, ppl, NULL, &lhs_type);
    if (re) return re;
    if (lhs_type->kind != SC_STRUCT) { // TODO: pointers
        // TODO: errror
        assert(false);
        return RE_FATAL;
    }
    scope* sc = (scope*)lhs_type;
    // TODO: try to make this check more efficient
    access_modifier acc = check_member_access(st, sc);
    ureg rhs_ppl;
    symbol** rhs = symbol_table_lookup_limited(
        sc->body.symtab, ppl, acc, sc->body.symtab->parent, ema->target.name,
        &rhs_ppl);
    if (!*rhs) return report_unknown_symbol(r, (ast_node*)ema, sc->body.symtab);
    ema->target.sym = *rhs;
    resolve_ast_node(
        r, (ast_node*)*rhs, (**rhs).declaring_st, rhs_ppl, value, ctype);
    ast_flags_set_resolved(&ema->node.flags);
    return RE_OK;
}
// the symbol table is not the one that contains the symbol, but the one
// where it was declared and where the type name loopup should start
static inline resolve_error resolve_ast_node_raw(
    resolver* r, ast_node* n, symbol_table* st, ureg ppl, ast_elem** value,
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
        case SC_MACRO: {
            if (!resolved) ast_flags_set_resolved(&n->flags);
            RETURN_RESOLVED(value, ctype, n, TYPE_ELEM);
        }
        case PRIMITIVE: {
            RETURN_RESOLVED(value, ctype, &PRIMITIVES[n->pt_kind], TYPE_ELEM);
        }
        case EXPR_LITERAL: {
            RETURN_RESOLVED(value, ctype, n, &PRIMITIVES[n->pt_kind]);
        }
        case EXPR_IDENTIFIER: {
            expr_identifier* e = (expr_identifier*)n;
            if (resolved) {
                RETURN_RESOLVED(
                    value, ctype, e->value.sym,
                    get_resolved_symbol_ctype(e->value.sym));
            }
            ureg decl_ppl;
            symbol** s = symbol_table_lookup(
                st, ppl, AM_DEFAULT, e->value.str, &decl_ppl);
            if (!s) return report_unknown_symbol(r, n, st);
            re = resolve_ast_node(
                r, (ast_node*)*s, (**s).declaring_st, decl_ppl, value, ctype);
            if (re) return re;
            e->value.sym = *s;
            ast_flags_set_resolved(&n->flags);
            return RE_OK;
        }
        case EXPR_CALL: {
            expr_call* c = (expr_call*)n;
            if (resolved) {
                RETURN_RESOLVED(value, ctype, c, c->target.fn->return_ctype);
            }
            return resolve_call(r, c, st, ppl, ctype);
        }
        case EXPR_NO_BLOCK_MACRO_CALL: {
            expr_call* c = (expr_call*)n;
            assert(resolved); // otherwise this would still be a EXPR_CALL
            assert(!value);
            RETURN_RESOLVED(value, ctype, NULL, c->target.macro_block->ctype);
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
            re = choose_unary_operator_overload(r, ou, st, ppl, value, ctype);
            if (re) return re;
            ast_flags_set_resolved(&n->flags);
            return RE_OK;
        }
        case EXPR_PARENTHESES: {
            return resolve_ast_node(
                r, ((expr_parentheses*)n)->child, st, ppl, value, ctype);
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
            re = choose_binary_operator_overload(r, ob, st, ppl, value, ctype);
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
            access_modifier am = AM_DEFAULT;
            return resolve_expr_member_accesss(
                r, ema, st, ppl, &am, value, ctype);
        }
        case EXPR_SCOPE_ACCESS: {
            expr_scope_access* esa = (expr_scope_access*)n;
            if (resolved) {
                RETURN_RESOLVED(
                    value, ctype, n,
                    get_resolved_symbol_ctype(esa->target.sym));
            }
            access_modifier access = AM_DEFAULT;
            return resolve_expr_scope_access(
                r, esa, st, ppl, &access, value, ctype);
        }
        case SC_STRUCT:
        case SC_STRUCT_GENERIC:
        case SC_TRAIT:
        case SC_TRAIT_GENERIC: {
            if (resolved) RETURN_RESOLVED(value, ctype, n, TYPE_ELEM);
            re = resolve_body(r, &((scope*)n)->body, ppl);
            if (re) return re;
            ast_flags_set_resolved(&n->flags);
            RETURN_RESOLVED(value, ctype, n, TYPE_ELEM);
        }
        case SC_FUNC:
        case SC_FUNC_GENERIC: {
            // TODO: ctype should actually be some kind of func ptr
            if (ctype) *ctype = (ast_elem*)n;
            if (resolved) return RE_OK;
            return resolve_func(r, (sc_func*)n, ppl);
        }
        case SYM_IMPORT_PARENT: {
            if (!resolved) {
                re = resolve_import_parent(r, (sym_import_parent*)n, st);
                if (re) return re;
            }
            RETURN_RESOLVED(value, ctype, n, NULL);
        }
        case SYM_IMPORT_GROUP: {
            if (!resolved) ast_flags_set_resolved(&n->flags);
            RETURN_RESOLVED(value, ctype, n, NULL);
        }
        case SYM_IMPORT_MODULE: {
            if (!resolved) ast_flags_set_resolved(&n->flags);
            RETURN_RESOLVED(value, ctype, n, NULL);
        }
        case SYM_IMPORT_SYMBOL: {
            sym_import_symbol* is = (sym_import_symbol*)n;
            if (resolved) {
                RETURN_RESOLVED(
                    value, ctype, is->target.sym,
                    get_resolved_symbol_ctype(is->target.sym));
            }
            ureg decl_ppl;
            symbol** s = symbol_table_lookup(
                is->target_st, ppl, AM_PROTECTED, is->target.name, &decl_ppl);
            is->sym.declaring_st = st; // change the
            if (!s) return report_unknown_symbol(r, n, st);
            is->target.sym = *s;
            re = resolve_ast_node(
                r, (ast_node*)*s, (**s).declaring_st, decl_ppl, value, ctype);
            if (re) return re;
            ast_flags_set_resolved(&n->flags);
            return RE_OK;
        }
        case STMT_USING:
        case SYM_NAMED_USING:
        case STMT_COMPOUND_ASSIGN: {
            // TODO
            assert(false);
            return RE_FATAL;
        }
        case SYM_VAR: {
            sym_var* v = (sym_var*)n;
            if (resolved) RETURN_RESOLVED(value, ctype, v, v->ctype);
            symbol* prev_sym_decl = r->curr_symbol_decl;
            r->curr_symbol_decl = (symbol*)v;
            ast_elem* type;
            re = resolve_ast_node(r, v->type, st, ppl, &type, NULL);
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
                re = resolve_ast_node(
                    r, vi->var.type, st, ppl, &vi->var.ctype, NULL);
                if (!re) {
                    ast_elem* val_type;
                    re = resolve_ast_node(
                        r, vi->initial_value, st, ppl, NULL, &val_type);
                    if (!re) {
                        if (!ctypes_unifiable(vi->var.ctype, val_type)) {
                            error_log_report_annotated_twice(
                                r->tc->err_log, ES_RESOLVER, false,
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
                    r, vi->initial_value, st, ppl, NULL, &vi->var.ctype);
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
            re = resolve_ast_node(r, b->value, st, ppl, NULL, &b->value_ctype);
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
                    r->tc->err_log, ES_RESOLVER, false, "type missmatch",
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
                    r, (ast_node*)b, &b->body, ppl, value, &b->ctype);
                if (re) return re;
            }
            else {
                assert(!value);
            }
            RETURN_RESOLVED(value, ctype, value, b->ctype);
        }
        case EXPR_IF: {
            expr_if* ei = (expr_if*)n;
            if (resolved) RETURN_RESOLVED(value, ctype, n, ei->ctype);
            ast_elem *ctype_if, *ctype_else;
            resolve_error re =
                resolve_ast_node(r, ei->condition, st, ppl, NULL, NULL);
            if (re) return re;
            ast_node* old_expr_block_owner = r->curr_expr_block_owner;
            r->curr_expr_block_owner = NULL;
            re = resolve_ast_node(r, ei->if_body, st, ppl, NULL, &ctype_if);
            if (!re) {
                re = resolve_ast_node(
                    r, ei->else_body, st, ppl, NULL, &ctype_else);
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
                    r->tc->err_log, ES_RESOLVER, false, "type missmatch",
                    ast_node_get_file(n, st), src_range_get_start(n->srange),
                    src_range_get_end(n->srange),
                    "if body and else body evaluate to differently typed "
                    "values");
                return RE_TYPE_MISSMATCH;
            }
            else {
                ei->ctype = ctype_if;
            }
            ast_flags_set_resolved(&n->flags);
            RETURN_RESOLVED(value, ctype, ei, ei->ctype);
        }

        case EXPR_LOOP: {
            expr_loop* l = (expr_loop*)n;
            if (resolved) {
                assert(!value);
                RETURN_RESOLVED(value, ctype, NULL, l->ctype);
            }
            re = resolve_expr_body(r, n, &l->body, ppl, value, &l->ctype);
            if (re) return re;
            RETURN_RESOLVED(value, ctype, value, l->ctype);
        }
        case EXPR_MACRO_CALL: {
            // TODO ctype
            if (resolved) {
                assert(!value);
                *ctype = ((expr_macro_call*)n)->ctype;
                return RE_OK;
            }
            return resolve_macro_call(r, (expr_macro_call*)n, st, value, ctype);
        }
        case EXPR_PP: {
            expr_pp* ppe = (expr_pp*)n;
            if (resolved) {
                assert(!value);
                if (ctype) *ctype = ppe->ctype;
                return RE_OK;
            }
            if (r->pp_mode && ast_flags_get_pasting_pp_expr(n->flags)) {
                if (ppe->result != NULL) return RE_SYMBOL_NOT_FOUND_YET;
            }
            re = resolve_ast_node_raw(
                r, ppe->pp_expr, st, ppl + 1, value, &ppe->ctype);
            if (re) return re;
            if (ctype) *ctype = ppe->ctype;
            ast_flags_set_resolved(&n->flags);
            if (!r->pp_mode && !ppe->result) {
                llvm_error lle = llvm_backend_run_pp(
                    r->backend, r->id_space - PRIV_SYMBOL_OFFSET, ppe);
                if (lle) return lle;
            }
            return RE_OK;
        }
        case EXPR_MATCH: {
            // TODO ctype
            if (ctype) *ctype = NULL;
            if (resolved) return RE_OK;
            expr_match* em = (expr_match*)n;
            re = resolve_ast_node(r, em->match_expr, st, ppl, NULL, NULL);
            if (re) return re;
            for (match_arm** ma = (match_arm**)em->body.elements; *ma != NULL;
                 ma++) {
                re = resolve_ast_node(r, (**ma).condition, st, ppl, NULL, NULL);
                if (re) return re;
                re = resolve_ast_node(r, (**ma).value, st, ppl, NULL, NULL);
                if (re) return re;
            }
            return RE_OK;
        }
        case SYM_PARAM: {
            assert(!value);
            sym_param* p = (sym_param*)n;
            if (resolved) RETURN_RESOLVED(value, ctype, NULL, p->ctype);
            return resolve_param(r, p, ppl, ctype);
        }
        default: assert(false); return RE_UNKNOWN_SYMBOL;
    }
}
static inline void report_type_loop(resolver* r, ureg ppl)
{
    ast_node* n = (ast_node*)stack_pop(&r->error_stack);
    symbol_table* st = (symbol_table*)stack_pop(&r->error_stack);
    assert(n && st);
    ureg stack_s = stack_size(&r->error_stack);
    if (stack_peek_nth(&r->error_stack, stack_s - 2) != n) {
        r->retracing_type_loop = true;
        stack_clear(&r->error_stack);
        resolve_ast_node(r, n, st, ppl, NULL, NULL);
    }
    src_range_large srl;
    src_range_unpack(n->srange, &srl);
    if (!srl.file) srl.file = ast_node_get_file(n, st);
    ureg annot_count = stack_size(&r->error_stack) / 2;
    annot_count--; // the starting type is on the stack twice
    error* e = error_log_create_error(
        r->tc->err_log, ES_RESOLVER, false, "type inference cycle", srl.file,
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
    error_log_report(r->tc->err_log, e);
}
static resolve_error resolve_ast_node(
    resolver* r, ast_node* n, symbol_table* st, ureg ppl, ast_elem** value,
    ast_elem** ctype)
{
    resolve_error re = resolve_ast_node_raw(r, n, st, ppl, value, ctype);
    if (re == RE_TYPE_LOOP) {
        if (!r->allow_type_loops) {
            stack_push(&r->error_stack, st);
            stack_push(&r->error_stack, n);
        }
        if (r->type_loop_start == n) {
            if (!ast_flags_get_resolving(n->flags)) {
                ast_flags_clear_resolving(&n->flags);
                report_type_loop(r, ppl);
                return RE_ERROR;
            }
        }
        ast_flags_clear_resolving(&n->flags);
    }
    return re;
}
resolve_error resolve_expr_body(
    resolver* r, ast_node* expr, ast_body* b, ureg ppl, ast_elem** value,
    ast_elem** ctype)
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
        re = resolve_ast_node(r, *n, b->symtab, ppl, NULL, stmt_ctype_ptr);
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
            re = resolve_ast_node(r, *n, b->symtab, ppl, NULL, stmt_ctype_ptr);
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
resolve_error resolve_func(resolver* r, sc_func* fn, ureg ppl)
{
    ast_body* b = &fn->sc.body;
    symbol_table* st = b->symtab;
    resolve_error re;
    for (ureg i = 0; i < fn->param_count; i++) {
        re = resolve_param(r, &fn->params[i], ppl, NULL);
        if (re) return re;
    }
    re = resolve_ast_node(r, fn->return_type, st, ppl, &fn->return_ctype, NULL);
    if (re) return re;
    ast_node** n = b->elements;
    ast_elem* stmt_ctype;
    ast_elem** stmt_ctype_ptr = &stmt_ctype;
    while (*n) {
        re = add_ast_node_decls(r, st, NULL, ppl, *n, false);
        if (re) return re;
        re = resolve_ast_node(r, *n, st, ppl, NULL, stmt_ctype_ptr);
        if (re) return re;
        if (stmt_ctype_ptr && stmt_ctype == UNREACHABLE_ELEM) {
            stmt_ctype_ptr = NULL;
        }
        n++;
    }
    if (stmt_ctype_ptr && fn->return_ctype != VOID_ELEM) {
        ureg brace_end = src_range_get_end(fn->sc.body.srange);
        src_file* f = ast_node_get_file((ast_node*)fn, fn->sc.sym.declaring_st);
        error_log_report_annotated_thrice(
            r->tc->err_log, ES_RESOLVER, false,
            "reachable end of non void function", f, brace_end - 1, brace_end,
            "missing return statement (or unreachable) ", f,
            src_range_get_start(fn->return_type->srange),
            src_range_get_end(fn->return_type->srange),
            "function returns non void type", f,
            src_range_get_start(fn->sc.sym.node.srange),
            src_range_get_end(fn->sc.sym.node.srange), NULL);
        return RE_TYPE_MISSMATCH;
    }
    ast_flags_set_resolved(&fn->sc.sym.node.flags);
    return RE_OK;
}
resolve_error resolve_body(resolver* r, ast_body* b, ureg ppl)
{
    resolve_error re;
    for (ast_node** n = b->elements; *n != NULL; n++) {
        re = resolve_ast_node(r, *n, b->symtab, ppl, NULL, NULL);
        if (re) return re;
    }
    return RE_OK;
}
static void adjust_node_ids(resolver* r, ureg* id_space, ast_node* n);
static inline void adjust_body_ids(resolver* r, ureg* id_space, ast_body* b)
{
    for (ast_node** i = b->elements; *i; i++) {
        adjust_node_ids(r, id_space, *i);
    }
}
static inline void update_id(resolver* r, ureg* tgt, ureg* id_space)
{
    ureg old = *tgt;
    *tgt = *id_space;
    *id_space = *id_space + 1;
    llvm_backend_remap_local_id(r->backend, old, *tgt);
}
static void adjust_node_ids(resolver* r, ureg* id_space, ast_node* n)
{
    // we don't need to recurse into expressions because the contained symbols
    // can never be public
    switch (n->kind) {
        case SC_FUNC: {
            if (ast_flags_get_access_mod(n->flags) < AM_PROTECTED) return;
            sc_func* fn = (sc_func*)n;
            update_id(r, &fn->id, id_space);
        } break;
        case SYM_VAR:
        case SYM_VAR_INITIALIZED: {
            if (ast_flags_get_access_mod(n->flags) < AM_PROTECTED) return;
            update_id(r, &((sym_var*)n)->var_id, id_space);
        } break;
        case SC_STRUCT: {
            if (ast_flags_get_access_mod(n->flags) < AM_PROTECTED) return;
            update_id(r, &((sc_struct*)n)->id, id_space);
            adjust_body_ids(r, id_space, &((sc_struct*)n)->sc.body);
        }
        default: return;
    }
}
resolve_error resolver_init_mdg_symtabs_and_handle_root(resolver* r)
{
    bool contains_root = false;
    for (mdg_node** i = r->mdgs_begin; i != r->mdgs_end; i++) {
        if (*i == r->tc->t->mdg.root_node) {
            if (tauc_request_finalize(r->tc->t)) return RE_FATAL;
            contains_root = true;
        }
        // TODO: init pp symtabs
        int res = symbol_table_init(
            &(**i).symtab, atomic_ureg_load(&(**i).decl_count),
            atomic_ureg_load(&(**i).using_count), true, (ast_elem*)*i);
        if (res) return RE_FATAL;
        if (!(**i).symtab) return RE_FATAL;
        set_parent_symtabs(&(**i).symtab, r->tc->t->root_symtab);
    }
    if (!contains_root) atomic_ureg_inc(&r->tc->t->linking_holdups);
    return RE_OK;
}
resolve_error resolver_setup_pass(resolver* r)
{
    for (mdg_node** i = r->mdgs_begin; i != r->mdgs_end; i++) {
        aseglist_iterator asi;
        aseglist_iterator_begin(&asi, &(**i).open_scopes);
        for (open_scope* osc = aseglist_iterator_next(&asi); osc != NULL;
             osc = aseglist_iterator_next(&asi)) {
            resolve_error re = add_body_decls(
                r, (**i).symtab, (**i).symtab, 0, &osc->sc.body, true);
            if (re) return re;
        }
    }
    return RE_OK;
}
resolve_error resolver_cleanup(resolver* r, ureg startid)
{
    llvm_backend_reserve_symbols(
        r->backend, r->id_space - PRIV_SYMBOL_OFFSET,
        startid + r->public_sym_count);
    int res = 0;
    for (mdg_node** n = r->mdgs_begin; n != r->mdgs_end; n++) {
        aseglist_iterator it;
        aseglist_iterator_begin(&it, &(**n).open_scopes);
        for (open_scope* osc = aseglist_iterator_next(&it); osc;
             osc = aseglist_iterator_next(&it)) {
            adjust_body_ids(r, &startid, &osc->sc.body);
        }
        res |= mdg_node_resolved(*n, r->tc);
    }
    if (res) return RE_FATAL;
    return RE_OK;
}
resolve_error resolver_run(resolver* r)
{
    resolve_error re;
    r->pp_mode = true;
    sbuffer_iterator sbi;
    bool progress;
    do {
        progress = false;
        sbi = sbuffer_iterator_begin_at_end(&r->pp_resolve_nodes);
        for (pp_resolve_node* rn =
                 sbuffer_iterator_previous(&sbi, sizeof(pp_resolve_node));
             rn != NULL;
             rn = sbuffer_iterator_previous(&sbi, sizeof(pp_resolve_node))) {
            re = resolve_ast_node(
                r, rn->node, rn->declaring_st, rn->ppl, NULL, NULL);
            if (re == RE_SYMBOL_NOT_FOUND_YET) continue;
            if (re) return re;
            if (rn->node->kind == EXPR_PP) {
                llvm_error lle = llvm_backend_run_pp(
                    r->backend, r->id_space - PRIV_SYMBOL_OFFSET,
                    (expr_pp*)rn->node);
                if (lle) return RE_FATAL;
            }
            progress = true;
            sbuffer_remove(&r->pp_resolve_nodes, &sbi, sizeof(pp_resolve_node));
        }
    } while (progress);
    r->pp_mode = false;
    // we rerun remaining pp nodes to generate unknown symbol errors
    for (pp_resolve_node* rn =
             sbuffer_iterator_next(&sbi, sizeof(pp_resolve_node));
         rn != NULL;
         rn = sbuffer_iterator_next(&sbi, sizeof(pp_resolve_node))) {
        re = resolve_ast_node(
            r, rn->node, rn->declaring_st, rn->ppl, NULL, NULL);
        if (re) return re;
    }

    // preprocessor is done, resolve the reset in peace
    for (mdg_node** i = r->mdgs_begin; i != r->mdgs_end; i++) {
        r->curr_mdg = *i;
        aseglist_iterator asi;
        aseglist_iterator_begin(&asi, &(**i).open_scopes);
        for (open_scope* osc = aseglist_iterator_next(&asi); osc != NULL;
             osc = aseglist_iterator_next(&asi)) {
            r->curr_osc = osc;
            re = resolve_body(r, &osc->sc.body, 0);
            if (re) return re;
        }
    }
    return RE_OK;
}
int resolver_resolve(
    resolver* r, mdg_node** start, mdg_node** end, ureg* startid)
{
    r->retracing_type_loop = false;
    r->public_sym_count = 0;
    r->private_sym_count = 0;
    r->id_space = PRIV_SYMBOL_OFFSET;
    r->mdgs_begin = start;
    r->mdgs_end = end;
    resolve_error re;
    print_debug_info(r);
    re = resolver_init_mdg_symtabs_and_handle_root(r);
    if (re) return re;
    re = resolver_setup_pass(r);
    if (re) return re;
    re = resolver_run(r);
    if (re) return re;
    *startid = atomic_ureg_add(&r->tc->t->node_ids, r->public_sym_count);
    re = resolver_cleanup(r, *startid);
    if (re) return re;
    return RE_OK;
}
int resolver_emit(resolver* r, ureg startid, llvm_module** module)
{
    if (tauc_success_so_far(r->tc->t) && r->tc->t->needs_emit_stage) {
        ureg endid = startid + r->public_sym_count;
        llvm_error lle = llvm_backend_emit_module(
            r->backend, startid, endid, r->private_sym_count);
        if (lle == LLE_FATAL) return RE_FATAL;
        if (lle) return RE_ERROR;
    }
    else {
        *module = NULL;
    }
    return OK;
}
int resolver_resolve_and_emit(
    resolver* r, mdg_node** start, mdg_node** end, llvm_module** module)
{
    ureg startid;
    int res;
    res = llvm_backend_init_module(r->backend, start, end, module);
    if (res) return ERR;
    TIME(res = resolver_resolve(r, start, end, &startid););
    if (res) return res;
    return resolver_emit(r, startid, module);
}
int resolver_partial_fin(resolver* r, int i, int res)
{
    switch (i) {
        case -1:
        case 4: llvm_backend_delete(r->backend);
        case 3: sbuffer_fin(&r->pp_resolve_nodes);
        case 2: sbuffer_fin(&r->call_types);
        case 1: stack_fin(&r->error_stack);
        case 0: break;
    }
    return res;
}
void resolver_fin(resolver* r)
{
    resolver_partial_fin(r, -1, OK);
}
int resolver_init(resolver* r, thread_context* tc)
{
    r->tc = tc;
    int e = stack_init(&r->error_stack, &r->tc->tempmem);
    if (e) return resolver_partial_fin(r, 0, e);
    e = sbuffer_init(&r->call_types, sizeof(ast_node*) * 32);
    if (e) return resolver_partial_fin(r, 1, e);
    e = sbuffer_init(&r->pp_resolve_nodes, sizeof(pp_resolve_node) * 32);
    if (e) return resolver_partial_fin(r, 2, e);
    r->backend = llvm_backend_new(r->tc);
    if (!r->backend) return resolver_partial_fin(r, 3, ERR);
    r->allow_type_loops = false;
    r->contains_paste = false;
    r->curr_symbol_decl = NULL;
    r->type_loop_start = NULL;
    r->curr_expr_block_owner = NULL;
    return OK;
}
ast_elem* get_resolved_ast_node_ctype(ast_node* n)
{
    ast_elem* ctype;
    resolve_ast_node_raw(NULL, n, NULL, 0, NULL, &ctype);
    return ctype;
}
