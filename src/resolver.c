#include "tauc.h"
#include "resolver.h"
#include "thread_context.h"
#include "utils/error.h"
#include "error_log.h"
#include "utils/panic.h"
#include "utils/zero.h"
#include "utils/debug_utils.h"
#include "utils/aseglist.h"
#include "print_ast.h"

static resolve_error add_body_decls(
    resolver* r, symbol_table* parent_st, symbol_table* shared_st, ureg ppl,
    ast_body* b, bool public_st);

resolve_error
resolve_param(resolver* r, sym_param* p, ureg ppl, ast_elem** ctype);
resolve_error
resolve_body(resolver* r, ast_node* block_owner, ast_body* b, ureg ppl);
static resolve_error resolve_ast_node(
    resolver* r, ast_node* n, symbol_table* st, ureg ppl, ast_elem** value,
    ast_elem** ctype);
static inline resolve_error resolve_ast_node_raw(
    resolver* r, ast_node* n, symbol_table* st, ureg ppl, ast_elem** value,
    ast_elem** ctype);
resolve_error resolve_func(resolver* r, sc_func* fn, ureg ppl);
resolve_error resolve_expr_body(
    resolver* r, symbol_table* parent_st, ast_node* expr, ast_body* b, ureg ppl,
    bool* end_reachable);
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
    symbol_table_inc_decl_count(tgtst);
    if (conflict) {
        return report_redeclaration_error(r, sym, *conflict, tgtst);
    }
    return RE_OK;
}
static pp_resolve_node* pp_resolve_node_create(
    resolver* r, ast_node* n, symbol_table* declaring_st, bool res_used,
    bool run_when_done, ureg ppl)
{
    pp_resolve_node* pprn = freelist_alloc(&r->pp_resolve_nodes);
    if (!pprn) return NULL;
    pprn->result_used = res_used;
    pprn->dep_count = 0;
    pprn->pending_pastes = 0;
    pprn->declaring_st = declaring_st;
    pprn->node = n;
    pprn->continue_block = NULL;
    pprn->ppl = ppl;
    pprn->next = NULL;
    pprn->last_child = NULL;
    pprn->parent = NULL;
    pprn->run_when_done = run_when_done;
    if (aseglist_init(&pprn->required_by)) return NULL;
    return pprn;
}
static inline pp_resolve_node** get_ast_node_pprn(ast_node* n)
{
    switch (n->kind) {
        case SC_STRUCT: return &((sc_struct*)n)->pprn;
        case SC_FUNC: return &((sc_func*)n)->pprn;
        case EXPR_BLOCK: return &((expr_block*)n)->pprn;
        // only funcs, blocks and structs have pprns
        default: panic("attempted to get pprn from ast_node without pprn");
    }
    return NULL;
}
static inline resolve_error get_curr_pprn(
    resolver* r, pp_resolve_node* for_dep, pp_resolve_node** curr_pprn)
{
    if (r->curr_pp_node) {
        *curr_pprn = r->curr_pp_node;
        return RE_OK;
    }

    if (r->curr_var_decl &&
        r->curr_block_owner == r->curr_var_decl_block_owner) {
        if (r->curr_var_pp_node) {
            *curr_pprn = r->curr_var_pp_node;
            return RE_OK;
        }
        *curr_pprn = pp_resolve_node_create(
            r, (ast_node*)r->curr_var_decl,
            ast_elem_get_body((ast_elem*)r->curr_var_decl_block_owner)->symtab,
            false, false, 0);
        if (!*curr_pprn) return RE_FATAL;
        r->curr_var_pp_node = *curr_pprn;
        return RE_OK;
    }

    if (ast_elem_is_open_scope((ast_elem*)r->curr_block_owner)) {
        // when in public scope, nobody depends on this
        //(for now, this might change with is_defined and so on)
        *curr_pprn = NULL;
        return RE_OK;
    }

    if (r->curr_block_pp_node) {
        *curr_pprn = r->curr_block_pp_node;
        return RE_OK;
    }

    *curr_pprn = pp_resolve_node_create(
        r, r->curr_block_owner,
        ast_elem_get_body((ast_elem*)r->curr_block_owner)->symtab, true, true,
        0);
    if (!*curr_pprn) return RE_FATAL;
    r->curr_block_pp_node = *curr_pprn;
    return RE_OK;
}
static resolve_error
curr_pprn_add_dependency(resolver* r, pp_resolve_node* dependency)
{
    pp_resolve_node* depending;
    resolve_error re = get_curr_pprn(r, dependency, &depending);
    if (re) return re;
    if (!depending) return RE_OK;
    if (aseglist_add(&dependency->required_by, depending)) return RE_FATAL;
    depending->dep_count++;
    return RE_OK;
}
static resolve_error
curr_pp_block_add_child(resolver* r, pp_resolve_node* child)
{
    pp_resolve_node* block;
    resolve_error re = get_curr_pprn(r, child, &block);
    if (re) return re;
    if (!block) return RE_OK;
    if (block->first_unresolved_child == NULL) {
        block->first_unresolved_child = child;
    }
    block->dep_count++;
    child->run_when_done = false;
    child->parent = block;
    if (block->last_child) {
        block->last_child->next = child;
    }
    block->last_child = child;
    return RE_OK;
}
resolve_error
pp_resolve_node_activate(resolver* r, pp_resolve_node* pprn, bool resolved)
{
    if (pprn->dep_count == 0) {
        int res;
        if (!resolved || !pprn->run_when_done) {
            res = ptrlist_append(&r->pp_resolve_nodes_pending, pprn);
        }
        else if (pprn->run_when_done) {
            res = ptrlist_append(&r->pp_resolve_nodes_ready, pprn);
        }
        if (res) return RE_FATAL;
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
            symbol_table_inc_decl_count(st);
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
            symbol_table_inc_decl_count(st);
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
    if (ast_flags_get_declared(n->flags)) return RE_OK;
    ast_flags_set_declared(&n->flags);
    resolve_error re;
    switch (n->kind) {
        case EXPR_LITERAL:
        case EXPR_IDENTIFIER: return RE_OK;
        case OSC_MODULE:
        case OSC_EXTEND: {
            // these guys are handled from their mdg node, not from
            // where they appear in source
            return RE_OK;
        }
        case EXPR_RETURN:
        case EXPR_BREAK:
        case EXPR_BLOCK:
        case EXPR_IF:
        case EXPR_LOOP:
        case EXPR_MACRO_CALL:
        case EXPR_MATCH:
        case EXPR_OP_BINARY:
        case EXPR_ACCESS:
        case EXPR_CALL:
        case EXPR_SCOPE_ACCESS:
        case EXPR_MEMBER_ACCESS:
        case EXPR_PARENTHESES:
        case EXPR_OP_UNARY: {
            // only called inside an expression context.
            // we add the symbols individually to avoid use before define
            return RE_OK;
        }

        case STMT_COMPOUND_ASSIGN:
            // TODO
            assert(false);
            return RE_OK;

        case STMT_USING: {
            pp_resolve_node* pprn =
                pp_resolve_node_create(r, n, st, false, false, ppl);
            if (!pprn) return RE_FATAL;
            if (ptrlist_append(&r->pp_resolve_nodes_pending, pprn))
                return RE_FATAL;
            return RE_OK;
        }
        case EXPR_PP: {
            expr_pp* epp = (expr_pp*)n;
            if (public_st) {
                pp_resolve_node* pprn =
                    pp_resolve_node_create(r, n, st, false, true, ppl);
                if (!pprn) return RE_FATAL;
                r->curr_pp_node = pprn;
                epp->result_buffer.state.pprn = pprn;
            }
            if (st && st->pp_symtab) st = st->pp_symtab;
            if (sst && sst->pp_symtab) sst = sst->pp_symtab;
            re = add_ast_node_decls(r, st, sst, ppl + 1, epp->pp_expr, false);
            // no need to reset curr_pp_node here, since we are nested
            if (re) return re;
            if (public_st) r->curr_pp_node = NULL;
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
            if (!conflict) {
                symbol_table_inc_decl_count(tgtst);
            }
            else {
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
            // we only do the parameters here because the declaration and
            // use func body vars is strongly ordered
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
        case SYM_VAR_INITIALIZED:
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
            return add_symbol(r, st, sst, (symbol*)n);
        }
        default:
            assert(false); // unknown node_kind
            return RE_FATAL;
    }
    assert(false);
    return RE_FATAL;
}

static resolve_error add_body_decls(
    resolver* r, symbol_table* parent_st, symbol_table* shared_st, ureg ppl,
    ast_body* b, bool public_st)
{
    set_parent_symtabs(&b->symtab, parent_st);
    for (ast_node** n = b->elements; *n; n++) {
        resolve_error re =
            add_ast_node_decls(r, b->symtab, shared_st, ppl, *n, public_st);
        r->curr_pp_node = NULL;
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
        resolve_error re = resolve_ast_node(
            r, fn->return_type, overload->sym.declaring_st, ppl,
            &fn->return_ctype, NULL);
        if (ctype) *ctype = fn->return_ctype;
        return re;
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

resolve_error
resolve_func_from_call(resolver* r, sc_func* fn, ureg ppl, ast_elem** ctype)
{
    ureg fnppl = fn->sc.sym.declaring_st->ppl;
    resolve_error re;
    if (ast_flags_get_resolved(fn->sc.sym.node.flags)) {
        if (!fn->pprn) return RE_OK;
    }
    else {
        if (!fn->pprn) {
            fn->pprn = pp_resolve_node_create(
                r, (ast_node*)fn, fn->sc.sym.declaring_st, false, false, fnppl);
            if (!fn->pprn) return RE_FATAL;
        }
        re = resolve_ast_node(
            r, fn->return_type, fn->sc.sym.declaring_st, fnppl,
            &fn->return_ctype, NULL);
        if (re) return re;
    }
    if (ctype) *ctype = fn->return_ctype;
    re = curr_pprn_add_dependency(r, fn->pprn);
    if (re) return re;
    return RE_OK;
    // return resolve_func(r, fn, ppl, true);
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
        symbol** s = symbol_table_lookup(lt, ppl, AM_DEFAULT, func_name);
        if (!s) {
            // we use args_st here because thats the scope that the call is
            // in
            re = report_unknown_symbol(r, c->lhs, st);
            break;
        }
        symbol* sym = *s;
        ureg fn_ppl = (**s).declaring_st->ppl;
        bool applicable;
        while (sym->node.kind == SYM_IMPORT_SYMBOL) {
            re = resolve_ast_node(r, (ast_node*)*s, lt, ppl, NULL, NULL);
            if (re) return re;
            sym = ((sym_import_symbol*)*s)->target.sym;
            lt = ((sym_import_symbol*)*s)->target_st;
        }
        if (sym->node.kind == SYM_FUNC_OVERLOADED) {
            sym_func_overloaded* sfo = (sym_func_overloaded*)sym;
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
        else if (sym->node.kind == SC_FUNC) {
            re = overload_applicable(
                r, call_arg_types, c->arg_count, (scope*)sym, fn_ppl,
                &applicable, ctype);
            if (applicable) tgt = (scope*)sym;
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
            re = resolve_no_block_macro_call(r, c, st, (sc_macro*)tgt, ctype);
        }
        else {
            c->target.fn = (sc_func*)tgt;
            ast_flags_set_resolved(&c->node.flags);
            // we sadly need to do this so the resolved flag means
            //"ready to emit and run" which we need for the pp
            re = resolve_func_from_call(r, (sc_func*)tgt, ppl, ctype);
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
        symbol** s = symbol_table_lookup(
            lt, ppl, AM_DEFAULT, op_to_str(ob->node.op_kind));
        if (!s) return report_unknown_symbol(r, (ast_node*)ob, lt);
        ureg op_ppl = (**s).declaring_st->ppl;
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
        default: return NULL;
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
    if (symbol_table_init(&pst, children_count, use, true, (ast_elem*)ip, 0)) {
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
            symbol_table_inc_decl_count(pst);
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
    if (ast_flags_get_resolved(p->sym.node.flags)) {
        if (ctype) *ctype = p->ctype;
        return RE_OK;
    }
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
    symbol** s = symbol_table_lookup(lhs_st, ppl, *access, esa->target.name);
    if (!*s) {
        return report_unknown_symbol(r, (ast_node*)esa, st);
    }
    ureg rhs_ppl = (**s).declaring_st->ppl;
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
    symbol** rhs = symbol_table_lookup_limited(
        sc->body.symtab, ppl, acc, sc->body.symtab->parent, ema->target.name);
    if (!*rhs) return report_unknown_symbol(r, (ast_node*)ema, sc->body.symtab);
    ureg rhs_ppl = (**rhs).declaring_st->ppl;
    ema->target.sym = *rhs;
    resolve_ast_node(
        r, (ast_node*)*rhs, (**rhs).declaring_st, rhs_ppl, value, ctype);
    ast_flags_set_resolved(&ema->node.flags);
    return RE_OK;
}
resolve_error check_var_ppl(resolver* r, sym_var* v, ureg ppl)
{
    if (v->sym.declaring_st->ppl != ppl) {
        stack_push(&r->error_stack, v);
        return RE_DIFFERENT_PP_LEVEL;
    }
    return RE_OK;
}
static inline resolve_error resolve_var(
    resolver* r, symbol_table* st, ureg ppl, sym_var* v, ast_elem** value,
    ast_elem** ctype)
{
    resolve_error re;
    sym_var* prev_var_decl = r->curr_var_decl;
    pp_resolve_node* prev_var_pp_node = r->curr_var_pp_node;
    ast_node* prev_var_decl_block_owner = r->curr_var_decl_block_owner;
    r->curr_var_decl = v;
    r->curr_var_pp_node = NULL;
    r->curr_var_decl_block_owner = r->curr_block_owner;
    if (v->sym.node.kind == SYM_VAR) {
        re = check_var_ppl(r, v, ppl);
        if (!re) {
            ast_elem* type;
            re = resolve_ast_node(r, v->type, st, ppl, &type, NULL);
            if (!re) v->ctype = type;
        }
    }
    else {
        sym_var_initialized* vi = (sym_var_initialized*)v;
        re = check_var_ppl(r, (sym_var*)vi, ppl);
        if (re) {
            // fallthrough
        }
        else if (vi->var.type) {
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
            // this could become needed again once we support typeof,
            // it allows one retry in cases of a variable initially
            // assigned to a self referential expr block
            /*  if (re == RE_TYPE_LOOP && r->type_loop_start == n &&
                 !r->retracing_type_loop) {
                 if (vi->var.ctype) {
                     ast_flags_set_resolved(&n->flags);
                     stack_clear(&r->error_stack);
                     re = resolve_ast_node(
                         r, vi->initial_value, st, ppl, NULL,
                         &vi->var.ctype);
                }
                else{
                    report_type_loop(r, n, st, ppl);
                }
                }
            }*/
        }
    }
    if (!re) v->pprn = r->curr_var_pp_node;
    r->curr_var_decl = prev_var_decl;
    r->curr_var_pp_node = prev_var_pp_node;
    r->curr_var_decl_block_owner = prev_var_decl_block_owner;
    if (re) return re;
    if (v->pprn) {
        re = curr_pp_block_add_child(r, v->pprn);
        if (re) return re;
    }
    ast_flags_set_resolved(&v->sym.node.flags);
    RETURN_RESOLVED(value, ctype, v, v->ctype);
}
static inline resolve_error
resolve_return_or_break(resolver* r, symbol_table* st, ureg ppl, expr_break* b)
{
    resolve_error re =
        resolve_ast_node(r, b->value, st, ppl, NULL, &b->value_ctype);
    if (re) return re;
    ast_flags_set_resolved(&b->node.flags);
    ast_elem* tgt_type;
    if (b->node.kind == EXPR_BREAK) {
        ast_elem** tgtt = get_break_target_ctype(b->target);
        if (*tgtt) {
            tgt_type = *tgtt;
        }
        else {
            *tgtt = b->value_ctype;
            return RE_OK;
        }
    }
    else {
        // if we return from a block, this block's result becomes
        // unreachable
        if (r->curr_block_owner) {
            ast_elem** tgtt = get_break_target_ctype(r->curr_block_owner);
            if (tgtt) {
                if (*tgtt) {
                    if (*tgtt != UNREACHABLE_ELEM) {
                        assert(false); // TODO: error, unreachable not
                        // unifiable
                    }
                }
                else {
                    *tgtt = UNREACHABLE_ELEM;
                }
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
    return RE_OK;
}
static inline resolve_error resolve_identifier(
    resolver* r, symbol_table* st, ureg ppl, expr_identifier* e,
    ast_elem** value, ast_elem** ctype)
{
    symbol** s = symbol_table_lookup(st, ppl, AM_DEFAULT, e->value.str);
    if (!s) return report_unknown_symbol(r, (ast_node*)e, st);
    symbol* sym;
    resolve_error re = resolve_ast_node(
        r, (ast_node*)*s, (**s).declaring_st, ppl, (ast_elem**)&sym, ctype);
    if (re == RE_DIFFERENT_PP_LEVEL) {
        sym = stack_pop(&r->error_stack);
        assert(ast_elem_is_symbol((ast_elem*)sym));
        src_range_large id_sr, sym_sr;
        src_range_unpack(e->node.srange, &id_sr);
        id_sr.file = ast_node_get_file((ast_node*)e, st);
        src_range_unpack(sym->node.srange, &sym_sr);
        sym_sr.file = ast_node_get_file((ast_node*)sym, sym->declaring_st);
        error_log_report_annotated_twice(
            r->tc->err_log, ES_RESOLVER, false,
            "cannot access variable of a different preprocessing "
            "level",
            id_sr.file, id_sr.start, id_sr.end, "usage here", sym_sr.file,
            sym_sr.start, sym_sr.end, "variable defined here");
        return RE_ERROR;
    }
    if (re) return re;
    e->value.sym = (symbol*)sym;
    if (value) *value = (ast_elem*)sym;
    ast_flags_set_resolved(&e->node.flags);
    return RE_OK;
}
static inline resolve_error resolve_if(
    resolver* r, symbol_table* st, ureg ppl, expr_if* ei, ast_elem** value,
    ast_elem** ctype)
{
    ast_elem *ctype_if, *ctype_else;
    bool cond_type_loop = false;
    bool if_branch_type_loop = false;
    bool else_branch_type_loop = false;
    resolve_error re = resolve_ast_node(r, ei->condition, st, ppl, NULL, NULL);
    if (re == RE_TYPE_LOOP) {
        cond_type_loop = true;
    }
    else {
        if (re) return re;
    }
    re = resolve_ast_node(r, ei->if_body, st, ppl, NULL, &ctype_if);
    if (re == RE_TYPE_LOOP) {
        if_branch_type_loop = true;
    }
    else {
        if (re) return re;
    }
    re = resolve_ast_node(r, ei->else_body, st, ppl, NULL, &ctype_else);
    if (re == RE_TYPE_LOOP) {
        else_branch_type_loop = true;
        if (if_branch_type_loop || ctype_if == UNREACHABLE_ELEM) {
            return RE_TYPE_LOOP;
        }
    }
    else {
        if (re) return re;
    }
    if (if_branch_type_loop || ctype_if == UNREACHABLE_ELEM) {
        ei->ctype = ctype_else; // TODO: this could lead to void instead
    }
    else if (else_branch_type_loop || ctype_else == UNREACHABLE_ELEM) {
        ei->ctype = ctype_if;
    }
    else if (!ctypes_unifiable(ctype_if, ctype_else)) {
        error_log_report_annotated(
            r->tc->err_log, ES_RESOLVER, false, "type missmatch",
            ast_node_get_file((ast_node*)ei, st),
            src_range_get_start(ei->node.srange),
            src_range_get_end(ei->node.srange),
            "if body and else body evaluate to differently typed "
            "values");
        return RE_TYPE_MISSMATCH;
    }
    else {
        ei->ctype = ctype_if;
    }
    if (ctype) *ctype = ei->ctype;
    if (value) *value = (ast_elem*)ei;
    if (cond_type_loop || if_branch_type_loop || else_branch_type_loop) {
        return RE_TYPE_LOOP;
    }
    ast_flags_set_resolved(&ei->node.flags);
    return RE_OK;
}
static inline resolve_error resolve_expr_pp(
    resolver* r, symbol_table* st, ureg ppl, expr_pp* ppe, ast_elem** value,
    ast_elem** ctype)
{
    pp_resolve_node* pprn = NULL;
    if (r->curr_pp_node == NULL) {
        if (ppe->result_buffer.state.pprn) {
            pprn = ppe->result_buffer.state.pprn;
        }
        else {
            // TODO: find a better way to determine this,
            // since lots of places use the ctype to determine
            // reachability despite not needing the value
            bool res_used = (value || ctype);
            pprn = pp_resolve_node_create(
                r, (ast_node*)ppe, st, res_used, true, ppl);
            if (!pprn) return RE_FATAL;
            ppe->result_buffer.state.pprn = pprn;
            if (curr_pp_block_add_child(r, pprn)) return RE_FATAL;
        }
        r->curr_pp_node = pprn;
    }
    resolve_error re =
        resolve_ast_node_raw(r, ppe->pp_expr, st, ppl + 1, value, &ppe->ctype);

    if (pprn) {
        r->curr_pp_node = NULL;
        if (pp_resolve_node_activate(r, pprn, re == RE_OK)) {
            return RE_FATAL;
        }
    }
    if (re) return re;
    if (ctype) *ctype = ppe->ctype;
    ast_flags_set_resolved(&ppe->node.flags);
    return RE_OK;
}

static inline void
report_type_loop(resolver* r, ast_node* n, symbol_table* st, ureg ppl);
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
        if (value) *value = (ast_elem*)n;
        if (ctype) *ctype = VOID_ELEM;
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
            if (!resolved) ast_flags_set_resolved(&n->flags);
            RETURN_RESOLVED(value, ctype, &PRIMITIVES[n->pt_kind], TYPE_ELEM);
        }
        case EXPR_LITERAL: {
            if (!resolved) ast_flags_set_resolved(&n->flags);
            RETURN_RESOLVED(value, ctype, n, &PRIMITIVES[n->pt_kind]);
        }
        case EXPR_IDENTIFIER: {
            expr_identifier* e = (expr_identifier*)n;
            if (resolved) {
                RETURN_RESOLVED(
                    value, ctype, e->value.sym,
                    get_resolved_symbol_ctype(e->value.sym));
            }
            return resolve_identifier(r, st, ppl, e, value, ctype);
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
            RETURN_RESOLVED(value, ctype, c, c->target.macro_block->ctype);
        }

        case EXPR_CONTINUE:
        case EXPR_OP_UNARY: {
            expr_op_unary* ou = (expr_op_unary*)n;
            if (resolved) {
                assert(!value && ctype);
                if (ou->op->kind == PRIMITIVE || ou->op->kind == TYPE_POINTER) {
                    *ctype =
                        (ast_elem*)&PRIMITIVES[((ast_node*)ou->op)->pt_kind];
                }
                else {
                    assert(ou->op->kind == SC_FUNC);
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
            // we set this even on error because we jump through to get the
            // required values anyways, so at least make it tail recursive
            ast_flags_set_resolved(&n->flags);
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
            re = resolve_body(r, n, &((scope*)n)->body, ppl);
            if (re) return re;
            ast_flags_set_resolved(&n->flags);
            RETURN_RESOLVED(value, ctype, n, TYPE_ELEM);
        }
        case SC_FUNC:
        case SC_FUNC_GENERIC: {
            assert(!value);
            if (ctype) *ctype = VOID_ELEM;
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

            symbol** s = symbol_table_lookup(
                is->target_st, ppl, AM_PROTECTED, is->target.name);
            if (!s) return report_unknown_symbol(r, n, st);
            is->sym.declaring_st = st; // change the
            is->target.sym = *s;
            re = resolve_ast_node(
                r, (ast_node*)*s, (**s).declaring_st, ppl, value, ctype);
            return re;
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
        case SYM_VAR:
        case SYM_VAR_INITIALIZED: {
            sym_var* v = (sym_var*)n;
            if (resolved) RETURN_RESOLVED(value, ctype, v, v->ctype);
            return resolve_var(r, st, ppl, v, value, ctype);
        }
        case EXPR_RETURN:
        case EXPR_BREAK: {
            if (ctype) *ctype = UNREACHABLE_ELEM;
            if (value) *value = UNREACHABLE_ELEM;
            if (resolved) return RE_OK;
            return resolve_return_or_break(r, st, ppl, (expr_break*)n);
        }
        case EXPR_BLOCK: {
            expr_block* b = (expr_block*)n;
            if (!resolved) {
                bool end_reachable;
                re = resolve_expr_body(
                    r, st, (ast_node*)b, &b->body, ppl, &end_reachable);
                if (ctype) *ctype = b->ctype;
                if (re) return re;
                if (end_reachable) {
                    assert(!b->ctype); // TODO: error
                    b->ctype = VOID_ELEM;
                }
                else {
                    if (!b->ctype) b->ctype = UNREACHABLE_ELEM;
                }
            }
            else {
                assert(!value);
            }
            ast_flags_set_resolved(&n->flags);
            RETURN_RESOLVED(value, ctype, NULL, b->ctype);
        }
        case EXPR_IF: {
            expr_if* ei = (expr_if*)n;
            if (resolved) RETURN_RESOLVED(value, ctype, n, ei->ctype);
            return resolve_if(r, st, ppl, ei, value, ctype);
        }
        case EXPR_LOOP: {
            expr_loop* l = (expr_loop*)n;
            if (resolved) {
                assert(!value);
                RETURN_RESOLVED(value, ctype, NULL, l->ctype);
            }
            bool end_reachable;
            re = resolve_expr_body(r, st, n, &l->body, ppl, &end_reachable);
            if (ctype) *ctype = l->ctype;
            if (re) return re;
            assert(end_reachable); // TODO: error: why loop then?
            if (!l->ctype) l->ctype = UNREACHABLE_ELEM;
            ast_flags_set_resolved(&n->flags);
            RETURN_RESOLVED(value, ctype, value, l->ctype);
        }
        case EXPR_MACRO_CALL: {
            // TODO ctype
            if (resolved) {
                *value = (ast_elem*)n;
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
            return resolve_expr_pp(r, st, ppl, ppe, value, ctype);
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
            sym_param* p = (sym_param*)n;
            if (resolved) RETURN_RESOLVED(value, ctype, p, p->ctype);
            return resolve_param(r, p, ppl, ctype);
        }
        default: assert(false); return RE_UNKNOWN_SYMBOL;
    }
}
static inline void
report_type_loop(resolver* r, ast_node* n, symbol_table* st, ureg ppl)
{
    ureg stack_s = stack_size(&r->error_stack);
    // we are at the peek of the type loop. unwind and report again.
    if (stack_s == 0) return;
    if (stack_peek_nth(&r->error_stack, stack_s - 2) != n) {
        r->retracing_type_loop = true;
        stack_clear(&r->error_stack);
        ast_flags_clear_resolving(&n->flags);
        resolve_ast_node(r, n, st, ppl, NULL, NULL);
        stack_pop(&r->error_stack);
        stack_pop(&r->error_stack);
        stack_s = stack_size(&r->error_stack);
    }
    src_range_large srl;
    src_range_unpack(n->srange, &srl);
    if (!srl.file) srl.file = ast_node_get_file(n, st);
    ureg annot_count = stack_s / 2;
    annot_count--; // the starting type is on the stack too
    error* e = error_log_create_error(
        r->tc->err_log, ES_RESOLVER, false, "type inference cycle", srl.file,
        srl.start, srl.end, "type definition depends on itself", annot_count);
    for (ureg i = 0; i < annot_count; i++) {
        n = (ast_node*)stack_pop(&r->error_stack);
        if (!n) break;
        st = (symbol_table*)stack_pop(&r->error_stack);
        if (n->kind == EXPR_OP_BINARY || n->kind == EXPR_OP_UNARY ||
            n->kind == EXPR_MEMBER_ACCESS) {
            continue; // skip stuff that isn't helpful in the report
        }
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
            if (n == r->type_loop_start && !r->retracing_type_loop) {
                report_type_loop(r, n, st, ppl);
            }
            else {
                stack_push(&r->error_stack, st);
                stack_push(&r->error_stack, n);
            }
            ast_flags_clear_resolving(&n->flags);
        }
        else if (n != r->type_loop_start) {
            ast_flags_clear_resolving(&n->flags);
        }
    }
    return re;
}
resolve_error resolve_expr_body(
    resolver* r, symbol_table* parent_st, ast_node* expr, ast_body* b, ureg ppl,
    bool* end_reachable)
{
    ast_node* parent_block_owner = r->curr_block_owner;
    r->curr_block_owner = expr;
    resolve_error re;
    ast_elem* stmt_ctype = NULL;
    ast_elem** stmt_ctype_ptr = &stmt_ctype;
    ureg saved_decl_count = 0;
    pp_resolve_node* prev_block_pprn = r->curr_block_pp_node;
    r->curr_block_pp_node = NULL;

    bool parent_allows_type_loops = r->allow_type_loops;
    bool parenting_type_loop = false;
    if (!r->retracing_type_loop) {
        r->allow_type_loops = true;
        set_parent_symtabs(&b->symtab, parent_st);
    }
    if (b->symtab->owning_node == (ast_elem*)expr && b->symtab->decl_count) {
        // if we already have decls this is the second pass.
        // all local syms are already defined so we don't need to check this
        // symtab during lookup. this way we prevent use before define
        saved_decl_count = b->symtab->decl_count;
        b->symtab->decl_count = 0;
    }
    for (ast_node** n = b->elements; *n != NULL; n++) {
        re = add_ast_node_decls(r, b->symtab, NULL, ppl, *n, false);
        if (re) break;
        re = resolve_ast_node(r, *n, b->symtab, ppl, NULL, stmt_ctype_ptr);
        if (r->curr_block_pp_node && r->curr_block_pp_node->pending_pastes) {
            r->curr_block_pp_node->continue_block = n + 1;
            re = RE_SYMBOL_NOT_FOUND_YET;
            break;
        }
        if (re && re != RE_TYPE_LOOP) break;
        if (stmt_ctype_ptr && stmt_ctype == UNREACHABLE_ELEM) {
            stmt_ctype_ptr = NULL;
        }
        if (!re) continue;
        assert(re == RE_TYPE_LOOP);
        if (r->type_loop_start == expr) {
            if (r->retracing_type_loop) {
                stack_clear(&r->error_stack);
            }
            parenting_type_loop = true;
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
    if (saved_decl_count) {
        b->symtab->decl_count = saved_decl_count;
    }
    pp_resolve_node* bpprn = r->curr_block_pp_node;
    r->curr_block_owner = parent_block_owner;
    r->curr_block_pp_node = prev_block_pprn;
    if (bpprn) {
        assert(bpprn->node == expr);
        bpprn->node = (ast_node*)expr;
        bpprn->declaring_st = parent_st;
        bpprn->ppl = ppl;
        bpprn->run_when_done = false;
        if (curr_pprn_add_dependency(r, bpprn)) return RE_FATAL;
        if (pp_resolve_node_activate(r, bpprn, re == RE_OK)) {
            return RE_FATAL;
        }
    }
    *end_reachable = (stmt_ctype_ptr != NULL);
    r->allow_type_loops = parent_allows_type_loops;
    r->curr_block_owner = parent_block_owner;
    if (re) return re;
    if (parenting_type_loop) {
        return RE_TYPE_LOOP;
    }
    return RE_OK;
}
// TODO: make sure we return!
resolve_error resolve_func(resolver* r, sc_func* fn, ureg ppl)
{
    ast_body* b = &fn->sc.body;
    symbol_table* st = b->symtab;
    resolve_error re;
    ast_node* parent_block_owner = r->curr_block_owner;
    r->curr_block_owner = (ast_node*)fn;
    for (ureg i = 0; i < fn->param_count; i++) {
        re = resolve_param(r, &fn->params[i], ppl, NULL);
        if (re) {
            r->curr_block_owner = parent_block_owner;
            return re;
        }
    }
    re = resolve_ast_node(r, fn->return_type, st, ppl, &fn->return_ctype, NULL);
    if (re) {
        return re;
        r->curr_block_owner = parent_block_owner;
    }
    if (b->srange == SRC_RANGE_INVALID) { // hack for external functions
        ast_flags_set_resolved(&fn->sc.sym.node.flags);
        r->curr_block_owner = parent_block_owner;
        return RE_OK;
    }
    pp_resolve_node* prev_block_pprn = r->curr_block_pp_node;
    r->curr_block_pp_node = fn->pprn;
    ast_node** n = b->elements;
    ast_elem* stmt_ctype;
    ast_elem** stmt_ctype_ptr = &stmt_ctype;
    pp_resolve_node* prev_pprn = r->curr_pp_node;
    r->curr_pp_node = NULL;
    while (*n) {
        re = add_ast_node_decls(r, st, NULL, ppl, *n, false);
        if (re) break;
        re = resolve_ast_node(r, *n, st, ppl, NULL, stmt_ctype_ptr);
        if (re) break;
        r->curr_pp_node = NULL;
        if (r->curr_block_pp_node && r->curr_block_pp_node->pending_pastes) {
            r->curr_block_pp_node->continue_block = n + 1;
            re = RE_SYMBOL_NOT_FOUND_YET;
            break;
        }
        if (stmt_ctype_ptr && stmt_ctype == UNREACHABLE_ELEM) {
            stmt_ctype_ptr = NULL;
        }
        n++;
    }
    r->curr_pp_node = prev_pprn;
    // this must be reset before we call add dependency
    r->curr_block_owner = parent_block_owner;
    pp_resolve_node* bpprn = r->curr_block_pp_node;
    if (bpprn) {
        assert(bpprn->node == (ast_node*)fn);
        bpprn->node = (ast_node*)fn;
        bpprn->declaring_st = fn->sc.sym.declaring_st;
        bpprn->ppl = ppl;
        r->curr_block_pp_node = prev_block_pprn;
        if (pp_resolve_node_activate(r, bpprn, re == RE_OK)) return RE_FATAL;
        fn->pprn = bpprn;
    }
    else {
        r->curr_block_pp_node = prev_block_pprn;
    }
    if (re) return re;
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
resolve_error
resolve_body(resolver* r, ast_node* block_owner, ast_body* b, ureg ppl)
{
    resolve_error re;
    ast_node* parent_block_owner = r->curr_block_owner;
    r->curr_block_owner = block_owner;
    for (ast_node** n = b->elements; *n != NULL; n++) {
        re = resolve_ast_node(r, *n, b->symtab, ppl, NULL, NULL);
        if (re) break;
    }
    r->curr_block_owner = parent_block_owner;
    return re;
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
    // we don't need to recurse into expressions because the contained
    // symbols can never be public
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
        case EXPR_PP: {
            adjust_node_ids(r, id_space, ((expr_pp*)n)->pp_expr);
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
            atomic_ureg_load(&(**i).using_count), true, (ast_elem*)*i, 0);
        if (res) return RE_FATAL;
        if (!(**i).symtab) return RE_FATAL;
        set_parent_symtabs(&(**i).symtab, r->tc->t->root_symtab);
    }
    if (!contains_root) atomic_ureg_inc(&r->tc->t->linking_holdups);
    return RE_OK;
}
resolve_error resolver_add_osc_decls(resolver* r)
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
// this is just for debugging purposes
void print_pprns(resolver* r)
{
    if (!ptrlist_is_empty(&r->pp_resolve_nodes_ready)) {
        puts("ready pprns:");
        pli it = pli_begin(&r->pp_resolve_nodes_ready);
        for (pp_resolve_node* rn = pli_next(&it); rn; rn = pli_next(&it)) {
            assert(rn->node != NULL);
            printf("    dep count: %zu\n", rn->dep_count);
            print_indent(1);
            print_ast_node(rn->node, r->curr_mdg, 1);
            puts("");
        }
    }
}
resolve_error
pp_resolve_node_dep_done(resolver* r, pp_resolve_node* pprn, bool* progress);
resolve_error
pp_resolve_node_done(resolver* r, pp_resolve_node* pprn, bool* progress)
{
    assert(pprn->node);
    aseglist_iterator asit;
    resolve_error re;
    aseglist_iterator_begin(&asit, &pprn->required_by);
    for (pp_resolve_node* rn = aseglist_iterator_next(&asit); rn;
         rn = aseglist_iterator_next(&asit)) {
        re = pp_resolve_node_dep_done(r, rn, progress);
        if (re) return re;
    }
    if (pprn->parent) {
        re = pp_resolve_node_dep_done(r, pprn->parent, progress);
        if (re) return re;
    }
    aseglist_fin(&pprn->required_by);
    freelist_free(&r->pp_resolve_nodes, pprn);
    return RE_OK;
}
resolve_error
pp_resolve_node_dep_done(resolver* r, pp_resolve_node* pprn, bool* progress)
{
    assert(pprn->dep_count);
    assert(pprn->node);
    pprn->dep_count--;
    if (pprn->dep_count == 0) {
        *progress = true;
        if (pprn->parent) {
            if (ptrlist_append(&r->pp_resolve_nodes_pending, pprn)) {
                return RE_FATAL;
            }
        }
        else {
            if (ptrlist_append(&r->pp_resolve_nodes_ready, pprn)) {
                return RE_FATAL;
            }
        }
    }
    return RE_OK;
}
resolve_error resolver_run_pp_resolve_nodes(resolver* r)
{
    llvm_error lle;
    pli it;
    bool progress;
    do {
        progress = false;
        if (!ptrlist_is_empty(&r->pp_resolve_nodes_ready)) {
            print_pprns(r);
            lle = llvm_backend_run_pp(
                r->backend, r->id_space - PRIV_SYMBOL_OFFSET,
                &r->pp_resolve_nodes_ready);
            if (lle) return RE_FATAL;
            it = pli_begin(&r->pp_resolve_nodes_ready);
            for (pp_resolve_node* rn = pli_next(&it); rn; rn = pli_next(&it)) {
                pp_resolve_node_done(r, rn, &progress);
            }
            sbuffer_clear(&r->pp_resolve_nodes_ready);
        }
        // we try to resolve pending nodes again
        it = pli_begin(&r->pp_resolve_nodes_pending);
        for (pp_resolve_node* rn = pli_next(&it); rn; rn = pli_next(&it)) {
            if (ast_flags_get_resolved(rn->node->flags)) {
                if (ptrlist_append(&r->pp_resolve_nodes_ready, rn)) {
                    return RE_FATAL;
                }
                pli_prev(&it);
                ptrlist_remove(&r->pp_resolve_nodes_pending, &it);
                progress = true;
            }
        }
    } while (progress);
    return RE_OK;
}

resolve_error resolver_handle_osc_level_pp(resolver* r)
{
    resolve_error re;
    // at this point the initial add_decls has run and added
    // all top level pp exprs as pp_resolve_nodes
    // now we run through all of these to create a list of all
    // pp_resolve_nodes that require unique evaluation
    r->pp_generation = 0;
    r->multi_evaluation_ctx = false;
    pli it = pli_rbegin(&r->pp_resolve_nodes_pending);
    for (pp_resolve_node* rn = pli_prev(&it); rn; rn = pli_prev(&it)) {
        r->curr_pp_node = rn;
        re = resolve_ast_node(
            r, ((expr_pp*)rn->node)->pp_expr, rn->declaring_st, rn->ppl + 1,
            NULL, &((expr_pp*)rn->node)->ctype);
        if (re == RE_SYMBOL_NOT_FOUND_YET) continue;
        if (re) return re;
        if (rn->dep_count == 0) {
            ptrlist_remove(&r->pp_resolve_nodes_pending, &it);
            if (ptrlist_append(&r->pp_resolve_nodes_ready, rn)) return RE_FATAL;
        }
    }
    re = resolver_run_pp_resolve_nodes(r);
    if (re) return re;
    // we rerun remaining pp nodes to generate unknown symbol errors
    it = pli_rbegin(&r->pp_resolve_nodes_pending);
    for (pp_resolve_node* rn = pli_prev(&it); rn; rn = pli_prev(&it)) {
        re = resolve_ast_node(
            r, ((expr_pp*)rn->node)->pp_expr, rn->declaring_st, rn->ppl + 1,
            NULL, NULL);
        if (re) return re;
    }
    return RE_OK;
}
resolve_error resolver_handle_post_pp(resolver* r)
{
    resolve_error re;
    for (mdg_node** i = r->mdgs_begin; i != r->mdgs_end; i++) {
        r->curr_mdg = *i;
        aseglist_iterator asi;
        aseglist_iterator_begin(&asi, &(**i).open_scopes);
        for (open_scope* osc = aseglist_iterator_next(&asi); osc != NULL;
             osc = aseglist_iterator_next(&asi)) {
            r->curr_osc = osc;
            r->curr_block_owner = (ast_node*)osc;
            re = resolve_body(r, (ast_node*)osc, &osc->sc.body, 0);
            ast_flags_set_resolved(&osc->sc.sym.node.flags);
            if (re) return re;
        }
    }
    return resolver_run_pp_resolve_nodes(r);
}
int resolver_resolve(
    resolver* r, mdg_node** start, mdg_node** end, ureg* startid)
{
    r->curr_pp_node = NULL;
    r->curr_block_pp_node = NULL;
    r->curr_var_decl = NULL;
    r->curr_var_decl_block_owner = NULL;
    r->curr_var_pp_node = NULL;
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
    re = resolver_add_osc_decls(r);
    if (re) return re;
    re = resolver_handle_osc_level_pp(r);
    if (re) return re;
    re = resolver_handle_post_pp(r);
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
        case 6: llvm_backend_delete(r->backend);
        case 5: ptrlist_fin(&r->pp_resolve_nodes_ready);
        case 4: ptrlist_fin(&r->pp_resolve_nodes_pending);
        case 3: freelist_fin(&r->pp_resolve_nodes);
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
    e = freelist_init(
        &r->pp_resolve_nodes, &r->tc->permmem, sizeof(pp_resolve_node));
    if (e) return resolver_partial_fin(r, 2, e);
    e = ptrlist_init(&r->pp_resolve_nodes_pending, 16);
    if (e) return resolver_partial_fin(r, 3, e);
    e = ptrlist_init(&r->pp_resolve_nodes_ready, 16);
    if (e) return resolver_partial_fin(r, 4, e);
    r->backend = llvm_backend_new(r->tc);
    if (!r->backend) return resolver_partial_fin(r, 3, ERR);
    r->allow_type_loops = false;
    r->type_loop_start = NULL;
    r->curr_block_owner = NULL;
    return OK;
}
ast_elem* get_resolved_ast_node_ctype(ast_node* n)
{
    ast_elem* ctype;
    resolve_ast_node_raw(NULL, n, NULL, 0, NULL, &ctype);
    return ctype;
}
