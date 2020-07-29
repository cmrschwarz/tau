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
#include "generic_instance_resolution.h"
#include <assert.h>
#include "symbol_lookup.h"

resolve_error resolver_run_pp_resolve_nodes(resolver* r);
resolve_error
resolve_param(resolver* r, sym_param* p, bool generic, ast_elem** ctype);
resolve_error
resolve_module_frame(resolver* r, module_frame* block_owner, ast_body* b);
static inline resolve_error resolve_ast_node_raw(
    resolver* r, ast_node* n, symbol_table* st, ast_elem** value,
    ast_elem** ctype);
resolve_error
resolve_func(resolver* r, sc_func_base* fnb, ast_node** continue_block);
resolve_error
resolve_struct(resolver* r, sc_struct* st, ast_elem** value, ast_elem** ctype);
resolve_error resolve_expr_body(
    resolver* r, symbol_table* parent_st, ast_node* expr, ast_body* b,
    pp_resolve_node** block_pprn, bool* end_reachable);
resolve_error resolve_expr_scope_access(
    resolver* r, expr_scope_access* esa, symbol_table* st,
    access_modifier* access, ast_elem** value, ast_elem** ctype);
resolve_error
get_resolved_symbol_symtab(resolver* r, symbol* s, symbol_table** tgt_st);
resolve_error
resolve_func_from_call(resolver* r, sc_func* fn, ast_elem** ctype);
resolve_error
pp_resolve_node_done(resolver* r, pp_resolve_node* pprn, bool* progress);
resolve_error pp_resolve_node_dep_ready(resolver* r, pp_resolve_node* pprn);
resolve_error
pp_resolve_node_ready(resolver* r, pp_resolve_node* pprn, bool fin_independant);
void free_pprns(resolver* r);
void print_pprn(resolver* r, pp_resolve_node* pprn, bool verbose, ureg ident);
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
        ast_node_get_smap(n, st), srl.start, srl.end,
        "use of an undefined symbol");
    return RE_UNKNOWN_SYMBOL;
}
resolve_error report_redeclaration_error(
    resolver* r, symbol* redecl, symbol* prev, symbol_table* st)
{
    src_range_large prev_st, redecl_st;
    ast_node_get_src_range((ast_node*)redecl, redecl->declaring_st, &redecl_st);
    ast_node_get_src_range((ast_node*)prev, prev->declaring_st, &prev_st);
    error_log_report_annotated_twice(
        r->tc->err_log, ES_RESOLVER, false, "symbol redeclaration",
        prev_st.smap, prev_st.start, prev_st.end,
        "a symbol of this name is already defined in this "
        "scope",
        redecl_st.smap, redecl_st.start, redecl_st.end,
        "previous definition here");
    return RE_SYMBOL_REDECLARATION;
}

static resolve_error
add_symbol(resolver* r, symbol_table* st, symbol_table* sst, symbol* sym)
{
    sym->declaring_st = st;
    symbol_table* tgtst =
        (sst && ast_flags_get_access_mod(sym->node.flags) != AM_LOCAL) ? sst
                                                                       : st;
    symbol** conflict;
    conflict = symbol_table_insert(tgtst, sym);
    // symbol_table_inc_decl_count(tgtst);
    if (conflict) {
        return report_redeclaration_error(r, sym, *conflict, tgtst);
    }
    return RE_OK;
}
void remove_pprn_from_waiting_list(resolver* r, pp_resolve_node* pprn)
{
    assert(pprn->waiting_list_entry);
    assert(!sbuffer_is_empty(&r->pp_resolve_nodes_waiting));
    sbuffer_iterator sbi =
        sbuffer_iterator_begin_at_end(&r->pp_resolve_nodes_waiting);
    pp_resolve_node** last =
        sbuffer_iterator_previous(&sbi, sizeof(pp_resolve_node*));
    *pprn->waiting_list_entry = *last;
    (*last)->waiting_list_entry = pprn->waiting_list_entry;
    sbuffer_remove_back(
        &r->pp_resolve_nodes_waiting, sizeof(pp_resolve_node**));
    pprn->waiting_list_entry = NULL;
}
void pprn_fin(resolver* r, pp_resolve_node* pprn)
{
    ast_node* n = pprn->node;
    assert(n);
    assert(pprn->dep_count == 0);
    if (r->tc->t->verbosity_flags & VERBOSITY_FLAGS_PPRNS) {
        tprintf("freeing pprn: ");
        print_pprn(r, pprn, false, 0);
    }
    if (pprn->waiting_list_entry) {
        remove_pprn_from_waiting_list(r, pprn);
    }
    switch (n->kind) {
        case SC_FUNC:
        case SC_FUNC_GENERIC: ((sc_func_base*)n)->pprn = NULL; break;
        case SYM_VAR:
        case SYM_VAR_INITIALIZED: ((sym_var*)n)->pprn = NULL; break;
        case EXPR_BLOCK: ((expr_block*)n)->ebb.pprn = NULL; break;
        case EXPR_LOOP: ((expr_loop*)n)->ebb.pprn = NULL; break;
        case SC_STRUCT: ((sc_struct*)n)->sb.pprn = NULL; break;
        case SYM_IMPORT_GROUP: ((sym_import_group*)n)->pprn = NULL; break;
        case SYM_IMPORT_MODULE: ((sym_import_module*)n)->pprn = NULL; break;
        case EXPR_PP: ((expr_pp*)n)->pprn = NULL; break;
        case EXPR_PASTE_EVALUATION:
        case STMT_PASTE_EVALUATION:
        default: assert(false); panic("invalid pprn node type");
    }
    aseglist_fin(&pprn->notify_when_done);
    aseglist_fin(&pprn->notify_when_ready);
    freelist_free(&r->pp_resolve_nodes, pprn);
}
static pp_resolve_node* pp_resolve_node_create(
    resolver* r, ast_node* n, symbol_table* declaring_st, bool res_used,
    bool run_when_ready, bool sequential_block)
{
    // print_pprns(r);
    pp_resolve_node* pprn = freelist_alloc(&r->pp_resolve_nodes);
    if (!pprn) return NULL;
    if (aseglist_init(&pprn->notify_when_ready)) {
        freelist_free(&r->pp_resolve_nodes, pprn);
        return NULL;
    }
    if (aseglist_init(&pprn->notify_when_done)) {
        freelist_free(&r->pp_resolve_nodes, pprn);
        aseglist_fin(&pprn->notify_when_ready);
        return NULL;
    }
    pprn->dep_count = 0;
    pprn->pending_pastes = 0;
    pprn->declaring_st = declaring_st;
    pprn->node = n;
    assert(n->kind != MF_MODULE);
    pprn->continue_block = NULL;
    pprn->next = NULL;
    pprn->ready = false;
    pprn->parent = NULL;
    pprn->run_when_ready = run_when_ready;
    pprn->block_pos_reachable = true;
    pprn->sequential_block = sequential_block;
    pprn->first_unresolved_child = NULL;
    pprn->waiting_list_entry = NULL;
    if (r->tc->t->verbosity_flags & VERBOSITY_FLAGS_PPRNS) {
        tprintf("allocated pprn: ");
        print_pprn(r, pprn, false, 0);
    }
    return pprn;
}
static inline resolve_error
get_curr_block_pprn(resolver* r, pp_resolve_node** curr_pprn)
{
    if (ast_elem_is_module_frame((ast_elem*)r->curr_block_owner)) {
        // when in public scope, nobody depends on this
        //(for now, this might change with is_defined and so on)
        *curr_pprn = NULL;
        return RE_OK;
    }
    if (r->curr_block_pp_node) {
        *curr_pprn = r->curr_block_pp_node;
        return RE_OK;
    }
    // the only other non sequential blocks are module frames
    // for that case we already returned null earlier
    bool sequential = !ast_elem_is_struct((ast_elem*)r->curr_block_owner);
    *curr_pprn = pp_resolve_node_create(
        r, r->curr_block_owner,
        ast_elem_get_body((ast_elem*)r->curr_block_owner)->symtab, true, true,
        sequential);
    if (!*curr_pprn) return RE_FATAL;
    r->curr_block_pp_node = *curr_pprn;
    return RE_OK;
}
static inline resolve_error
get_curr_pprn(resolver* r, pp_resolve_node** curr_pprn)
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
            false, false, false);
        if (!*curr_pprn) return RE_FATAL;
        r->curr_var_pp_node = *curr_pprn;
        return RE_OK;
    }
    return get_curr_block_pprn(r, curr_pprn);
}
static resolve_error
curr_pprn_run_with(resolver* r, pp_resolve_node* dependency)
{
    pp_resolve_node* depending;
    resolve_error re = get_curr_pprn(r, &depending);
    if (re) return re;
    if (!depending) return RE_OK;
    if (!dependency->ready) {
        if (aseglist_add(&dependency->notify_when_ready, depending))
            return RE_FATAL;
        depending->dep_count++;
    }
    return RE_OK;
}
static resolve_error
curr_pprn_run_after(resolver* r, pp_resolve_node* dependency)
{
    pp_resolve_node* depending;
    resolve_error re = get_curr_pprn(r, &depending);
    if (re) return re;
    if (!depending) return RE_OK;
    if (aseglist_add(&dependency->notify_when_done, depending)) return RE_FATAL;
    depending->dep_count++;
    return RE_OK;
}
static resolve_error
curr_pp_block_add_child(resolver* r, pp_resolve_node* child)
{
    // for structs we want the block to depend on the children but the children
    // should run indivitually
    if (ast_elem_is_struct((ast_elem*)r->curr_block_owner)) {
    }
    pp_resolve_node* parent;
    resolve_error re = get_curr_block_pprn(r, &parent);
    if (re) return re;
    if (!parent) return RE_OK;
    child->parent = parent;
    if (!parent->sequential_block) {
        if (aseglist_add(&child->notify_when_done, parent)) return RE_FATAL;
        parent->dep_count++;
        return RE_OK;
    }
    child->run_when_ready = false;
    if (parent->first_unresolved_child == NULL) {
        parent->first_unresolved_child = child;
        parent->last_unresolved_child = child;
    }
    else {
        parent->last_unresolved_child->next = child;
        parent->last_unresolved_child = child;
    }
    return RE_OK;
}
resolve_error
pp_resolve_node_activate(resolver* r, pp_resolve_node* pprn, bool resolved)
{
    if (pprn->dep_count == 0 && r->module_group_constructor) {
        // HACK: make sure the module group ctor runs first
        // TODO: implement this properly
        resolve_func_from_call(r, r->module_group_constructor, NULL);
    }
    if (pprn->dep_count == 0) {
        if (resolved) {
            return pp_resolve_node_ready(r, pprn, true);
        }
        if (!pprn->parent) {
            if (ptrlist_append(&r->pp_resolve_nodes_pending, pprn)) {
                return RE_FATAL;
            }
        }
        return RE_OK;
    }
    if (pprn->parent) {
        if (aseglist_add(&pprn->notify_when_ready, pprn->parent)) {
            return RE_FATAL;
        }
        pprn->parent->dep_count++;
        return RE_OK;
    }
    pp_resolve_node** res =
        sbuffer_append(&r->pp_resolve_nodes_waiting, sizeof(pp_resolve_node*));
    if (!res) return RE_FATAL;
    *res = pprn;
    pprn->waiting_list_entry = res;
    return RE_OK;
}
resolve_error add_sym_import_module_decl(
    resolver* r, symbol_table* st, sym_import_module* im, mdg_node* stop,
    mdg_node* start, sym_import_parent** tgt_parent)
{
    im->osym.sym.declaring_st = st;
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
            // symbol_table_inc_decl_count(st);
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
                im->osym.sym.name = "";
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
        p->osym.sym.node.kind = SYM_IMPORT_PARENT;
        p->osym.sym.node.flags = AST_NODE_FLAGS_DEFAULT;
        p->osym.sym.name = start->name;
        p->osym.sym.next = next;
        p->children.symbols = children;
        *tgt = (symbol*)p;
        *tgt_parent = p;
    }
    else {
        *tgt = (symbol*)im;
        im->osym.sym.next = next;
    }
    ast_flags_set_declared(&im->osym.sym.node.flags);
    return RE_OK;
}
resolve_error add_import_group_decls(
    resolver* r, mdg_node* curr_mdg_node, sym_import_group* ig,
    symbol_table* st)
{
    symbol* next;
    if (!ig->osym.sym.name) {
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
            // symbol_table_inc_decl_count(st);
            s->declaring_st = st;
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
    ast_flags_set_declared(&ig->osym.sym.node.flags);
    return RE_OK;
}
static inline void
set_parent_symtabs(symbol_table** tgt, symbol_table* parent_st)
{
    assert(parent_st);
    if (*tgt == NULL) {
        *tgt = parent_st;
    }
    else {
        assert(
            (*tgt)->parent == NULL ||
            (*tgt)->owning_node->kind == STMT_PASTE_EVALUATION);
        symbol_table_set_parent(*tgt, parent_st);
    }
}
// we differentiate between local ast nodes and shared ones
// local nodes are only visible within the module and can be
// freed once the module was emitted. shared nodes might be used from
// other modules and therefore need to stay around for the whole compilation
static bool is_local_node(ast_flags flags)
{
    access_modifier m = ast_flags_get_access_mod(flags);
    return m != AM_PUBLIC && m != AM_PROTECTED;
}
ureg ast_node_claim_id(resolver* r, ast_node* n, bool public_st)
{
    if (public_st && !is_local_node(n->flags)) {
        r->public_sym_count++;
    }
    else {
        r->private_sym_count++;
    }
    return r->id_space++;
}
ureg claim_symbol_id(resolver* r, symbol* s, bool public_st)
{
    symbol_table* decl_st = symbol_table_nonmeta(s->declaring_st);
    ast_elem* on = decl_st->owning_node;
    if (ast_elem_is_var((ast_elem*)s) && !ast_flags_get_static(s->node.flags) &&
        ast_elem_is_struct(on)) {
        ast_flags_set_instance_member(&s->node.flags);
        // since these are per instance we don't give them a variable id
        // the backend will assign struct member ids starting from 0
        return UREG_MAX;
    }
    else {
        return ast_node_claim_id(r, (ast_node*)s, public_st);
    }
}

static resolve_error add_ast_node_decls(
    resolver* r, symbol_table* st, symbol_table* sst, ast_node* n,
    bool public_st)
{
    if (n == NULL) return RE_OK;
    if (ast_flags_get_declared(n->flags)) return RE_OK;
    ast_flags_set_declared(&n->flags);
    resolve_error re;
    switch (n->kind) {
        case EXPR_LITERAL:
        case EXPR_IDENTIFIER: return RE_OK;
        case MF_MODULE:
        case MF_EXTEND: {
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
        case EXPR_PASTE_STR:
        case EXPR_MACRO_STR_CALL:
        case EXPR_OP_UNARY: {
            // only called inside an expression context.
            // we add the symbols individually to avoid use before define
            return RE_OK;
        }

        case STMT_COMPOUND_ASSIGN:
            // TODO
            assert(false);
            return RE_OK;

        case STMT_USE: {
            pp_resolve_node* pprn =
                pp_resolve_node_create(r, n, st, false, false, false);
            if (!pprn) return RE_FATAL;
            if (ptrlist_append(&r->pp_resolve_nodes_pending, pprn))
                return RE_FATAL;
            return RE_OK;
        }
        case EXPR_PP: {
            expr_pp* epp = (expr_pp*)n;
            pp_resolve_node* prevcurr = r->curr_pp_node;
            if (public_st) {
                // we need to add these during add decls already
                // because we want to handle these before resolving
                // any non pp stuff in the scope
                bool is_expr = ast_elem_is_expr((ast_elem*)epp->pp_expr);
                pp_resolve_node* pprn = pp_resolve_node_create(
                    r, is_expr ? n : epp->pp_expr, st, false, is_expr, false);
                if (!pprn) return RE_FATAL;
                re = pp_resolve_node_activate(r, pprn, false);
                if (re) return re;
                r->curr_pp_node = pprn;
                epp->pprn = pprn;
            }
            re = add_ast_node_decls(r, st, sst, epp->pp_expr, false);
            if (public_st) r->curr_pp_node = prevcurr;
            if (re) return re;
            return RE_OK;
        }
        case SC_STRUCT_GENERIC: {
            sc_struct_generic* sg = (sc_struct_generic*)n;
            for (ureg i = 0; i < sg->generic_param_count; i++) {
                re = add_symbol(
                    r, sg->sb.sc.body.symtab, NULL,
                    (symbol*)&sg->generic_params[i]);
                if (re) return re;
            }
        } // fallthrough
        case SC_STRUCT:
        case SC_STRUCT_GENERIC_INST: {
            // generic inst 'inherits' from struct
            sc_struct* s = (sc_struct*)n;
            re = add_symbol(r, st, sst, (symbol*)s);
            bool members_public_st = public_st && !is_local_node(n->flags);
            re = add_body_decls(r, st, NULL, &s->sb.sc.body, members_public_st);
            if (re) return re;
            s->id = claim_symbol_id(r, (symbol*)s, public_st);
            return RE_OK;
        }
        case SC_MACRO:
        case SC_FUNC:
        case SC_FUNC_GENERIC: {
            ast_body* b = &((scope*)n)->body;
            ureg param_count;
            sym_param* params;
            if (n->kind == SC_FUNC) {
                sc_func* fn = (sc_func*)n;
                fn->id = ast_node_claim_id(r, n, public_st);
                param_count = fn->fnb.param_count;
                params = fn->fnb.params;
            }
            else if (n->kind == SC_MACRO) {
                sc_macro* m = (sc_macro*)n;
                param_count = m->param_count;
                params = m->params;
            }
            else if (n->kind == SC_FUNC_GENERIC) {
                sc_func_generic* fng = (sc_func_generic*)n;
                param_count = fng->fnb.param_count;
                params = fng->fnb.params;
                assert(
                    fng->generic_param_count == 0 ||
                    b->symtab->owning_node == (ast_elem*)n);
                for (ureg i = 0; i < fng->generic_param_count; i++) {
                    re = add_symbol(
                        r, b->symtab, NULL, (symbol*)&fng->generic_params[i]);
                    if (re) return re;
                }
            }
            symbol_table* tgtst =
                (sst && ast_flags_get_access_mod(n->flags) != AM_LOCAL) ? sst
                                                                        : st;
            symbol** conflict;
            symbol* sym = (symbol*)n;
            sym->declaring_st = st;
            conflict = symbol_table_insert(tgtst, sym);
            if (!conflict) {
                // symbol_table_inc_decl_count(tgtst);
            }
            else {
                sym_func_overloaded* sfo;
                if ((**conflict).node.kind == SC_FUNC) {
                    sfo = (sym_func_overloaded*)pool_alloc(
                        &r->tc->permmem, sizeof(sym_func_overloaded));
                    if (!sfo) return RE_FATAL;
                    sfo->sym.node.kind = SYM_FUNC_OVERLOADED;
                    sfo->sym.node.flags = AST_NODE_FLAGS_DEFAULT;
                    ast_flags_set_access_mod(&sfo->sym.node.flags, AM_PUBLIC);
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
                    sfo = (sym_func_overloaded*)*conflict;
                    sym->next = (symbol*)sfo->overloads;
                    sfo->overloads = (scope*)n;
                }
                else {
                    return report_redeclaration_error(r, sym, *conflict, tgtst);
                }
            }
            // we only do the parameters here because the declaration and
            // use func body vars is strongly ordered
            assert(param_count == 0 || b->symtab->owning_node == (ast_elem*)n);
            for (ureg i = 0; i < param_count; i++) {
                re = add_symbol(r, b->symtab, NULL, (symbol*)&params[i]);
                if (re) return re;
            }
            set_parent_symtabs(&b->symtab, st);
            if (n->kind == SC_FUNC) {
                sc_func* fn = (sc_func*)n;
                if (cstr_eq(fn->fnb.sc.osym.sym.name, COND_KW_CONSTRUCT)) {
                    if (r->module_group_constructor) {
                        src_range_large mgc1_srl;
                        ast_node_get_src_range(
                            (ast_node*)r->module_group_constructor,
                            r->module_group_constructor->fnb.sc.osym.sym
                                .declaring_st,
                            &mgc1_srl);
                        src_range_large mgc2_srl;
                        ast_node_get_src_range(
                            (ast_node*)r->module_group_constructor,
                            r->module_group_constructor->fnb.sc.osym.sym
                                .declaring_st,
                            &mgc2_srl);
                        error_log_report_annotated_twice(
                            r->tc->err_log, ES_PARSER, false,
                            "multiple module constructors in one cyclic module "
                            "group",
                            mgc1_srl.smap, mgc1_srl.start, mgc1_srl.end,
                            "first module constructor here", mgc2_srl.smap,
                            mgc2_srl.start, mgc2_srl.end,
                            "second module constructor here");
                        return RE_ERROR;
                    }
                    pp_resolve_node* pprn = pp_resolve_node_create(
                        r, (ast_node*)fn, st, true, true, true);
                    if (!pprn) return RE_FATAL;
                    fn->fnb.pprn = pprn;
                    r->module_group_constructor = fn;
                }
                else if (cstr_eq(fn->fnb.sc.osym.sym.name, COND_KW_DESTRUCT)) {
                    if (r->module_group_destructor) {
                        src_range_large mgc1_srl;
                        ast_node_get_src_range(
                            (ast_node*)r->module_group_constructor,
                            r->module_group_constructor->fnb.sc.osym.sym
                                .declaring_st,
                            &mgc1_srl);
                        src_range_large mgc2_srl;
                        ast_node_get_src_range(
                            (ast_node*)r->module_group_constructor,
                            r->module_group_constructor->fnb.sc.osym.sym
                                .declaring_st,
                            &mgc2_srl);
                        error_log_report_annotated_twice(
                            r->tc->err_log, ES_PARSER, false,
                            "multiple module destructors in one cyclic module "
                            "group",
                            mgc1_srl.smap, mgc1_srl.start, mgc1_srl.end,
                            "first module destructor here", mgc2_srl.smap,
                            mgc2_srl.start, mgc2_srl.end,
                            "second module destructor here");
                        return RE_ERROR;
                    }
                    pp_resolve_node* pprn = pp_resolve_node_create(
                        r, (ast_node*)fn, st, true, true, true);
                    if (!pprn) return RE_FATAL;
                    fn->fnb.pprn = pprn;
                    r->module_group_destructor = fn;
                }
            }
            return RE_OK;
        }
        case SYM_IMPORT_GROUP: {
            sym_import_group* ig = (sym_import_group*)n;
            ig->osym.sym.declaring_st = st;
            if (ig->osym.sym.name) {
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
            v->var_id = claim_symbol_id(r, (symbol*)v, public_st);
            return RE_OK;
        }
        case STMT_PASTE_EVALUATION: {
            stmt_paste_evaluation* spe = (stmt_paste_evaluation*)n;
            for (ast_node** e = spe->body.elements; *e; e++) {
                re =
                    add_ast_node_decls(r, spe->body.symtab, sst, *e, public_st);
                if (re) return re;
            }
            return RE_OK;
        }
        case SYM_NAMED_USE: {
            return add_symbol(r, st, sst, (symbol*)n);
        }
        default:
            assert(false); // unknown node_kind
            return RE_FATAL;
    }
    assert(false);
    return RE_FATAL;
}
static inline resolve_error parse_int_literal(
    resolver* r, expr_literal* lit, symbol_table* st, ureg* result,
    bool* negative)
{
    char* str = lit->value.str;
    if (*str == '-') {
        *negative = true;
        str++;
    }
    else {
        if (*str == '+') str++;
        *negative = false;
    }
    ureg res = 0;
    ureg digit_val = 1;
    bool overflow = false;
    while (*str) {
        if (*str < '0' || *str > '9') {
            assert(false); // should have ben caught by the lexer
            return RE_ERROR;
        }
        ureg digit = (*str - '0') * digit_val;
        if (UREG_MAX - res < digit) {
            overflow = true;
            break;
        }
        res += digit;
        digit_val *= 10;
        str++;
    }
    if (!overflow) {
        if (*negative) {
            if (res > SREG_MAX) {
                overflow = true;
            }
            else {
                *(sreg*)result = -(sreg)res;
            }
        }
        else {
            *result = res;
        }
        if (!overflow) return RE_OK;
    }
    src_range_large srl;
    ast_node_get_src_range((ast_node*)lit, st, &srl);
    error_log_report_annotated(
        r->tc->err_log, ES_RESOLVER, false, "integer literal overflow",
        srl.smap, srl.start, srl.end, "in this integer literal");
    return RE_ERROR;
}
static resolve_error evaluate_array_bounds(
    resolver* r, expr_array_type* ad, symbol_table* st, ureg* res)
{
    ast_elem* t;
    resolve_error re = resolve_ast_node(r, ad->length_spec, st, NULL, &t);
    if (re) return re;
    bool negative = false;
    bool incompatible = false;
    if (ad->length_spec->kind == EXPR_LITERAL) {
        expr_literal* lit = (expr_literal*)ad->length_spec;
        if (lit->node.pt_kind == PT_UINT || lit->node.pt_kind == PT_INT) {
            resolve_error re = parse_int_literal(r, lit, st, res, &negative);
            if (re) return re;
        }
        else {
            incompatible = true;
        }
    }
    else if (ad->length_spec->kind == EXPR_PP) {
        expr_pp* epp = (expr_pp*)ad->length_spec;
        if (!epp->result) return RE_UNREALIZED_COMPTIME;
        // HACK
        if (ctypes_unifiable(epp->ctype, (ast_elem*)&PRIMITIVES[PT_UINT])) {
            *res = *(ureg*)epp->result;
        }
        else if (ctypes_unifiable(epp->ctype, (ast_elem*)&PRIMITIVES[PT_INT])) {
            sreg i = *(sreg*)epp->result; // HACK
            if (i < 0) negative = true;
            *res = (ureg)i;
        }
        else {
            incompatible = true;
        }
    }
    else {
        incompatible = true;
    }
    if (incompatible) {
        src_range_large bounds_srl;
        src_range_large array_srl;
        ast_node_get_src_range(ad->length_spec, st, &bounds_srl);
        ast_node_get_src_range((ast_node*)ad, st, &array_srl);
        // TODO: different error for negative values
        error_log_report_annotated_twice(
            r->tc->err_log, ES_RESOLVER, false, "invalid type for array bounds",
            bounds_srl.smap, bounds_srl.start, bounds_srl.end, "expected uint",
            array_srl.smap, array_srl.start, array_srl.end,
            "in the array bounds for this array");
    }
    if (negative) {
        src_range_large bounds_srl;
        src_range_large array_srl;
        ast_node_get_src_range(ad->length_spec, st, &bounds_srl);
        ast_node_get_src_range((ast_node*)ad, st, &array_srl);
        error_log_report_annotated_twice(
            r->tc->err_log, ES_RESOLVER, false,
            "array length can't be negative", bounds_srl.smap, bounds_srl.start,
            bounds_srl.end, "expected positive integer", array_srl.smap,
            array_srl.start, array_srl.end,
            "in the array bounds for this array");
        return RE_ERROR;
    }
    return RE_OK;
}
resolve_error add_body_decls(
    resolver* r, symbol_table* parent_st, symbol_table* shared_st, ast_body* b,
    bool public_st)
{
    set_parent_symtabs(&b->symtab, parent_st);
    for (ast_node** n = b->elements; *n; n++) {
        resolve_error re =
            add_ast_node_decls(r, b->symtab, shared_st, *n, public_st);
        assert(r->curr_pp_node == NULL);
        if (re) return re;
    }
    return RE_OK;
}
static inline void print_debug_info(resolver* r, char* flavortext)
{
    tprintf("%s {", flavortext);
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
    if (a->kind == TYPE_ARRAY && b->kind == TYPE_ARRAY) {
        type_array* aa = (type_array*)a;
        type_array* ab = (type_array*)b;
        return (aa->length == ab->length) &&
               ctypes_unifiable(
                   aa->slice_type.ctype_members, ab->slice_type.ctype_members);
    }
    if (b == (ast_elem*)&PRIMITIVES[PT_UNDEFINED] ||
        (ast_elem*)&PRIMITIVES[PT_DEFINED]) {
        return true;
    }
    return false; // TODO
    /*
    switch (a->kind) {
        case TYPE_MODIFIERS:
    }
     */
}
resolve_error operator_func_applicable(
    resolver* r, sc_func* f, ast_elem* lhs, ast_elem* rhs, bool* applicable,
    ast_elem** ctype)
{
    // ensure func has exactly 2 parameters
    // [varargs not allowed for operators]
    if (f->fnb.param_count != 2) {
        *applicable = false;
        return RE_OK;
    }
    ast_elem* param_ctype;
    resolve_error re = resolve_param(r, &f->fnb.params[0], false, &param_ctype);
    if (re) return re;
    if (!ctypes_unifiable(lhs, param_ctype)) {
        *applicable = false;
        return RE_OK;
    }
    re = resolve_param(r, &f->fnb.params[0], false, &param_ctype);
    if (re) return re;
    if (!ctypes_unifiable(rhs, param_ctype)) {
        *applicable = false;
        return RE_OK;
    }
    if (ctype) *ctype = f->fnb.return_ctype;
    *applicable = true;
    return RE_OK;
}
resolve_error overload_applicable(
    resolver* r, ast_elem** call_arg_types, ureg arg_count, scope* overload,
    bool* applicable, ast_elem** ctype)
{
    sc_macro* m =
        (overload->osym.sym.node.kind == SC_MACRO) ? (sc_macro*)overload : NULL;
    sc_func* fn = !m ? (sc_func*)overload : NULL;
    ureg param_count = m ? m->param_count : fn->fnb.param_count;
    sym_param* params = m ? m->params : fn->fnb.params;
    // works cause varags are not in the lang yet
    if (param_count != arg_count) {
        *applicable = false;
        return RE_OK;
    }
    for (ureg i = 0; i < arg_count; i++) {
        ast_elem* ctype;
        resolve_error re = resolve_param(r, &params[i], false, &ctype);
        if (re) return re;
        if (!ctypes_unifiable(ctype, call_arg_types[i])) {
            *applicable = false;
            return RE_OK;
        }
    }
    *applicable = true;
    if (fn) {
        if (!ast_flags_get_resolved(fn->fnb.sc.osym.sym.node.flags)) {
            resolve_error re = resolve_ast_node(
                r, fn->fnb.return_type, overload->osym.sym.declaring_st,
                &fn->fnb.return_ctype, NULL);
            if (re) return re;
        }
        if (ctype) *ctype = fn->fnb.return_ctype;
    }
    else {
        // TODO: allow non void macros
        *ctype = VOID_ELEM;
    }
    return RE_OK;
}

static inline resolve_error resolve_macro_call(
    resolver* r, expr_macro_call* emc, symbol_table* st, ast_elem** value,
    ast_elem** ctype)
{
    assert(false); // TODO
    return RE_FATAL;
}
static inline resolve_error resolve_no_block_macro_call(
    resolver* r, expr_call* nbmc, symbol_table* st, sc_macro* m,
    ast_elem** ctype)
{
    assert(false); // TODO
    return RE_FATAL;
}

// for ex. wth foo::bar() func_st is foo's st, st is the table to look up
// args
resolve_error resolve_func_call(
    resolver* r, expr_call* c, symbol_table* st, char* func_name,
    symbol_table* func_st, ast_elem** ctype)
{
    ast_elem** call_arg_types =
        sbuffer_append(&r->temp_stack, c->arg_count * sizeof(ast_elem*));
    resolve_error re = RE_OK;
    for (ureg i = 0; i < c->arg_count; i++) {
        re = resolve_ast_node(r, c->args[i], st, NULL, &call_arg_types[i]);
        if (re) return re;
    }
    scope* tgt;
    symbol_lookup_iterator sli;
    re = symbol_lookup_iterator_init(
        &sli, r, func_st, NULL, st, func_name, false, true);
    bool applicable = false;
    if (re) {
        sbuffer_remove_back(&r->temp_stack, c->arg_count * sizeof(ast_elem*));
        return re;
    }
    bool overload_existant = false;
    while (true) {
        symbol* sym;
        re = symbol_lookup_iterator_next(&sli, &sym);
        if (re) {
            sbuffer_remove_back(
                &r->temp_stack, c->arg_count * sizeof(ast_elem*));
            return re;
        }
        if (sym == NULL) {
            if (overload_existant) break;
            // we use st instead of func_st here because thats the scope that
            // the call is in
            sbuffer_remove_back(
                &r->temp_stack, c->arg_count * sizeof(ast_elem*));
            return report_unknown_symbol(r, c->lhs, st);
        }
        overload_existant = true;
        if (sym->node.kind == SC_FUNC) {
            re = overload_applicable(
                r, call_arg_types, c->arg_count, (scope*)sym, &applicable,
                ctype);
            if (applicable) {
                // TODO: for now we just pick the first applicable overload
                // instead of
                // having some sort of matching heuristic
                tgt = (scope*)sym;
                break;
            }
        }
        else {
            // TODO: generic overload resolution and instantation selection
            assert(sym->node.kind == SC_FUNC_GENERIC);
            assert(false);
            re = RE_FATAL;
        }
    }
    sbuffer_remove_back(&r->temp_stack, c->arg_count * sizeof(ast_elem*));
    if (!applicable) {
        src_range_large srl;
        ast_node_get_src_range(c->lhs, st, &srl);
        error_log_report_annotated(
            r->tc->err_log, ES_RESOLVER, false, "no matching overload",
            srl.smap, srl.start, srl.end,
            "no available overload is applicable for the given arguments");
        return RE_ERROR;
    }
    if (re) return re;
    if (tgt->osym.sym.node.kind == SC_MACRO) {
        c->node.kind = EXPR_NO_BLOCK_MACRO_CALL;
        re = resolve_no_block_macro_call(r, c, st, (sc_macro*)tgt, ctype);
    }
    else {
        c->target.fn = (sc_func*)tgt;
        ast_flags_set_resolved(&c->node.flags);
        // we sadly need to do this so the resolved flag means
        //"ready to emit and run" which we need for the pp
        re = resolve_func_from_call(r, (sc_func*)tgt, ctype);
    }
    return re;
}

resolve_error
resolve_call(resolver* r, expr_call* c, symbol_table* st, ast_elem** ctype)
{
    if (c->lhs->kind == EXPR_IDENTIFIER) {
        return resolve_func_call(
            r, c, st, ((expr_identifier*)c->lhs)->value.str, st, ctype);
    }
    if (c->lhs->kind == EXPR_SCOPE_ACCESS) {
        expr_scope_access* esa = (expr_scope_access*)c->lhs;
        ast_elem* esa_lhs;
        symbol_table* lhs_st;
        resolve_error re = resolve_ast_node(r, esa->lhs, st, &esa_lhs, NULL);
        if (re) return re;
        assert(ast_elem_is_symbol(esa_lhs));
        re = get_resolved_symbol_symtab(r, (symbol*)esa_lhs, &lhs_st);
        if (re) return re;
        return resolve_func_call(r, c, st, esa->target.name, lhs_st, ctype);
    }
    if (c->lhs->kind == EXPR_MEMBER_ACCESS) {
        expr_member_access* esa = (expr_member_access*)c->lhs;
        ast_elem* esa_lhs;
        ast_elem* esa_lhs_ctype;
        resolve_error re =
            resolve_ast_node(r, esa->lhs, st, &esa_lhs, &esa_lhs_ctype);
        if (re) return re;
        assert(ast_elem_is_struct(esa_lhs_ctype)); // TODO: error
        ast_flags_set_instance_member(&c->node.flags);
        return resolve_func_call(
            r, c, st, esa->target.name,
            ((sc_struct*)esa_lhs_ctype)->sb.sc.body.symtab, ctype);
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
    resolver* r, expr_op_unary* ou, symbol_table* st, ast_elem** value,
    ast_elem** ctype)
{
    ast_elem *child_type, *child_value;
    resolve_error re =
        resolve_ast_node(r, ou->child, st, &child_value, &child_type);
    if (re) return re;

    if (child_type == TYPE_ELEM || child_type == GENERIC_TYPE_ELEM) {
        ast_flags_set_type_operator(&ou->node.flags);
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
        else if (child_type->kind != SYM_PRIMITIVE) {
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
    if (expr->kind == EXPR_ACCESS) return true;
    return false;
}
resolve_error choose_binary_operator_overload(
    resolver* r, expr_op_binary* ob, symbol_table* st, ast_elem** value,
    ast_elem** ctype)
{
    ast_elem *lhs_ctype, *rhs_ctype;
    bool unrealized_comptime = false;
    resolve_error re = resolve_ast_node(r, ob->lhs, st, NULL, &lhs_ctype);
    // evaluate rhs even if lhs is #'ed so we can eval #foo + #bar
    // in one run_pp
    if (re == RE_UNREALIZED_COMPTIME) {
        unrealized_comptime = true;
    }
    else if (re) {
        return re;
    }
    re = resolve_ast_node(r, ob->rhs, st, NULL, &rhs_ctype);
    if (re) return re;
    if (unrealized_comptime) return RE_UNREALIZED_COMPTIME;

    if (ob->node.op_kind == OP_ASSIGN) {
        assert(is_lvalue(ob->lhs)); // TODO: error
        assert(ctypes_unifiable(lhs_ctype, rhs_ctype)); // TODO: error
        if (ctype) *ctype = VOID_ELEM;
        ob->op = lhs_ctype;
        return RE_OK;
    }
    if (lhs_ctype->kind == SYM_PRIMITIVE && rhs_ctype->kind == SYM_PRIMITIVE) {
        // TODO: proper inbuild operator resolution
        // maybe create a cat_prim_kind and switch over cat'ed lhs and rhs
        ob->op = lhs_ctype;
        if (ctype) *ctype = lhs_ctype;
        return RE_OK;
    }
    symbol* s;
    symbol_lookup_iterator sli;
    re = symbol_lookup_iterator_init(
        &sli, r, st, NULL, st, op_to_str(ob->node.op_kind), false, true);
    if (re) return re;
    while (true) {
        re = symbol_lookup_iterator_next(&sli, &s);
        if (re) return re;
        if (!s) break;
        bool applicable;
        if (s->node.kind == SC_FUNC) {
            re = operator_func_applicable(
                r, (sc_func*)s, lhs_ctype, rhs_ctype, &applicable, ctype);
            if (re) return re;
            if (applicable) return RE_OK;
        }
        else {
            assert(false); // TODO: error
        }
    }
    return report_unknown_symbol(r, (ast_node*)ob, st);
}
ast_elem* get_resolved_symbol_ctype(symbol* s)
{
    switch (s->node.kind) {
        case SYM_VAR:
        case SYM_VAR_INITIALIZED: return ((sym_var*)s)->ctype; break;
        case SYM_NAMED_USE: assert(false); return NULL; // TODO
        case SYM_PRIMITIVE: assert(false); return NULL; // would be ctype "Type"
        default: return (ast_elem*)s;
    }
}
ast_elem** get_break_target_ctype(ast_node* n)
{
    switch (n->kind) {
        case EXPR_BLOCK: return &((expr_block*)n)->ebb.ctype;
        case EXPR_IF: return &((expr_if*)n)->ctype;
        case EXPR_LOOP: return &((expr_loop*)n)->ebb.ctype;
        default: return NULL;
    }
}
// might return null if inside module frame
ast_node* get_current_ebb(resolver* r)
{
    if (ast_elem_is_paste_evaluation((ast_elem*)r->curr_block_owner)) {
        return ((paste_evaluation*)r->curr_block_owner)->parent_ebb;
    }
    else if (
        r->curr_block_owner->kind == SC_FUNC ||
        r->curr_block_owner->kind == SC_FUNC_GENERIC) {
        return r->curr_block_owner;
    }
    else if (ast_elem_is_struct((ast_elem*)r->curr_block_owner)) {
        return r->curr_block_owner;
    }
    else if (ast_elem_is_module_frame((ast_elem*)r->curr_block_owner)) {
        return NULL;
    }
    else {
        assert(ast_elem_is_expr_block_base((ast_elem*)r->curr_block_owner));
        return r->curr_block_owner;
    }
}
resolve_error resolve_break_target(
    resolver* r, char* name, expr_block_base** tgt_ebb, ast_elem*** ctype)
{
    // TODO: error
    ast_node* ebb_or_func = get_current_ebb(r);
    while (ebb_or_func) {
        if (ebb_or_func->kind == SC_FUNC ||
            ebb_or_func->kind == SC_FUNC_GENERIC) {
            assert(false); // TODO error
        }
        assert(ast_elem_is_expr_block_base((ast_elem*)ebb_or_func));
        expr_block_base* ebb = (expr_block_base*)ebb_or_func;
        if ((name && ebb->name && cstr_eq(ebb->name, name)) ||
            (name == ebb->name)) {
            *tgt_ebb = ebb;
            *ctype = get_break_target_ctype((ast_node*)ebb);
            return RE_OK;
        }
        ebb_or_func = ebb->parent;
    }
    assert(false); // TODO: error
    return RE_ERROR;
}
resolve_error
get_resolved_symbol_symtab(resolver* r, symbol* s, symbol_table** tgt_st)
{
    if (ast_elem_is_scope((ast_elem*)s)) {
        // TODO: handle access change here
        *tgt_st = ((scope*)s)->body.symtab;
        return RE_OK;
    }
    if (s->node.kind == SYM_IMPORT_SYMBOL) {
        return get_resolved_symbol_symtab(
            r, ((sym_import_symbol*)s)->target.sym, tgt_st);
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
            // symbol_table_inc_decl_count(pst);
        }
        else {
            symbol_table_insert_use(
                pst, AM_PUBLIC, (ast_node*)s,
                ((sym_import_module*)s)->target->symtab);
        }
    } while (next);
    ip->children.symtab = pst;
    ast_flags_set_resolved(&ip->osym.sym.node.flags);
    return RE_OK;
}
resolve_error
resolve_param(resolver* r, sym_param* p, bool generic, ast_elem** ctype)
{
    if (ast_flags_get_resolved(p->sym.node.flags)) {
        if (ctype) *ctype = p->ctype;
        return RE_OK;
    }
    ast_flags_set_resolving(&p->sym.node.flags);
    resolve_error re;
    if (p->type) {
        re = resolve_ast_node(r, p->type, p->sym.declaring_st, &p->ctype, NULL);
        if (re) return re;
        if (p->default_value) {
            ast_elem* val;
            re = resolve_ast_node(
                r, (ast_node*)p->default_value, p->sym.declaring_st, NULL,
                &val);
            if (re) return re;
            if (!ctypes_unifiable(p->ctype, val)) {
                assert(false); // TODO: error
            }
        }
    }
    else {
        if (!p->default_value) {
            if (generic) {
                p->ctype = GENERIC_TYPE_ELEM;
            }
            else {
                assert(false); // would cause a parser error
            }
        }
        else {
            re = resolve_ast_node(
                r, p->default_value, p->sym.declaring_st, NULL, &p->ctype);
            if (re) return re;
        }
    }
    if (ctype) *ctype = p->ctype;
    ast_flags_set_resolved(&p->sym.node.flags);
    return PE_OK;
}
resolve_error resolver_lookup_single(
    resolver* r, symbol_table* st, sc_struct* struct_inst,
    symbol_table* looking_st, const char* tgt_name, symbol** res,
    symbol** ambiguity)
{
    symbol_lookup_iterator sli;
    resolve_error re = symbol_lookup_iterator_init(
        &sli, r, st, struct_inst, looking_st, tgt_name, true, true);
    if (re) return re;
    re = symbol_lookup_iterator_next(&sli, res);
    if (re) return re;
    return symbol_lookup_iterator_next(&sli, ambiguity);
}
resolve_error resolve_scoped_identifier(
    resolver* r, expr_scope_access* esa, symbol_table* st, ast_elem** value,
    ast_elem** ctype)
{
    resolve_error re;
    symbol_table* lhs_st;
    ast_elem* lhs_val;
    re = resolve_ast_node(r, esa->lhs, st, &lhs_val, NULL);
    if (re) return re;
    assert(lhs_val != NULL && ast_elem_is_symbol(lhs_val)); // TODO: log error
    re = get_resolved_symbol_symtab(r, (symbol*)lhs_val, &lhs_st);
    if (re) return re;
    symbol* idf;
    symbol* amb;
    re = resolver_lookup_single(
        r, lhs_st, NULL, st, esa->target.name, &idf, &amb);
    if (re) return re;
    if (!idf) {
        return report_unknown_symbol(r, (ast_node*)esa, st);
    }
    if (amb) {
        assert(false); // TODO report ambiguity error
    }
    ast_elem* idf_val;

    resolve_ast_node(r, (ast_node*)idf, idf->declaring_st, &idf_val, ctype);
    assert(idf_val != NULL && ast_elem_is_symbol(idf_val)); // TODO: log error
    esa->target.sym = idf;
    if (value) *value = idf_val;
    ast_flags_set_resolved(&esa->node.flags);
    return RE_OK;
}
access_modifier check_member_access(symbol_table* st, scope* tgt)
{
    return AM_LOCAL; // TODO
}
resolve_error resolve_expr_member_accesss(
    resolver* r, expr_member_access* ema, symbol_table* st, ast_elem** value,
    ast_elem** ctype)
{
    ast_elem* lhs_type;
    resolve_error re = resolve_ast_node(r, ema->lhs, st, NULL, &lhs_type);
    if (re) return re;
    if (!ast_elem_is_struct(lhs_type)) { // TODO: pointers
        // TODO: errror
        assert(false);
        return RE_FATAL;
    }
    symbol* mem;
    symbol* amb;
    symbol_table* struct_st = ((scope*)lhs_type)->body.symtab;
    re = resolver_lookup_single(
        r, struct_st, (sc_struct*)lhs_type, st, ema->target.name, &mem, &amb);
    if (re) return re;
    if (!mem) return report_unknown_symbol(r, (ast_node*)ema, struct_st);
    if (amb) {
        assert(false); // TODO: report ambiguity
    }
    ema->target.sym = mem;
    re = resolve_ast_node(r, (ast_node*)mem, mem->declaring_st, value, ctype);
    if (re) return re;
    ast_flags_set_resolved(&ema->node.flags);
    return RE_OK;
}
static inline resolve_error resolve_var(
    resolver* r, symbol_table* st, sym_var* v, ast_elem** value,
    ast_elem** ctype)
{
    resolve_error re;
    bool comptime = ast_flags_get_comptime(v->osym.sym.node.flags);
    bool public_symbol = symbol_table_is_public(st);
    bool pub_rt_var = !comptime && public_symbol;
    sym_var* prev_var_decl;
    pp_resolve_node* prev_var_pp_node;
    ast_node* prev_var_decl_block_owner;

    if (comptime) {
        v->pprn = pp_resolve_node_create(
            r, (ast_node*)v, v->osym.sym.declaring_st, false, true, false);
        if (!v->pprn) return RE_FATAL;
    }
    // we only need this in public scope because for function scopes / ordered
    // scopes we will never reach following lines if the var above is still
    // unresolved
    else if (pub_rt_var) {
        prev_var_decl = r->curr_var_decl;
        prev_var_pp_node = r->curr_var_pp_node;
        prev_var_decl_block_owner = r->curr_var_decl_block_owner;
        r->curr_var_decl = v;
        r->curr_var_pp_node = NULL;
        r->curr_var_decl_block_owner = r->curr_block_owner;
    }
    if (v->osym.sym.node.kind == SYM_VAR) {
        ast_elem* type;
        re = resolve_ast_node(r, v->type, st, &type, NULL);
        if (!re) v->ctype = type;
    }
    else {
        sym_var_initialized* vi = (sym_var_initialized*)v;
        if (vi->var.type) {
            re = resolve_ast_node(r, vi->var.type, st, &vi->var.ctype, NULL);
            if (!re) {
                ast_elem* val_type;
                re =
                    resolve_ast_node(r, vi->initial_value, st, NULL, &val_type);
                if (!re) {
                    if (!ctypes_unifiable(vi->var.ctype, val_type)) {
                        src_range_large vi_type_srl, vi_val_srl;
                        ast_node_get_src_range(vi->var.type, st, &vi_type_srl);
                        ast_node_get_src_range(
                            vi->initial_value, st, &vi_val_srl);
                        error_log_report_annotated_twice(
                            r->tc->err_log, ES_RESOLVER, false,
                            "type missmatch in variable declaration",
                            vi_type_srl.smap, vi_type_srl.start,
                            vi_type_srl.end, "declared type here",
                            vi_val_srl.smap, vi_val_srl.start, vi_val_srl.end,
                            "doesn' t match type of the initial value");
                        re = RE_TYPE_MISSMATCH;
                    }
                }
            }
        }
        else {
            re = resolve_ast_node(
                r, vi->initial_value, st, NULL, &vi->var.ctype);
            // this could become needed again once we support typeof,
            // it allows one retry in cases of a variable initially
            // assigned to a self referential expr block
            /*  if (re == RE_TYPE_LOOP && r->type_loop_start == n &&
                 !r->retracing_type_loop) {
                 if (vi->var.ctype) {
                     ast_flags_set_resolved(&n->flags);
                     stack_clear(&r->error_stack);
                     re = resolve_ast_node(
                         r, vi->initial_value, st, NULL,
                         &vi->var.ctype);
                }
                else{
                    report_type_loop(r, n, st);
                }
                }
            }*/
        }
    }
    if (pub_rt_var) {
        v->pprn = r->curr_var_pp_node;
        r->curr_var_decl = prev_var_decl;
        r->curr_var_pp_node = prev_var_pp_node;
        r->curr_var_decl_block_owner = prev_var_decl_block_owner;
    }
    if (v->pprn) {
        resolve_error re_prev = re;
        re = curr_pp_block_add_child(r, v->pprn);
        if (re) return re;
        re = pp_resolve_node_activate(r, v->pprn, re == RE_OK);
        if (re) return re;
        re = re_prev;
    }
    if (re) return re;
    ast_flags_set_resolved(&v->osym.sym.node.flags);
    RETURN_RESOLVED(value, ctype, v, v->ctype);
}
static inline resolve_error resolve_return_target(resolver* r, ast_node** tgt)
{
    ast_node* ebb_or_func = get_current_ebb(r);
    while (ebb_or_func) {
        if (ebb_or_func->kind == SC_FUNC ||
            ebb_or_func->kind == SC_FUNC_GENERIC) {
            *tgt = ebb_or_func;
            return RE_OK;
        }
        assert(ast_elem_is_expr_block_base((ast_elem*)ebb_or_func));
        ebb_or_func = ((expr_block_base*)ebb_or_func)->parent;
    }
    assert(false); // TODO: error
    return RE_ERROR;
}

static inline resolve_error
resolve_return(resolver* r, symbol_table* st, expr_return* er)
{
    resolve_error re =
        resolve_ast_node(r, er->value, st, NULL, &er->value_ctype);
    if (re) return re;
    ast_flags_set_resolved(&er->node.flags);
    ast_elem* tgt_type;
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
    re = resolve_return_target(r, &er->target);
    if (re) return re;
    if (ast_elem_is_func_base((ast_elem*)er->target)) {
        // must already be resolved since parenting function
        tgt_type = ((sc_func_base*)er->target)->return_ctype;
    }
    else {
        assert(false);
    }
    if (!ctypes_unifiable(er->value_ctype, tgt_type)) {
        ureg vstart, vend;
        ast_node_get_bounds(er->value, &vstart, &vend);
        error_log_report_annotated_twice(
            r->tc->err_log, ES_RESOLVER, false, "type missmatch",
            ast_node_get_smap((ast_node*)er, st), vstart, vend,
            "the type returned from here doesn't match the target "
            "scope's",
            // TODO: st is kinda wrong here
            ast_node_get_smap((ast_node*)er->target, st),
            src_range_get_start(er->target->srange),
            src_range_get_end(er->target->srange), "target scope here");
        return RE_TYPE_MISSMATCH;
    }
    return RE_OK;
}
static inline resolve_error
resolve_break(resolver* r, symbol_table* st, expr_break* b)
{
    resolve_error re = resolve_ast_node(r, b->value, st, NULL, &b->value_ctype);
    if (re) return re;
    ast_flags_set_resolved(&b->node.flags);
    ast_elem** tgt_ctype;
    re = resolve_break_target(r, b->target.label, &b->target.ebb, &tgt_ctype);
    if (re) return re;
    if (*tgt_ctype) {
        if (!ctypes_unifiable(b->value_ctype, *tgt_ctype)) {
            ureg vstart, vend;
            ast_node_get_bounds(b->value, &vstart, &vend);
            error_log_report_annotated_twice(
                r->tc->err_log, ES_RESOLVER, false, "type missmatch",
                ast_node_get_smap((ast_node*)b, st), vstart, vend,
                "the type returned from here doesn't match the target "
                "scope's",
                // TODO: st is kinda wrong here
                ast_node_get_smap((ast_node*)b->target.ebb, st),
                src_range_get_start(b->target.ebb->node.srange),
                src_range_get_end(b->target.ebb->node.srange),
                "target scope here");
            return RE_TYPE_MISSMATCH;
        }
    }
    else {
        *tgt_ctype = b->value_ctype;
    }
    return RE_OK;
}

static inline resolve_error resolve_identifier(
    resolver* r, symbol_table* st, expr_identifier* e, ast_elem** value,
    ast_elem** ctype)
{
    symbol* sym;
    symbol* amb;
    resolve_error re =
        resolver_lookup_single(r, st, NULL, st, e->value.str, &sym, &amb);
    if (re) return re;
    if (!sym) return report_unknown_symbol(r, (ast_node*)e, st);
    if (amb) {
        assert(false); // TODO: report ambiguity
    }
    re = resolve_ast_node(
        r, (ast_node*)sym, sym->declaring_st, (ast_elem**)&sym, ctype);
    if (re == RE_DIFFERENT_PP_LEVEL) {
        sym = stack_pop(&r->error_stack);
        assert(ast_elem_is_symbol((ast_elem*)sym));
        src_range_large id_sr, sym_sr;
        ast_node_get_src_range((ast_node*)e, st, &id_sr);
        ast_node_get_src_range((ast_node*)sym, sym->declaring_st, &sym_sr);
        error_log_report_annotated_twice(
            r->tc->err_log, ES_RESOLVER, false,
            "cannot access variable of a different preprocessing "
            "level",
            id_sr.smap, id_sr.start, id_sr.end, "usage here", sym_sr.smap,
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
    resolver* r, symbol_table* st, expr_if* ei, ast_elem** value,
    ast_elem** ctype)
{
    ast_elem *ctype_if, *ctype_else;
    bool cond_type_loop = false;
    bool if_branch_type_loop = false;
    bool else_branch_type_loop = false;
    resolve_error re = resolve_ast_node(r, ei->condition, st, NULL, NULL);
    if (re == RE_TYPE_LOOP) {
        cond_type_loop = true;
    }
    else {
        if (re) return re;
    }
    re = resolve_ast_node(r, ei->if_body, st, NULL, &ctype_if);
    if (re == RE_TYPE_LOOP) {
        if_branch_type_loop = true;
    }
    else {
        if (re) return re;
    }
    re = resolve_ast_node(r, ei->else_body, st, NULL, &ctype_else);
    if (re == RE_TYPE_LOOP) {
        else_branch_type_loop = true;
        if (if_branch_type_loop || ctype_if == UNREACHABLE_ELEM) {
            return RE_TYPE_LOOP;
        }
    }
    else {
        if (re) return re;
    }
    if (!ei->else_body) {
        // TODO: maybe check for breaks and cause an error here
        ei->ctype = VOID_ELEM;
    }
    else if (if_branch_type_loop || ctype_if == UNREACHABLE_ELEM) {
        ei->ctype = ctype_else; // TODO: this could lead to void instead
    }
    else if (else_branch_type_loop || ctype_else == UNREACHABLE_ELEM) {
        ei->ctype = ctype_if;
    }
    else if (!ctypes_unifiable(ctype_if, ctype_else)) {
        src_range_large srl;
        ast_node_get_src_range((ast_node*)ei, st, &srl);
        error_log_report_annotated(
            r->tc->err_log, ES_RESOLVER, false, "type missmatch", srl.smap,
            srl.start, srl.end,
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
    resolver* r, symbol_table* st, expr_pp* ppe, ast_elem** value,
    ast_elem** ctype)
{
    if (ppe->pprn && ppe->pprn->pending_pastes) return RE_UNREALIZED_COMPTIME;
    bool is_stmt = !ast_flags_get_pp_expr_res_used(ppe->node.flags);
    if (ppe->ctype == PASTED_EXPR_ELEM) {
        *ppe->result_buffer.paste_result.last_next = NULL;
        parse_error pe;
        if (!is_stmt) {
            pe =
                parser_parse_paste_expr(&r->tc->p, ppe, st, get_current_ebb(r));
        }
        else {
            ast_body* b = ast_elem_get_body((ast_elem*)r->curr_block_owner);
            pe = parser_parse_paste_stmt(
                &r->tc->p, ppe, &b->symtab, get_current_ebb(r),
                b->symtab->owning_node == (ast_elem*)r->curr_block_owner);
        }
        if (pe) return RE_ERROR;
        return resolve_ast_node_raw(r, (ast_node*)ppe, st, value, ctype);
    }
    pp_resolve_node* pprn = NULL;
    resolve_error re;
    if (ppe->pprn) {
        pprn = ppe->pprn;
    }
    else {
        pprn =
            pp_resolve_node_create(r, (ast_node*)ppe, st, is_stmt, true, false);
        if (!pprn) return RE_FATAL;
        ppe->pprn = pprn;
        if (r->curr_pp_node) {
            if (curr_pprn_run_after(r, pprn)) return RE_FATAL;
        }
        // disable run_when_ready even if we don't add this immediately
        // to avoid it being run once the deps are done
        // for structs the children run individually, so we actually want it to
        // run
        if (!ast_elem_is_struct((ast_elem*)r->curr_block_owner)) {
            re = get_curr_block_pprn(r, &pprn->parent);
            if (pprn->parent) {
                pprn->run_when_ready = false;
            }
            if (re) return re;
        }
    }
    pp_resolve_node* parent_pprn = r->curr_pp_node;
    r->curr_pp_node = pprn;
    re = resolve_ast_node(r, ppe->pp_expr, st, value, &ppe->ctype);
    r->curr_pp_node = parent_pprn;
    if (pprn->pending_pastes) {
        if (ppe->ctype != VOID_ELEM) {
            src_range_large pp_srl;
            ast_node_get_src_range((ast_node*)ppe, st, &pp_srl);
            // TODO: report where the value is coming from
            error_log_report_annotated(
                r->tc->err_log, ES_RESOLVER, false,
                "pasting preprocessor expression can't return a value",
                pp_srl.smap, pp_srl.start, pp_srl.end, "in this pp expression");
            return RE_TYPE_MISSMATCH;
        }
        ppe->ctype = PASTED_EXPR_ELEM;
    }
    if (re == RE_OK) {
        if (curr_pp_block_add_child(r, pprn)) return RE_FATAL;
        if (pp_resolve_node_activate(r, pprn, true)) {
            return RE_FATAL;
        }
    }
    if (re) return re;
    if (ctype) *ctype = ppe->ctype;
    if (pprn->pending_pastes) return RE_UNREALIZED_COMPTIME;
    ast_flags_set_resolved(&ppe->node.flags);
    // HACK: this is a really inefficient way of doing things
    // in cases like foo(bar(baz(#x))) we unnecessarily repeat a
    // whole lot of resoution
    // better would be to add the pprn to the parent's list immediately
    // and check for ready in the parent
    if (parent_pprn) return RE_UNREALIZED_COMPTIME;
    return RE_OK;
}
static inline resolve_error resolve_expr_paste_str(
    resolver* r, symbol_table* st, expr_paste_str* eps, ast_elem** value,
    ast_elem** ctype)
{
    ast_elem* val_type;
    resolve_error re = resolve_ast_node(r, eps->value, st, NULL, &val_type);
    if (re) return re;
    ast_flags_set_resolved(&eps->node.flags);
    assert(r->curr_pp_node->node->kind == EXPR_PP); // TODO: error
    eps->target = (expr_pp*)r->curr_pp_node->node;
    if (!ctypes_unifiable(val_type, (ast_elem*)&PRIMITIVES[PT_STRING])) {
        src_range_large paste_srl, val_srl;
        ast_node_get_src_range((ast_node*)eps, st, &paste_srl);
        ast_node_get_src_range(eps->value, st, &val_srl);
        error_log_report_annotated_twice(
            r->tc->err_log, ES_RESOLVER, false,
            "incompatible type in paste argument", val_srl.smap, val_srl.start,
            val_srl.end, "paste expects a string as an argument",
            paste_srl.smap, paste_srl.start, paste_srl.end, NULL);
        return RE_TYPE_MISSMATCH;
    }
    r->curr_pp_node->pending_pastes++;
    RETURN_RESOLVED(value, ctype, VOID_ELEM, VOID_ELEM);
}
static inline void report_type_loop(resolver* r, ast_node* n, symbol_table* st);
// the symbol table is not the one that contains the symbol, but the one
// where it was declared and where the type name loopup should start

static inline resolve_error require_module_in_pp(
    resolver* r, ast_node* n, symbol_table* st, mdg_node* mdg,
    atomic_boolean* done, pp_resolve_node** tgt_pprn)
{
    resolve_error re;
    if (*tgt_pprn) {
        if (atomic_boolean_load(done)) {
            re = pp_resolve_node_ready(r, *tgt_pprn, false);
            if (re) return re;
            *tgt_pprn = NULL;
        }
        return RE_OK;
    }
    bool pp_done;
    bool diy = false;
    atomic_boolean_init(done, false);
    rwlock_write(&mdg->lock);
    pp_done = (mdg->stage >= MS_GENERATED);
    // TODO: do this properly
    if (mdg->stage > MS_GENERATED) {
        rwlock_end_write(&mdg->lock);
        return RE_FATAL;
    }
    if (pp_done) {
        if (mdg->ppe_stage == PPES_DONE) {
            pp_done = true;
        }
        else if (mdg->ppe_stage != PPES_RUNNING) {
            diy = true;
            mdg->ppe_stage = PPES_RUNNING;
            list_append(&mdg->notify, NULL, n);
        }
    }
    else {
        if (mdg->ppe_stage == PPES_SKIPPED) {
            diy = true;
            mdg->ppe_stage = PPES_RUNNING;
        }
        else {
            mdg->ppe_stage = PPES_REQUESTED;
            list_append(&mdg->notify, NULL, n);
        }
    }
    rwlock_end_write(&mdg->lock);
    if (!pp_done || diy) {
        pp_resolve_node* pprn =
            pp_resolve_node_create(r, n, st, false, false, false);
        if (!pprn) return PE_FATAL;
        *tgt_pprn = pprn;
        re = pp_resolve_node_activate(r, pprn, false);
        if (re) return re;
    }
    else {
        *tgt_pprn = NULL;
    }
    if (diy) {
        if (tauc_request_pp_module(r->tc->t, mdg)) return RE_FATAL;
    }
    return RE_OK;
}
static inline resolve_error resolve_expr_block(
    resolver* r, expr_block* b, symbol_table* st, ast_elem** value,
    ast_elem** ctype)
{
    bool end_reachable;
    resolve_error re = resolve_expr_body(
        r, st, (ast_node*)b, &b->body, &b->ebb.pprn, &end_reachable);
    if (re == RE_UNREALIZED_COMPTIME) {
        if (b->ebb.ctype || !end_reachable) re = RE_OK;
    }
    if (ctype) *ctype = b->ebb.ctype;
    if (re) return re;
    if (end_reachable) {
        assert(!b->ebb.ctype || b->ebb.ctype == VOID_ELEM); // TODO: error
        b->ebb.ctype = VOID_ELEM;
    }
    else {
        if (!b->ebb.ctype) b->ebb.ctype = UNREACHABLE_ELEM;
    }
    ast_flags_set_resolved(&b->ebb.node.flags);
    assert(!value);
    RETURN_RESOLVED(value, ctype, NULL, b->ebb.ctype);
}
static inline bool is_symbol_kind_overloadable(ast_node_kind k)
{
    switch (k) {
        case SC_FUNC:
        case SC_FUNC_GENERIC:
        case SC_STRUCT:
        case SC_STRUCT_GENERIC:
        case SC_MACRO: return true;
        default: return false;
    }
}
resolve_error resolve_import_symbol(resolver* r, sym_import_symbol* is)
{
    ast_flags* flags = &is->osym.sym.node.flags;
    if (ast_flags_get_resolved(*flags)) return RE_OK;
    if (ast_flags_get_resolving(*flags)) {
        // TOOD: report loop
        assert(false);
    }
    ast_flags_set_resolving(flags);
    resolve_error re;
    symbol_table* decl_st = is->osym.sym.declaring_st;
    symbol_table* tgt_st = is->import_group->parent_mdgn->symtab;
    if (is->import_group->osym.sym.name == NULL) {
        re = resolve_ast_node_raw(
            r, (ast_node*)is->import_group, decl_st, NULL, NULL);
        if (re) return re;
    }
    symbol* sym;
    symbol* amb;
    re = resolver_lookup_single(
        r, tgt_st, NULL, decl_st, is->target.name, &sym, &amb);
    if (re) return re;
    if (!sym) return report_unknown_symbol(r, (ast_node*)is, decl_st);
    if (is_symbol_kind_overloadable(sym->node.kind)) {
        is->target_st = tgt_st;
    }
    else {
        if (amb) {
            assert(false); // TODO: report ambiguity
        }
        if (sym->node.kind == SYM_IMPORT_SYMBOL) {
            sym_import_symbol* tgt_is = (sym_import_symbol*)is;
            re = resolve_import_symbol(r, tgt_is);
            if (re) return re;
            is->target = tgt_is->target;
            is->target_st = tgt_is->target_st;
        }
        else {
            is->target_st = NULL;
            is->target.sym = sym;
        }
    }
    ast_flags_set_resolved(flags);
    return RE_OK;
}
static inline resolve_error resolve_ast_node_raw(
    resolver* r, ast_node* n, symbol_table* st, ast_elem** value,
    ast_elem** ctype)
{
    if (!n) {
        // TODO: investigate where we need this
        RETURN_RESOLVED(value, ctype, VOID_ELEM, VOID_ELEM);
    }
    if (ast_elem_is_module_frame((ast_elem*)n)) {
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
        case SYM_PRIMITIVE: {
            if (!resolved) ast_flags_set_resolved(&n->flags);
            if (value) *value = (ast_elem*)&PRIMITIVES[n->pt_kind];
            if (ctype) {
                switch (n->pt_kind) {
                    case PT_DEFINED:
                    case PT_UNDEFINED: {
                        *ctype = (ast_elem*)&PRIMITIVES[n->pt_kind];
                    } break;
                    default: {
                        *ctype = TYPE_ELEM;
                    } break;
                }
            }
            return RE_OK;
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
            return resolve_identifier(r, st, e, value, ctype);
        }
        case EXPR_CALL: {
            expr_call* c = (expr_call*)n;
            if (resolved) {
                RETURN_RESOLVED(
                    value, ctype, c, c->target.fn->fnb.return_ctype);
            }
            return resolve_call(r, c, st, ctype);
        }
        case EXPR_NO_BLOCK_MACRO_CALL: {
            expr_call* c = (expr_call*)n;
            assert(resolved); // otherwise this would still be a EXPR_CALL
            RETURN_RESOLVED(value, ctype, c, c->target.macro_block->ebb.ctype);
        }
        case SYM_FUNC_OVERLOADED: { // used during import resolution
            assert(!ctype);
            RETURN_RESOLVED(value, ctype, n, NULL);
        }
        case EXPR_CONTINUE:
        case EXPR_OP_UNARY: {
            expr_op_unary* ou = (expr_op_unary*)n;
            if (resolved) {
                if (ou->op->kind == SC_FUNC) {
                    RETURN_RESOLVED(
                        value, ctype, ou, ((sc_func*)ou->op)->fnb.return_ctype);
                }
                if (ast_flags_get_type_operator(ou->node.flags)) {
                    RETURN_RESOLVED(value, ctype, ou->op, TYPE_ELEM);
                }
                RETURN_RESOLVED(value, ctype, ou, ou->op);
            }
            re = choose_unary_operator_overload(r, ou, st, value, ctype);
            if (re) return re;
            ast_flags_set_resolved(&n->flags);
            return RE_OK;
        }
        case EXPR_PARENTHESES: {
            // we set this even on error because we jump through to get the
            // required values anyways, so at least make it tail recursive
            ast_flags_set_resolved(&n->flags);
            return resolve_ast_node(
                r, ((expr_parentheses*)n)->child, st, value, ctype);
        }
        case EXPR_OP_BINARY: {
            expr_op_binary* ob = (expr_op_binary*)n;
            if (resolved) {
                if (ob->op->kind == SYM_PRIMITIVE) {
                    *ctype =
                        (ast_elem*)&PRIMITIVES[((ast_node*)ob->op)->pt_kind];
                }
                else {
                    *ctype = ((sc_func*)ob->op)->fnb.return_ctype;
                }
                return RE_OK;
            }
            re = choose_binary_operator_overload(r, ob, st, value, ctype);
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
            return resolve_expr_member_accesss(r, ema, st, value, ctype);
        }
        case EXPR_SCOPE_ACCESS: {
            expr_scope_access* esa = (expr_scope_access*)n;
            if (resolved) {
                RETURN_RESOLVED(
                    value, ctype, n,
                    get_resolved_symbol_ctype(esa->target.sym));
            }
            return resolve_scoped_identifier(r, esa, st, value, ctype);
        }
        case SC_STRUCT_GENERIC: {
            // sc_struct_generic* sg = (sc_struct_generic*)n;
            if (!resolved) ast_flags_set_resolved(&n->flags);
            // TODO: handle scope escaped pp exprs
            RETURN_RESOLVED(value, ctype, n, GENERIC_TYPE_ELEM);
        }
        case SC_STRUCT:
        case SC_STRUCT_GENERIC_INST: {
            if (resolved) RETURN_RESOLVED(value, ctype, n, TYPE_ELEM);
            return resolve_struct(r, (sc_struct*)n, value, ctype);
        }
        case SC_FUNC:
        case SC_FUNC_GENERIC: {
            if (value) *value = (ast_elem*)n;
            if (ctype) *ctype = VOID_ELEM;
            if (resolved) return RE_OK;
            return resolve_func(r, (sc_func_base*)n, NULL);
        }
        case SYM_IMPORT_PARENT: {
            if (!resolved) {
                re = resolve_import_parent(r, (sym_import_parent*)n, st);
                if (re) return re;
            }
            RETURN_RESOLVED(value, ctype, n, NULL);
        }
        case SYM_IMPORT_GROUP: {
            sym_import_group* ig = (sym_import_group*)n;
            if (resolved) {
                RETURN_RESOLVED(value, ctype, n, NULL);
            }
            if (ast_flags_get_import_group_module_used(n->flags)) {
                re = require_module_in_pp(
                    r, n, st, ig->parent_mdgn, &ig->done, &ig->pprn);
                if (re) return re;
                if (!ig->pprn) {
                    // we can't mark it as resolved since we need to go
                    // though it again
                    ast_flags_set_resolved(&n->flags);
                }
                else {
                    ast_flags_clear_resolving(&n->flags);
                    re = curr_pprn_run_with(r, ig->pprn);
                    if (re) return re;
                }
            }
            RETURN_RESOLVED(value, ctype, n, NULL);
        }
        case SYM_IMPORT_MODULE: {
            sym_import_module* im = (sym_import_module*)n;
            if (!resolved) {
                re = require_module_in_pp(
                    r, n, st, im->target, &im->done, &im->pprn);
                if (re) return re;
                if (!im->pprn) {
                    // we can't mark it as resolved since we need to go
                    // though it again
                    ast_flags_set_resolved(&n->flags);
                }
                else {
                    re = curr_pprn_run_with(r, im->pprn);
                    ast_flags_clear_resolving(&n->flags);
                    if (re) return re;
                }
            }
            RETURN_RESOLVED(value, ctype, n, NULL);
        }
        case STMT_USE:
        case SYM_NAMED_USE:
        case STMT_COMPOUND_ASSIGN: {
            // TODO
            assert(false);
            return RE_FATAL;
        }
        case SYM_VAR:
        case SYM_VAR_INITIALIZED: {
            sym_var* v = (sym_var*)n;
            if (resolved) {
                if (v->pprn) {
                    // if the symbol table isn't public we are looking up a
                    // symbol inside a function since we can find it we're in
                    // the same function therefore we don't want to depend on
                    // ourselves
                    // TODO: this simple detection for "is the var in my
                    // function" will no longer work once we have nested
                    // functions
                    if (symbol_table_is_public(v->osym.sym.declaring_st)) {
                        re = curr_pprn_run_with(r, v->pprn);
                        if (re) return re;
                    }
                    else {
                        // otherwise the var shouldn't have a pprn
                        assert(ast_flags_get_comptime(v->osym.sym.node.flags));
                    }
                }
                RETURN_RESOLVED(value, ctype, v, v->ctype);
            }
            return resolve_var(r, st, v, value, ctype);
        }
        case EXPR_RETURN: {
            if (ctype) *ctype = UNREACHABLE_ELEM;
            if (value) *value = UNREACHABLE_ELEM;
            if (resolved) return RE_OK;
            return resolve_return(r, st, (expr_return*)n);
        }
        case EXPR_BREAK: {
            if (ctype) *ctype = UNREACHABLE_ELEM;
            if (value) *value = UNREACHABLE_ELEM;
            if (resolved) return RE_OK;
            return resolve_break(r, st, (expr_break*)n);
        }
        case EXPR_BLOCK: {
            expr_block* b = (expr_block*)n;
            if (resolved) {
                assert(!value);
                RETURN_RESOLVED(value, ctype, NULL, b->ebb.ctype);
            }
            return resolve_expr_block(r, b, st, value, ctype);
        }
        case EXPR_IF: {
            expr_if* ei = (expr_if*)n;
            if (resolved) RETURN_RESOLVED(value, ctype, n, ei->ctype);
            return resolve_if(r, st, ei, value, ctype);
        }
        case EXPR_LOOP: {
            expr_loop* l = (expr_loop*)n;
            if (resolved) {
                assert(!value);
                RETURN_RESOLVED(value, ctype, NULL, l->ebb.ctype);
            }
            bool end_reachable;
            re = resolve_expr_body(
                r, st, n, &l->body, &l->ebb.pprn, &end_reachable);
            if (re == RE_UNREALIZED_COMPTIME) {
                if (l->ebb.ctype || !end_reachable) re = RE_OK;
            }
            if (ctype) *ctype = l->ebb.ctype;
            if (re) return re;
            if (!l->ebb.ctype) l->ebb.ctype = UNREACHABLE_ELEM;
            ast_flags_set_resolved(&n->flags);
            RETURN_RESOLVED(value, ctype, value, l->ebb.ctype);
        }
        case EXPR_PASTE_EVALUATION: {
            expr_paste_evaluation* epe = (expr_paste_evaluation*)n;
            re = resolve_ast_node_raw(r, epe->expr, st, value, ctype);
            if (!resolved && !re) ast_flags_set_resolved(&epe->pe.node.flags);
            return re;
        }
        case STMT_PASTE_EVALUATION: {
            stmt_paste_evaluation* spe = (stmt_paste_evaluation*)n;
            if (resolved) {
                assert(!value);
                RETURN_RESOLVED(
                    value, ctype, NULL,
                    ast_flags_get_pp_stmt_end_unreachabale(n->flags)
                        ? UNREACHABLE_ELEM
                        : VOID_ELEM);
            }
            bool end_reachable;
            re = resolve_expr_body(r, st, n, &spe->body, NULL, &end_reachable);
            if (re == RE_UNREALIZED_COMPTIME) {
                if (!end_reachable) re = RE_OK;
            }
            if (ctype) {
                *ctype = (end_reachable) ? VOID_ELEM : UNREACHABLE_ELEM;
            }
            if (value) *value = VOID_ELEM;
            if (re) return re;

            if (!end_reachable) {
                ast_flags_set_pp_stmt_end_unreachabale(&n->flags);
            }
            ast_flags_set_resolved(&n->flags);
            return RE_OK;
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
            return resolve_expr_pp(r, st, ppe, value, ctype);
        }
        case EXPR_PASTE_STR: {
            if (resolved) {
                RETURN_RESOLVED(
                    value, ctype, PASTED_EXPR_ELEM, PASTED_EXPR_ELEM);
                return RE_OK;
            }
            return resolve_expr_paste_str(
                r, st, (expr_paste_str*)n, value, ctype);
        } break;
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
        case SYM_GENERIC_PARAM:
        case SYM_PARAM: {
            sym_param* p = (sym_param*)n;
            if (resolved) RETURN_RESOLVED(value, ctype, p, p->ctype);
            re = resolve_param(r, p, (n->kind == SYM_GENERIC_PARAM), ctype);
            if (value) *value = (ast_elem*)n;
            return re;
        }
        case SYM_PARAM_GENERIC_INST: {
            assert(resolved);
            sym_param_generic_inst* pgi = (sym_param_generic_inst*)n;
            RETURN_RESOLVED(value, ctype, pgi->value, pgi->ctype);
        }
        case EXPR_ARRAY_TYPE:
        case EXPR_SLICE_TYPE: {
            expr_slice_type* est = (expr_slice_type*)n;
            expr_array_type* eat =
                (expr_array_type*)(n->kind == EXPR_ARRAY_TYPE ? n : NULL);
            if (resolved) RETURN_RESOLVED(value, ctype, est->ctype, TYPE_ELEM);
            ast_elem* base_type;
            re = resolve_ast_node(r, est->base_type, st, &base_type, NULL);
            if (re) return re;
            if (!eat) {
                type_slice* ts = (type_slice*)pool_alloc(
                    &r->tc->permmem, sizeof(type_slice));
                if (!ts) return RE_FATAL;
                est->ctype = ts;
                ts->ctype_members = base_type;
                ts->kind = TYPE_SLICE;
                est->ctype = ts;
            }
            else {
                type_array* ta = (type_array*)pool_alloc(
                    &r->tc->permmem, sizeof(type_array));
                if (!ta) return RE_FATAL;
                ta->slice_type.ctype_members = base_type;
                ta->slice_type.kind = TYPE_ARRAY;
                re = evaluate_array_bounds(r, eat, st, &ta->length);
                if (re) return re;
                est->ctype = (type_slice*)ta;
            }
            ast_flags_set_resolved(&est->node.flags);
            RETURN_RESOLVED(value, ctype, est->ctype, TYPE_ELEM);
        }
        case EXPR_ARRAY: {
            expr_array* ea = (expr_array*)n;
            if (resolved) RETURN_RESOLVED(value, ctype, ea, ea->ctype);
            if (ea->explicit_decl && !ea->ctype) {
                re = resolve_ast_node(
                    r, (ast_node*)ea->explicit_decl, st, (ast_elem**)&ea->ctype,
                    NULL);
                if (re) return re;
                if (ea->ctype->kind == TYPE_ARRAY) {
                    if (((type_array*)ea->ctype)->length != ea->elem_count) {
                        assert(false); // TODO: error msg
                    }
                }
                else {
                    assert(ea->ctype->kind == TYPE_SLICE);
                }
            }
            ast_node** e = ea->elements;
            ast_elem* elem_ctype;
            for (ureg i = 0; i < ea->elem_count; i++) {
                re = resolve_ast_node(r, *e, st, NULL, &elem_ctype);
                if (re) return re;
                if (!ea->ctype) {
                    type_array* ta = (type_array*)pool_alloc(
                        &r->tc->permmem, sizeof(type_array));
                    if (!ta) return RE_FATAL;
                    ta->slice_type.ctype_members = elem_ctype;
                    ta->slice_type.kind = TYPE_ARRAY;
                    ta->length = ea->elem_count;
                    ea->ctype = (type_slice*)ta;
                }
                else {
                    if (!ctypes_unifiable(
                            (ast_elem*)ea->ctype->ctype_members, elem_ctype)) {
                        src_range_large elem_srl;
                        src_range_large array_srl;
                        ast_node_get_src_range(*e, st, &elem_srl);
                        ast_node_get_src_range((ast_node*)ea, st, &array_srl);
                        // TODO: different error for negative values
                        error_log_report_annotated_twice(
                            r->tc->err_log, ES_RESOLVER, false,
                            "invalid array element type", elem_srl.smap,
                            elem_srl.start, elem_srl.end,
                            "element type not convertible to array type",
                            array_srl.smap, array_srl.start, array_srl.end,
                            NULL);
                    }
                }
                e++;
            }
            ast_flags_set_resolved(&ea->node.flags);
            RETURN_RESOLVED(value, ctype, ea, ea->ctype);
        }
        case EXPR_CAST: {
            // TODO: check whether convertible, for now we just allow
            // everything
            expr_cast* ec = (expr_cast*)n;
            if (resolved) {
                RETURN_RESOLVED(value, ctype, NULL, ec->target_ctype);
            }
            re = resolve_ast_node(
                r, ec->target_type, st, &ec->target_ctype, NULL);
            if (re) return re;
            re = resolve_ast_node(r, ec->value, st, NULL, NULL);
            if (re) return re;
            ast_flags_set_resolved(&ec->node.flags);
            RETURN_RESOLVED(value, ctype, NULL, ec->target_ctype);
        }
        case EXPR_ACCESS: {
            expr_access* ea = (expr_access*)n;
            if (resolved) {
                RETURN_RESOLVED(value, ctype, NULL, ea->ctype);
            }
            ast_elem* lhs_ctype;
            ast_elem* lhs_val;
            re = resolve_ast_node(r, ea->lhs, st, &lhs_val, &lhs_ctype);
            if (re) return re;
            if (ea->arg_count == 1 && ast_elem_is_type_slice(lhs_ctype)) {
                ast_elem* rhs_ctype;
                re = resolve_ast_node(r, ea->args[0], st, NULL, &rhs_ctype);
                if (!ctypes_unifiable(
                        (ast_elem*)&PRIMITIVES[PT_INT], rhs_ctype)) {
                    src_range_large array_srl;
                    src_range_large index_srl;
                    ast_node_get_src_range(ea->lhs, st, &array_srl);
                    ast_node_get_src_range(ea->args[0], st, &index_srl);
                    // TODO: different error for negative values
                    error_log_report_annotated_twice(
                        r->tc->err_log, ES_RESOLVER, false,
                        "invalid array index type", index_srl.smap,
                        index_srl.start, index_srl.end,
                        "index type is not convertible to integer",
                        array_srl.smap, array_srl.start, array_srl.end,
                        "in the element access of this array");
                    return RE_ERROR;
                }
                ea->node.op_kind = OP_ARRAY_ACCESS;
                ast_flags_set_resolved(&ea->node.flags);
                ea->ctype = ((type_slice*)lhs_ctype)->ctype_members;
                RETURN_RESOLVED(value, ctype, NULL, ea->ctype);
            }
            if (lhs_val->kind == SC_STRUCT_GENERIC) {
                return resolve_generic_struct(
                    r, ea, (sc_struct_generic*)lhs_val, st, value, ctype);
            }
            assert(false); // TODO operator overloading / generics
            return RE_FATAL;
        }
        case EXPR_MACRO_STR_CALL: {
            // TODO: implement this properly
            expr_macro_str_call* emsc = (expr_macro_str_call*)n;
            assert(emsc->lhs->kind == EXPR_IDENTIFIER);
            expr_identifier* id = (expr_identifier*)emsc->lhs;
            assert(cstr_eq(id->value.str, "asm"));
            UNUSED(id); // make release build happy
            ast_flags_set_resolved(&n->flags);
            RETURN_RESOLVED(value, ctype, VOID_ELEM, VOID_ELEM);
        }
        default: assert(false); return RE_UNKNOWN_SYMBOL;
    }
}
static inline void report_type_loop(resolver* r, ast_node* n, symbol_table* st)
{
    if (r->tc->t->trap_on_error) debugbreak();
    ureg stack_ec = stack_element_count(&r->error_stack);
    // we are at the peek of the type loop. unwind and report again.
    if (stack_ec == 0) return;
    if (stack_peek_nth(&r->error_stack, stack_ec - 2) != n) {
        r->retracing_type_loop = true;
        stack_clear(&r->error_stack);
        ast_flags_clear_resolving(&n->flags);
        resolve_ast_node(r, n, st, NULL, NULL);
        stack_ec = stack_element_count(&r->error_stack);
        assert(stack_peek_nth(&r->error_stack, stack_ec - 2) == n);
    }
    ast_node* n_s = (ast_node*)stack_pop(&r->error_stack);
    assert(n == n_s);
    stack_pop(&r->error_stack);
    stack_ec -= 2;
    src_range_large srl;
    ast_node_get_src_range(n, st, &srl);
    ureg annot_count = stack_ec / 2;
    annot_count--; // the starting type is at the bottom of the stack
    error* e = error_log_create_error(
        r->tc->err_log, ES_RESOLVER, false, "type inference cycle", srl.smap,
        srl.start, srl.end, "type definition depends on itself", annot_count);
    for (ureg i = 0; i < annot_count; i++) {
        n = (ast_node*)stack_pop(&r->error_stack);
        if (!n) break;
        st = (symbol_table*)stack_pop(&r->error_stack);
        if (n->kind == EXPR_OP_BINARY || n->kind == EXPR_OP_UNARY ||
            n->kind == EXPR_MEMBER_ACCESS || n->kind == EXPR_BREAK ||
            n->kind == EXPR_BLOCK) {
            // don't highlight too much stuff
            ast_node_get_src_range(n, st, &srl);
            error_add_annotation(e, srl.smap, srl.start, srl.end, NULL);
            continue;
        }
        ast_node_get_src_range(n, st, &srl);
        char* annot = (i + 1 == annot_count) ? "loop detected" : "";
        error_add_annotation(e, srl.smap, srl.start, srl.end, annot);
    }
    stack_pop(&r->error_stack);
    stack_pop(&r->error_stack);
    assert(stack_element_count(&r->error_stack) == 0);
    error_log_report(r->tc->err_log, e);
}
resolve_error resolve_ast_node(
    resolver* r, ast_node* n, symbol_table* st, ast_elem** value,
    ast_elem** ctype)
{
    resolve_error re = resolve_ast_node_raw(r, n, st, value, ctype);
    if (!re) return RE_OK;
    if (re == RE_TYPE_LOOP) {
        if (!r->allow_type_loops) {
            if (n == r->type_loop_start && !r->retracing_type_loop) {
                report_type_loop(r, n, st);
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
    else {
        if (re != RE_UNREALIZED_COMPTIME && r->tc->t->trap_on_error) {
            debugbreak();
        }
        ast_flags_clear_resolving(&n->flags);
    }
    return re;
}
resolve_error resolve_expr_body(
    resolver* r, symbol_table* parent_st, ast_node* expr, ast_body* b,
    pp_resolve_node** block_pprn, bool* end_reachable)
{
    ast_node* parent_block_owner = r->curr_block_owner;
    r->curr_block_owner = expr;
    resolve_error re;
    ast_elem* stmt_ctype = NULL;
    ast_elem** stmt_ctype_ptr = &stmt_ctype;
    ureg saved_decl_count = 0;
    pp_resolve_node* prev_block_pprn = r->curr_block_pp_node;
    pp_resolve_node* pprn = block_pprn ? *block_pprn : NULL;
    bool resumed = (pprn != NULL);
    r->curr_block_pp_node = pprn;

    bool parent_allows_type_loops = r->allow_type_loops;
    bool parenting_type_loop = false;
    if (!r->retracing_type_loop) {
        r->allow_type_loops = true;
        if (!resumed) set_parent_symtabs(&b->symtab, parent_st);
    }
    // if we already have decls this is the second pass.
    // TODO prevent use before define

    ast_node** continue_at = b->elements;
    if (pprn) {
        continue_at = pprn->continue_block;
        if (!pprn->block_pos_reachable) stmt_ctype_ptr = NULL;
    }
    ast_node** n = continue_at;
    for (; *n != NULL; n++) {
        re = add_ast_node_decls(r, b->symtab, NULL, *n, false);
        if (re) break;
        re = resolve_ast_node(r, *n, b->symtab, NULL, stmt_ctype_ptr);
        if (r->curr_block_pp_node && r->curr_block_pp_node->pending_pastes) {
            r->curr_block_pp_node->continue_block = n;
            re = RE_SYMBOL_NOT_FOUND_YET;
            break;
        }
        if (stmt_ctype_ptr && stmt_ctype == UNREACHABLE_ELEM) {
            stmt_ctype_ptr = NULL;
        }
        if (re == RE_TYPE_LOOP) {
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
        }
        else {
            if (re) break; // this includes RE_UNREALIZED_COMPTIME
        }
    }
    if (saved_decl_count) {
        b->symtab->decl_count = saved_decl_count;
    }
    pprn = r->curr_block_pp_node;
    if (block_pprn) *block_pprn = pprn;
    r->curr_block_owner = parent_block_owner;
    r->curr_block_pp_node = prev_block_pprn;
    *end_reachable = (stmt_ctype_ptr != NULL);
    r->allow_type_loops = parent_allows_type_loops;
    r->curr_block_owner = parent_block_owner;
    if (pprn) {
        pprn->block_pos_reachable = *end_reachable;
        pprn->declaring_st = parent_st;
        if (re) {
            pprn->continue_block = n;
        }
        else {
            pprn->continue_block = NULL;
        }
        if (!pprn->first_unresolved_child) {
            pprn->run_when_ready = false;
        }
        resolve_error re2;
        if (!pprn->first_unresolved_child && pprn->dep_count == 0) {
            // this is a rerun and everyting got resolved
            // detach this from parent and free it individually
            pprn->parent = NULL;
            re2 = pp_resolve_node_ready(r, pprn, true);
        }
        else {
            if (curr_pp_block_add_child(r, pprn)) return RE_FATAL;
            re2 = pp_resolve_node_activate(r, pprn, re == RE_OK);
        }
        if (re2) return re2;
    }
    if (re) return re; // This includes unrealized paste
    if (parenting_type_loop) {
        return RE_TYPE_LOOP;
    }
    return re;
}

resolve_error resolve_func_from_call(resolver* r, sc_func* fn, ast_elem** ctype)
{
    resolve_error re;
    if (ast_flags_get_resolved(fn->fnb.sc.osym.sym.node.flags)) {
        if (ctype) *ctype = fn->fnb.return_ctype;
        if (fn->fnb.pprn == NULL) return RE_OK; // fn already emitted
        mdg_node* fn_mdg = (mdg_node*)symbol_table_get_module_table(
                               fn->fnb.sc.osym.sym.declaring_st)
                               ->owning_node;
        assert(fn_mdg == r->curr_mdg);
        UNUSED(fn_mdg); // make Release build happy
    }
    else {
        if (!fn->fnb.pprn) {
            fn->fnb.pprn = pp_resolve_node_create(
                r, (ast_node*)fn, fn->fnb.sc.osym.sym.declaring_st, false,
                false, true);
            if (!fn->fnb.pprn) return RE_FATAL;
        }
        re = resolve_ast_node(
            r, fn->fnb.return_type, fn->fnb.sc.osym.sym.declaring_st,
            &fn->fnb.return_ctype, NULL);
        if (re) return re;
        if (ctype) *ctype = fn->fnb.return_ctype;
    }
    // TODO: optimize to run with in some cases
    re = curr_pprn_run_after(r, fn->fnb.pprn);
    if (re) return re;
    return RE_OK;
}
resolve_error
resolve_struct(resolver* r, sc_struct* st, ast_elem** value, ast_elem** ctype)
{
    resolve_error re = RE_OK;
    if (st->sb.pprn && st->sb.pprn->dep_count) {
        re = curr_pprn_run_after(r, st->sb.pprn);
        if (re) return re;
        return RE_UNREALIZED_COMPTIME;
    }
    ast_node* parent_block_owner = r->curr_block_owner;
    pp_resolve_node* parent_pprn = r->curr_block_pp_node;
    r->curr_block_pp_node = st->sb.pprn;
    r->curr_block_owner = (ast_node*)st;
    ast_body* b = &st->sb.sc.body;
    bool unrealized_comptime = false;
    for (ast_node** n = b->elements; *n != NULL; n++) {
        re = resolve_ast_node(r, *n, b->symtab, NULL, NULL);
        if (re == RE_UNREALIZED_COMPTIME) {
            unrealized_comptime = true;
            re = RE_OK;
        }
        else if (re) {
            break;
        }
    }
    st->sb.pprn = r->curr_block_pp_node;
    r->curr_block_owner = parent_block_owner;
    r->curr_block_pp_node = parent_pprn;
    if (st->sb.pprn) {
        resolve_error re2 = curr_pprn_run_after(r, st->sb.pprn);
        if (!re2) re2 = pp_resolve_node_activate(r, st->sb.pprn, re == RE_OK);
        if (re2) {
            assert(re2 == RE_FATAL);
            return re2;
        }
    }

    if (re) return re;
    if (unrealized_comptime) return RE_UNREALIZED_COMPTIME;
    ast_flags_set_resolved(&st->sb.sc.osym.sym.node.flags);
    RETURN_RESOLVED(value, ctype, st, TYPE_ELEM);
}
// TODO: make sure we return!
resolve_error
resolve_func(resolver* r, sc_func_base* fnb, ast_node** continue_block)
{
    bool generic = (fnb->sc.osym.sym.node.kind == SC_FUNC_GENERIC);
    bool generic_parent = r->generic_context;
    r->generic_context = generic || generic_parent;
    ast_body* b = &fnb->sc.body;
    symbol_table* st = b->symtab;
    resolve_error re;
    ast_node* parent_block_owner = r->curr_block_owner;
    pp_resolve_node* prev_block_pprn = r->curr_block_pp_node;
    r->curr_block_owner = (ast_node*)fnb;
    if (!continue_block) {
        if (!ast_flags_get_static(fnb->sc.osym.sym.node.flags)) {
            if (ast_elem_is_struct(
                    symbol_table_nonmeta(fnb->sc.osym.sym.declaring_st)
                        ->owning_node)) {
                ast_flags_set_instance_member(&fnb->sc.osym.sym.node.flags);
            }
        }
        if (generic) {
            sc_func_generic* fng = (sc_func_generic*)fnb;
            for (ureg i = 0; i < fng->generic_param_count; i++) {
                re = resolve_param(r, &fng->generic_params[i], false, NULL);
                if (re) {
                    r->curr_block_owner = parent_block_owner;
                    r->generic_context = generic_parent;
                    return re;
                }
            }
        }
        for (ureg i = 0; i < fnb->param_count; i++) {
            re = resolve_param(r, &fnb->params[i], false, NULL);
            if (re) {
                r->curr_block_owner = parent_block_owner;
                r->generic_context = generic_parent;
                return re;
            }
        }
        re =
            resolve_ast_node(r, fnb->return_type, st, &fnb->return_ctype, NULL);
        if (re) {
            r->curr_block_owner = parent_block_owner;
            r->generic_context = generic_parent;
            return re;
        }
        // handle function declarations
        if (fnb->sc.body.srange == SRC_RANGE_INVALID) {
            ast_flags_set_resolved(&fnb->sc.osym.sym.node.flags);
            r->curr_block_owner = parent_block_owner;
            r->generic_context = generic_parent;
            if (fnb->pprn) {
                return pp_resolve_node_activate(r, fnb->pprn, true);
            }
            return RE_OK;
        }
    }
    r->curr_block_pp_node = fnb->pprn;
    ast_elem* stmt_ctype;
    ast_elem** stmt_ctype_ptr = &stmt_ctype;
    ast_node** n = b->elements;
    if (continue_block) {
        n = continue_block;
        if (!fnb->pprn->block_pos_reachable) stmt_ctype_ptr = NULL;
    }
    // since curr_pprn is expression based we want to reset it so
    // we don't add children to the caller of our func
    pp_resolve_node* prev_pprn = r->curr_pp_node;
    r->curr_pp_node = NULL;
    while (*n) {
        // TODO: move this kind of error into the prp
        if (stmt_ctype_ptr == NULL && n != continue_block) {
            src_range_large srl;
            ast_node_get_src_range(*n, st, &srl);
            error_log_report_annotated(
                r->tc->err_log, ES_RESOLVER, false,
                "unreachable statement in function", srl.smap, srl.start,
                srl.end, "after return statement");
            re = RE_TYPE_MISSMATCH;
            break;
        }
        re = add_ast_node_decls(r, st, NULL, *n, false);
        if (re) break;
        re = resolve_ast_node(r, *n, st, NULL, stmt_ctype_ptr);
        if (re) break;
        assert(r->curr_pp_node == NULL);
        if (r->curr_block_pp_node && r->curr_block_pp_node->pending_pastes) {
            r->curr_block_pp_node->continue_block = n;
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
    fnb->pprn = bpprn;
    r->curr_block_pp_node = prev_block_pprn;
    r->generic_context = generic_parent;
    if (re == RE_UNREALIZED_COMPTIME) {
        assert(bpprn);
        if (bpprn->dep_count == 0) {
            if (ptrlist_append(&r->pp_resolve_nodes_ready, bpprn))
                return RE_FATAL;
        }
        bpprn->continue_block = n;
        bpprn->block_pos_reachable = (stmt_ctype_ptr != NULL);
        return RE_OK;
    }
    if (bpprn) {
        if (!re) bpprn->continue_block = NULL;
        if (!fnb->pprn->first_unresolved_child &&
            fnb != (sc_func_base*)r->module_group_constructor) {
            bpprn->run_when_ready = false;
        }
        resolve_error re2 = pp_resolve_node_activate(r, fnb->pprn, re == RE_OK);
        if (re2) {
            assert(re2 == RE_FATAL);
            return re2;
        }
    }
    if (re) return re;
    if (stmt_ctype_ptr && fnb->return_ctype != VOID_ELEM) {
        ureg brace_end = src_range_get_end(fnb->sc.body.srange);
        src_map* smap =
            ast_node_get_smap((ast_node*)fnb, fnb->sc.osym.sym.declaring_st);
        error_log_report_annotated_thrice(
            r->tc->err_log, ES_RESOLVER, false,
            "reachable end of non void function", smap, brace_end - 1,
            brace_end, "missing return statement", smap,
            src_range_get_start(fnb->return_type->srange),
            src_range_get_end(fnb->return_type->srange),
            "function returns non void type", smap,
            src_range_get_start(fnb->sc.osym.sym.node.srange),
            src_range_get_end(fnb->sc.osym.sym.node.srange), NULL);
        return RE_TYPE_MISSMATCH;
    }
    ast_flags_set_resolved(&fnb->sc.osym.sym.node.flags);
    return RE_OK;
}
resolve_error resolve_module_frame(resolver* r, module_frame* mf, ast_body* b)
{
    resolve_error re = RE_OK;
    ast_node* parent_block_owner = r->curr_block_owner;
    r->curr_block_owner = (ast_node*)mf;
    for (ast_node** n = b->elements; *n != NULL; n++) {
        re = resolve_ast_node(r, *n, b->symtab, NULL, NULL);
        if (re == RE_UNREALIZED_COMPTIME) re = RE_OK;
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

// assign public symbol with the aquired ids from global id space
static void adjust_node_ids(resolver* r, ureg* id_space, ast_node* n)
{
    // we don't need to recurse into expressions because the contained
    // symbols can never be public
    switch (n->kind) {
        case SC_FUNC: {
            if (is_local_node(n->flags)) return;
            sc_func* fn = (sc_func*)n;
            update_id(r, &fn->id, id_space);
        } break;
        case SYM_VAR:
        case SYM_VAR_INITIALIZED: {
            if (is_local_node(n->flags)) return;
            update_id(r, &((sym_var*)n)->var_id, id_space);
        } break;
        case SC_STRUCT:
        case SC_STRUCT_GENERIC_INST: {
            if (is_local_node(n->flags)) return;
            update_id(r, &((sc_struct*)n)->id, id_space);
            adjust_body_ids(r, id_space, &((sc_struct*)n)->sb.sc.body);
        } break;
        case SC_STRUCT_GENERIC: {
            sc_struct_generic* sg = (sc_struct_generic*)n;
            for (sc_struct_generic_inst* sgi = sg->instances; sgi;
                 sgi = (sc_struct_generic_inst*)sgi->st.sb.sc.osym.sym.next) {
                adjust_node_ids(r, id_space, (ast_node*)sgi);
            }
        } break;
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
            atomic_ureg_load(&(**i).using_count), true, (ast_elem*)*i);
        if (res) return RE_FATAL;
        (**i).symtab->parent = NULL; // assertion in set parent symtabs
        set_parent_symtabs(&(**i).symtab, r->tc->t->root_symtab);
    }
    if (!contains_root) atomic_ureg_inc(&r->tc->t->linking_holdups);
    return RE_OK;
}
resolve_error resolver_add_mf_decls(resolver* r)
{
    for (mdg_node** i = r->mdgs_begin; i != r->mdgs_end; i++) {
        aseglist_iterator asi;
        aseglist_iterator_begin(&asi, &(**i).module_frames);
        for (module_frame* mf = aseglist_iterator_next(&asi); mf != NULL;
             mf = aseglist_iterator_next(&asi)) {
            resolve_error re =
                add_body_decls(r, (**i).symtab, (**i).symtab, &mf->body, true);
            if (re) return re;
            if (r->curr_pp_node) {
                re = pp_resolve_node_activate(r, r->curr_pp_node, false);
                if (re) return re;
            }
        }
    }
    return RE_OK;
}
void print_pprn(resolver* r, pp_resolve_node* pprn, bool verbose, ureg ident)
{
    assert(pprn->node != NULL);
    if (verbose) {
        aseglist_iterator it;
        print_indent(ident);
        tprintf("dependencies: %zu\n", pprn->dep_count);
        aseglist_iterator_begin(&it, &pprn->notify_when_ready);
        ureg ready_nots = aseglist_iterator_get_remaining_count(&it);
        print_indent(ident);
        tprintf("nofify ready: %zu\n", ready_nots);
        for (pp_resolve_node* p = (pp_resolve_node*)aseglist_iterator_next(&it);
             p; p = (pp_resolve_node*)aseglist_iterator_next(&it)) {
            print_pprn(r, p, false, ident + 1);
        }
        aseglist_iterator_begin(&it, &pprn->notify_when_done);
        ureg done_nots = aseglist_iterator_get_remaining_count(&it);
        print_indent(ident);
        tprintf("notify done:  %zu\n", done_nots);
        for (pp_resolve_node* p = (pp_resolve_node*)aseglist_iterator_next(&it);
             p; p = (pp_resolve_node*)aseglist_iterator_next(&it)) {
            print_pprn(r, p, false, ident + 1);
        }
    }

    pp_resolve_node* child = pprn->first_unresolved_child;
    print_indent(ident);
    if (ast_elem_is_func_base((ast_elem*)pprn->node)) {
        tprintf("func %s", ((sc_func_base*)pprn->node)->sc.osym.sym.name);
    }
    else {
        print_ast_node(pprn->node, r->curr_mdg, ident);
    }
    tputs("");
    if (child && verbose) {
        print_indent(ident);
        tputs("children:");
        while (child) {
            print_indent(ident + 1);
            print_ast_node(child->node, r->curr_mdg, ident + 1);
            tputs("");
            child = child->next;
        }
    }
    tputs("");
}
// this stuff is just for debugging purposes
void print_pprnlist(resolver* r, sbuffer* buff, char* msg, bool verbose)
{
    if (sbuffer_get_used_size(buff) == 0) return;
    tputs(msg);
    sbuffer_iterator sbi = sbuffer_iterator_begin(buff);
    for (pp_resolve_node** rn =
             sbuffer_iterator_next(&sbi, sizeof(pp_resolve_node*));
         rn; rn = sbuffer_iterator_next(&sbi, sizeof(pp_resolve_node*))) {
        print_pprn(r, *rn, verbose, 1);
    }
}
void print_pprns(resolver* r, char* msg, bool verbose)
{
    if (!(r->tc->t->verbosity_flags & VERBOSITY_FLAGS_PPRNS)) return;
    tprintf(msg);
    print_pprnlist(r, &r->pp_resolve_nodes_ready, "ready:", verbose);
    print_pprnlist(r, &r->pp_resolve_nodes_pending, "pending:", verbose);
    print_pprnlist(r, &r->pp_resolve_nodes_waiting, "waiting:", verbose);
    tflush();
}
resolve_error
pp_resolve_node_ready(resolver* r, pp_resolve_node* pprn, bool fin_independent)
{
    resolve_error re;
    pprn->ready = true;
    if (pprn->waiting_list_entry) {
        remove_pprn_from_waiting_list(r, pprn);
    }
    aseglist_iterator asit;
    aseglist_iterator_begin(&asit, &pprn->notify_when_ready);
    for (pp_resolve_node* rn = aseglist_iterator_next(&asit); rn;
         rn = aseglist_iterator_next(&asit)) {
        re = pp_resolve_node_dep_ready(r, rn);
        if (re) return re;
    }
    if (pprn->run_when_ready) {
        if (ptrlist_append(&r->pp_resolve_nodes_ready, pprn)) return RE_FATAL;
    }
    else if (fin_independent && !pprn->parent) {
        re = pp_resolve_node_done(r, pprn, NULL);
        if (re) return re;
    }
    return RE_OK;
}
resolve_error pp_resolve_node_dep_ready(resolver* r, pp_resolve_node* pprn)
{
    pprn->dep_count--;
    if (pprn->dep_count == 0) {
        resolve_error re = pp_resolve_node_ready(r, pprn, true);
        if (re) return re;
    }
    return RE_OK;
}

resolve_error
pp_resolve_node_dep_done(resolver* r, pp_resolve_node* pprn, bool* progress);
resolve_error
pp_resolve_node_done(resolver* r, pp_resolve_node* pprn, bool* progress)
{
    assert(pprn->node);
    resolve_error re;
    if (pprn->first_unresolved_child) {
        for (pp_resolve_node* rn = pprn->first_unresolved_child; rn;
             rn = rn->next) {
            re = pp_resolve_node_done(r, rn, progress);
            if (re) return re;
        }
        pprn->first_unresolved_child = NULL;
        pprn->last_unresolved_child = NULL;
    }
    if (pprn->continue_block) {
        if (pprn->parent == NULL) {
            if (progress) *progress = true;
            if (ptrlist_append(&r->pp_resolve_nodes_pending, pprn))
                return RE_FATAL;
        }
        return RE_OK;
    }
    aseglist_iterator asit;
    aseglist_iterator_begin(&asit, &pprn->notify_when_done);
    for (pp_resolve_node* rn = aseglist_iterator_next(&asit); rn;
         rn = aseglist_iterator_next(&asit)) {
        re = pp_resolve_node_dep_done(r, rn, progress);
        if (re) return re;
    }
    pprn_fin(r, pprn);
    return RE_OK;
}
resolve_error
pp_resolve_node_dep_done(resolver* r, pp_resolve_node* pprn, bool* progress)
{
    assert(pprn->dep_count);
    assert(pprn->node);
    pprn->dep_count--;
    if (pprn->dep_count == 0) {
        if (progress) *progress = true;
        resolve_error re = pp_resolve_node_ready(r, pprn, true);
        if (re) return re;
    }
    return RE_OK;
}
resolve_error report_cyclic_pp_deps(resolver* r)
{
    bool err = false;
    pp_resolve_node** rn;
    pli pit = pli_begin(&r->pp_resolve_nodes_pending);
    for (pp_resolve_node* pprn = pli_next(&pit); pprn; pprn = pli_next(&pit)) {
        rn = (pp_resolve_node**)sbuffer_append(
            &r->pp_resolve_nodes_waiting, sizeof(pp_resolve_node*));
        if (!rn) return RE_FATAL;
        *rn = pprn;
    }
    sbuffer_clear(&r->pp_resolve_nodes_pending);
    pit = pli_begin(&r->pp_resolve_nodes_ready);
    for (pp_resolve_node* pprn = pli_next(&pit); pprn; pprn = pli_next(&pit)) {
        rn = (pp_resolve_node**)sbuffer_append(
            &r->pp_resolve_nodes_waiting, sizeof(pp_resolve_node*));
        if (!rn) return RE_FATAL;
        *rn = pprn;
    }
    sbuffer_clear(&r->pp_resolve_nodes_ready);
    sbuffer_iterator sbi = sbuffer_iterator_begin(&r->pp_resolve_nodes_waiting);
    rn = sbuffer_iterator_next(&sbi, sizeof(pp_resolve_node*));
    if (!rn) return RE_OK;
    // TODO: create a nice cycle display instead of dumping out everything
    resolve_error re = RE_OK;
    do {
        if (!ast_elem_is_any_import_symbol((ast_elem*)(**rn).node)) {
            if (err == false) {
                err = true;
                print_pprns(r, "error: \n", true);
            }
            src_range_large srl;
            ast_node_get_src_range((**rn).node, (**rn).declaring_st, &srl);
            error_log_report_annotated(
                r->tc->err_log, ES_RESOLVER, false,
                "encountered cyclic dependency during preprocessor "
                "execution",
                srl.smap, srl.start, srl.end, "loop contains this element");
            re = RE_PP_DEPS_LOOP;
        }
        rn = sbuffer_iterator_next(&sbi, sizeof(pp_resolve_node*));
    } while (rn);
    return re;
}
resolve_error resolver_run_pp_resolve_nodes(resolver* r)
{
    llvm_error lle;
    pli it;
    resolve_error re;
    bool progress;
    bool import_pprns;
    bool non_import_pprns;
    do {
        progress = false;
        import_pprns = false;
        non_import_pprns = false;
        if (!ptrlist_is_empty(&r->pp_resolve_nodes_ready)) {
            print_pprns(r, "running ", true);
            ureg priv_count = r->id_space - PRIV_SYMBOL_OFFSET;
            llvm_backend_reserve_symbols(r->backend, priv_count, 0);
            lle = llvm_backend_run_pp(
                r->backend, priv_count, &r->pp_resolve_nodes_ready);
            if (lle) return RE_FATAL;
            it = pli_rbegin(&r->pp_resolve_nodes_ready);
            for (pp_resolve_node* rn = pli_prev(&it); rn; rn = pli_prev(&it)) {
                pp_resolve_node_done(r, rn, &progress);
                ptrlist_remove(&r->pp_resolve_nodes_ready, &it);
            }
        }
        // we try to resolve pending nodes again
        it = pli_begin(&r->pp_resolve_nodes_pending);
        for (pp_resolve_node* rn = pli_next(&it); rn; rn = pli_next(&it)) {
            if (rn->continue_block) {
                pli_prev(&it);
                ptrlist_remove(&r->pp_resolve_nodes_pending, &it);
                progress = true;
                if (rn->node->kind == SC_FUNC) {
                    re = resolve_func(
                        r, (sc_func_base*)rn->node, rn->continue_block);
                }
                else if (rn->node->kind == EXPR_BLOCK) {
                    re = resolve_expr_block(
                        r, (expr_block*)rn->node, rn->declaring_st, NULL, NULL);
                }
                else {
                    assert(false);
                }
                if (re) return re;
                continue;
            }
            if (rn->parent) {
                r->curr_pp_node = rn->parent;
                r->curr_block_owner = rn->parent->node;
            }
            else {
                r->curr_pp_node = NULL;
                // can't be the an mdg because of "declaring" st --> node
                assert(rn->node->kind != ELEM_MDG_NODE);
                r->curr_block_owner = (ast_node*)rn->declaring_st->owning_node;
            }
            ast_node* astn = rn->node;
            re = resolve_ast_node(r, rn->node, rn->declaring_st, NULL, NULL);
            if (re == RE_UNREALIZED_COMPTIME) {
                pli_prev(&it);
                ptrlist_remove(&r->pp_resolve_nodes_pending, &it);
                progress = true;
                non_import_pprns = true;
                continue;
            }
            if (re == RE_SYMBOL_NOT_FOUND_YET) {
                non_import_pprns = true;
                continue;
            }
            if (re) return re;
            if (ast_elem_is_any_import_symbol((ast_elem*)astn)) {
                import_pprns = true;
                if (ast_flags_get_resolved(astn->flags)) {
                    progress = true;
                    pli_prev(&it);
                    ptrlist_remove(&r->pp_resolve_nodes_pending, &it);
                    re = pp_resolve_node_done(r, rn, NULL);
                    if (re) return re;
                }
            }
            else {
                non_import_pprns = true;
                if (ast_flags_get_resolved(astn->flags)) {
                    progress = true;
                    pli_prev(&it);
                    ptrlist_remove(&r->pp_resolve_nodes_pending, &it);
                }
            }
        }
    } while (progress || (import_pprns && non_import_pprns) ||
             (import_pprns && !sbuffer_is_empty(&r->pp_resolve_nodes_waiting)));
    return RE_OK;
}

resolve_error resolver_handle_post_pp(resolver* r)
{
    resolve_error re;
    for (mdg_node** i = r->mdgs_begin; i != r->mdgs_end; i++) {
        r->curr_mdg = *i;
        aseglist_iterator asi;
        aseglist_iterator_begin(&asi, &(**i).module_frames);
        for (module_frame* mf = aseglist_iterator_next(&asi); mf != NULL;
             mf = aseglist_iterator_next(&asi)) {
            r->curr_mf = mf;
            r->curr_block_owner = (ast_node*)mf;
            re = resolve_module_frame(r, mf, &mf->body);
            if (re) return re;
            ast_flags_set_resolved(&mf->node.flags);
        }
    }
    re = resolver_run_pp_resolve_nodes(r);
    if (re) return re;
    return report_cyclic_pp_deps(r);
}

void free_pprnlist(resolver* r, sbuffer* buff)
{
    sbuffer_iterator sbi = sbuffer_iterator_begin(buff);
    for (pp_resolve_node** rn =
             sbuffer_iterator_next(&sbi, sizeof(pp_resolve_node*));
         rn; rn = sbuffer_iterator_next(&sbi, sizeof(pp_resolve_node*))) {
        if (buff == &r->pp_resolve_nodes_waiting && (**rn).waiting_list_entry) {
            (**rn).waiting_list_entry = NULL; // prevent iterator invalidation
        }
        pprn_fin(r, *rn);
    }
    sbuffer_clear(buff);
}
void free_pprns(resolver* r)
{
    free_pprnlist(r, &r->pp_resolve_nodes_pending);
    free_pprnlist(r, &r->pp_resolve_nodes_ready);
    free_pprnlist(r, &r->pp_resolve_nodes_waiting);
    assert(r->pp_resolve_nodes.alloc_count == 0);
}
int resolver_resolve(resolver* r)
{
    r->generic_context = false;
    r->curr_pp_node = NULL;
    r->curr_block_pp_node = NULL;
    r->curr_var_decl = NULL;
    r->curr_var_decl_block_owner = NULL;
    r->curr_var_pp_node = NULL;
    r->retracing_type_loop = false;
    r->public_sym_count = 0;
    r->private_sym_count = 0;
    r->id_space = PRIV_SYMBOL_OFFSET;
    r->module_group_constructor = NULL;
    r->module_group_destructor = NULL;
    resolve_error re;
    re = resolver_init_mdg_symtabs_and_handle_root(r);
    if (re) return re;
    re = resolver_add_mf_decls(r);
    if (re) return re;
    re = resolver_run_pp_resolve_nodes(r);
    if (re) return re;
    re = resolver_handle_post_pp(r);
    if (re) return re;
    free_pprns(r);
    return RE_OK;
}
resolve_error lookup_priv_module_symbol(
    resolver* r, mdg_node* mod, const char* name, symbol** result,
    const char* amb_message)
{
    symbol* res = NULL;
    symbol* ambiguity = NULL;
    symbol* ambiguity2 = NULL;
    symbol** result_1 = &res;
    symbol** result_2 = &ambiguity;
    resolve_error re = resolver_lookup_single(
        r, mod->symtab, NULL, mod->symtab, name, result_1, result_2);
    if (re) return re;
    if (!ambiguity) {
        aseglist_iterator it;
        aseglist_iterator_begin(&it, &mod->module_frames);
        for (module_frame* mf = aseglist_iterator_next(&it); mf;
             mf = aseglist_iterator_next(&it)) {
            if (res) {
                result_1 = &ambiguity;
                result_2 = &ambiguity2;
            }
            resolver_lookup_single(
                r, mf->body.symtab, NULL, mf->body.symtab, name, result_1,
                result_2);
            if (re) return re;
            // since we check multiple frames we might hit the same main twice
            if (ambiguity == res) ambiguity = ambiguity2;
            if (ambiguity) break;
        }
    }
    if (ambiguity) {
        src_range_large srl1, srl2;
        ast_node_get_src_range((ast_node*)res, res->declaring_st, &srl1);
        ast_node_get_src_range(
            (ast_node*)ambiguity, ambiguity->declaring_st, &srl2);
        error_log_report_annotated_twice(
            r->tc->err_log, ES_RESOLVER, false, amb_message, srl1.smap,
            srl1.start, srl1.end, "first here", srl2.smap, srl2.start, srl2.end,
            "second here");
        return RE_ERROR;
    }
    if (ambiguity) assert(false); // TODO: report ambiguity
    *result = res;
    return RE_OK;
}
int resolver_emit(resolver* r, llvm_module** module)
{
    // add module ctors and dtors
    if (r->module_group_constructor) {
        aseglist_add(&r->tc->t->module_ctors, r->module_group_constructor);
    }
    if (r->module_group_destructor) {
        // TODO: figure out when to run these
        aseglist_add(&r->tc->t->module_dtors, r->module_group_destructor);
    }

    // reserve symbols and claim ids
    ureg sym_count = r->private_sym_count + r->public_sym_count;
    ureg glob_id_start =
        atomic_ureg_add(&r->tc->t->node_ids, r->public_sym_count);
    llvm_error lle = llvm_backend_reserve_symbols(
        r->backend, sym_count, glob_id_start + r->public_sym_count);
    if (lle) return RE_ERROR;

    // mark nodes as resolved and remap ids
    ureg glob_id_head = glob_id_start;
    for (mdg_node** n = r->mdgs_begin; n != r->mdgs_end; n++) {
        aseglist_iterator it;
        aseglist_iterator_begin(&it, &(**n).module_frames);
        for (module_frame* mf = aseglist_iterator_next(&it); mf;
             mf = aseglist_iterator_next(&it)) {
            adjust_body_ids(r, &glob_id_head, &mf->body);
        }
    }
    int res = mdg_nodes_resolved(r->mdgs_begin, r->mdgs_end, r->tc);
    if (res) return RE_FATAL;
    assert(glob_id_head - glob_id_start == r->public_sym_count);

    // gen entrypoint in case if we are the root module
    mdg_node* root = r->tc->t->mdg.root_node;
    if (*r->mdgs_begin == root) {
        symbol* mainfn = NULL;
        symbol* startfn = NULL;
        resolve_error re = lookup_priv_module_symbol(
            r, root, COND_KW_START, &startfn,
            "multiple candidates for _start function");
        if (re) return re;
        if (!startfn) {
            re = lookup_priv_module_symbol(
                r, root, COND_KW_MAIN, &mainfn,
                "multiple candidates for main function");
            if (re) return re;
        }
        if (!mainfn && !startfn) {
            // TODO: maybe create a separate error kind for this?
            error_log_report_critical_failiure(
                r->tc->err_log,
                "no main or _start function found in root module");
            return RE_ERROR;
        }
        assert(!mainfn || mainfn->node.kind == SC_FUNC);
        assert(!startfn || startfn->node.kind == SC_FUNC);
        lle = llvm_backend_generate_entrypoint(
            r->backend, (sc_func*)mainfn, (sc_func*)startfn,
            &r->tc->t->module_ctors, &r->tc->t->module_dtors, glob_id_start,
            glob_id_head, sym_count);
        if (lle) return RE_ERROR;
    }

    if (tauc_success_so_far(r->tc->t) && r->tc->t->needs_emit_stage) {
        llvm_error lle = llvm_backend_emit_module(
            r->backend, glob_id_start, glob_id_head, r->private_sym_count);
        if (lle == LLE_FATAL) return RE_FATAL;
        if (lle) return RE_ERROR;
    }
    else {
        for (mdg_node** i = r->mdgs_begin; i != r->mdgs_end; i++) {
            rwlock_write(&(**i).lock);
            (**i).stage = MS_RESOLVING_ERROR;
            rwlock_end_write(&(**i).lock);
        }
        for (mdg_node** i = r->mdgs_begin; i != r->mdgs_end; i++) {
            list_rit rit;
            rwlock_read(&(**i).lock);
            list_rit_begin_at_end(&rit, &(**i).notify);
            rwlock_end_read(&(**i).lock);
            while (true) {
                mdg_node* dep = list_rit_prev(&rit);
                if (!dep) break;
                if (sccd_run(&r->tc->sccd, dep, SCCD_NOTIFY_DEP_ERROR)) {
                    return ERR;
                }
            }
        }
        *module = NULL;
    }
    return OK;
}
int resolver_resolve_and_emit(
    resolver* r, mdg_node** start, mdg_node** end, llvm_module** module)
{
    // reserve global symbol slots so that the pprn doesn't overrun the global
    // ids of other modules it might use
    ureg curr_max_glob_id = atomic_ureg_load(&r->tc->t->node_ids);
    llvm_error lle =
        llvm_backend_reserve_symbols(r->backend, 0, curr_max_glob_id);
    if (lle) return RE_ERROR;
    r->mdgs_begin = start;
    r->mdgs_end = end;
    int res;
    res = llvm_backend_init_module(r->backend, start, end, module);
    if (res) return ERR;
    TAU_TIME_STAGE_CTX(r->tc->t, res = resolver_resolve(r);
                       , print_debug_info(r, "resolving"));
    if (res) {
        free_pprns(r);
        return ERR;
    }
    TAU_TIME_STAGE_CTX(
        r->tc->t, res = prp_run_modules(&r->prp, r->mdgs_begin, r->mdgs_end);
        , print_debug_info(r, "running prp for"));
    if (res) return ERR;
    return resolver_emit(r, module);
}
int resolver_partial_fin(resolver* r, int i, int res)
{
    switch (i) {
        case -1:
        case 9: llvm_backend_delete(r->backend); // fallthrough
        case 8: prp_fin(&r->prp); // fallthrough
        case 7: ptrlist_fin(&r->pp_resolve_nodes_ready); // fallthrough
        case 6: ptrlist_fin(&r->pp_resolve_nodes_pending); // fallthrough
        case 5: sbuffer_fin(&r->pp_resolve_nodes_waiting); // fallthrough
        case 4: freelist_fin(&r->pp_resolve_nodes); // fallthrough
        case 3: pool_fin(&r->pprn_mem); // fallthrough
        case 2: sbuffer_fin(&r->temp_stack); // fallthrough
        case 1: stack_fin(&r->error_stack); // fallthrough
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
    e = sbuffer_init(&r->temp_stack, sizeof(ast_node*) * 32);
    if (e) return resolver_partial_fin(r, 1, e);
    e = pool_init(&r->pprn_mem);
    if (e) return resolver_partial_fin(r, 2, e);
    e = freelist_init(
        &r->pp_resolve_nodes, &r->pprn_mem, sizeof(pp_resolve_node));
    if (e) return resolver_partial_fin(r, 3, e);
    e = sbuffer_init(
        &r->pp_resolve_nodes_waiting, sizeof(pp_resolve_node*) * 16);
    if (e) return resolver_partial_fin(r, 4, e);
    e = ptrlist_init(&r->pp_resolve_nodes_pending, 16);
    if (e) return resolver_partial_fin(r, 5, e);
    e = ptrlist_init(&r->pp_resolve_nodes_ready, 16);
    if (e) return resolver_partial_fin(r, 6, e);
    e = prp_init(&r->prp, r->tc);
    if (e) return resolver_partial_fin(r, 7, e);
    r->backend = llvm_backend_new(r->tc);
    if (!r->backend) return resolver_partial_fin(r, 8, ERR);
    r->allow_type_loops = false;
    r->type_loop_start = NULL;
    r->curr_block_owner = NULL;
    return OK;
}
ast_elem* get_resolved_ast_node_ctype(ast_node* n)
{
    // we could optimize this but it's currently not worth it
    ast_elem* ctype;
    resolve_ast_node_raw(NULL, n, NULL, NULL, &ctype);
    return ctype;
}
