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
#include "trait_resolution.h"

typedef enum type_cast_result_e {
    TYPE_CAST_SUCCESS = 0,
    TYPE_CAST_INCOMPATIBLE,
    TYPE_CAST_POISONED,
} type_cast_result;
resolve_error
resolve_param(resolver* r, sym_param* p, bool generic, ast_elem** ctype);
resolve_error
resolve_module_frame(resolver* r, module_frame* block_owner, ast_body* b);
static inline resolve_error resolve_ast_node_raw(
    resolver* r, ast_node* n, ast_body* body, ast_elem** value,
    ast_elem** ctype);
resolve_error resolve_func(
    resolver* r, ast_body* requesting_body, sc_func_base* fnb,
    ast_node** continue_block);
resolve_error resolve_expr_body(
    resolver* r, ast_body* requesting_body, ast_node* expr, ast_body* b,
    bool* end_reachable);
resolve_error resolve_expr_scope_access(
    resolver* r, expr_scope_access* esa, symbol_table* st,
    access_modifier* access, ast_elem** value, ast_elem** ctype);
resolve_error
get_resolved_symbol_body(resolver* r, symbol* s, ast_body** tgt_body);
resolve_error resolve_func_from_call(
    resolver* r, ast_body* body, sc_func* fn, ast_elem** ctype);
resolve_error
pp_resolve_node_done(resolver* r, pp_resolve_node* pprn, bool* progress);
resolve_error pp_resolve_node_dep_ready(resolver* r, pp_resolve_node* pprn);
resolve_error pp_resolve_node_ready(resolver* r, pp_resolve_node* pprn);
resolve_error resolve_importing_node(
    resolver* r, import_module_data* im_data, ast_node* node, ast_body* body,
    bool dep_prop, ast_elem** value, ast_elem** ctype);
resolve_error resolver_suspend(resolver* r);
void print_pprns(resolver* r, char* msg, bool verbose);
void free_pprns(resolver* r, bool error_occured);
void print_pprn(resolver* r, pp_resolve_node* pprn, bool verbose, ureg ident);
static resolve_error add_ast_node_decls(
    resolver* r, ast_body* body, ast_body* shared_body, ast_node* n,
    bool public_st);
resolve_error handle_resolve_error(
    resolver* r, ast_node* n, ast_body* body, resolve_error re,
    ast_elem** value, ast_elem** ctype);
type_cast_result type_cast(
    ast_elem* source_type, ast_elem* target_type, ast_node** src_node_ptr);
resolve_error resolve_unordered_body(
    resolver* r, ast_body* requesting_body, ast_body* unordered_body,
    ast_node* owning_node, ast_elem** value, ast_elem** ctype);
// must be a macro so value and ctype become lazily evaluated

#define SET_VAL_CTYPE(pvalue, pctype, value, ctype)                            \
    do {                                                                       \
        if (pvalue) *(ast_elem**)(pvalue) = (ast_elem*)(value);                \
        if (pctype) *(ast_elem**)(pctype) = (ast_elem*)(ctype);                \
    } while (false)

#define SET_THEN_RETURN_IF_RESOLVED(resolved, pvalue, pctype, value, ctype)    \
    do {                                                                       \
        SET_VAL_CTYPE(pvalue, pctype, value, ctype);                           \
        if (resolved) return RE_OK;                                            \
    } while (false)

#define SET_THEN_RETURN(pvalue, pctype, value, ctype)                          \
    do {                                                                       \
        SET_VAL_CTYPE(pvalue, pctype, value, ctype);                           \
        return RE_OK;                                                          \
    } while (false)

#define RETURN_POISONED(r, re, node, body)                                     \
    do {                                                                       \
        ast_node* n = (ast_node*)(node);                                       \
        if (re != RE_FATAL) {                                                  \
            r->error_occured = true;                                           \
            if (curr_pprn_propagate_error(r, body)) return RE_FATAL;           \
            ast_node_set_poisoned(n);                                          \
            ast_node_set_resolved(n);                                          \
        }                                                                      \
        return re;                                                             \
    } while (false)

#define SET_THEN_RETURN_POISONED(                                              \
    r, re, node, body, pvalue, pctype, value, ctype)                           \
    do {                                                                       \
        SET_VAL_CTYPE(pvalue, pctype, value, ctype);                           \
        RETURN_POISONED(r, re, node, body);                                    \
    } while (false)

resolve_error add_resolve_error(resolve_error res, resolve_error add)
{
    if (!res) return add;
    if (res == RE_FATAL) return res;
    if (add == RE_SUSPENDED) return add;
    return res;
}
static void report_unknown_symbol(
    resolver* r, ast_node* n, ast_body* body, symbol* hidden_match, bool inst)
{
    // returned from symbol_lookup_iterator_get_hint_for_unknown
    // if the lookup traversed a poisoned scope
    if (hidden_match == (symbol*)NULL_PTR_PTR) return;
    src_range_large srl, srl_hidden;
    if (n->kind == EXPR_SCOPE_ACCESS || n->kind == EXPR_MEMBER_ACCESS) {
        src_range_unpack(((expr_scope_access*)n)->target_srange, &srl);
    }
    else {
        src_range_unpack(n->srange, &srl);
    }
    srl.smap = ast_body_get_smap(body);
    const char* msg = "unknown symbol";
    const char* annot = "use of an undefined symbol";
    if (hidden_match) {
        ast_node_get_src_range(
            (ast_node*)hidden_match, hidden_match->declaring_body, &srl_hidden);
        const char* hidden_msg;
        if (inst && !ast_node_get_instance_member((ast_node*)hidden_match)) {
            hidden_msg = "did you mean this? it's not available since it's not "
                         "an instance member";
        }
        else {
            hidden_msg =
                "did you mean this? it's not accessible in the current context";
        }
        error_log_report_annotated_twice(
            r->tc->err_log, ES_RESOLVER, false, msg, srl.smap, srl.start,
            srl.end, annot, srl_hidden.smap, srl_hidden.start, srl_hidden.end,
            hidden_msg);
    }
    else {
        error_log_report_annotated(
            r->tc->err_log, ES_RESOLVER, false, msg, srl.smap, srl.start,
            srl.end, annot);
    }
    r->error_occured = true;
}
void report_redeclaration_error(resolver* r, symbol* redecl, symbol* prev)
{
    src_range_large prev_st, redecl_st;
    ast_node_get_src_range(
        (ast_node*)redecl, redecl->declaring_body, &redecl_st);
    ast_node_get_src_range((ast_node*)prev, prev->declaring_body, &prev_st);
    error_log_report_annotated_twice(
        r->tc->err_log, ES_RESOLVER, false, "symbol redeclaration",
        prev_st.smap, prev_st.start, prev_st.end,
        "a symbol of this name is already defined in this "
        "scope",
        redecl_st.smap, redecl_st.start, redecl_st.end,
        "previous definition here");
    r->error_occured = true;
}
static inline ast_body*
get_decl_target_body(ast_node* decl, ast_body* body, ast_body* shared_body)
{
    ast_body* tgt;
    if (shared_body && ast_node_get_access_mod(decl) != AM_LOCAL) {
        tgt = shared_body;
    }
    else {
        tgt = body;
    }
    return ast_body_get_non_paste_parent(tgt);
}
int ast_node_propagate_error(resolver* r, ast_node* n)
{
    if (ast_node_get_contains_error(n)) return OK;
    ast_node_set_contains_error(n);
    int res;
    pp_resolve_node** pprnp = ast_node_try_get_pprn_ptr(n);
    if (pprnp && *pprnp) {
        pp_resolve_node* pprn = *pprnp;
        list_it it;
        list_it_begin(&it, &pprn->notify);
        pp_resolve_node* pprn_it;
        while ((pprn_it = list_it_next(&it, &pprn->notify))) {
            res = ast_node_propagate_error(r, pprn_it->node);
            if (res) return res;
        }
    }
    ast_body* b = ast_elem_try_get_body((ast_elem*)n);
    if (b) {
        res = ast_body_propagate_error(r, b->parent);
        if (res) return res;
    }
    return OK;
}
int pprn_propagate_error(resolver* r, pp_resolve_node* pprn)
{
    return ast_node_propagate_error(r, pprn->node);
}
int ast_body_propagate_error(resolver* r, ast_body* body)
{
    return ast_node_propagate_error(r, body->owning_node);
}
int curr_body_propagate_error(resolver* r, ast_body* body)
{
    assert(r->error_occured); // sanity check
    int res = ast_node_propagate_error(r, body->owning_node);
    if (res) return res;
    if (!r->curr_pp_node) return RE_OK;
    return pprn_propagate_error(r, r->curr_pp_node);
}
int curr_pprn_propagate_error(resolver* r, ast_body* body)
{
    pp_resolve_node* pprn;
    if (get_curr_pprn(r, body, &pprn)) return ERR;
    if (pprn) {
        int res = pprn_propagate_error(r, pprn);
        if (res) return res;
    }
    return curr_body_propagate_error(r, body);
}
bool is_curr_resolution_for_pastes(resolver* r, ast_body* body)
{
    if (r->curr_pp_node) {
        return !ast_body_pastes_done(r, r->curr_pp_node->declaring_body);
    }
    return !ast_body_pastes_done(r, body);
}
static resolve_error
add_symbol(resolver* r, ast_body* body, ast_body* shared_body, symbol* sym)
{
    sym->declaring_body = body;
    ast_body* tgt_body =
        get_decl_target_body((ast_node*)sym, body, shared_body);
    symbol* conflict;
    conflict = symbol_table_insert(tgt_body->symtab, sym);
    // symbol_table_inc_decl_count(tgtst);
    if (conflict) {
        report_redeclaration_error(r, sym, conflict);
        assert(ast_elem_is_node((ast_elem*)sym->declaring_body->owning_node));
        ast_node_set_poisoned(sym->declaring_body->owning_node);
        // TODO: poison the target body?
        // what to do for modules
        // symbol lookup should probably respect this
    }
    else {
        if (!ast_body_pastes_done(r, tgt_body)) {
            if (ppdct_add_symbol(&r->ppdct, sym, tgt_body, NULL))
                return RE_FATAL;
        }
    }
    return RE_OK;
}
ptrlist* get_pprn_list_from_state(resolver* r, pprn_state state)
{
    switch (state) {
        case PPRN_WAITING: return &r->pp_resolve_nodes_waiting;
        case PPRN_PENDING: return &r->pp_resolve_nodes_pending;
        case PPRN_READY: return &r->pp_resolve_nodes_ready;
        default: break;
    }
    panic("compiler bug");
    return NULL;
}
void remove_pprn_list_raw(resolver* r, pp_resolve_node* pprn)
{
    assert(pprn->pprn_list_entry);
    ptrlist* list = get_pprn_list_from_state(r, pprn->state);
    assert(!ptrlist_is_empty(list));
    pp_resolve_node* last = ptrlist_pop_back(list);
    *pprn->pprn_list_entry = last;
    last->pprn_list_entry = pprn->pprn_list_entry;
    pprn->pprn_list_entry = NULL;
    if (pprn->considered_committed) {
        pprn->considered_committed = false;
        assert(r->committed_waiters);
        r->committed_waiters--;
    }
}
resolve_error
pprn_set_state(resolver* r, pp_resolve_node* pprn, pprn_state state)
{
    if (pprn->state == state && pprn->pprn_list_entry) return RE_OK;
    if (pprn->pprn_list_entry) {
        remove_pprn_list_raw(r, pprn);
    }
    int res = ptrlist_append_get_pos(
        get_pprn_list_from_state(r, state), pprn,
        (void***)&pprn->pprn_list_entry);
    if (res) return RE_FATAL;
    if (r->tc->t->verbosity_flags & VERBOSITY_FLAGS_PPRNS) {
        char* msg[4] = {"", "waiting", "pending", "ready"};
        tprintf("pprn %s: ", msg[state]);
        print_pprn(r, pprn, false, 0);
        tflush();
    }
    if (state == PPRN_WAITING && ast_node_get_used_in_pp(pprn->node) &&
        !pprn->considered_committed) {
        r->committed_waiters++;
        pprn->considered_committed = true;
    }
    pprn->state = state;
    return RE_OK;
}
void remove_pprn_from_list(resolver* r, pp_resolve_node* pprn)
{
    remove_pprn_list_raw(r, pprn);
    if (r->tc->t->verbosity_flags & VERBOSITY_FLAGS_PPRNS) {
        tprintf("removed waiter: ");
        print_pprn(r, pprn, false, 0);
        tflush();
    }
}
void pprn_fin(resolver* r, pp_resolve_node* pprn, bool error_occured)
{
    ast_node* n = pprn->node;
    assert(n);
    assert(
        error_occured || pprn->dep_count == 0 || !ast_node_get_used_in_pp(n) ||
        ast_node_get_contains_error(n));
    for (pp_resolve_node* rn = pprn->first_unresolved_child; rn;
         rn = rn->next) {
        pprn_fin(r, rn, error_occured);
    }
    if (r->tc->t->verbosity_flags & VERBOSITY_FLAGS_PPRNS) {
        tprintf("freeing pprn: ");
        print_pprn(r, pprn, false, 0);
        tflush();
    }
    if (pprn->pprn_list_entry) {
        remove_pprn_from_list(r, pprn);
    }
    *ast_node_get_pprn_ptr(n) = NULL;
    list_fin(&pprn->notified_by, true);
    list_fin(&pprn->notify, true);
    freelist_free(&r->pp_resolve_nodes, pprn);
    // print_pprns(r, "", true);
}
static pp_resolve_node* pp_resolve_node_create(
    resolver* r, ast_node* n, ast_body* declaring_body, bool run_individually,
    bool sequential_block, bool dummy, bool notify_when_ready)
{
    pp_resolve_node* pprn = freelist_alloc(&r->pp_resolve_nodes);
    if (!pprn) return NULL;
    if (list_init(&pprn->notify)) {
        freelist_free(&r->pp_resolve_nodes, pprn);
        return NULL;
    }
    if (list_init(&pprn->notified_by)) {
        list_fin(&pprn->notify, true);
        freelist_free(&r->pp_resolve_nodes, pprn);
        return NULL;
    }
    pprn->dep_count = 0;
    pprn->activated = false;
    pprn->needs_further_resolution = false;
    pprn->notify_when_ready = notify_when_ready;
    pprn->pending_pastes = false;
    pprn->declaring_body = declaring_body;
    pprn->node = n;
    assert(n->kind != MF_MODULE);
    pprn->continue_block = NULL;
    pprn->next = NULL;
    // pprn->parent = NULL;
    pprn->run_individually = run_individually;
    pprn->considered_committed = false;
    pprn->block_pos_reachable = true;
    pprn->sequential_block = sequential_block;
    pprn->first_unresolved_child = NULL;
    pprn->pprn_list_entry = NULL;
    pprn->dummy = dummy;
    pprn->state = PPRN_INFANT;
    if (r->tc->t->verbosity_flags & VERBOSITY_FLAGS_PPRNS) {
        tprintf("allocated pprn: ");
        print_pprn(r, pprn, false, 0);
        tflush();
    }
    return pprn;
}
static inline resolve_error
get_curr_block_pprn(resolver* r, ast_body* body, pp_resolve_node** curr_pprn)
{
    body = ast_body_get_non_paste_parent(body);
    if (ast_elem_is_module_frame((ast_elem*)body->owning_node)) {
        // when in public scope, nobody depends on this
        //(for now, this might change with is_defined and so on)
        *curr_pprn = NULL;
        return RE_OK;
    }
    if (body->pprn) {
        *curr_pprn = body->pprn;
        return RE_OK;
    }
    // the only other non sequential blocks are module frames
    // for that case we already returned null earlier
    bool is_struct = ast_elem_is_struct((ast_elem*)body->owning_node);
    // TODO: body->parent might be wrong here
    body->pprn = pp_resolve_node_create(
        r, (ast_node*)body->owning_node, body->parent, !is_struct, !is_struct,
        is_struct, false);
    if (!body->pprn) return RE_FATAL;
    if (is_struct) body->pprn->pending_pastes = true;
    *curr_pprn = body->pprn;
    return RE_OK;
}
resolve_error
get_curr_pprn(resolver* r, ast_body* body, pp_resolve_node** curr_pprn)
{
    if (r->curr_pp_node) {
        *curr_pprn = r->curr_pp_node;
        return RE_OK;
    }
    return get_curr_block_pprn(r, body, curr_pprn);
}
static inline pp_resolve_node* try_get_curr_pprn(resolver* r, ast_body* body)
{
    if (r->curr_pp_node) return r->curr_pp_node;
    return body->pprn;
}
void resolver_set_ast_node_used_in_pp(resolver* r, ast_node* n)
{
    assert(!ast_node_get_used_in_pp(n));
    ast_node_set_used_in_pp(n);
    if (r->tc->t->verbosity_flags & VERBOSITY_FLAGS_USED_IN_PP) {
        tprintf("used in pp: ");
        print_ast_node(n, NULL, 0);
        tputs("");
        tflush();
    }
}
mdg_node* resolver_resolving_root(resolver* r)
{
    // since these are sorted ascending by id from the
    // sccd, root will always be the first
    mdg_node* rn = *r->mdgs_begin;
    return (rn == r->tc->t->mdg.root_node) ? rn : NULL;
}
bool is_lvalue(ast_elem* expr)
{
    if (expr->kind == EXPR_PARENTHESES) {
        return is_lvalue((ast_elem*)((expr_parentheses*)expr)->child);
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
resolve_error pprn_set_used_in_pp(resolver* r, pp_resolve_node* pprn)
{
    if (ast_node_get_used_in_pp(pprn->node)) return RE_OK;
    resolver_set_ast_node_used_in_pp(r, pprn->node);
    resolve_error re;
    if (ast_elem_is_any_import((ast_elem*)pprn->node) &&
        !ast_node_get_resolved(pprn->node) &&
        !ast_node_get_resolving(pprn->node)) {
        import_module_data* im_data = NULL;
        if (pprn->node->kind == SYM_IMPORT_MODULE) {
            sym_import_module* im = (sym_import_module*)pprn->node;
            im_data = &im->im_data;
        }
        else if (ast_elem_is_import_group((ast_elem*)pprn->node)) {
            import_group_get_data(pprn->node, NULL, &im_data, NULL, NULL);
        }
        if (im_data) {
            ast_node_set_resolving(pprn->node);
            resolve_error re = resolve_importing_node(
                r, im_data, pprn->node, pprn->declaring_body, true, NULL, NULL);
            if (re) {
                return handle_resolve_error(
                    r, pprn->node, pprn->declaring_body, re, NULL, NULL);
            }
        }
    }
    if (pprn->state == PPRN_WAITING && !pprn->considered_committed) {
        r->committed_waiters++;
        pprn->considered_committed = true;
    }
    list_it it;
    list_it_begin(&it, &pprn->notified_by);
    pp_resolve_node** pprnp;
    while ((pprnp = list_it_next(&it, &pprn->notified_by))) {
        if (!*pprnp) continue;
        re = pprn_set_used_in_pp(r, *pprnp);
        if (re) return re;
    }
    return RE_OK;
}
static inline resolve_error
pprn_depend_on_raw(pp_resolve_node** dependency_p, pp_resolve_node* depending)
{
    if (list_append(&(**dependency_p).notify, NULL, depending)) return RE_FATAL;
    if (list_append(&depending->notified_by, NULL, dependency_p))
        return RE_FATAL;
    depending->dep_count++;
    return RE_OK;
}
static inline resolve_error
curr_pprn_depend_on(resolver* r, ast_body* body, pp_resolve_node** dependency_p)
{
    pp_resolve_node* dependency = *dependency_p;
    pp_resolve_node* depending;
    resolve_error re = get_curr_pprn(r, body, &depending);
    if (re) return re;
    if (!depending) return RE_OK;
    if (ast_node_get_used_in_pp(depending->node)) {
        re = pprn_set_used_in_pp(r, dependency);
        if (re) return re;
    }
    if (ast_node_get_contains_error(dependency->node)) {
        curr_pprn_propagate_error(r, body);
    }
    if (dependency->notify_when_ready && dependency->state == PPRN_READY)
        return RE_OK;
    return pprn_depend_on_raw(dependency_p, depending);
}
static resolve_error
pprn_add_child_raw(pp_resolve_node* block_parent, pp_resolve_node** child_p)
{
    pp_resolve_node* child = *child_p;
    child->run_individually = false;
    child->notify_when_ready = true;
    if (block_parent->first_unresolved_child == NULL) {
        block_parent->first_unresolved_child = child;
        block_parent->last_unresolved_child = child;
    }
    else {
        block_parent->last_unresolved_child->next = child;
        block_parent->last_unresolved_child = child;
    }
    if (child->dep_count) {
        if (pprn_depend_on_raw(child_p, block_parent)) return RE_FATAL;
    }
    return RE_OK;
}
static resolve_error
curr_pp_block_add_child(resolver* r, ast_body* body, pp_resolve_node** child_p)
{
    pp_resolve_node* child = *child_p;
    pp_resolve_node* block_parent;
    assert(!child->activated);
    if (child->next) return RE_OK;
    resolve_error re = get_curr_block_pprn(r, body, &block_parent);
    if (re) return re;
    if (!block_parent) return RE_OK;
    // child->parent = parent;
    if (!block_parent->sequential_block) {
        if (pprn_depend_on_raw(child_p, block_parent)) return RE_FATAL;
        if (ast_node_get_used_in_pp(block_parent->node)) {
            pprn_set_used_in_pp(r, child);
        }
        return RE_OK;
    }
    return pprn_add_child_raw(block_parent, child_p);
}
resolve_error pp_resolve_node_activate(
    resolver* r, ast_body* body, pp_resolve_node** pprn_p, bool resolved)
{
    pp_resolve_node* pprn = *pprn_p;
    pprn->activated = true;
    assert(!resolved || ast_node_get_resolved(pprn->node));
    if (pprn->dep_count == 0 && r->module_group_constructor) {
        // HACK: make sure the module group ctor runs first
        // TODO: implement this properly
        resolve_func_from_call(r, body, r->module_group_constructor, NULL);
    }
    if (pprn->dep_count == 0) {
        if (resolved || ast_node_get_contains_error(pprn->node)) {
            pprn->needs_further_resolution = false;
            return pp_resolve_node_ready(r, pprn);
        }
        if (pprn->run_individually) {
            if (pprn->first_unresolved_child) {
                return pprn_set_state(r, pprn, PPRN_READY);
            }
            else {
                return pprn_set_state(r, pprn, PPRN_PENDING);
            }
        }
        return RE_OK;
    }
    return pprn_set_state(r, pprn, PPRN_WAITING);
}
// we differentiate between local ast nodes and shared ones
// local nodes are only visible within the module and can be
// freed once the module was emitted. shared nodes might be used from
// other modules and therefore need to stay around for the whole compilation
static bool is_local_node(ast_elem* e)
{
    if (!ast_elem_is_node(e)) return false;
    access_modifier m = ast_node_get_access_mod((ast_node*)e);
    return m != AM_PUBLIC && m != AM_PROTECTED;
}
ureg ast_node_claim_id(resolver* r, ast_node* n, bool public_st)
{
    if (public_st && !is_local_node((ast_elem*)n)) {
        r->public_sym_count++;
    }
    else {
        r->private_sym_count++;
    }
    return r->id_space++;
}
ureg claim_symbol_id(resolver* r, symbol* s, bool public_st)
{
    ast_body* decl_body = ast_body_get_non_paste_parent(s->declaring_body);
    ast_elem* on = (ast_elem*)decl_body->owning_node;
    if (ast_elem_is_var((ast_elem*)s) && !ast_node_get_static(&s->node) &&
        ast_elem_is_struct(on)) {
        ast_node_set_instance_member(&s->node);
        // since these are per instance we don't give them a variable id
        // the backend will assign struct member ids starting from 0
        return UREG_MAX;
    }
    else {
        return ast_node_claim_id(r, (ast_node*)s, public_st);
    }
}
static resolve_error add_func_decl(
    resolver* r, ast_body* body, ast_body* shared_body, ast_node* n,
    bool public_st)
{
    ast_body* b = &((scope*)n)->body;
    ureg param_count;
    resolve_error re;
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
        assert(fng->generic_param_count == 0 || b->owning_node == n);
        for (ureg i = 0; i < fng->generic_param_count; i++) {
            symbol* p = (symbol*)&fng->generic_params[i];
            re = add_symbol(r, b, NULL, p);
            if (re) return re;
            ast_node_set_declared((ast_node*)p);
        }
    }
    ast_body* tgt_body = get_decl_target_body(n, body, shared_body);
    symbol** conflict;
    symbol* sym = (symbol*)n;
    sym->declaring_body = body;
    conflict = symbol_table_lookup(tgt_body->symtab, sym->name);
    if (!*conflict) {
        symbol_table_inc_sym_count(tgt_body->symtab);
        *conflict = sym;
        sym->next = NULL;
    }
    else {
        sym_func_overloaded* sfo;
        if ((**conflict).node.kind == SC_FUNC) {
            sfo = (sym_func_overloaded*)pool_alloc(
                &r->tc->permmem, sizeof(sym_func_overloaded));
            if (!sfo) return RE_FATAL;
            sfo->sym.node.kind = SYM_FUNC_OVERLOADED;
            sfo->sym.node.flags = AST_NODE_FLAGS_DEFAULT;
            ast_node_set_access_mod(&sfo->sym.node, AM_PUBLIC);
            sfo->sym.node.srange = SRC_RANGE_INVALID;
            sfo->sym.next = (**conflict).next;
            sfo->sym.name = (**conflict).name;
            sfo->sym.declaring_body = tgt_body;
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
            report_redeclaration_error(r, sym, *conflict);
        }
    }
    // we only do the parameters here because the declaration and
    // use func body vars is strongly ordered
    assert(param_count == 0 || b->owning_node == n);
    for (ureg i = 0; i < param_count; i++) {
        re = add_symbol(r, b, NULL, (symbol*)&params[i]);
        if (re) return re;
        ast_node_set_declared((ast_node*)&params[i]);
    }
    if (n->kind == SC_FUNC) {
        sc_func* fn = (sc_func*)n;
        if (cstr_eq(fn->fnb.sc.osym.sym.name, COND_KW_CONSTRUCT)) {
            if (r->module_group_constructor) {
                src_range_large mgc1_srl;
                ast_node_get_src_range(
                    (ast_node*)r->module_group_constructor,
                    r->module_group_constructor->fnb.sc.osym.sym.declaring_body,
                    &mgc1_srl);
                src_range_large mgc2_srl;
                ast_node_get_src_range(
                    (ast_node*)r->module_group_constructor,
                    r->module_group_constructor->fnb.sc.osym.sym.declaring_body,
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
                r, (ast_node*)fn, body, true, true, false, false);
            if (!pprn) return RE_FATAL;
            fn->fnb.sc.body.pprn = pprn;
            r->module_group_constructor = fn;
        }
        else if (cstr_eq(fn->fnb.sc.osym.sym.name, COND_KW_DESTRUCT)) {
            if (r->module_group_destructor) {
                src_range_large mgc1_srl;
                ast_node_get_src_range(
                    (ast_node*)r->module_group_constructor,
                    r->module_group_constructor->fnb.sc.osym.sym.declaring_body,
                    &mgc1_srl);
                src_range_large mgc2_srl;
                ast_node_get_src_range(
                    (ast_node*)r->module_group_constructor,
                    r->module_group_constructor->fnb.sc.osym.sym.declaring_body,
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
                r, (ast_node*)fn, body, true, true, false, false);
            if (!pprn) return RE_FATAL;
            fn->fnb.sc.body.pprn = pprn;
            r->module_group_destructor = fn;
        }
    }
    return RE_OK;
}
static resolve_error get_import_parent(
    resolver* r, ast_body* body, mdg_node* parent, mdg_node* relative_to,
    sym_import_parent** tgt)
{
    resolve_error re;
    sym_import_parent* parent_ip;
    if (parent == relative_to || parent == r->tc->t->mdg.root_node) {
        *tgt = NULL;
        return RE_OK;
    };
    re = get_import_parent(r, body, parent->parent, relative_to, &parent_ip);
    if (re) return re;
    symbol_table* st = body->symtab;
    symbol_table** stp = &body->symtab;
    if (parent_ip) {
        if (!ast_node_get_resolved(&parent_ip->osym.sym.node)) {
            *tgt = parent_ip;
            return RE_OK;
        }
        st = parent_ip->symtab;
        stp = &parent_ip->symtab;
    }
    symbol** conflict = symbol_table_lookup(st, parent->name);
    if (*conflict) {
        if ((**conflict).node.kind == SYM_IMPORT_PARENT) {
            *tgt = (sym_import_parent*)*conflict;
            if ((**tgt).module != parent) {
                assert(false); // TODO: error message different module
            }
            return RE_OK;
        }
        assert(false); // TODO: error message for symbol redeclaration
    }
    sym_import_parent* ip =
        pool_alloc(&r->tc->permmem, sizeof(sym_import_parent));
    if (!ip) return RE_FATAL;
    ip->children = NULL;
    ip->module = parent;
    ip->osym.visible_within_body = NULL;
    ip->osym.sym.declaring_body = body;
    ip->osym.sym.name = parent->name;
    ip->osym.sym.node.srange = SRC_RANGE_INVALID;
    ip->osym.sym.node.flags = AST_NODE_FLAGS_DEFAULT;
    ip->osym.sym.node.kind = SYM_IMPORT_PARENT;
    *conflict = (symbol*)ip;
    ip->osym.sym.next = NULL;
    if (symbol_table_amend(stp, 1, 0)) return RE_FATAL;
    if (*stp != st) { // insert because the amend realloced
        st = *stp;
        symbol* res = symbol_table_insert(st, (symbol*)ip);
        assert(!res); // we checked that before
        UNUSED(res);
    }
    else {
        *conflict = (symbol*)ip;
        ip->osym.sym.next = NULL;
    }
    *tgt = ip;
    return RE_OK;
}
static inline resolve_error add_anonymous_import_group_decls(
    resolver* r, ast_body* body, ast_body* shared_body, ast_node* n,
    bool public_st)
{
    list* l;
    if (n->kind == ASTN_ANONYMOUS_MOD_IMPORT_GROUP) {
        l = &((astn_anonymous_mod_import_group*)n)->ig_data.children_ordered;
    }
    else {
        assert(n->kind == ASTN_ANONYMOUS_SYM_IMPORT_GROUP);
        l = &((astn_anonymous_sym_import_group*)n)->ig_data.children_ordered;
    }
    list_it it;
    list_it_begin(&it, l);
    ast_node* el;
    resolve_error re;
    while ((el = list_it_next(&it, l))) {
        re = add_ast_node_decls(r, body, shared_body, el, public_st);
        if (re) return re;
    }
    return RE_OK;
}
// public st needs to be a seperate parameter since even when there's no shared
// st a public structs public members are public
static resolve_error add_ast_node_decls(
    resolver* r, ast_body* body, ast_body* shared_body, ast_node* n,
    bool public_st)
{
    if (n == NULL) return RE_OK;
    if (ast_node_get_declared(n)) return RE_OK;
    ast_node_set_declared(n);
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
                pp_resolve_node_create(r, n, body, false, false, true, true);
            if (!pprn) return RE_FATAL;
            if (ptrlist_append(&r->pp_resolve_nodes_pending, pprn))
                return RE_FATAL;
            return RE_OK;
        }
        case EXPR_PP: {
            expr_pp* epp = (expr_pp*)n;
            pp_resolve_node* prevcurr = r->curr_pp_node;
            bool create_pp =
                shared_body || ast_elem_is_struct((ast_elem*)body->owning_node);
            if (create_pp) {
                // we need to add these during add decls already
                // because we want to handle these before resolving
                // any non pp stuff in the scope
                assert(epp->pprn == NULL);
                bool is_expr = ast_elem_is_expr((ast_elem*)epp->pp_expr);
                pp_resolve_node* pprn = pp_resolve_node_create(
                    r, is_expr ? n : epp->pp_expr, body, true, false, false,
                    false);
                if (!pprn) return RE_FATAL;
                epp->pprn = pprn;
                re = pprn_set_state(r, epp->pprn, PPRN_PENDING);
                if (re) return RE_FATAL;
                r->curr_pp_node = pprn;
            }
            re = add_ast_node_decls(
                r, body, shared_body, epp->pp_expr, public_st);
            if (create_pp) r->curr_pp_node = prevcurr;
            if (re) return re;
            return RE_OK;
        }
        case SC_STRUCT_GENERIC: {
            sc_struct_generic* sg = (sc_struct_generic*)n;
            for (ureg i = 0; i < sg->generic_param_count; i++) {
                re = add_symbol(
                    r, &sg->sb.sc.body, NULL, (symbol*)&sg->generic_params[i]);
                if (re) return re;
            }
            return add_symbol(r, body, shared_body, (symbol*)sg);
        }
        case SC_STRUCT:
        case SC_STRUCT_GENERIC_INST: {
            // generic inst 'inherits' from struct
            sc_struct* s = (sc_struct*)n;
            s->type_derivs.ptr_id = ptr_map_claim_id(&r->pm);
            s->type_derivs.slice_id = ptr_map_claim_id(&r->pm);
            re = add_symbol(r, body, shared_body, (symbol*)s);
            if (re) return re;
            // bool members_public_st =
            // shared_body && !is_local_node((ast_elem*)n);
            s->backend_id = claim_symbol_id(r, (symbol*)s, public_st);

            return RE_OK;
        }
        case SC_MACRO:
        case SC_FUNC:
        case SC_FUNC_GENERIC: {
            return add_func_decl(r, body, shared_body, n, public_st);
        }
        case SYM_VAR_INITIALIZED:
        case SYM_VAR: {
            re = add_symbol(r, body, shared_body, (symbol*)n);
            if (re) return re;
            sym_var* v = (sym_var*)n;
            v->var_id = claim_symbol_id(r, (symbol*)v, public_st);
            return RE_OK;
        }
        case STMT_PASTE_EVALUATION: {
            paste_evaluation* pe = (paste_evaluation*)n;
            for (ast_node** e = pe->body.elements; *e; e++) {
                re = add_ast_node_decls(
                    r, &pe->body, shared_body, *e, public_st);
                if (re) return re;
            }
            return RE_OK;
        }
        case SYM_NAMED_USE: {
            return add_symbol(r, body, shared_body, (symbol*)n);
        }
        case SYM_IMPORT_MODULE: {
            sym_import_module* im = (sym_import_module*)n;
            im->osym.sym.declaring_body = body;
            mdg_node* rel_to;
            if (ast_node_get_relative_import(n)) {
                rel_to = im->im_data.importing_module;
            }
            else {
                rel_to = r->tc->t->mdg.root_node;
            }
            sym_import_parent* ip;
            re = get_import_parent(
                r, body, im->im_data.imported_module->parent, rel_to, &ip);
            if (re) return re;
            symbol_table* st;
            if (ip) {
                if (ast_node_get_resolved(&ip->osym.sym.node)) {
                    im->osym.sym.next = ip->children;
                    ip->children = (symbol*)im;
                    return RE_OK;
                }
                st = ip->symtab;
            }
            else {
                st = get_decl_target_body((ast_node*)im, body, shared_body)
                         ->symtab;
                assert(st); // TODO: alloc table
            }
            symbol* conflict = symbol_table_insert(st, (symbol*)im);
            if (conflict) {
                report_redeclaration_error(r, (symbol*)im, conflict);
            }
            symbol_table_insert_use(
                st, ast_node_get_access_mod(n), n,
                &im->im_data.imported_module->body, true, false);
            return RE_OK;
        }
        // this gets here because anonymous modules delegate their members
        case SYM_IMPORT_SYMBOL: {
            sym_import_symbol* s = (sym_import_symbol*)n;
            return add_symbol(r, body, shared_body, (symbol*)s);
        }
        case SYM_NAMED_MOD_IMPORT_GROUP: {
            symbol* sym = (symbol*)n;
            sym->declaring_body = body;
            ast_body* tgt_body = get_decl_target_body(n, body, shared_body);
            symbol** conflict;
            conflict = symbol_table_lookup(tgt_body->symtab, sym->name);
            if (!*conflict) {
                *conflict = sym;
                sym->next = NULL;
                symbol_table_inc_sym_count(tgt_body->symtab);
                return RE_OK;
            }
            assert(false); // TODO: throw redeclaration error
            return RE_ERROR;
        }
        case ASTN_ANONYMOUS_SYM_IMPORT_GROUP:
        case ASTN_ANONYMOUS_MOD_IMPORT_GROUP: {
            return add_anonymous_import_group_decls(
                r, body, shared_body, n, public_st);
        } break;
        case SC_TRAIT_GENERIC: {
            sc_trait_generic* tg = (sc_trait_generic*)n;
            for (ureg i = 0; i < tg->generic_param_count; i++) {
                re = add_symbol(
                    r, &tg->sb.sc.body, NULL, (symbol*)&tg->generic_params[i]);
                if (re) return re;
            }
        } // fallthrough
        case SC_TRAIT: {
            sc_struct_base* t = (sc_struct_base*)n;
            re = add_symbol(r, body, shared_body, (symbol*)n);
            bool members_public_st =
                shared_body && !is_local_node((ast_elem*)n);
            re = add_body_decls(r, &t->sc.body, NULL, members_public_st);
            if (re) return re;
            return RE_OK;
        } break;
        case TRAIT_IMPL_GENERIC: {
            ast_body* tgt_body = get_decl_target_body(n, body, shared_body);
            assert(tgt_body->symtab->tt);
            int res = trait_table_append_generic_impl(
                tgt_body->symtab->tt, (trait_impl_generic*)n);
            if (res) return RE_FATAL;
            return RE_OK;
        } break;
        case TRAIT_IMPL: {
            trait_impl* ti = (trait_impl*)n;
            ast_body* tgt_body = get_decl_target_body(n, body, shared_body);
            assert(tgt_body->symtab->tt);
            if (ast_elem_has_unordered_body((ast_elem*)body->owning_node)) {
                int res = trait_table_append_unresolved_impl(
                    tgt_body->symtab->tt, ti);
                if (res) return RE_FATAL;
            }
            ti->backend_id = ast_node_claim_id(r, n, public_st);
            return RE_OK;
        } break;
        default:
            assert(false); // unknown node_kind
            return RE_FATAL;
    }
    assert(false);
    return RE_FATAL;
}
static inline resolve_error parse_int_literal(
    resolver* r, expr_literal* lit, ast_body* body, ureg* result,
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
        res *= 10;
        res += digit;
        str++;
    }
    if (!overflow) {
        if (lit->node.pt_kind == PT_FLUID_INT) {
            lit->node.pt_kind = negative ? PT_INT : PT_UINT;
        }
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
    lit->node.pt_kind = PT_ERROR;
    if (ast_node_propagate_error(r, (ast_node*)lit)) return RE_FATAL;
    if (curr_body_propagate_error(r, body)) return RE_FATAL;
    src_range_large srl;
    ast_node_get_src_range((ast_node*)lit, body, &srl);
    error_log_report_annotated(
        r->tc->err_log, ES_RESOLVER, false, "integer literal overflow",
        srl.smap, srl.start, srl.end, "in this integer literal");
    return RE_OK;
}
static resolve_error evaluate_array_bounds(
    resolver* r, expr_array_type* ad, ast_body* body, ureg arr_expr_len,
    ureg* res)
{
    ast_elem* t;
    if (!ad->length_spec && arr_expr_len == UREG_MAX) {
        assert(false); // TODO: error we need either explcit or implicit bounds
        return RE_ERROR;
    }
    if (!ad->length_spec) {
        *res = arr_expr_len;
        return RE_OK;
    }
    resolve_error re = resolve_ast_node(r, ad->length_spec, body, NULL, &t);
    if (re) return re;
    if (t == ERROR_ELEM) {
        ast_node_set_poisoned((ast_node*)ad);
        if (curr_body_propagate_error(r, body)) return RE_FATAL;
        return RE_OK;
    }
    bool negative = false;
    bool incompatible = false;
    if (ad->length_spec->kind == EXPR_LITERAL) {
        expr_literal* lit = (expr_literal*)ad->length_spec;
        if (lit->node.pt_kind == PT_UINT || lit->node.pt_kind == PT_INT) {
            *res = lit->value.val_ureg;
            negative = (lit->node.pt_kind == PT_INT && (sreg)*res < 0);
        }
        else {
            incompatible = true;
        }
    }
    else if (ad->length_spec->kind == EXPR_PP) {
        expr_pp* epp = (expr_pp*)ad->length_spec;
        if (!ast_node_get_emitted_for_pp((ast_node*)epp)) {
            return RE_UNREALIZED_COMPTIME;
        }
        // HACK
        if (!type_cast(epp->ctype, (ast_elem*)&PRIMITIVES[PT_UINT], NULL)) {
            *res = *(ureg*)epp->result;
        }
        else if (!type_cast(epp->ctype, (ast_elem*)&PRIMITIVES[PT_INT], NULL)) {
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
        ast_node_get_src_range(ad->length_spec, body, &bounds_srl);
        ast_node_get_src_range((ast_node*)ad, body, &array_srl);
        // TODO: different error for negative values
        error_log_report_annotated_twice(
            r->tc->err_log, ES_RESOLVER, false, "invalid type for array bounds",
            bounds_srl.smap, bounds_srl.start, bounds_srl.end, "expected uint",
            array_srl.smap, array_srl.start, array_srl.end,
            "in the array bounds for this array");
        r->error_occured = true;
        return RE_ERROR;
    }
    if (negative) {
        src_range_large bounds_srl;
        src_range_large array_srl;
        ast_node_get_src_range(ad->length_spec, body, &bounds_srl);
        ast_node_get_src_range((ast_node*)ad, body, &array_srl);
        error_log_report_annotated_twice(
            r->tc->err_log, ES_RESOLVER, false,
            "array length can't be negative", bounds_srl.smap, bounds_srl.start,
            bounds_srl.end, "expected positive integer", array_srl.smap,
            array_srl.start, array_srl.end,
            "in the array bounds for this array");
        r->error_occured = true;
        return RE_ERROR;
    }
    return RE_OK;
}
resolve_error add_body_decls(
    resolver* r, ast_body* body, ast_body* shared_body, bool public_st)
{
    if (body->elements[0] && ast_node_get_declared(body->elements[0])) {
        return RE_OK;
    }
    for (ast_node** n = body->elements; *n; n++) {
        resolve_error re =
            add_ast_node_decls(r, body, shared_body, *n, public_st);
        assert(r->curr_pp_node == NULL);
        if (re) return re;
    }
    return RE_OK;
}
static inline void print_debug_info(resolver* r, const char* flavortext)
{
    tprintf("%s {", flavortext);
    mdg_node** i = r->mdgs_begin;
    for (; i + 1 != r->mdgs_end; i++) {
        tprintf("%s, ", (**i).name);
    }
    tprintf("%s} ", (**i).name);
}
type_cast_result
type_cast(ast_elem* target_type, ast_elem* source_type, ast_node** src_node_ptr)
{
    if (source_type == ERROR_ELEM || target_type == ERROR_ELEM) {
        return TYPE_CAST_POISONED;
    }
    // immediately return true e.g. for primitives
    if (source_type == target_type) return TYPE_CAST_SUCCESS;
    if (source_type->kind == TYPE_POINTER &&
        target_type->kind == TYPE_POINTER) {
        // TODO: insert cast
        return type_cast(
            ((type_pointer*)target_type)->base_type,
            ((type_pointer*)source_type)->base_type, NULL);
    }
    if (source_type->kind == TYPE_ARRAY && target_type->kind == TYPE_ARRAY) {
        type_array* src_arr = (type_array*)source_type;
        type_array* tgt_arr = (type_array*)target_type;
        if (src_arr->length != tgt_arr->length) return TYPE_CAST_INCOMPATIBLE;
        ast_node* res_node = NULL;
        type_cast_result r = type_cast(
            tgt_arr->slice_type.ctype_members,
            src_arr->slice_type.ctype_members, &res_node);
        // TODO: insert cast
        return r;
    }
    if (source_type == (ast_elem*)&PRIMITIVES[PT_UNDEFINED] ||
        source_type == (ast_elem*)&PRIMITIVES[PT_DEFINED]) {
        return TYPE_CAST_SUCCESS;
    }
    return TYPE_CAST_INCOMPATIBLE; // TODO
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
    if (type_cast(lhs, param_ctype, NULL)) {
        *applicable = false;
        return RE_OK;
    }
    re = resolve_param(r, &f->fnb.params[0], false, &param_ctype);
    if (re) return re;
    if (type_cast(rhs, param_ctype, NULL)) {
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
        if (type_cast(ctype, call_arg_types[i], NULL)) {
            *applicable = false;
            return RE_OK;
        }
    }
    *applicable = true;
    if (fn) {
        if (!ast_node_get_resolved((ast_node*)fn)) {
            if (!fn->fnb.return_type) {
                fn->fnb.return_ctype = VOID_ELEM;
            }
            else {
                resolve_error re = resolve_ast_node(
                    r, fn->fnb.return_type, overload->osym.sym.declaring_body,
                    &fn->fnb.return_ctype, NULL);
                if (re) return re;
            }
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
    resolver* r, expr_macro_call* emc, ast_body* body, ast_elem** value,
    ast_elem** ctype)
{
    assert(false); // TODO
    return RE_FATAL;
}
static inline resolve_error resolve_no_block_macro_call(
    resolver* r, expr_call* nbmc, ast_body* body, sc_macro* m, ast_elem** ctype)
{
    assert(false); // TODO
    return RE_FATAL;
}

// for ex. wth foo::bar() lookup_body is foo's st, looking_body is the body
// containing the call, args are looked up there
resolve_error resolve_func_call(
    resolver* r, expr_call* c, ast_elem* lhs_ctype, bool lhs_is_instance,
    ast_body* looking_body, const char* func_name, ast_body* lookup_body,
    ast_elem** ctype)
{
    ast_elem** call_arg_types =
        sbuffer_append(&r->temp_buffer, c->arg_count * sizeof(ast_elem*));
    resolve_error re = RE_OK;
    for (ureg i = 0; i < c->arg_count; i++) {
        re = resolve_ast_node(
            r, c->args[i], looking_body, NULL, &call_arg_types[i]);
        if (re) return re;
    }
    scope* tgt;
    symbol_lookup_iterator sli;
    re = symbol_lookup_iterator_init(
        &sli, r, lookup_body, lhs_ctype, looking_body, func_name,
        lhs_is_instance, false, true);
    bool applicable = false;
    if (re) {
        sbuffer_remove_back(&r->temp_buffer, c->arg_count * sizeof(ast_elem*));
        return re;
    }
    bool overload_existant = false;
    while (true) {
        symbol* sym;
        re = symbol_lookup_iterator_next(&sli, &sym);
        if (re) {
            sbuffer_remove_back(
                &r->temp_buffer, c->arg_count * sizeof(ast_elem*));
            return re;
        }
        if (sym == NULL) {
            if (overload_existant) break;
            // we use st instead of func_st here because thats the scope
            // that the call is in
            sbuffer_remove_back(
                &r->temp_buffer, c->arg_count * sizeof(ast_elem*));
            bool notifier_added = false;
            int res = ppdct_curr_pprn_require_symbol(
                &r->ppdct, looking_body, lhs_ctype, func_name, &notifier_added);
            if (res) return RE_FATAL;
            if (notifier_added) return RE_UNKNOWN_SYMBOL;
            report_unknown_symbol(
                r, c->lhs, looking_body,
                symbol_lookup_iterator_get_hint_for_unknown(&sli),
                sli.lhs_is_instance);
            SET_THEN_RETURN_POISONED(
                r, RE_OK, c, looking_body, NULL, ctype, c, ERROR_ELEM);
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
    sbuffer_remove_back(&r->temp_buffer, c->arg_count * sizeof(ast_elem*));
    if (!applicable) {
        bool notifier_added = false;
        int res = ppdct_curr_pprn_require_symbol(
            &r->ppdct, looking_body, lhs_ctype, func_name, &notifier_added);
        if (res) return RE_FATAL;
        if (notifier_added) return RE_UNKNOWN_SYMBOL;
        src_range_large srl;
        ast_node_get_src_range(c->lhs, looking_body, &srl);
        error_log_report_annotated(
            r->tc->err_log, ES_RESOLVER, false, "no matching overload",
            srl.smap, srl.start, srl.end,
            "no available overload is applicable for the given arguments");
        return RE_ERROR;
    }
    if (re) return re;
    if (tgt->osym.sym.node.kind == SC_MACRO) {
        c->node.kind = EXPR_NO_BLOCK_MACRO_CALL;
        re = resolve_no_block_macro_call(
            r, c, looking_body, (sc_macro*)tgt, ctype);
    }
    else {
        c->target.fn = (sc_func*)tgt;
        // we sadly need to do this so the resolved flag means
        //"ready to emit and run" which we need for the pp
        re = resolve_func_from_call(r, looking_body, (sc_func*)tgt, ctype);
        if (!re) ast_node_set_resolved(&c->node);
    }
    return re;
}
// see func call for why we need body and st (foo::bar(args + 1))
resolve_error
resolve_call(resolver* r, expr_call* c, ast_body* body, ast_elem** ctype)
{
    if (c->lhs->kind == EXPR_IDENTIFIER) {
        return resolve_func_call(
            r, c, NULL, false, body, ((expr_identifier*)c->lhs)->value.str,
            body, ctype);
    }
    if (c->lhs->kind == EXPR_SCOPE_ACCESS) {
        expr_scope_access* esa = (expr_scope_access*)c->lhs;
        ast_elem* esa_lhs;
        resolve_error re = resolve_ast_node(r, esa->lhs, body, &esa_lhs, NULL);
        if (re) return re;
        if (esa_lhs == ERROR_ELEM) {
            SET_THEN_RETURN_POISONED(
                r, RE_OK, c, body, NULL, ctype, NULL, ERROR_ELEM);
        }
        if (ast_elem_is_from_module(esa_lhs) ||
            ast_elem_is_any_import(esa_lhs)) {
            assert(ast_elem_is_symbol(esa_lhs));
            ast_body* lhs_body;
            re = get_resolved_symbol_body(r, (symbol*)esa_lhs, &lhs_body);
            if (re) return re;
            return resolve_func_call(
                r, c, NULL, false, body, esa->target.name, lhs_body, ctype);
        }
        else {
            return resolve_func_call(
                r, c, esa_lhs, false, body, esa->target.name, body, ctype);
        }
    }
    if (c->lhs->kind == EXPR_MEMBER_ACCESS) {
        expr_member_access* esa = (expr_member_access*)c->lhs;
        ast_elem* esa_lhs;
        ast_elem* esa_lhs_ctype;
        resolve_error re =
            resolve_ast_node(r, esa->lhs, body, &esa_lhs, &esa_lhs_ctype);
        if (re) return re;
        if (esa_lhs_ctype == ERROR_ELEM) {
            SET_THEN_RETURN_POISONED(
                r, RE_OK, c, body, NULL, ctype, NULL, ERROR_ELEM);
        }
        ast_node_set_instance_member(&c->node);
        return resolve_func_call(
            r, c, esa_lhs_ctype, true, body, esa->target.name, body, ctype);
    }
    assert(false); // TODO
    return RE_OK;
}
resolve_error create_pointer_to(
    resolver* r, ast_elem* base_type, bool is_const, ast_elem** tgt)
{
    ureg id = ast_elem_get_type_derivs(base_type)->ptr_id;
    type_pointer* t =
        ptr_map_get_pointer(&r->pm, base_type, id, false, 0, &r->tc->permmem);
    if (!t) return RE_FATAL;
    if (is_const) {
        t = ptr_map_get_pointer(
            &r->pm, base_type, t->flipped_const_id, true, id, &r->tc->permmem);
        if (!t) return RE_FATAL;
    }
    *tgt = (ast_elem*)t;
    // sanity check for ptr_map
    assert(t->base_type == base_type && t->tb.is_const == is_const);
    return RE_OK;
}
resolve_error choose_unary_operator_overload(
    resolver* r, expr_op_unary* ou, ast_body* body, ast_elem** value,
    ast_elem** ctype)
{
    ast_elem *child_type, *child_value;
    resolve_error re =
        resolve_ast_node(r, ou->child, body, &child_value, &child_type);
    if (re) return re;
    if (child_type == ERROR_ELEM) {
        SET_THEN_RETURN_POISONED(
            r, re, ou, body, value, ctype, ERROR_ELEM, ERROR_ELEM);
    }

    if (child_type == TYPE_ELEM || child_type == GENERIC_TYPE_ELEM) {
        ast_node_set_type_operator(&ou->node);
        if (ou->node.op_kind == OP_DEREF) {
            re = create_pointer_to(r, child_value, false, &child_type);
            if (re) return re;
            ou->op = child_type;
            SET_THEN_RETURN(value, ctype, child_type, TYPE_ELEM);
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
                if (!is_lvalue(child_value)) {
                    src_range_large child_srl, op_srl;
                    ast_node_get_src_range((ast_node*)ou, body, &op_srl);
                    ast_node_get_full_src_range(ou->child, body, &child_srl);
                    error_log_report_annotated_twice(
                        r->tc->err_log, ES_RESOLVER, false,
                        "cannot take the address of an rvalue", child_srl.smap,
                        child_srl.start, child_srl.end,
                        "the expression that doesn't have a storage "
                        "location",
                        op_srl.smap, op_srl.start, op_srl.end,
                        "cannot take the address of this expression");
                    r->error_occured = true;
                    re = curr_body_propagate_error(r, body);
                    if (re) return re;
                    ast_node_set_poisoned(&ou->node);
                }
            }
            re = create_pointer_to(r, child_type, false, &child_type);
            if (re) return re;
            ou->op = child_type;
            SET_THEN_RETURN(value, ctype, ou, child_type);
        }
        else if (ou->node.op_kind == OP_DEREF) {
            assert(child_type->kind == TYPE_POINTER); // TODO: error
            type_pointer* tp = (type_pointer*)child_type;
            ou->op = tp->base_type;
            SET_THEN_RETURN(value, ctype, ou, tp->base_type);
            return RE_FATAL;
        }
        else if (child_type->kind != SYM_PRIMITIVE) {
            assert(false); // TODO
            return RE_FATAL;
        }
        else {
            ou->op = child_type;
            SET_THEN_RETURN(value, ctype, ou, child_type);
        }
    }
}
resolve_error choose_binary_operator_overload(
    resolver* r, expr_op_binary* ob, ast_body* body, ast_elem** value,
    ast_elem** ctype)
{
    ast_elem *lhs_ctype, *rhs_ctype;
    bool unrealized_comptime = false;
    resolve_error re = resolve_ast_node(r, ob->lhs, body, NULL, &lhs_ctype);
    // evaluate rhs even if lhs is #'ed so we can eval #foo + #bar
    // in one run_pp
    if (re == RE_UNREALIZED_COMPTIME) {
        unrealized_comptime = true;
    }
    else if (re) {
        return re;
    }
    re = resolve_ast_node(r, ob->rhs, body, NULL, &rhs_ctype);
    if (re) return re;
    if (unrealized_comptime) return RE_UNREALIZED_COMPTIME;
    if (ob->node.op_kind == OP_ASSIGN) {
        type_cast_result tcr = type_cast(lhs_ctype, rhs_ctype, NULL);
        if (tcr) {
            if (tcr == TYPE_CAST_INCOMPATIBLE) {
                src_range_large rhs_srl;
                src_range_large op_srl;
                ast_node_get_full_src_range(ob->rhs, body, &rhs_srl);
                ast_node_get_src_range((ast_node*)ob, body, &op_srl);
                error_log_report_annotated_twice(
                    r->tc->err_log, ES_RESOLVER, false,
                    "assignment type missmatch", rhs_srl.smap, rhs_srl.start,
                    rhs_srl.end,
                    "the right hand sides expression's type is not implicitly "
                    "convertible to "
                    "the left hand sides",
                    op_srl.smap, op_srl.start, op_srl.end,
                    "in this assignment");
                r->error_occured = true;
            }
        }
        else if (!is_lvalue((ast_elem*)ob->lhs)) {
            src_range_large lhs_srl;
            src_range_large op_srl;
            ast_node_get_full_src_range(ob->lhs, body, &lhs_srl);
            ast_node_get_src_range((ast_node*)ob, body, &op_srl);
            error_log_report_annotated_twice(
                r->tc->err_log, ES_RESOLVER, false,
                "cannot assign to an rvalue", lhs_srl.smap, lhs_srl.start,
                lhs_srl.end, "this expression doesn't have a storage location",
                op_srl.smap, op_srl.start, op_srl.end,
                "therefore it is not assignable");
            r->error_occured = true;
            ast_node_set_poisoned(&ob->node);
        }
        else {
            if (ctype) *ctype = VOID_ELEM;
            ob->op = lhs_ctype;
            return RE_OK;
        }
        SET_THEN_RETURN_POISONED(
            r, RE_OK, ob, body, value, ctype, ob, ERROR_ELEM);
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
    const char* op_str = op_to_str(ob->node.op_kind);
    re = symbol_lookup_iterator_init(
        &sli, r, body, NULL, body, op_str, false, false, true);
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
    bool notifier_added = false;
    if (ppdct_curr_pprn_require_symbol(
            &r->ppdct, body, lhs_ctype, op_str, &notifier_added))
        return RE_FATAL;
    if (notifier_added) return RE_UNKNOWN_SYMBOL;
    src_range_large srl;
    src_range_unpack(ob->node.srange, &srl);
    error_log_report_annotated(
        r->tc->err_log, ES_RESOLVER, false, "invalid arguments for operator",
        ast_body_get_smap(body), srl.start, srl.end,
        "found no valid overload for this operator");
    SET_THEN_RETURN_POISONED(r, RE_OK, ob, body, value, ctype, ob, ERROR_ELEM);
}
ast_elem* ast_elem_get_ctype(ast_elem* s)
{
    switch (s->kind) {
        case SYM_VAR:
        case SYM_VAR_INITIALIZED: return ((sym_var*)s)->ctype; break;
        case SYM_NAMED_USE: assert(false); return NULL; // TODO
        case SYM_PRIMITIVE: {
            ast_elem* ctype = ((primitive*)s)->ctype;
            assert(ctype != ERROR_ELEM);
            return ctype;
        }
        case SC_TRAIT:
        case SC_TRAIT_GENERIC_INST: return TRAIT_ELEM;
        case SC_TRAIT_GENERIC: return GENERIC_TRAIT_ELEM;
        case SC_STRUCT_GENERIC: return GENERIC_TYPE_ELEM;
        case SC_STRUCT:
        case SC_STRUCT_GENERIC_INST: return TYPE_ELEM;
        default: assert(false); return NULL;
    }
}
ast_elem** get_break_target_ctype(ast_elem* n)
{
    switch (n->kind) {
        case EXPR_BLOCK: return &((expr_block*)n)->ebb.ctype;
        case EXPR_IF: return &((expr_if*)n)->ctype;
        case EXPR_LOOP: return &((expr_loop*)n)->ebb.ctype;
        default: return NULL;
    }
}
resolve_error resolve_break_target(
    resolver* r, const char* name, ast_body* body, expr_block_base** tgt_ebb,
    ast_elem*** ctype)
{
    // TODO: error
    while (body) {
        body = ast_body_get_non_paste_parent(body);
        if (body->owning_node->kind == SC_FUNC ||
            body->owning_node->kind == SC_FUNC_GENERIC) {
            assert(false); // TODO error
        }
        assert(ast_elem_is_expr_block_base((ast_elem*)body->owning_node));
        expr_block_base* ebb = (expr_block_base*)body->owning_node;
        if ((name && ebb->name && cstr_eq(ebb->name, name)) ||
            (name == ebb->name)) {
            *tgt_ebb = ebb;
            *ctype = get_break_target_ctype((ast_elem*)ebb);
            return RE_OK;
        }
        body = body->parent;
    }
    assert(false); // TODO: error
    return RE_ERROR;
}
resolve_error
get_resolved_symbol_body(resolver* r, symbol* s, ast_body** tgt_body)
{
    if (ast_elem_is_scope((ast_elem*)s)) {
        // TODO: handle access change here
        *tgt_body = &((scope*)s)->body;
        return RE_OK;
    }
    if (s->node.kind == SYM_IMPORT_SYMBOL) {
        return get_resolved_symbol_body(
            r, ((sym_import_symbol*)s)->target.sym, tgt_body);
    }
    else if (s->node.kind == SYM_IMPORT_MODULE) {
        *tgt_body = &((sym_import_module*)s)->im_data.imported_module->body;
    }
    else {
        assert(false); // TODO: error
    }
    return RE_OK;
}
resolve_error
resolve_param(resolver* r, sym_param* p, bool generic, ast_elem** ctype)
{
    if (ast_node_get_resolved(&p->sym.node)) {
        if (ctype) *ctype = p->ctype;
        return RE_OK;
    }
    ast_node_set_resolving(&p->sym.node);
    resolve_error re;
    // PERF: eeeh
    ast_body* declaring_body =
        ast_elem_get_body((ast_elem*)p->sym.declaring_body->owning_node);
    if (p->type) {
        re = resolve_ast_node(r, p->type, declaring_body, &p->ctype, NULL);
        if (re) return re;
        if (p->default_value) {
            ast_elem* val;
            re = resolve_ast_node(
                r, (ast_node*)p->default_value, declaring_body, NULL, &val);
            if (re) return re;
            if (type_cast(p->ctype, val, NULL)) {
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
                r, p->default_value, declaring_body, NULL, &p->ctype);
            if (re) return re;
        }
    }
    if (ctype) *ctype = p->ctype;
    ast_node_set_resolved(&p->sym.node);
    return RE_OK;
}
// if no symbol is found, error symbol is set to the first hidden match
// if multiple symbols are founnd, error symbol is set to the second one
// to report an ambiguity
resolve_error resolver_lookup_single(
    resolver* r, ast_body* body, ast_elem* lhs_ctype, bool lhs_is_instance,
    ast_body* looking_body, const char* tgt_name, symbol** res,
    symbol** error_symbol)
{
    symbol_lookup_iterator sli;
    resolve_error re = symbol_lookup_iterator_init(
        &sli, r, body, lhs_ctype, looking_body, tgt_name, lhs_is_instance, true,
        true);
    if (re) return re;
    re = symbol_lookup_iterator_next(&sli, res);
    if (re) return re;
    if (!*res) {
        *error_symbol = sli.first_hidden_match;
        return RE_OK;
    }
    return symbol_lookup_iterator_next(&sli, error_symbol);
}
resolve_error resolve_scoped_identifier(
    resolver* r, expr_scope_access* esa, ast_body* body, ast_elem** value,
    ast_elem** ctype)
{
    resolve_error re;
    ast_body* lhs_body;
    ast_elem* lhs_val;
    re = resolve_ast_node(r, esa->lhs, body, &lhs_val, NULL);
    if (re) return re;
    if (lhs_val == ERROR_ELEM) {
        SET_THEN_RETURN_POISONED(
            r, re, esa, body, value, ctype, ERROR_ELEM, ERROR_ELEM);
    }
    assert(lhs_val != NULL && ast_elem_is_symbol(lhs_val)); // TODO: log error
    re = get_resolved_symbol_body(r, (symbol*)lhs_val, &lhs_body);
    if (re) return re;
    symbol* idf;
    symbol* amb_err;
    re = resolver_lookup_single(
        r, lhs_body, NULL, false, body, esa->target.name, &idf, &amb_err);
    if (re) return re;
    if (!idf) {
        bool notif_added = false;
        ppdct_curr_pprn_require_symbol(
            &r->ppdct, body, lhs_val, esa->target.name, &notif_added);
        if (notif_added) return RE_UNKNOWN_SYMBOL;
        report_unknown_symbol(r, (ast_node*)esa, body, amb_err, false);
        SET_THEN_RETURN_POISONED(
            r, RE_OK, esa, body, value, ctype, ERROR_ELEM, ERROR_ELEM);
    }
    if (amb_err) {
        assert(false); // TODO report ambiguity error
    }
    ast_elem* idf_val;
    resolve_ast_node(r, (ast_node*)idf, idf->declaring_body, &idf_val, ctype);
    assert(idf_val != NULL && ast_elem_is_symbol(idf_val)); // TODO: log error
    esa->target.sym = idf;
    if (value) *value = idf_val;
    ast_node_set_resolved(&esa->node);
    return RE_OK;
}
access_modifier check_member_access(symbol_table* st, scope* tgt)
{
    return AM_LOCAL; // TODO
}
resolve_error resolve_expr_member_accesss(
    resolver* r, expr_member_access* ema, ast_body* body, ast_elem** value,
    ast_elem** ctype)
{
    ast_elem* lhs_ctype;
    resolve_error re = resolve_ast_node(r, ema->lhs, body, NULL, &lhs_ctype);
    if (re) return re;
    if (lhs_ctype == ERROR_ELEM) {
        SET_THEN_RETURN_POISONED(
            r, RE_OK, ema, body, value, ctype, ema, ERROR_ELEM);
    }
    // TODO:
    assert(ast_elem_get_ctype(lhs_ctype) == TYPE_ELEM);
    symbol* mem;
    symbol* amb_err;
    re = resolver_lookup_single(
        r, body, lhs_ctype, true, body, ema->target.name, &mem, &amb_err);
    if (re) return re;
    if (!mem) {
        bool notif_added = false;
        int res = ppdct_curr_pprn_require_symbol(
            &r->ppdct, body, lhs_ctype, ema->target.name, &notif_added);
        if (res) return RE_FATAL;
        if (notif_added) return RE_UNKNOWN_SYMBOL;
        report_unknown_symbol(r, (ast_node*)ema, body, amb_err, true);
        SET_THEN_RETURN_POISONED(
            r, RE_OK, ema, body, value, ctype, ema, ERROR_ELEM);
    }
    if (amb_err) {
        assert(false); // TODO: report ambiguity
    }
    ema->target.sym = mem;
    re = resolve_ast_node(r, (ast_node*)mem, body, value, ctype);
    if (re) return re;
    ast_node_set_resolved(&ema->node);
    return RE_OK;
}
static inline bool is_legal_type_for_var(ast_elem* ctype)
{
    // TOOD: is this sufficient?
    return ctype != TYPE_ELEM && ctype != VOID_ELEM;
}
static inline resolve_error resolve_var(
    resolver* r, ast_body* requesting_body, sym_var* v, ast_elem** value,
    ast_elem** ctype)
{
    resolve_error re;
    ast_body* declaring_body = v->osym.sym.declaring_body;
    bool comptime = ast_node_get_comptime((ast_node*)v);
    ast_elem* owner =
        (ast_elem*)ast_body_get_non_paste_parent(declaring_body)->owning_node;
    bool public_symbol = ast_elem_is_module_frame(owner);
    if (!public_symbol) {
        bool is_static = ast_node_get_static((ast_node*)v);
        public_symbol = (ast_elem_is_struct(owner) && is_static);
    }
    if ((comptime || public_symbol) && !v->pprn) {
        v->pprn = pp_resolve_node_create(
            r, (ast_node*)v, declaring_body, true, false, !comptime, true);
        if (!v->pprn) return RE_FATAL;
        if (!ast_node_get_used_in_pp((ast_node*)v)) {
            resolver_set_ast_node_used_in_pp(r, (ast_node*)v);
        }
    }
    pp_resolve_node* prev_pp_node = r->curr_pp_node;
    r->curr_pp_node = v->pprn;
    if (v->osym.sym.node.kind == SYM_VAR) {
        ast_elem* type;
        re = resolve_ast_node(r, v->type, declaring_body, &type, NULL);
        if (!re) v->ctype = type;
    }
    else {
        sym_var_initialized* vi = (sym_var_initialized*)v;
        if (vi->var.type) {
            re = resolve_ast_node(
                r, vi->var.type, declaring_body, &vi->var.ctype, NULL);
            if (!re) {
                ast_elem* val_type;
                re = resolve_ast_node(
                    r, vi->initial_value, declaring_body, NULL, &val_type);
                if (!re) {
                    type_cast_result tcr =
                        type_cast(vi->var.ctype, val_type, &vi->initial_value);
                    if (tcr) {
                        if (tcr == TYPE_CAST_POISONED) {
                            SET_THEN_RETURN_POISONED(
                                r, RE_OK, vi, requesting_body, value, ctype, vi,
                                ERROR_ELEM);
                        }
                        src_range_large vi_type_srl, vi_val_srl;
                        ast_node_get_src_range(
                            vi->var.type, declaring_body, &vi_type_srl);
                        ast_node_get_src_range(
                            vi->initial_value, declaring_body, &vi_val_srl);
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
                r, vi->initial_value, declaring_body, NULL, &vi->var.ctype);
            // this could become needed again once we support typeof,
            // it allows one retry in cases of a variable initially
            // assigned to a self referential expr block
            /*  if (re == RE_TYPE_LOOP && r->type_loop_start == n &&
                 !r->retracing_type_loop) {
                 if (vi->var.ctype) {
                     ast_node_set_resolved(&n->flags);
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
    r->curr_pp_node = prev_pp_node;
    if (re && re != RE_UNREALIZED_COMPTIME && re != RE_UNKNOWN_SYMBOL) {
        // so we can free it... sigh
        if (v->pprn) {
            if (pprn_set_state(r, v->pprn, PPRN_WAITING)) return RE_FATAL;
        }
        return re;
    }
    if (!re) {
        ast_node_set_resolved((ast_node*)v);
        if (!is_legal_type_for_var(v->ctype)) {
            ureg len;
            char* tgt_type = ctype_to_string(
                r->tc, &r->tc->tempmem, v->osym.sym.declaring_body, v->ctype,
                &len);
            if (!tgt_type) return RE_FATAL;
            char* msg = error_log_cat_strings_3(
                r->tc->err_log, "variable of type '", tgt_type,
                "' is not allowed");
            if (!msg) return RE_FATAL;
            pool_undo_last_alloc(&r->tc->tempmem, len);
            src_range_large var_srl, srl_2;
            ast_node_get_src_range((ast_node*)v, declaring_body, &var_srl);
            char* msg2;
            if (!v->type) {
                ast_node_get_src_range(
                    ((sym_var_initialized*)v)->initial_value, declaring_body,
                    &srl_2);
                msg2 = "illegal type deduced from this initializer expression";
            }
            else {
                ast_node_get_src_range(v->type, declaring_body, &srl_2);
                msg2 = "illegal type specified here";
            }
            error_log_report_annotated_twice(
                r->tc->err_log, ES_RESOLVER, false, msg, var_srl.smap,
                var_srl.start, var_srl.end, "for this variable", srl_2.smap,
                srl_2.start, srl_2.end, msg2);
            r->error_occured = true;
            v->ctype = ERROR_ELEM;
            ast_node_set_poisoned((ast_node*)v);
            ast_node_set_contains_error((ast_node*)v);
            if (v->pprn) {
                re = pp_resolve_node_activate(
                    r, requesting_body, &v->pprn, false);
            }
            SET_THEN_RETURN_POISONED(
                r, re, v, requesting_body, value, ctype, ERROR_ELEM,
                ERROR_ELEM);
        }
    }
    if (v->pprn) {
        resolve_error re2;
        if (public_symbol) {
            re2 = curr_pprn_depend_on(r, requesting_body, &v->pprn);
        }
        else {
            re2 = curr_pp_block_add_child(r, requesting_body, &v->pprn);
        }
        if (re2) return re2;
        re2 =
            pp_resolve_node_activate(r, requesting_body, &v->pprn, re == RE_OK);
        if (re2) return re2;
    }
    SET_THEN_RETURN_IF_RESOLVED(re == RE_OK, value, ctype, v, v->ctype);
    return re;
}
static inline int resolve_return_target(
    resolver* r, expr_return* er, ast_body* body, ast_node** tgt)
{
    ast_body* body_orig = body;
    while (body) {
        if (body->owning_node->kind == SC_FUNC ||
            body->owning_node->kind == SC_FUNC_GENERIC) {
            *tgt = (ast_node*)body->owning_node;
            return OK;
        }
        body = body->parent;
    }
    ureg vstart, vend;
    ast_node_get_bounds((ast_node*)er, &vstart, &vend);
    error_log_report_annotated(
        r->tc->err_log, ES_RESOLVER, false, "orphaned return statement",
        ast_body_get_smap(body_orig), vstart, vend,
        "the return statement is not inside a function");
    r->error_occured = true;
    return ERR;
}

static inline resolve_error
resolve_return(resolver* r, ast_body* body, expr_return* er)
{
    resolve_error re =
        resolve_ast_node(r, er->value, body, NULL, &er->value_ctype);
    if (re) return re;
    ast_node_set_resolved(&er->node);
    ast_elem* tgt_type;
    // make the current body ctype unrachable since we exit early
    if (ast_elem_is_expr_block_base((ast_elem*)body->owning_node)) {
        ast_elem** tgtt = get_break_target_ctype((ast_elem*)body->owning_node);
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
    if (resolve_return_target(r, er, body, &er->target)) {
        RETURN_POISONED(r, RE_OK, er, body);
    }
    if (re) return re;
    if (ast_elem_is_func_base((ast_elem*)er->target)) {
        // must already be resolved since parenting function
        tgt_type = ((sc_func_base*)er->target)->return_ctype;
    }
    else {
        assert(false); // TODO: error
        return RE_ERROR;
    }
    type_cast_result tcr = type_cast(tgt_type, er->value_ctype, &er->value);
    if (tcr) {
        if (tcr == TYPE_CAST_POISONED) {
            RETURN_POISONED(r, RE_OK, er, body);
        }
        ureg vstart, vend;
        ast_node_get_bounds(er->value, &vstart, &vend);
        ureg len;
        char* ret_type_str = ctype_to_string(
            r->tc, &r->tc->tempmem, body, er->value_ctype, &len);
        if (!ret_type_str) return RE_FATAL;
        ret_type_str = error_log_cat_strings_3(
            r->tc->err_log, "trying to return '", ret_type_str, "'");
        if (!ret_type_str) return RE_FATAL;
        pool_undo_last_alloc(&r->tc->tempmem, len);
        char* tgt_type_str =
            ctype_to_string(r->tc, &r->tc->tempmem, body, tgt_type, &len);
        if (!tgt_type_str) return RE_FATAL;
        tgt_type_str = error_log_cat_strings_3(
            r->tc->err_log, "target expects '", tgt_type_str, "'");
        pool_undo_last_alloc(&r->tc->tempmem, len);
        if (!tgt_type) return RE_FATAL;
        error_log_report_annotated_twice(
            r->tc->err_log, ES_RESOLVER, false, "return type missmatch",
            ast_body_get_smap(body), vstart, vend, ret_type_str,
            ast_body_get_smap(body), src_range_get_start(er->target->srange),
            src_range_get_end(er->target->srange), tgt_type_str);
        RETURN_POISONED(r, RE_OK, er, body);
    }
    return RE_OK;
}
static inline resolve_error
resolve_break(resolver* r, ast_body* body, expr_break* b)
{
    resolve_error re = RE_OK;
    if (b->value) {
        re = resolve_ast_node(r, b->value, body, NULL, &b->value_ctype);
        if (re) return re;
    }
    else {
        b->value_ctype = VOID_ELEM;
    }
    ast_node_set_resolved(&b->node);
    ast_elem** tgt_ctype;
    re = resolve_break_target(
        r, b->target.label, body, &b->target.ebb, &tgt_ctype);
    if (re) return re;
    if (*tgt_ctype) {
        type_cast_result tcr = type_cast(*tgt_ctype, b->value_ctype, &b->value);
        if (tcr) {
            if (tcr == TYPE_CAST_POISONED) {
                RETURN_POISONED(r, RE_OK, b, body);
            }
            ureg vstart, vend;
            ast_node_get_bounds(b->value, &vstart, &vend);
            error_log_report_annotated_twice(
                r->tc->err_log, ES_RESOLVER, false, "type missmatch",
                ast_body_get_smap(body), vstart, vend,
                "the type returned from here doesn't match the target "
                "scope's",
                // TODO: st is kinda wrong here
                ast_body_get_smap(body),
                src_range_get_start(b->target.ebb->node.srange),
                src_range_get_end(b->target.ebb->node.srange),
                "target scope here");
            RETURN_POISONED(r, RE_OK, b, body);
        }
    }
    else {
        *tgt_ctype = b->value_ctype;
    }
    return RE_OK;
}
static inline resolve_error resolve_special_identifier(
    resolver* r, ast_body* body, expr_identifier* e, ast_elem** value,
    ast_elem** ctype)
{
    ast_node_kind kinds[7];
    ureg kinds_count = 0;
    token_kind sik = e->value.special_ident_kind;
    switch (sik) {
        case TK_KW_MODULE: kinds[kinds_count++] = ELEM_MDG_NODE; break;
        case TK_KW_SUPER: kinds[kinds_count++] = ELEM_MDG_NODE;
        // fallthrough
        case TK_KW_SELF_UPPERCASE:
            kinds[kinds_count++] = SC_TRAIT;
            kinds[kinds_count++] = SC_TRAIT_GENERIC_INST;
        // fallthrough
        case TK_KW_SELF:
            kinds[kinds_count++] = SC_STRUCT;
            kinds[kinds_count++] = SC_STRUCT_GENERIC_INST;
            kinds[kinds_count++] = TRAIT_IMPL;
            kinds[kinds_count++] = TRAIT_IMPL_GENERIC_INST;
            break;
        default: panic("compiler bug");
    }
    ast_body* tgt_body = NULL;
    ast_body* b = body;
    sc_func* fn = NULL;
    do {
        for (ureg i = 0; i < kinds_count; i++) {
            if (b->owning_node->kind == kinds[i]) {
                tgt_body = b;
                break;
            }
        }
        if (sik == TK_KW_SELF && !fn) {
            if (ast_node_get_instance_member(b->owning_node)) {
                fn = (sc_func*)b->owning_node;
            }
            else {
                fn = (sc_func*)NULL_PTR_PTR;
            }
        }
        if (tgt_body) break;
        b = b->parent;
    } while (b);
    if (!tgt_body) {
        src_range_large srl;
        ast_node_get_src_range((ast_node*)e, body, &srl);
        error_log_report_annotated(
            r->tc->err_log, ES_RESOLVER, false, "unbound relative identifier",
            srl.smap, srl.start, srl.end,
            "no target for this relative identifer in the current scope");
        SET_THEN_RETURN_POISONED(
            r, RE_OK, e, body, value, ctype, ERROR_ELEM, ERROR_ELEM);
    }
    symbol* tgt_sym;
    ast_elem* tgt_ctype;
    if (tgt_body->owning_node->kind == TRAIT_IMPL ||
        tgt_body->owning_node->kind == TRAIT_IMPL_GENERIC_INST) {
        trait_impl* ti = (trait_impl*)tgt_body->owning_node;
        assert(ast_elem_is_symbol(ti->impl_for_ctype));
        tgt_sym = (symbol*)ti->impl_for_ctype;
    }
    else {
        tgt_sym = (symbol*)tgt_body->owning_node;
    }
    if (sik == TK_KW_SELF) {
        if (!fn && fn != (sc_func*)NULL_PTR_PTR) {
            src_range_large srl;
            ast_node_get_src_range((ast_node*)e, body, &srl);
            error_log_report_annotated(
                r->tc->err_log, ES_RESOLVER, false,
                "no struct instance 'self' accesible in the current scope",
                srl.smap, srl.start, srl.end,
                "self must be inside a member function");
            SET_THEN_RETURN_POISONED(
                r, RE_OK, e, body, value, ctype, ERROR_ELEM, ERROR_ELEM);
        }
        ast_node_set_instance_member((ast_node*)e);
        assert(!value);
        tgt_ctype = (ast_elem*)tgt_sym;
    }
    else {
        tgt_ctype = ast_elem_get_ctype((ast_elem*)tgt_sym);
    }
    e->node.kind = EXPR_IDENTIFIER;
    e->value.sym = tgt_sym;
    ast_node_set_resolved(&e->node);
    if (value) *value = (ast_elem*)tgt_sym;
    if (ctype) *ctype = tgt_ctype;
    return RE_OK;
}
static inline resolve_error resolve_identifier(
    resolver* r, ast_body* body, expr_identifier* e, ast_elem** value,
    ast_elem** ctype)
{
    symbol* sym;
    symbol* amb_err;
    resolve_error re = resolver_lookup_single(
        r, body, NULL, false, body, e->value.str, &sym, &amb_err);
    if (re) return re;
    if (!sym) {
        bool notif_added = false;
        int res = ppdct_curr_pprn_require_symbol(
            &r->ppdct, body, NULL, e->value.str, &notif_added);
        if (res) return RE_FATAL;
        if (notif_added) return RE_UNKNOWN_SYMBOL;
        report_unknown_symbol(r, (ast_node*)e, body, amb_err, false);
        SET_THEN_RETURN_POISONED(
            r, RE_OK, e, body, value, ctype, ERROR_ELEM, ERROR_ELEM);
    }
    if (amb_err) {
        assert(false); // TODO: report ambiguity
    }
    if (ppdct_use_symbol(&r->ppdct, sym, body, NULL, (ast_node*)e))
        return RE_FATAL;
    re = resolve_ast_node(r, (ast_node*)sym, body, (ast_elem**)&sym, ctype);
    if (re) return re;
    e->value.sym = (symbol*)sym;
    if (value) *value = (ast_elem*)sym;
    ast_node_set_resolved(&e->node);
    return RE_OK;
}
static inline resolve_error resolve_if(
    resolver* r, ast_body* body, expr_if* ei, ast_elem** value,
    ast_elem** ctype)
{
    ast_elem *ctype_if, *ctype_else;
    bool cond_type_loop = false;
    bool if_branch_type_loop = false;
    bool else_branch_type_loop = false;
    // TODO: check for bool here
    resolve_error re = resolve_ast_node(r, ei->condition, body, NULL, NULL);
    if (re == RE_TYPE_LOOP) {
        cond_type_loop = true;
    }
    else {
        if (re) return re;
    }
    re = resolve_ast_node(r, ei->if_body, body, NULL, &ctype_if);
    if (re == RE_TYPE_LOOP) {
        if_branch_type_loop = true;
    }
    else {
        if (re) return re;
    }
    if (ei->else_body) {
        re = resolve_ast_node(r, ei->else_body, body, NULL, &ctype_else);
        if (re == RE_TYPE_LOOP) {
            else_branch_type_loop = true;
            if (if_branch_type_loop || ctype_if == UNREACHABLE_ELEM) {
                return RE_TYPE_LOOP;
            }
        }
        else {
            if (re) return re;
        }
        if (if_branch_type_loop || ctype_if == UNREACHABLE_ELEM ||
            ctype_if == ERROR_ELEM) {
            ei->ctype = ctype_else; // TODO: this could lead to void instead
        }
        else if (
            else_branch_type_loop || ctype_else == UNREACHABLE_ELEM ||
            ctype_else == ERROR_ELEM) {
            ei->ctype = ctype_if;
        }
        else if (ctype_if != ctype_else) {
            // TODO: get type hint from parent and try to cast
            src_range_large srl, srl_if, srl_else;
            ast_node_get_src_range((ast_node*)ei, body, &srl);
            ast_node_get_src_range(ei->if_body, body, &srl_if);
            ast_node_get_src_range(ei->else_body, body, &srl_else);
            error_log_report_annotated_thrice(
                r->tc->err_log, ES_RESOLVER, false, "type missmatch", srl.smap,
                srl.start, srl.end,
                "if body and else body evaluate to differently typed "
                "values",
                srl_if.smap, srl_if.start, srl_if.end, NULL, srl_else.smap,
                srl_else.start, srl_else.end, NULL);
            return RE_TYPE_MISSMATCH;
        }
        else {
            // TODO: choose the unified type?
            ei->ctype = ctype_if;
        }
    }
    else {
        // TODO: maybe check for breaks and cause an error here
        ei->ctype = VOID_ELEM;
    }
    if (cond_type_loop || if_branch_type_loop || else_branch_type_loop) {
        return RE_TYPE_LOOP;
    }
    ast_node_set_resolved(&ei->node);
    SET_THEN_RETURN(value, ctype, ei, ei->ctype);
}
bool is_body_public_st(ast_body* b)
{
    while (true) {
        ast_elem* oe = (ast_elem*)b->owning_node;
        if (ast_elem_is_module_frame(oe)) {
            return true;
        }
        if (ast_elem_is_struct(oe) || ast_elem_is_trait_impl(oe) ||
            oe->kind == SC_STRUCT_GENERIC || oe->kind == SC_TRAIT_GENERIC) {
            if (is_local_node(oe)) {
                return false;
                break;
            }
        }
        else {
            return false;
            break;
        }
        b = ast_body_get_non_paste_parent(b->parent);
    }
}
static inline resolve_error handle_expr_pp_paste(
    resolver* r, expr_pp* ppe, ast_body* body, ast_elem** value,
    ast_elem** ctype)
{
    pp_resolve_node* ex_pprn = ppe->pprn;
    parse_error pe;
    ast_body* npp_body = ast_body_get_non_paste_parent(body);
    ast_body* shared_body = NULL;
    resolve_error re;
    bool is_stmt = !ast_node_get_pp_expr_res_used(&ppe->node);
    if (ast_elem_is_module_frame((ast_elem*)npp_body->owning_node)) {
        shared_body = ast_body_get_non_paste_parent(npp_body->parent);
    }
    if (!is_stmt) {
        pe = parser_parse_paste_expr(&r->tc->p, ppe, body, shared_body);
        if (!pe) {
            ast_node* ev = (ast_node*)ppe->result_buffer.paste_eval;
            re = resolve_ast_node(r, ev, body, value, ctype);
            if (ast_node_get_resolved(ev)) {
                ast_node_set_resolved((ast_node*)ppe);
            }
        }
    }
    else {
        pe = parser_parse_paste_stmt(&r->tc->p, ppe, body, shared_body);
        if (!pe) {
            bool public_st = is_body_public_st(npp_body);
            ast_node* ev = (ast_node*)ppe->result_buffer.paste_eval;
            re = add_ast_node_decls(
                r, &ppe->result_buffer.paste_eval->body, shared_body, ev,
                public_st);
            if (!re) {
                re = resolve_ast_node(r, ev, body, value, &ppe->ctype);
                if (ctype) *ctype = ppe->ctype;
                if (ast_node_get_resolved(ev)) {
                    ast_node_set_resolved((ast_node*)ppe);
                }
            }
        }
    }
    if (ex_pprn) {
        resolve_error re2 = pp_resolve_node_done(r, ex_pprn, NULL);
        if (re2) return re2;
    }
    if (pe) return (pe == PE_FATAL) ? RE_FATAL : RE_ERROR;
    return re;
}
static inline resolve_error resolve_expr_pp(
    resolver* r, ast_body* body, expr_pp* ppe, bool from_pprnlist,
    ast_elem** value, ast_elem** ctype)
{
    bool rerun = false;
    if (ppe->pprn) {
        if (ppe->pprn->pending_pastes) {
            return RE_UNREALIZED_COMPTIME;
        }
        if (!from_pprnlist && ppe->pprn->run_individually &&
            ast_elem_has_unordered_body((ast_elem*)body->owning_node)) {
            assert(ppe->pprn->activated);
            return RE_UNREALIZED_COMPTIME;
        }
        ppe->pprn->activated = false;
        rerun = true;
    }
    resolve_error re;
    if (ppe->ctype == PASTED_EXPR_ELEM) {
        return handle_expr_pp_paste(r, ppe, body, value, ctype);
    }
    pp_resolve_node* pprn = NULL;
    if (ppe->pprn) {
        pprn = ppe->pprn;
    }
    else {
        pprn = pp_resolve_node_create(
            r, (ast_node*)ppe, body, true, false, false, false);
        if (!pprn) return RE_FATAL;
        ppe->pprn = pprn;
        resolver_set_ast_node_used_in_pp(r, &ppe->node);
    }
    pp_resolve_node* parent_pprn = r->curr_pp_node;
    r->curr_pp_node = pprn;
    pprn->needs_further_resolution = false;
    re = resolve_ast_node(r, ppe->pp_expr, body, value, &ppe->ctype);
    if (re) pprn->needs_further_resolution = true;
    r->curr_pp_node = parent_pprn;
    if (pprn->pending_pastes) {
        if (ppe->ctype != VOID_ELEM) {
            src_range_large pp_srl;
            ast_node_get_src_range((ast_node*)ppe, body, &pp_srl);
            // TODO: report where the value is coming from (it's type, etc.)
            // especiallly nasty on missing semicolon -> expr is array
            error_log_report_annotated(
                r->tc->err_log, ES_RESOLVER, false,
                "pasting preprocessor expression can't return a value",
                pp_srl.smap, pp_srl.start, pp_srl.end, "in this pp expression");
            re = RE_TYPE_MISSMATCH;
        }
        else {
            ppe->ctype = PASTED_EXPR_ELEM;
        }
    }
    if (re == RE_OK || re == RE_UNKNOWN_SYMBOL) {
        if (!re) {
            ast_node_set_resolved(&ppe->node);
        }
        if (!rerun) {
            if (curr_pp_block_add_child(r, body, &ppe->pprn)) return RE_FATAL;
        }
        if (pp_resolve_node_activate(r, body, &ppe->pprn, re == RE_OK)) {
            return RE_FATAL;
        }
        if (re) return re;
        // we just need to mark it for reexec, no specific value required
        if (ppe->pprn && pprn->pending_pastes && pprn->run_individually) {
            pprn->continue_block = (ast_node**)NULL_PTR_PTR;
        }
    }
    if (re) {
        pprn_set_state(r, pprn, PPRN_WAITING); // so we can free it... sigh
        return re;
    }
    if (ctype) *ctype = ppe->ctype;
    if (parent_pprn) {
        parent_pprn->needs_further_resolution = true;
        return RE_UNREALIZED_COMPTIME;
    }
    if (pprn->pending_pastes) return RE_UNREALIZED_COMPTIME;
    return RE_OK;
}
static inline resolve_error resolve_expr_paste_str(
    resolver* r, ast_body* body, expr_paste_str* eps, ast_elem** value,
    ast_elem** ctype)
{
    resolve_error re;
    ast_elem* val_type;
    re = resolve_ast_node(r, eps->value, body, NULL, &val_type);
    if (re) return re;
    type_cast_result tcr =
        type_cast((ast_elem*)&PRIMITIVES[PT_STRING], val_type, &eps->value);
    if (tcr) {
        if (tcr == TYPE_CAST_INCOMPATIBLE) {
            src_range_large paste_srl, val_srl;
            ast_node_get_src_range((ast_node*)eps, body, &paste_srl);
            ast_node_get_src_range(eps->value, body, &val_srl);
            error_log_report_annotated_twice(
                r->tc->err_log, ES_RESOLVER, false,
                "incompatible type in paste argument", val_srl.smap,
                val_srl.start, val_srl.end,
                "paste expects a string as an argument", paste_srl.smap,
                paste_srl.start, paste_srl.end, NULL);
        }
        SET_THEN_RETURN_POISONED(
            r, re, eps, body, value, ctype, VOID_ELEM, ERROR_ELEM);
    }
    if (!r->curr_pp_node) {
        src_range_large paste_srl;
        ast_node_get_src_range((ast_node*)eps, body, &paste_srl);
        error_log_report_annotated(
            r->tc->err_log, ES_RESOLVER, false,
            "paste call outside of preprocessor expression", paste_srl.smap,
            paste_srl.start, paste_srl.end,
            "paste call must reside in a preprocessor expression "
            "result");
        r->error_occured = true;
        SET_THEN_RETURN_POISONED(
            r, re, eps, body, value, ctype, VOID_ELEM, ERROR_ELEM);
    }
    ast_node_set_resolved(&eps->node);
    expr_pp* tgt_ppe = (expr_pp*)r->curr_pp_node->node;
    assert(((ast_elem*)tgt_ppe)->kind == EXPR_PP);
    if (!tgt_ppe->result_buffer.pasted_src) {
        tgt_ppe->result_buffer.pasted_src = file_map_create_pasted_source(
            &r->tc->t->filemap, r->tc, tgt_ppe,
            ast_body_get_smap(r->curr_pp_node->declaring_body));
        if (!tgt_ppe->result_buffer.pasted_src) return RE_FATAL;
    }
    eps->target = tgt_ppe->result_buffer.pasted_src;
    r->curr_pp_node->pending_pastes = true;
    SET_THEN_RETURN(value, ctype, VOID_ELEM, VOID_ELEM);
}
static inline resolve_error
report_type_loop(resolver* r, ast_node* n, ast_body* body);
resolve_error resolve_importing_node(
    resolver* r, import_module_data* im_data, ast_node* node, ast_body* body,
    bool dep_prop, ast_elem** value, ast_elem** ctype)
{
    resolve_error re;
    bool used_in_pp, previously_used_in_pp;
    if (dep_prop) {
        // this gets set before calling this, but JUST before
        assert(ast_node_get_used_in_pp(node));
        previously_used_in_pp = false;
        used_in_pp = true;
    }
    else if (im_data->pprn) {
        previously_used_in_pp = ast_node_get_used_in_pp(node);
        re = curr_pprn_depend_on(r, body, &im_data->pprn);
        if (re) return re;
        used_in_pp = ast_node_get_used_in_pp(node);
        if (!used_in_pp) {
            ast_node_clear_resolving(node);
            SET_THEN_RETURN(value, ctype, node, NULL);
        }
    }
    else {
        previously_used_in_pp = false;
        pp_resolve_node* depending;
        re = get_curr_pprn(r, body, &depending);
        if (re) return re;
        used_in_pp = depending && ast_node_get_used_in_pp(depending->node);
        if (used_in_pp && !ast_node_get_used_in_pp(node)) {
            ast_node_set_used_in_pp(node);
        }
    }
    bool available = false;
    bool request_pp = false;
    mdg_node* mdg = im_data->imported_module;
    mdg_node* im_mdg = NULL; // only set this when incrementing dep count!
    if (used_in_pp && !previously_used_in_pp) {
        atomic_boolean_init(&im_data->done, false);
        im_mdg = im_data->importing_module;
        atomic_ureg_inc(&im_mdg->ungenerated_pp_deps);
    }
    rwlock_write(&mdg->lock);
    // we can check this since we are sure it is resolved since we
    // otherwise wouldn't be resolving this which depends on it
    if (mdg->error_occured) {
        // TODO: some poisoning or error message idk
        rwlock_end_write(&mdg->lock);
        r->error_occured = true;
        SET_THEN_RETURN_POISONED(
            r, RE_OK, node, body, value, ctype, ERROR_ELEM, ERROR_ELEM);
    }
    switch (mdg->ppe_stage) {
        case PPES_UNNEEDED: mdg->ppe_stage = PPES_REQUESTED; break;
        case PPES_REQUESTED: break;
        case PPES_RUNNING: break;
        case PPES_DONE: available = true; break;
        case PPES_SKIPPED:
            mdg->ppe_stage = PPES_REQUESTED;
            request_pp = true;
            break;
    }
    if (!available && used_in_pp && !previously_used_in_pp) {
        if (list_append(&mdg->notify, NULL, im_data)) {
            rwlock_end_write(&mdg->lock);
            return RE_FATAL;
        }
    }
    rwlock_end_write(&mdg->lock);
    if (request_pp) {
        if (tauc_request_pp_module(r->tc->t, mdg)) return RE_FATAL;
    }
    if (im_data->pprn) {
        if (available) {
            re = pp_resolve_node_ready(r, im_data->pprn);
            if (re) return re;
        }
    }
    else if (!available) {
        im_data->pprn =
            pp_resolve_node_create(r, node, body, false, false, true, true);
        if (!im_data->pprn) return re;
        if (ptrlist_append(&r->import_module_data_nodes, im_data)) {
            pprn_fin(r, im_data->pprn, true);
            return RE_FATAL;
        }
        re = curr_pprn_depend_on(r, body, &im_data->pprn);
        if (re) return re;
        re = pp_resolve_node_activate(r, body, &im_data->pprn, false);
        if (re) return re;
    }
    if (available && used_in_pp) {
        assert(im_mdg);
        atomic_ureg_dec(&im_mdg->ungenerated_pp_deps);
    }
    if (used_in_pp && available) {
        ast_node_set_resolved(node);
        ast_node_set_emitted_for_pp(node);
    }
    else {
        ast_node_clear_resolving(node);
    }
    SET_THEN_RETURN(value, ctype, node, NULL);
}
static inline resolve_error resolve_expr_block(
    resolver* r, expr_block* b, ast_body* parent_body, ast_elem** value,
    ast_elem** ctype)
{
    bool end_reachable;
    resolve_error re = resolve_expr_body(
        r, parent_body, (ast_node*)b, &b->ebb.body, &end_reachable);
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
    ast_node_set_resolved(&b->ebb.node);
    assert(!value);
    SET_THEN_RETURN(value, ctype, NULL, b->ebb.ctype);
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
type_slice* get_slice_of(resolver* r, ast_elem* base_type, bool is_const)
{
    ureg id = ast_elem_get_type_derivs(base_type)->slice_id;
    type_slice* ts =
        ptr_map_get_slice(&r->pm, base_type, id, false, 0, &r->tc->permmem);
    if (!ts) return NULL;
    if (is_const) {
        ts = ptr_map_get_slice(
            &r->pm, base_type, ts->flipped_const_id, true, id, &r->tc->permmem);
        if (!ts) return NULL;
    }
    assert(ts->tb.kind == TYPE_SLICE && ts->ctype_members == base_type);
    return ts;
}
type_array*
get_array_of(resolver* r, ast_elem* base_type, ureg len, bool is_const)
{
    type_array* ta = type_map_get_array(
        &ast_elem_get_type_derivs(base_type)->tm, &r->pm, base_type, len,
        is_const, &r->tc->permmem);
    if (!ta) return NULL;
    assert(ta->length == len);
    return ta;
}
resolve_error resolve_array_or_slice_type(
    resolver* r, ast_node* n, ast_body* body, ureg arr_expr_len,
    ast_elem** value, ast_elem** ctype)
{
    expr_slice_type* est = (expr_slice_type*)n;
    expr_array_type* eat =
        (expr_array_type*)(n->kind == EXPR_ARRAY_TYPE ? n : NULL);
    ast_elem* base_type;
    resolve_error re =
        resolve_ast_node(r, est->base_type, body, &base_type, NULL);
    if (re) return re;
    if (!eat) {
        est->ctype = get_slice_of(r, base_type, false);
        if (!est->ctype) return RE_FATAL;
    }
    else {
        ureg len;
        re = evaluate_array_bounds(r, eat, body, arr_expr_len, &len);
        if (re) {
            SET_THEN_RETURN_POISONED(
                r, re, est, body, value, ctype, est, ERROR_ELEM);
        }
        est->ctype = (type_slice*)get_array_of(r, base_type, len, false);
        if (!est->ctype) return RE_FATAL;
    }
    ast_node_set_resolved(&est->node);
    SET_THEN_RETURN(value, ctype, est->ctype, TYPE_ELEM);
}
resolve_error
resolve_import_symbol(resolver* r, sym_import_symbol* is, ast_body* body)
{
    resolve_error re;
    ast_node* node = &is->osym.sym.node;
    ast_body* decl_body = is->osym.sym.declaring_body;
    if (ast_node_get_resolving(node)) {
        // TOOD: report loop
        assert(false);
    }
    ast_node_set_resolving(node);
    import_module_data* im_data;
    import_group_get_data(is->import_group, NULL, &im_data, NULL, NULL);
    assert(im_data);
    re = resolve_ast_node_raw(r, is->import_group, body, NULL, NULL);
    if (re) {
        ast_node_clear_resolving(node);
        return re;
    }
    if (ast_node_get_resolved(node)) return RE_OK;
    symbol* sym;
    symbol* amb_err;
    re = resolver_lookup_single(
        r, &im_data->imported_module->body, NULL, false, decl_body,
        is->target.name, &sym, &amb_err);
    if (re || !sym) ast_node_clear_resolving(node);
    if (re) return re;
    if (!sym) {
        bool notif_added = false;
        int res = ppdct_curr_pprn_require_symbol(
            &r->ppdct, decl_body, NULL, is->target.name, &notif_added);
        if (res) return RE_FATAL;
        if (notif_added) return RE_UNKNOWN_SYMBOL;
        report_unknown_symbol(r, (ast_node*)is, decl_body, amb_err, false);
        is->target.sym = (symbol*)ERROR_ELEM;
        RETURN_POISONED(r, RE_OK, is, body);
    }
    if (is_symbol_kind_overloadable(sym->node.kind)) {
        is->target_body = &im_data->imported_module->body;
    }
    else {
        if (amb_err) {
            assert(false); // TODO: report ambiguity
        }
        if (sym->node.kind == SYM_IMPORT_SYMBOL) {
            sym_import_symbol* tgt_is = (sym_import_symbol*)is;
            re = resolve_import_symbol(r, tgt_is, body);
            if (re) return re;
            is->target = tgt_is->target;
            is->target_body = tgt_is->target_body;
        }
        else {
            is->target_body = NULL;
            is->target.sym = sym;
        }
    }
    ast_node_set_resolved(node);
    return RE_OK;
}
static inline resolve_error resolve_ast_node_raw(
    resolver* r, ast_node* n, ast_body* body, ast_elem** value,
    ast_elem** ctype)
{
    assert(n);
    bool resolved = ast_node_get_resolved(n);
    if (!resolved) {
        if (ast_node_get_resolving(n)) {
            return RE_TYPE_LOOP;
        }
        ast_node_set_resolving(n);
    }
    resolve_error re;
    switch (n->kind) {
        case MF_EXTEND:
        case MF_EXTEND_GENERIC:
        case MF_MODULE:
        case MF_MODULE_GENERIC: {
            if (!resolved) ast_node_set_resolved(n);
            SET_THEN_RETURN(value, ctype, n, VOID_ELEM);
        }
        case SC_MACRO: {
            if (!resolved) ast_node_set_resolved(n);
            SET_THEN_RETURN(value, ctype, n, TYPE_ELEM);
        }
        case SYM_PRIMITIVE: {
            if (!resolved) ast_node_set_resolved(n);
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
            if (resolved) {
                SET_THEN_RETURN(value, ctype, n, &PRIMITIVES[n->pt_kind]);
            }
            expr_literal* lit = (expr_literal*)n;
            if (n->pt_kind == PT_FLUID_INT) {
                ureg res;
                bool neg;
                re = parse_int_literal(r, lit, body, &res, &neg);
                if (re) return re;
                lit->value.val_ureg = res;
            }
            if (!resolved) ast_node_set_resolved(n);
            SET_THEN_RETURN(value, ctype, n, &PRIMITIVES[n->pt_kind]);
        }
        case EXPR_IDENTIFIER:
        case EXPR_SPECIAL_IDENTIFIER: {
            expr_identifier* e = (expr_identifier*)n;
            if (resolved) {
                if (ast_node_get_poisoned(n)) {
                    re = curr_body_propagate_error(r, body);
                    if (re) return re;
                    SET_THEN_RETURN(value, ctype, e->value.sym, ERROR_ELEM);
                }
                SET_THEN_RETURN(
                    value, ctype, e->value.sym,
                    ast_elem_get_ctype((ast_elem*)e->value.sym));
            }
            if (n->kind == EXPR_SPECIAL_IDENTIFIER) {
                return resolve_special_identifier(r, body, e, value, ctype);
            }
            else {
                return resolve_identifier(r, body, e, value, ctype);
            }
        }
        case EXPR_CALL: {
            expr_call* c = (expr_call*)n;
            if (resolved) {
                SET_THEN_RETURN(
                    value, ctype, c, c->target.fn->fnb.return_ctype);
            }
            return resolve_call(r, c, body, ctype);
        }
        case EXPR_NO_BLOCK_MACRO_CALL: {
            expr_call* c = (expr_call*)n;
            assert(resolved); // otherwise this would still be a EXPR_CALL
            SET_THEN_RETURN(value, ctype, c, c->target.macro_block->ebb.ctype);
        }
        case SYM_FUNC_OVERLOADED: { // used during import resolution
            assert(!ctype);
            // doesn't really matter, but for consistency
            if (!resolved) ast_node_set_resolved(n);
            SET_THEN_RETURN(value, ctype, n, NULL);
        }
        case EXPR_CONTINUE:
        case EXPR_OP_UNARY: {
            expr_op_unary* ou = (expr_op_unary*)n;
            if (resolved) {
                if (ou->op->kind == SC_FUNC) {
                    SET_THEN_RETURN(
                        value, ctype, ou, ((sc_func*)ou->op)->fnb.return_ctype);
                }
                if (ast_node_get_type_operator(&ou->node)) {
                    SET_THEN_RETURN(value, ctype, ou->op, TYPE_ELEM);
                }
                SET_THEN_RETURN(value, ctype, ou, ou->op);
            }
            re = choose_unary_operator_overload(r, ou, body, value, ctype);
            if (re) return re;
            ast_node_set_resolved(n);
            return RE_OK;
        }
        case EXPR_PARENTHESES: {
            // we set this even on error because we jump through to get the
            // required values anyways, so at least make it tail recursive
            if (!resolved) ast_node_set_resolved(n);
            return resolve_ast_node(
                r, ((expr_parentheses*)n)->child, body, value, ctype);
        }
        case EXPR_OP_BINARY: {
            expr_op_binary* ob = (expr_op_binary*)n;
            if (resolved) {
                if (ob->op->kind == SYM_PRIMITIVE) {
                    SET_THEN_RETURN(
                        value, ctype, ob,
                        (ast_elem*)&PRIMITIVES[((ast_node*)ob->op)->pt_kind]);
                }
                SET_THEN_RETURN(
                    value, ctype, ob, ((sc_func*)ob->op)->fnb.return_ctype);
            }
            re = choose_binary_operator_overload(r, ob, body, value, ctype);
            if (re) return re;
            // we do this here since choose has many return statements
            ast_node_set_resolved(n);
            return RE_OK;
        }
        case EXPR_MEMBER_ACCESS: {
            expr_member_access* ema = (expr_member_access*)n;
            if (resolved) {
                SET_THEN_RETURN(
                    value, ctype, n,
                    ast_elem_get_ctype((ast_elem*)ema->target.sym));
            }
            return resolve_expr_member_accesss(r, ema, body, value, ctype);
        }
        case EXPR_SCOPE_ACCESS: {
            expr_scope_access* esa = (expr_scope_access*)n;
            if (resolved) {
                if (ast_node_get_poisoned(n)) {
                    SET_THEN_RETURN(value, ctype, ERROR_ELEM, ERROR_ELEM);
                }
                SET_THEN_RETURN(
                    value, ctype, (ast_elem*)esa->target.sym,
                    ast_elem_get_ctype((ast_elem*)esa->target.sym));
            }
            return resolve_scoped_identifier(r, esa, body, value, ctype);
        }
        case SC_STRUCT_GENERIC: {
            if (!resolved) {
                sc_struct_generic* sg = (sc_struct_generic*)n;
                // TODO: handle scope escaped pp exprs
                /*re = add_body_decls(
                    r, &sg->sb.sc.body, NULL, !is_local_node((ast_elem*)sg));
                if (re) return re;*/
                for (ureg i = 0; i < sg->generic_param_count; i++) {
                    re = resolve_param(r, &sg->generic_params[i], true, NULL);
                    if (re) return re;
                }
                ast_node_set_resolved(n);
            }
            SET_THEN_RETURN(value, ctype, n, GENERIC_TYPE_ELEM);
        }
        case SC_TRAIT_GENERIC: {
            if (!resolved) ast_node_set_resolved(n);
            // TODO: handle scope escaped pp exprs
            SET_THEN_RETURN(value, ctype, n, GENERIC_TRAIT_ELEM);
        }
        case TRAIT_IMPL_GENERIC_INST:
        case TRAIT_IMPL_GENERIC:
        case TRAIT_IMPL:
        case SC_TRAIT:
        case SC_STRUCT:
        case SC_STRUCT_GENERIC_INST: {
            if (resolved) SET_THEN_RETURN(value, ctype, n, TYPE_ELEM);
            return resolve_unordered_body(
                r, body, ast_elem_get_body((ast_elem*)n), n, value, ctype);
        }
        case SC_FUNC:
        case SC_FUNC_GENERIC: {
            SET_THEN_RETURN_IF_RESOLVED(resolved, value, ctype, n, VOID_ELEM);
            return resolve_func(r, body, (sc_func_base*)n, NULL);
        }
        case SYM_IMPORT_PARENT: {
            SET_THEN_RETURN_IF_RESOLVED(resolved, value, ctype, n, NULL);
            assert(false);
            return RE_ERROR;
        }
        case ASTN_ANONYMOUS_SYM_IMPORT_GROUP:
        case SYM_IMPORT_MODULE: {
            import_module_data* im_data;
            if (n->kind == SYM_IMPORT_MODULE) {
                im_data = &((sym_import_module*)n)->im_data;
            }
            else {
                im_data = &((astn_anonymous_sym_import_group*)n)->im_data;
            }
            if (resolved) {
                if (im_data->pprn) {
                    resolve_error re =
                        curr_pprn_depend_on(r, body, &im_data->pprn);
                    if (re) return re;
                }
                SET_THEN_RETURN(value, ctype, n, NULL);
            }
            return resolve_importing_node(
                r, im_data, n, body, false, value, ctype);
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
                if (ast_node_get_poisoned(n)) {
                    re = curr_body_propagate_error(r, body);
                    if (re) return re;
                    SET_THEN_RETURN(value, ctype, v, ERROR_ELEM);
                }
                if (v->pprn) {
                    // if the symbol table isn't public we are looking up a
                    // symbol inside a function since we can find it we're
                    // in the same function therefore we don't want to
                    // depend on ourselves
                    // TODO: this simple detection for "is the var in my
                    // function" will no longer work once we have nested
                    // functions
                    if (ast_body_is_public(v->osym.sym.declaring_body)) {
                        re = curr_pprn_depend_on(r, body, &v->pprn);
                        if (re) return re;
                    }
                    else {
                        // otherwise the var shouldn't have a pprn
                        assert(ast_node_get_comptime((ast_node*)v));
                    }
                }
                SET_THEN_RETURN(value, ctype, v, v->ctype);
            }
            return resolve_var(r, body, v, value, ctype);
        }
        case EXPR_RETURN: {
            SET_THEN_RETURN_IF_RESOLVED(
                resolved, value, ctype, UNREACHABLE_ELEM, UNREACHABLE_ELEM);
            return resolve_return(r, body, (expr_return*)n);
        }
        case EXPR_BREAK: {
            SET_THEN_RETURN_IF_RESOLVED(
                resolved, value, ctype, UNREACHABLE_ELEM, UNREACHABLE_ELEM);
            return resolve_break(r, body, (expr_break*)n);
        }
        case EXPR_BLOCK: {
            expr_block* b = (expr_block*)n;
            if (resolved) SET_THEN_RETURN(value, ctype, NULL, b->ebb.ctype);
            return resolve_expr_block(r, b, body, value, ctype);
        }
        case EXPR_IF: {
            expr_if* ei = (expr_if*)n;
            if (resolved) SET_THEN_RETURN(value, ctype, n, ei->ctype);
            return resolve_if(r, body, ei, value, ctype);
        }
        case EXPR_LOOP: {
            expr_loop* l = (expr_loop*)n;
            if (resolved) {
                assert(!value);
                SET_THEN_RETURN(value, ctype, NULL, l->ebb.ctype);
            }
            bool end_reachable;
            re = resolve_expr_body(r, body, n, &l->ebb.body, &end_reachable);
            if (re == RE_UNREALIZED_COMPTIME) {
                if (l->ebb.ctype || !end_reachable) re = RE_OK;
            }
            if (ctype) *ctype = l->ebb.ctype;
            if (re) return re;
            if (!l->ebb.ctype) l->ebb.ctype = UNREACHABLE_ELEM;
            ast_node_set_resolved(n);
            SET_THEN_RETURN(value, ctype, value, l->ebb.ctype);
        }
        case EXPR_PASTE_EVALUATION: {
            paste_evaluation* pe = (paste_evaluation*)n;
            // HACK: to get owning node -> correct smap for errors
            re = resolve_ast_node(r, pe->expr, &pe->body, value, ctype);
            if (!resolved && !re) ast_node_set_resolved(n);
            return re;
        }
        case STMT_PASTE_EVALUATION: {
            paste_evaluation* pe = (paste_evaluation*)n;
            if (resolved) {
                assert(!value);
                SET_THEN_RETURN(
                    value, ctype, NULL,
                    ast_node_get_pp_stmt_end_unreachabale(n) ? UNREACHABLE_ELEM
                                                             : VOID_ELEM);
            }
            bool end_reachable;
            re = resolve_expr_body(r, body, n, &pe->body, &end_reachable);
            if (re == RE_UNREALIZED_COMPTIME) {
                if (!end_reachable) re = RE_OK;
            }
            if (ctype) {
                *ctype = (end_reachable) ? VOID_ELEM : UNREACHABLE_ELEM;
            }
            if (value) *value = VOID_ELEM;
            if (re) return re;

            if (!end_reachable) {
                ast_node_set_pp_stmt_end_unreachabale(n);
            }
            ast_node_set_resolved(n);
            return RE_OK;
        }
        case EXPR_MACRO_CALL: {
            // TODO ctype
            expr_macro_call* emc = (expr_macro_call*)n;
            if (resolved) SET_THEN_RETURN(value, ctype, n, emc->ctype);
            return resolve_macro_call(r, emc, body, value, ctype);
        }
        case EXPR_PP: {
            expr_pp* ppe = (expr_pp*)n;
            if (ast_node_get_pp_expr_contains_paste_eval(n)) {
                ast_node* ev = (ast_node*)ppe->result_buffer.paste_eval;
                re = resolve_ast_node(r, ev, body, value, ctype);
                if (!resolved && ast_node_get_resolved(ev)) {
                    ast_node_set_resolved(n);
                }
                return re;
            }
            if (resolved && ppe->ctype != PASTED_EXPR_ELEM &&
                (!ppe->pprn || !ppe->pprn->needs_further_resolution)) {
                assert(!value);
                SET_THEN_RETURN(value, ctype, NULL, ppe->ctype);
            }
            return resolve_expr_pp(r, body, ppe, false, value, ctype);
        }
        case EXPR_PASTE_STR: {
            if (resolved) {
                SET_THEN_RETURN(
                    value, ctype, PASTED_EXPR_ELEM, PASTED_EXPR_ELEM);
            }
            return resolve_expr_paste_str(
                r, body, (expr_paste_str*)n, value, ctype);
        } break;
        case EXPR_MATCH: {
            // TODO ctype
            if (ctype) *ctype = NULL;
            if (resolved) return RE_OK;
            expr_match* em = (expr_match*)n;
            re = resolve_ast_node(r, em->match_expr, body, NULL, NULL);
            if (re) return re;
            for (match_arm** ma = (match_arm**)em->body.elements; *ma != NULL;
                 ma++) {
                re = resolve_ast_node(r, (**ma).condition, body, NULL, NULL);
                if (re) return re;
                re = resolve_ast_node(r, (**ma).value, body, NULL, NULL);
                if (re) return re;
            }
            return RE_OK;
        }
        case SYM_GENERIC_PARAM:
        case SYM_PARAM: {
            sym_param* p = (sym_param*)n;
            if (resolved) SET_THEN_RETURN(value, ctype, p, p->ctype);
            re = resolve_param(r, p, (n->kind == SYM_GENERIC_PARAM), ctype);
            if (value) *value = (ast_elem*)n;
            return re;
        }
        case SYM_PARAM_GENERIC_INST: {
            assert(resolved);
            sym_param_generic_inst* pgi = (sym_param_generic_inst*)n;
            SET_THEN_RETURN(value, ctype, pgi->value, pgi->ctype);
        }
        case EXPR_ARRAY_TYPE:
        case EXPR_SLICE_TYPE: {
            if (resolved) {
                SET_THEN_RETURN(
                    value, ctype, ((expr_slice_type*)n)->ctype, TYPE_ELEM);
            }
            return resolve_array_or_slice_type(
                r, n, body, UREG_MAX, value, ctype);
        }
        case EXPR_ARRAY: {
            expr_array* ea = (expr_array*)n;
            if (resolved) SET_THEN_RETURN(value, ctype, ea, ea->ctype);
            if (ea->explicit_decl) {
                re = resolve_array_or_slice_type(
                    r, (ast_node*)ea->explicit_decl, body, ea->elem_count,
                    (ast_elem**)&ea->ctype, NULL);
                if (re) return re;
                if (ea->ctype->tb.kind == TYPE_ARRAY) {
                    if (((type_array*)ea->ctype)->length != ea->elem_count) {
                        assert(false); // TODO: error msg
                    }
                }
                else {
                    assert(ea->ctype->tb.kind == TYPE_SLICE);
                }
            }
            ast_node** e = ea->elements;
            ast_elem* elem_ctype;
            for (ureg i = 0; i < ea->elem_count; i++) {
                re = resolve_ast_node(r, *e, body, NULL, &elem_ctype);
                if (re) return re;
                if (!ea->ctype) {
                    ea->ctype = (type_slice*)get_array_of(
                        r, elem_ctype, ea->elem_count, false);
                    if (!ea->ctype) return RE_FATAL;
                }
                else {
                    type_cast_result tcr;
                    tcr = type_cast(
                        (ast_elem*)ea->ctype->ctype_members, elem_ctype, e);
                    if (tcr == TYPE_CAST_INCOMPATIBLE) {
                        src_range_large elem_srl;
                        src_range_large array_srl;
                        ast_node_get_src_range(*e, body, &elem_srl);
                        ast_node_get_src_range((ast_node*)ea, body, &array_srl);
                        // TODO: different error for negative values
                        error_log_report_annotated_twice(
                            r->tc->err_log, ES_RESOLVER, false,
                            "invalid array element type", elem_srl.smap,
                            elem_srl.start, elem_srl.end,
                            "element type not convertible to array type",
                            array_srl.smap, array_srl.start, array_srl.end,
                            NULL);
                        r->error_occured = true;
                    }
                    if (tcr) {
                        ast_node_set_poisoned((ast_node*)ea);
                    }
                }
                e++;
            }
            ast_node_set_resolved(&ea->node);
            SET_THEN_RETURN(value, ctype, ea, ea->ctype);
        }
        case EXPR_CAST: {
            // TODO: check whether convertible, for now we just allow
            // everything
            expr_cast* ec = (expr_cast*)n;
            if (resolved) {
                SET_THEN_RETURN(value, ctype, NULL, ec->target_ctype);
            }
            re = resolve_ast_node(
                r, ec->target_type, body, &ec->target_ctype, NULL);
            if (re) return re;
            re = resolve_ast_node(r, ec->value, body, NULL, NULL);
            if (re) return re;
            ast_node_set_resolved(&ec->node);
            SET_THEN_RETURN(value, ctype, NULL, ec->target_ctype);
        }
        case EXPR_ACCESS: {
            expr_access* ea = (expr_access*)n;
            if (resolved) {
                SET_THEN_RETURN(value, ctype, NULL, ea->ctype);
            }
            ast_elem* lhs_ctype;
            ast_elem* lhs_val;
            re = resolve_ast_node(r, ea->lhs, body, &lhs_val, &lhs_ctype);
            if (re) return re;
            if (ea->arg_count == 1 && ast_elem_is_type_slice(lhs_ctype)) {
                ast_elem* rhs_ctype;
                re = resolve_ast_node(r, ea->args[0], body, NULL, &rhs_ctype);
                type_cast_result tcr = type_cast(
                    (ast_elem*)&PRIMITIVES[PT_INT], rhs_ctype, &ea->args[0]);
                if (tcr) {
                    if (tcr == TYPE_CAST_INCOMPATIBLE) {
                        src_range_large array_srl;
                        src_range_large index_srl;
                        ast_node_get_src_range(ea->lhs, body, &array_srl);
                        ast_node_get_src_range(ea->args[0], body, &index_srl);
                        // TODO: different error for negative values
                        error_log_report_annotated_twice(
                            r->tc->err_log, ES_RESOLVER, false,
                            "invalid array index type", index_srl.smap,
                            index_srl.start, index_srl.end,
                            "index type is not convertible to integer",
                            array_srl.smap, array_srl.start, array_srl.end,
                            "in the element access of this array");
                        r->error_occured = true;
                    }
                    SET_THEN_RETURN_POISONED(
                        r, RE_OK, ea, body, value, ctype, ea, ERROR_ELEM);
                }
                ea->node.op_kind = OP_ARRAY_ACCESS;
                ast_node_set_resolved(&ea->node);
                ea->ctype = ((type_slice*)lhs_ctype)->ctype_members;
                SET_THEN_RETURN(value, ctype, NULL, ea->ctype);
            }
            if (lhs_val->kind == SC_STRUCT_GENERIC) {
                return resolve_generic_struct(
                    r, ea, (sc_struct_generic*)lhs_val, body, value, ctype);
            }
            else if (lhs_val == ERROR_ELEM) {
                SET_THEN_RETURN_POISONED(
                    r, RE_OK, ea, body, value, ctype, ERROR_ELEM, ERROR_ELEM);
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
            ast_node_set_resolved(n);
            SET_THEN_RETURN(value, ctype, VOID_ELEM, VOID_ELEM);
        }
        case ASTN_ANONYMOUS_MOD_IMPORT_GROUP: {
            ast_node_set_resolved(n);
            return RE_OK;
        }
        default: assert(false); return RE_UNKNOWN_SYMBOL;
    }
}
static inline resolve_error
report_type_loop(resolver* r, ast_node* n, ast_body* body)
{
    if (r->tc->t->trap_on_error) debugbreak();
    r->error_occured = true;
    r->retracing_type_loop = true;
    r->type_loop_start = NULL;
    assert(!ast_node_get_resolving(n));
    resolve_error re = resolve_ast_node_raw(r, n, body, NULL, NULL);
    r->retracing_type_loop = false;
    if (re != RE_TYPE_LOOP) {
        assert(re == RE_FATAL);
        return re;
    }
    ureg stack_ec = stack_element_count(&r->error_stack);
    // we are at the peek of the type loop. unwind and report again.
    src_range_large srl;
    ast_node_get_src_range(n, body, &srl);
    ureg annot_count = stack_ec / 2;
    // TODO: this error message isn't quite right for cyclic pprn deps
    error* e = error_log_create_error(
        r->tc->err_log, ES_RESOLVER, false, "resolution cycle", srl.smap,
        srl.start, srl.end, "type definition depends on itself", annot_count);
    bool skip_next = false;
    annot_count++;
    while (true) {
        ast_node_set_resolved(n);
        ast_node_set_poisoned(n);
        re = ast_node_propagate_error(r, n);
        pp_resolve_node** pprnp = ast_node_try_get_pprn_ptr(n);
        if (pprnp && *pprnp) {
            pp_resolve_node_activate(r, body, pprnp, false);
        }
        if (n->kind == EXPR_RETURN || n->kind == EXPR_OP_UNARY ||
            n->kind == EXPR_OP_BINARY || n->kind == EXPR_PP) {
            skip_next = true;
        }
        annot_count--;
        if (annot_count == 0) break;
        n = (ast_node*)stack_pop(&r->error_stack);
        assert(n);
        ast_body* err_body = (ast_body*)stack_pop(&r->error_stack);
        ast_node_get_src_range(n, err_body, &srl);
        bool last_annot = (annot_count == 1);
        if (!last_annot && (skip_next || n->kind == EXPR_BLOCK)) {
            // don't highlight too much stuff
            ast_node_get_src_range(n, err_body, &srl);
            error_add_annotation(e, srl.smap, srl.start, srl.end, NULL);
            skip_next = false;
        }
        else {
            char* annot = last_annot ? "loop detected" : "";
            error_add_annotation(e, srl.smap, srl.start, srl.end, annot);
        }
        if (re) return re;
    }
    assert(stack_element_count(&r->error_stack) == 0);
    error_log_report(r->tc->err_log, e);
    return RE_OK;
}
resolve_error add_error_entry(resolver* r, ast_node* n, ast_body* body)
{
    if (stack_push(&r->error_stack, body)) return RE_FATAL;
    if (stack_push(&r->error_stack, n)) return RE_FATAL;
    return RE_OK;
}
resolve_error handle_resolve_error(
    resolver* r, ast_node* n, ast_body* body, resolve_error re,
    ast_elem** value, ast_elem** ctype)
{
    if (!re) return RE_OK;
    bool started_here = false;
    if (re == RE_TYPE_LOOP) {
        if (!r->type_loop_start) {
            r->type_loop_start = n;
            started_here = true;
        }
        if (!r->allow_type_loops && !started_here) {
            if (n == r->type_loop_start) {
                if (r->retracing_type_loop) return re;
                re = report_type_loop(r, n, body);
                SET_VAL_CTYPE(value, ctype, ERROR_ELEM, ERROR_ELEM);
                return re;
            }
            if (r->retracing_type_loop) {
                add_error_entry(r, n, body);
            }
        }
        ast_node_clear_resolving(n);
    }
    else {
        if (!ast_node_get_resolved(n) && ast_node_get_resolving(n)) {
            ast_node_clear_resolving(n);
        }
    }
    if (re != RE_UNREALIZED_COMPTIME && re != RE_TYPE_LOOP &&
        r->tc->t->trap_on_error) {
        debugbreak();
    }
    return re;
}
resolve_error resolve_ast_node(
    resolver* r, ast_node* n, ast_body* body, ast_elem** value,
    ast_elem** ctype)
{
    resolve_error re = resolve_ast_node_raw(r, n, body, value, ctype);
    return handle_resolve_error(r, n, body, re, value, ctype);
}
resolve_error resolve_expr_body(
    resolver* r, ast_body* parent_body, ast_node* expr, ast_body* b,
    bool* end_reachable)
{
    resolve_error re = RE_OK;
    ast_elem* stmt_ctype = NULL;
    ast_elem** stmt_ctype_ptr = &stmt_ctype;
    pp_resolve_node* pprn = b->pprn;

    bool parent_allows_type_loops = r->allow_type_loops;
    bool parenting_type_loop = false;
    if (!r->retracing_type_loop) {
        r->allow_type_loops = true;
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
        re = add_ast_node_decls(r, b, NULL, *n, false);
        if (re) break;
        re = ast_node_add_trait_decls(r, b, *n);
        if (re) break;
        re = resolve_ast_node(r, *n, b, NULL, stmt_ctype_ptr);
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
        if (re) break;
        if (stmt_ctype_ptr && stmt_ctype == UNREACHABLE_ELEM) {
            stmt_ctype_ptr = NULL;
        }
        if (b->pprn && b->pprn->pending_pastes) {
            b->pprn->continue_block = n;
            re = RE_UNKNOWN_SYMBOL;
            break;
        }
    }
    pprn = b->pprn;
    *end_reachable = (stmt_ctype_ptr != NULL);
    r->allow_type_loops = parent_allows_type_loops;
    if (!re && parenting_type_loop) {
        return RE_TYPE_LOOP;
    }
    if (pprn) {
        pprn->block_pos_reachable = *end_reachable;
        pprn->declaring_body = parent_body;
        resolve_error re2;
        if (!pprn->first_unresolved_child) {
            // this is a rerun and everyting got resolved
            // detach this from parent and free it individually
            pprn->dummy = true;
        }
        else {
            if (curr_pp_block_add_child(r, parent_body, &b->pprn))
                return RE_FATAL;
        }
        if (re) {
            pprn->continue_block = n;
            // so it gets freed on error
            re2 = pprn_set_state(r, pprn, PPRN_WAITING);
            if (re2) return re2;
        }
        else {
            pprn->continue_block = NULL;
        }
        if (!re) {
            ast_node_set_resolved(expr);
        }
        re2 = pp_resolve_node_activate(r, parent_body, &b->pprn, re == RE_OK);
        if (re2) return re2;
    }
    return re;
}

resolve_error resolve_func_from_call(
    resolver* r, ast_body* body, sc_func* fn, ast_elem** ctype)
{
    resolve_error re;
    if (ast_node_get_resolved((ast_node*)fn)) {
        if (ctype) *ctype = fn->fnb.return_ctype;
        // fn already fully ready
        if (fn->fnb.sc.body.pprn == NULL) {
            if (ast_node_get_contains_error((ast_node*)fn)) {
                return curr_body_propagate_error(r, body);
            }
            return RE_OK;
        }
    }
    else {
        ast_body* decl_body = fn->fnb.sc.osym.sym.declaring_body;
        if (!fn->fnb.sc.body.pprn) {
            fn->fnb.sc.body.pprn = pp_resolve_node_create(
                r, (ast_node*)fn, decl_body, true, true, false, false);
            if (!fn->fnb.sc.body.pprn) return RE_FATAL;
        }
        if (fn->fnb.return_type) {
            if (!fn->fnb.return_ctype) {
                re = resolve_ast_node(
                    r, fn->fnb.return_type, decl_body, &fn->fnb.return_ctype,
                    NULL);
                if (re) return re;
            }
        }
        else {
            fn->fnb.return_ctype = VOID_ELEM;
        }
        if (ctype) *ctype = fn->fnb.return_ctype;
    }
    re = curr_pprn_depend_on(r, body, &fn->fnb.sc.body.pprn);
    if (re) return re;
    if (ast_node_get_contains_error((ast_node*)fn)) {
        return curr_body_propagate_error(r, body);
    }
    if (is_curr_resolution_for_pastes(r, body)) {
        pp_resolve_node_activate(r, body, &fn->fnb.sc.body.pprn, false);
    }
    return RE_OK;
}
// used for structs, traits, trait impls and their generic counterparts
resolve_error resolve_unordered_body(
    resolver* r, ast_body* requesting_body, ast_body* unordered_body,
    ast_node* owning_node, ast_elem** value, ast_elem** ctype)
{
    pp_resolve_node* prev_curr_pp = r->curr_pp_node;
    r->curr_pp_node = NULL;
    resolve_error re;
    ast_body* b = unordered_body;
    re = add_body_decls(r, b, NULL, !is_local_node((ast_elem*)owning_node));
    if (re) return re;
    bool unrealized_comptime = false;
    bool unknown_symbol = false;
    r->curr_pp_node = NULL;
    re = unordered_body_add_trait_decls(r, b);
    if (!re) {
        re = resolver_run_pp_resolve_nodes(r, NULL);
        if (!re && b->pprn) {
            if (b->pprn->dep_count) {
                b->pprn->needs_further_resolution = true;
                re = RE_UNREALIZED_COMPTIME;
            }
            else {
                b->pprn->pending_pastes = false;
            }
        }
        if (!re && (!b->pprn || b->pprn->pending_pastes == false)) {
            ast_node_set_pp_done(b->owning_node);
            re = ppdct_seal_body(&r->ppdct, b);
        }
    }
    if (!re) {
        for (ast_node** n = b->elements; *n != NULL; n++) {
            re = resolve_ast_node(r, *n, b, NULL, NULL);
            if (re == RE_UNREALIZED_COMPTIME) {
                unrealized_comptime = true;
                re = RE_OK;
            }
            else if (re == RE_UNKNOWN_SYMBOL) {
                unknown_symbol = true;
                re = RE_OK;
            }
            else if (re) {
                break;
            }
        }
    }
    assert(!r->curr_pp_node);
    r->curr_pp_node = prev_curr_pp;
    if (!re) {
        if (unrealized_comptime) re = RE_UNREALIZED_COMPTIME;
        if (unknown_symbol) re = RE_UNKNOWN_SYMBOL;
        if (re) {
            assert(b->pprn);
            b->pprn->needs_further_resolution = true;
        }
    }
    if (!re) ast_node_set_resolved((ast_node*)owning_node);
    if (b->pprn) {
        resolve_error re2 = curr_pprn_depend_on(r, requesting_body, &b->pprn);
        if (!re2) {
            re2 = pp_resolve_node_activate(
                r, requesting_body, &b->pprn, re == RE_OK);
        }
        if (re2) {
            assert(re2 == RE_FATAL);
            return re2;
        }
    }
    SET_VAL_CTYPE(value, ctype, owning_node, TYPE_ELEM);
    return re;
}
// TODO: make sure we return!
resolve_error resolve_func(
    resolver* r, ast_body* requesting_body, sc_func_base* fnb,
    ast_node** continue_block)
{
    bool generic = (fnb->sc.osym.sym.node.kind == SC_FUNC_GENERIC);
    bool generic_parent = r->generic_context;
    r->generic_context = generic || generic_parent;
    resolve_error re = RE_OK;
    if (!continue_block) {
        if (!ast_node_get_static((ast_node*)fnb)) {
            ast_body* npp =
                ast_body_get_non_paste_parent(fnb->sc.osym.sym.declaring_body);
            ast_elem* on = (ast_elem*)npp->owning_node;
            if (ast_elem_is_struct(on) || ast_elem_is_trait_impl(on)) {
                ast_node_set_instance_member((ast_node*)fnb);
            }
        }
        if (generic) {
            sc_func_generic* fng = (sc_func_generic*)fnb;
            for (ureg i = 0; i < fng->generic_param_count; i++) {
                re = resolve_param(r, &fng->generic_params[i], false, NULL);
                if (re) {
                    r->generic_context = generic_parent;
                    return re;
                }
            }
        }
        for (ureg i = 0; i < fnb->param_count; i++) {
            re = resolve_param(r, &fnb->params[i], false, NULL);
            if (re) {
                r->generic_context = generic_parent;
                return re;
            }
        }
        if (fnb->return_type) {
            re = resolve_ast_node(
                r, fnb->return_type, fnb->sc.osym.sym.declaring_body,
                &fnb->return_ctype, NULL);
            if (re) {
                r->generic_context = generic_parent;
                fnb->return_ctype = ERROR_ELEM;
                if (re == RE_UNREALIZED_COMPTIME || re == RE_FATAL) return re;
                ast_node_set_poisoned((ast_node*)fnb);
            }
        }
        else {
            fnb->return_ctype = VOID_ELEM;
        }
        // handle function declarations
        if (fnb->sc.body.srange == SRC_RANGE_INVALID) {
            ast_node_set_resolved((ast_node*)fnb);
            r->generic_context = generic_parent;
            if (fnb->sc.body.pprn) {
                return pp_resolve_node_activate(
                    r, requesting_body, &fnb->sc.body.pprn, true);
            }
            return RE_OK;
        }
    }
    ast_elem* stmt_ctype;
    ast_elem** stmt_ctype_ptr = &stmt_ctype;
    ast_node** n = fnb->sc.body.elements;
    if (continue_block) {
        n = continue_block;
        if (!fnb->sc.body.pprn->block_pos_reachable) stmt_ctype_ptr = NULL;
    }
    // since curr_pprn is expression based we want to reset it so
    // we don't add children to the caller of our func
    pp_resolve_node* prev_pprn = r->curr_pp_node;
    r->curr_pp_node = NULL;
    while (*n) {
        // TODO: move this kind of error into the prp
        if (stmt_ctype_ptr == NULL && n != continue_block) {
            src_range_large srl;
            ast_node_get_src_range(*n, &fnb->sc.body, &srl);
            error_log_report_annotated(
                r->tc->err_log, ES_RESOLVER, false,
                "unreachable statement in function", srl.smap, srl.start,
                srl.end, "after return statement");
            re = RE_TYPE_MISSMATCH;
            break;
        }
        re = add_ast_node_decls(r, &fnb->sc.body, NULL, *n, false);
        if (re) break;
        re = ast_node_add_trait_decls(r, &fnb->sc.body, *n);
        if (re) break;
        re = resolve_ast_node(r, *n, &fnb->sc.body, NULL, stmt_ctype_ptr);
        if (re) break;
        assert(r->curr_pp_node == NULL);
        if (fnb->sc.body.pprn && fnb->sc.body.pprn->pending_pastes) {
            fnb->sc.body.pprn->continue_block = n;
            re = RE_UNKNOWN_SYMBOL;
            break;
        }
        if (stmt_ctype_ptr && stmt_ctype == UNREACHABLE_ELEM) {
            stmt_ctype_ptr = NULL;
        }
        n++;
    }
    r->curr_pp_node = prev_pprn;
    // this must be reset before we call add dependency
    pp_resolve_node* bpprn = fnb->sc.body.pprn;
    r->generic_context = generic_parent;
    if (re == RE_UNREALIZED_COMPTIME || re == RE_UNKNOWN_SYMBOL) {
        assert(bpprn);
        bpprn->needs_further_resolution = true;
        resolve_error re2 = pp_resolve_node_activate(
            r, requesting_body, &fnb->sc.body.pprn, false);
        if (re2) return re2;
        bpprn->continue_block = n;
        bpprn->block_pos_reachable = (stmt_ctype_ptr != NULL);
        ast_node_clear_resolving((ast_node*)fnb);
        return re;
    }
    if (re == RE_TYPE_LOOP) return re;
    if (!re && stmt_ctype_ptr && fnb->return_ctype != VOID_ELEM) {
        ureg brace_end = src_range_get_end(fnb->sc.body.srange);
        src_map* smap = ast_body_get_smap(fnb->sc.osym.sym.declaring_body);
        error_log_report_annotated_thrice(
            r->tc->err_log, ES_RESOLVER, false,
            "reachable end of non void function", smap, brace_end - 1,
            brace_end, "missing return statement", smap,
            src_range_get_start(fnb->return_type->srange),
            src_range_get_end(fnb->return_type->srange),
            "function returns non void type", smap,
            src_range_get_start(fnb->sc.osym.sym.node.srange),
            src_range_get_end(fnb->sc.osym.sym.node.srange), NULL);
        re = RE_TYPE_MISSMATCH;
    }
    if (!re) ast_node_set_resolved((ast_node*)fnb);
    if (bpprn) {
        if (!re) bpprn->continue_block = NULL;
        if (!fnb->sc.body.pprn->first_unresolved_child &&
            fnb != (sc_func_base*)r->module_group_constructor) {
            fnb->sc.body.pprn->dummy = true;
        }
        resolve_error re2 = pp_resolve_node_activate(
            r, requesting_body, &fnb->sc.body.pprn, re == RE_OK);
        if (re2) {
            assert(re2 == RE_FATAL);
            return re2;
        }
    }
    if (re) {
        r->error_occured = true;
        ast_node_set_contains_error((ast_node*)fnb);
        RETURN_POISONED(r, re, fnb, requesting_body);
    }
    return RE_OK;
}
resolve_error resolve_module_frame(resolver* r, module_frame* mf, ast_body* b)
{
    resolve_error re = RE_OK;
    for (ast_node** n = b->elements; *n != NULL; n++) {
        re = resolve_ast_node(r, *n, b, NULL, NULL);
        // that must have been caught already
        assert(re != RE_TYPE_LOOP);
        if (re == RE_UNREALIZED_COMPTIME) re = RE_OK;
        if (re) break;
    }

    return re;
}
void adjust_node_ids(resolver* r, ureg* id_space, ast_node* n);
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
void adjust_node_ids(resolver* r, ureg* id_space, ast_node* n)
{
    // we don't need to recurse into expressions because the contained
    // symbols can never be public
    switch (n->kind) {
        case SC_FUNC: {
            if (is_local_node((ast_elem*)n)) return;
            sc_func* fn = (sc_func*)n;
            update_id(r, &fn->id, id_space);
        } break;
        case SYM_VAR:
        case SYM_VAR_INITIALIZED: {
            if (is_local_node((ast_elem*)n)) return;
            update_id(r, &((sym_var*)n)->var_id, id_space);
        } break;
        case TRAIT_IMPL:
        case TRAIT_IMPL_GENERIC_INST: {
            if (is_local_node((ast_elem*)n)) return;
            update_id(r, &((trait_impl*)n)->backend_id, id_space);
            adjust_body_ids(r, id_space, &((trait_impl*)n)->tib.body);
        } break;
        case SC_STRUCT_GENERIC: {
            if (is_local_node((ast_elem*)n)) return;
            sc_struct_generic* sg = (sc_struct_generic*)n;
            // adjust_body_ids(r, id_space, &sg->sb.sc.body);
            symbol** end =
                sg->inst_map.instances + ((ureg)1 << sg->inst_map.bitcount);
            for (symbol** s = sg->inst_map.instances; s != end; s++) {
                if (!*s) continue;
                adjust_node_ids(r, id_space, (ast_node*)*s);
            }
            sg->sb.sc.osym.sym.node.emitted_for_pp = true;
        } break;
        case SC_STRUCT:
        case SC_STRUCT_GENERIC_INST: {
            if (is_local_node((ast_elem*)n)) return;
            update_id(r, &((sc_struct*)n)->backend_id, id_space);
            adjust_body_ids(r, id_space, &((sc_struct*)n)->sb.sc.body);
        } break;
        case EXPR_PP: {
            expr_pp* epp = (expr_pp*)n;
            adjust_node_ids(r, id_space, epp->pp_expr);
            if (ast_node_get_pp_expr_contains_paste_eval(n)) {
                adjust_node_ids(
                    r, id_space, (ast_node*)epp->result_buffer.paste_eval);
            }
        } break;
        case STMT_PASTE_EVALUATION: {
            adjust_body_ids(r, id_space, &((paste_evaluation*)n)->body);
        } break;
        default: return;
    }
}

typedef struct unverified_module_frame_s {
    file_map_head* file;
    module_frame* frame;
    mdg_node* node;
    struct unverified_module_frame_s* next_verified;
} unverified_module_frame;

#define SORT_NAME unverified_module_frame
#define SORT_TYPE unverified_module_frame
#define SORT_CMP(x, y) (ptrcmp((x).file, (y).file))
#include "sort.h"
resolve_error resolver_mark_required_module_fill_buffer(
    resolver* r, mdg_node* n, unverified_module_frame** frames_buffer,
    unverified_module_frame** buffer_end)
{
    aseglist_iterator it;
    aseglist_iterator_begin(&it, &n->module_frames);
    sbuffer_iterator tb_begin = sbuffer_iterator_begin_at_end(&r->temp_buffer);
    assert(n->root || n == r->tc->t->mdg.root_node);
    ureg count = 0;
    for (module_frame* mf = aseglist_iterator_next(&it); mf != NULL;
         mf = aseglist_iterator_next(&it)) {
        if (mf->node.kind == MF_MODULE) {
            assert(n != r->tc->t->mdg.root_node);
            if (mf != n->root) {
                report_module_redeclaration(
                    r->tc, n->root, mf->smap, mf->node.srange);
                if (aseglist_add(
                        &r->tc->t->mdg.invalid_node->module_frames, mf)) {
                    return RE_FATAL;
                }
                continue;
            }
            continue;
        }
        ast_elem* src = mf->smap->source;
        while (src->kind != ELEM_SRC_FILE) {
            if (src->kind == ELEM_PASTED_SRC) {
                pasted_source* ps = (pasted_source*)src;
                src = src_range_get_smap(ps->source_pp_srange)->source;
            }
            else {
                assert(false);
            }
        }
        unverified_module_frame* f =
            sbuffer_append(&r->temp_buffer, sizeof(unverified_module_frame));
        if (!f) {
            sbuffer_set_end(&r->temp_buffer, &tb_begin);
            return RE_FATAL;
        }
        f->frame = mf;
        f->file = (file_map_head*)src;
        f->next_verified = NULL;
        count++;
    }
    ureg alloc_size = count * sizeof(unverified_module_frame);
    unverified_module_frame* frames = pool_alloc(&r->tc->tempmem, alloc_size);
    if (!frames) {
        sbuffer_set_end(&r->temp_buffer, &tb_begin);
        return RE_FATAL;
    }
    sbuffer_memcpy(frames, tb_begin, alloc_size);
    sbuffer_set_end(&r->temp_buffer, &tb_begin);
    unverified_module_frame_quick_sort(frames, count);
    *frames_buffer = frames;
    *buffer_end = frames + count;
    // assert(count > 0 || root);
    return RE_OK;
}
static inline void resolver_mark_required_frame(
    resolver* r, file_map_head* file, unverified_module_frame* frames,
    unverified_module_frame* frames_end, unverified_module_frame** verified,
    ureg* unverified_count)
{
    unverified_module_frame *left, *right, *center;
    assert(frames != frames_end);
    left = frames;
    right = frames_end - 1;
    while (true) {
        center = left + (right - left) / 2;
        int cmp = ptrcmp(file, center->file);
        if (cmp < 0) {
            right = center - 1;
        }
        else if (cmp > 0) {
            left = center + 1;
        }
        else {
            break;
        }
        if (left > right) return;
    }
    while (center < frames_end - 1) {
        if ((center + 1)->file == file)
            center = center + 1;
        else
            break;
    }
    while (center->file == file) {
        if (!center->next_verified) {
            center->next_verified = *verified;
            *verified = center;
            (*unverified_count)--;
        }
        if (center == frames) break;
        center--;
    }
    return;
}
resolve_error resolver_mark_required_modules(resolver* r, mdg_node* n)
{
    unverified_module_frame* frames;
    unverified_module_frame* end;
    unverified_module_frame* verified = (unverified_module_frame*)NULL_PTR_PTR;
    resolve_error re =
        resolver_mark_required_module_fill_buffer(r, n, &frames, &end);
    if (re) return re;
    ureg unverified_count = end - frames;

    module_frame* curr = n->root;
    if (!curr && unverified_count != 0) {
        assert(n == r->tc->t->mdg.root_node);
        list_it it;
        list_it_begin(&it, &r->tc->t->required_files);
        src_file* f;
        while ((f = list_it_next(&it, &r->tc->t->required_files))) {
            resolver_mark_required_frame(
                r, &f->head, frames, end, &verified, &unverified_count);
            if (!unverified_count) break;
        }
        if (verified != (unverified_module_frame*)NULL_PTR_PTR) {
            curr = verified->frame;
            verified = verified->next_verified;
        }
    }

    while (curr && unverified_count != 0) {
        for (file_require* req = curr->requires; req->fmh != NULL; req++) {
            if (req->is_extern) continue;
            resolver_mark_required_frame(
                r, req->fmh, frames, end, &verified, &unverified_count);
            if (!unverified_count) break;
        }
        if (verified == (unverified_module_frame*)NULL_PTR_PTR) break;
        curr = verified->frame;
        verified = verified->next_verified;
    }
    if (unverified_count != 0) {
        r->error_occured = true;
        aseglist old_list = n->module_frames;
        aseglist_init(&n->module_frames);
        if (n->root) {
            if (aseglist_add(&n->module_frames, n->root)) {
                aseglist_fin(&n->module_frames);
                n->module_frames = old_list;
                return RE_FATAL;
            };
        }
        for (unverified_module_frame* m = frames; m != end; m++) {
            if (!m->next_verified) continue;
            if (aseglist_add(&n->module_frames, m->frame)) {
                aseglist_fin(&n->module_frames);
                n->module_frames = old_list;
                return RE_FATAL;
            };
        }
        aseglist_fin(&old_list);
        for (unverified_module_frame* m = frames; m != end; m++) {
            if (m->next_verified) continue;
            if (report_unrequired_extend(
                    r->tc, n, m->frame->smap, m->frame->node.srange))
                return RE_FATAL;
            ast_node_set_poisoned((ast_node*)n);
            if (aseglist_add(
                    &r->tc->t->mdg.invalid_node->module_frames, m->frame)) {
                return RE_FATAL;
            }
        }
    }
    pool_undo_last_alloc(&r->tc->tempmem, ptrdiff(end, frames));
    return RE_OK;
}
resolve_error resolver_init_mdg_symtabs_and_handle_root(resolver* r)
{
    for (mdg_node** i = r->mdgs_begin; i != r->mdgs_end; i++) {
        resolve_error re = resolver_mark_required_modules(r, *i);
        if (re) return re;
        // TODO: init pp symtabs
        r->error_occured |= (**i).error_occured;
        ureg sym_count = atomic_ureg_load(&(**i).decl_count);
        ureg using_count = atomic_ureg_load(&(**i).using_count);
        ureg impl_count = atomic_ureg_load(&(**i).impl_count);
        ureg generic_impl_count = atomic_ureg_load(&(**i).generic_impl_count);
        if (sym_count || using_count) {
            (**i).body.symtab = symbol_table_create(
                sym_count, using_count, impl_count, generic_impl_count);
            if (!(**i).body.symtab) return RE_FATAL;
        }
        else {
            (**i).body.symtab = NULL;
        }
        (**i).body.parent =
            &r->tc->t->global_scope.body; // assertion in set parent symtabs
    }
    if (!resolver_resolving_root(r)) {
        // root is marked as a linking holdup from the beginning to prevent
        // immediate linking
        atomic_ureg_inc(&r->tc->t->linking_holdups);
    }
    return RE_OK;
}
resolve_error resolver_add_mf_decls(resolver* r)
{
    for (mdg_node** i = r->mdgs_begin; i != r->mdgs_end; i++) {
        aseglist_iterator asi;
        aseglist_iterator_begin(&asi, &(**i).module_frames);
        for (module_frame* mf = aseglist_iterator_next(&asi); mf != NULL;
             mf = aseglist_iterator_next(&asi)) {
            mf->body.parent = &(**i).body;
            resolve_error re = add_body_decls(r, &mf->body, &(**i).body, true);
            if (re) return re;
            assert(!r->curr_pp_node);
        }
    }
    return RE_OK;
}
void print_pprn(resolver* r, pp_resolve_node* pprn, bool verbose, ureg ident)
{
    assert(pprn->node != NULL);

    pp_resolve_node* child = pprn->first_unresolved_child;
    print_indent(ident);
    if (ast_elem_is_func_base((ast_elem*)pprn->node)) {
        tprintf("func %s", ((sc_func_base*)pprn->node)->sc.osym.sym.name);
    }
    else {
        print_ast_node(pprn->node, NULL, ident);
    }
    tputs("");
    if (verbose) {
        list_it it;
        print_indent(ident);
        tprintf("dependencies: %zu\n", pprn->dep_count);
        list_it_begin(&it, &pprn->notified_by);
        for (pp_resolve_node** p =
                 (pp_resolve_node**)list_it_next(&it, &pprn->notified_by);
             p; p = (pp_resolve_node**)list_it_next(&it, &pprn->notified_by)) {
            if (*p) print_pprn(r, *p, false, ident + 1);
        }
        list_it_begin(&it, &pprn->notify);
        ureg nots = list_length(&pprn->notify);
        print_indent(ident);
        tprintf(
            "nofify when %s: %zu\n", pprn->notify_when_ready ? "ready" : "done",
            nots);
        for (pp_resolve_node* p =
                 (pp_resolve_node*)list_it_next(&it, &pprn->notify);
             p; p = (pp_resolve_node*)list_it_next(&it, &pprn->notify)) {
            print_pprn(r, p, false, ident + 1);
        }
        if (child) {
            print_indent(ident);
            tputs("children:");
            while (child) {
                print_indent(ident + 1);
                print_ast_node(child->node, NULL, ident + 1);
                tputs("");
                child = child->next;
            }
        }
    }
}
// this stuff is just for debugging purposes
void print_pprnlist(resolver* r, sbuffer* buff, char* msg, bool verbose)
{
    if (sbuffer_get_used_size(buff) == 0) return;
    tputs(msg);
    sbuffer_iterator sbi = sbuffer_iterator_begin(buff);
    bool first = true;
    for (pp_resolve_node** rn =
             sbuffer_iterator_next(&sbi, sizeof(pp_resolve_node*));
         rn; rn = sbuffer_iterator_next(&sbi, sizeof(pp_resolve_node*))) {
        if (!first) {
            print_indent(1);
            tputs("----------");
        }
        first = false;
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
resolve_error pp_resolve_node_ready(resolver* r, pp_resolve_node* pprn)
{
    if (!pprn->activated) return RE_OK;
    resolve_error re;
    assert(pprn->state != PPRN_READY);
    if (pprn->needs_further_resolution) {
        return pprn_set_state(r, pprn, PPRN_PENDING);
    }
    if (pprn->notify_when_ready) {
        list_it it;
        list_it_begin(&it, &pprn->notify);
        for (pp_resolve_node* rn = list_it_next(&it, &pprn->notify); rn;
             rn = list_it_next(&it, &pprn->notify)) {
            re = pp_resolve_node_dep_ready(r, rn);
            if (re) return re;
        }
        list_clear(&pprn->notify);
    }
    if (pprn->dummy ||
        (pprn->run_individually && ast_node_get_contains_error(pprn->node))) {
        re = pp_resolve_node_done(r, pprn, NULL);
        if (re) return re;
    }
    else if (pprn->run_individually) {
        re = pprn_set_state(r, pprn, PPRN_READY);
    }
    return RE_OK;
}
resolve_error pp_resolve_node_dep_ready(resolver* r, pp_resolve_node* pprn)
{
    pprn->dep_count--;
    if (pprn->dep_count == 0) {
        resolve_error re = pp_resolve_node_ready(r, pprn);
        if (re) return re;
    }
    return RE_OK;
}

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
        if (pprn->run_individually) {
            if (progress) *progress = true;
            re = pprn_set_state(r, pprn, PPRN_PENDING);
            if (re) return re;
        }
        pprn->activated = false;
        pprn->needs_further_resolution = true;
        return RE_OK;
    }
    list_it it;
    list_it_begin(&it, &pprn->notify);
    for (pp_resolve_node* rn = list_it_next(&it, &pprn->notify); rn;
         rn = list_it_next(&it, &pprn->notify)) {
        assert(!pprn->notify_when_ready);
        re = pp_resolve_node_dep_done(r, rn, progress);
        if (re) return re;
    }
    pprn_fin(r, pprn, false);
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
        resolve_error re = pp_resolve_node_ready(r, pprn);
        if (re) return re;
    }
    return RE_OK;
}
resolve_error report_cyclic_pp_deps(resolver* r)
{
    bool err = false;
    assert(ptrlist_is_empty(&r->pp_resolve_nodes_ready));
    // we might have pending guys, but since they have no deps
    // they can't cause a cycle
    pli it = pli_rbegin(&r->pp_resolve_nodes_waiting);
    for (pp_resolve_node* pprn = pli_prev(&it); pprn; pprn = pli_prev(&it)) {
        if (pprn->dummy && !ast_node_get_used_in_pp(pprn->node)) {
            pprn_fin(r, pprn, true);
        }
    }

    if (ptrlist_is_empty(&r->pp_resolve_nodes_waiting)) return RE_OK;
    // TODO: create a nice cycle display instead of dumping out everything
    resolve_error re = RE_OK;
    it = pli_begin(&r->pp_resolve_nodes_waiting);
    for (pp_resolve_node* pprn = pli_next(&it); pprn; pprn = pli_next(&it)) {
        if (!ast_elem_is_any_import((ast_elem*)pprn->node)) {
            if (err == false) {
                err = true;
                print_pprns(r, "error: \n", true);
            }
            src_range_large srl;
            ast_node_get_src_range(pprn->node, pprn->declaring_body, &srl);
            error_log_report_annotated(
                r->tc->err_log, ES_RESOLVER, false,
                "encountered cyclic dependency during preprocessor "
                "execution",
                srl.smap, srl.start, srl.end, "loop contains this element");
            re = RE_ERROR;
            if (r->tc->t->trap_on_error) debugbreak();
        }
    }
    return re;
}
resolve_error handle_pending_pprns(resolver* r, bool* progress)
{
    resolve_error re;
    while (!ptrlist_is_empty(&r->pp_resolve_nodes_pending)) {
        pp_resolve_node* rn = ptrlist_pop_back(&r->pp_resolve_nodes_pending);
        rn->pprn_list_entry = NULL;
        if (rn->continue_block) {
            *progress = true;
            ast_node* node = rn->node;
            ast_body* body = rn->declaring_body;
            if (!ast_node_get_resolved(node)) ast_node_set_resolving(node);
            if (rn->node->kind == SC_FUNC) {
                re = resolve_func(
                    r, body, (sc_func_base*)node, rn->continue_block);
            }
            else if (rn->node->kind == EXPR_BLOCK) {
                re = resolve_expr_block(r, (expr_block*)node, body, NULL, NULL);
            }
            else if (rn->node->kind == STMT_PASTE_EVALUATION) {
                bool end_reachable;
                re = resolve_expr_body(
                    r, body, node, &((paste_evaluation*)node)->body,
                    &end_reachable);
            }
            else if (rn->node->kind == EXPR_PP) {
                rn->pending_pastes = false;
                rn->continue_block = NULL;
                re = resolve_expr_pp(r, body, (expr_pp*)node, true, NULL, NULL);
            }
            else {
                panic("compiler bug");
            }
            re = handle_resolve_error(r, node, body, re, NULL, NULL);
            if (re) {
                if (re == RE_UNREALIZED_COMPTIME) {
                    *progress = true;
                }
                else if (re == RE_UNKNOWN_SYMBOL) {
                    continue;
                }
                else {
                    return re;
                }
            }
            continue;
        }
        assert(r->curr_pp_node == NULL);
        // can't be the mdgn because of "declaring" st node
        assert(rn->node->kind != ELEM_MDG_NODE);
        ast_node* astn = rn->node;
        if (astn->kind == EXPR_PP) {
            // so we can set from_pprnlist to true
            re = resolve_expr_pp(
                r, rn->declaring_body, (expr_pp*)astn, true, NULL, NULL);
        }
        else {
            re = resolve_ast_node(r, astn, rn->declaring_body, NULL, NULL);
        }
        if (re == RE_UNREALIZED_COMPTIME || re == RE_UNKNOWN_SYMBOL) {
            *progress = true;
            continue;
        }
        if (re) return re;
        // these have their own list
        assert(!ast_elem_is_any_import((ast_elem*)astn));
        // otherwise we would have gotten an error?
        assert(ast_node_get_resolved(astn));
        *progress = true;
    }
    return RE_OK;
}
resolve_error resolver_run_pp_resolve_nodes(resolver* r, bool* made_progress)
{
    llvm_error lle;
    resolve_error re;
    bool progress = false;
    do {
        if (progress) {
            if (made_progress) *made_progress = true;
            progress = false;
        }
        // we try to resolve pending nodes again
        re = handle_pending_pprns(r, &progress);
        if (re) return re;
        if (!progress && r->committed_waiters != 0) {
            pli it = pli_rbegin(&r->import_module_data_nodes);
            bool awaiting = false;
            for (ast_node* n = pli_prev(&it); n; n = pli_prev(&it)) {
                import_module_data* im_data = (import_module_data*)n;
                if (im_data->pprn) {
                    if (ast_node_get_used_in_pp(im_data->pprn->node)) {
                        awaiting = true;
                        if (atomic_boolean_load(&im_data->done)) {
                            re = pp_resolve_node_ready(r, im_data->pprn);
                            if (re) return re;
                            progress = true;
                        }
                    }
                }
                if (!im_data->pprn) {
                    progress = true;
                    ptrlist_remove_next(&r->import_module_data_nodes, &it);
                }
            }
            if (!progress && awaiting) {
                // hack: busy wait for now until we have proper suspend
                // resume progress = true;
                return resolver_suspend(r);
            }
        }
        if (!ptrlist_is_empty(&r->pp_resolve_nodes_ready)) {
            print_pprns(r, "running ", true);
            ureg priv_count = r->id_space - PRIV_SYMBOL_OFFSET;
            llvm_backend_reserve_symbols(
                r->backend, priv_count, atomic_ureg_load(&r->tc->t->node_ids));
            if (!r->deps_required_for_pp) {
                r->deps_required_for_pp = true;
                for (mdg_node** n = r->mdgs_begin; n != r->mdgs_end; n++) {
                    int res = mdg_node_require_requirements(*n, r->tc, true);
                    if (res) return RE_FATAL;
                }
            }
            lle = llvm_backend_run_pp(
                r->backend, priv_count, &r->pp_resolve_nodes_ready);
            if (lle) return RE_FATAL;
            while (!ptrlist_is_empty(&r->pp_resolve_nodes_ready)) {
                pp_resolve_node* rn =
                    ptrlist_pop_back(&r->pp_resolve_nodes_ready);
                rn->pprn_list_entry = NULL;
                pp_resolve_node_done(r, rn, &progress);
            }
            if (made_progress) *made_progress = true;
        }
    } while (progress);
    return RE_OK;
}
resolve_error resolver_handle_post_pp(resolver* r)
{
    resolve_error re;
    re = resolver_run_pp_resolve_nodes(r, NULL);
    if (re) return re;
    for (mdg_node** i = r->mdgs_begin; i != r->mdgs_end; i++) {
        aseglist_iterator asi;
        aseglist_iterator_begin(&asi, &(**i).module_frames);
        for (module_frame* mf = aseglist_iterator_next(&asi); mf != NULL;
             mf = aseglist_iterator_next(&asi)) {
            re = resolve_module_frame(r, mf, &mf->body);
            if (re) return re;
            ast_node_set_resolved(&mf->node);
        }
    }
    re = resolver_run_pp_resolve_nodes(r, NULL);
    if (re) return re;
    return report_cyclic_pp_deps(r);
}

void free_pprnlist(resolver* r, sbuffer* buff, bool error_occured)
{
    while (true) {
        if (sbuffer_is_empty(buff)) break;
        pp_resolve_node** rn = sbuffer_back(buff, sizeof(pp_resolve_node*));
        if ((**rn).pprn_list_entry) {
            (**rn).pprn_list_entry = NULL;
        }
        sbuffer_remove_back(buff, sizeof(pp_resolve_node*));
        pprn_fin(r, *rn, error_occured);
    }
    if (buff != &r->pp_resolve_nodes_waiting) {
        sbuffer_clear(buff);
    }
    else {
        assert(sbuffer_is_empty(buff));
    }
}
void free_pprns(resolver* r, bool error_occured)
{
    pli it = pli_rbegin(&r->import_module_data_nodes);
    for (ast_node* n = pli_prev(&it); n; n = pli_prev(&it)) {
        import_module_data* im_data = (import_module_data*)n;
        if (im_data->pprn) {
            pprn_fin(r, im_data->pprn, error_occured);
        }
    }
    free_pprnlist(r, &r->pp_resolve_nodes_pending, error_occured);
    free_pprnlist(r, &r->pp_resolve_nodes_ready, error_occured);
    free_pprnlist(r, &r->pp_resolve_nodes_waiting, error_occured);
#if DEBUG
    assert(r->pp_resolve_nodes.alloc_count == 0);
#endif
    freelist_clear(&r->pp_resolve_nodes);
    pool_clear(&r->pprn_mem);
}
void resolver_reset_resolution_state(resolver* r)
{
    r->generic_context = false;
    r->curr_pp_node = NULL;
    r->retracing_type_loop = false;
    r->module_group_constructor = NULL;
    r->module_group_destructor = NULL;
}
static inline resolve_error mark_mf_pp_done(resolver* r)
{
    for (mdg_node** i = r->mdgs_begin; i != r->mdgs_end; i++) {
        aseglist_iterator asi;
        aseglist_iterator_begin(&asi, &(**i).module_frames);
        ast_node_set_pp_done((ast_node*)*i);
        if (ppdct_seal_body(&r->ppdct, &(**i).body)) return RE_FATAL;
        for (module_frame* mf = aseglist_iterator_next(&asi); mf != NULL;
             mf = aseglist_iterator_next(&asi)) {
            ast_node_set_pp_done((ast_node*)mf);
            if (ppdct_seal_body(&r->ppdct, &mf->body)) return RE_FATAL;
        }
    }
    return RE_OK;
}
static inline resolve_error resolver_resolve_raw(resolver* r)
{
    resolver_reset_resolution_state(r);
    resolve_error re;
    if (!r->resumed) {
        re = resolver_init_mdg_symtabs_and_handle_root(r);
        if (re) return re;
        re = resolver_add_mf_decls(r);
        if (re) return re;
    }
    re = add_mf_trait_decls(r);
    if (re) return re;
    re = resolver_run_pp_resolve_nodes(r, NULL);
    if (re) return re;
    re = mark_mf_pp_done(r);
    if (re) return re;
    re = resolver_handle_post_pp(r);
    if (re) return re;
    assert(r->committed_waiters == 0);
    return RE_OK;
}
resolve_error resolver_finalize_resolution(resolver* r)
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
    if (!r->error_occured) {
        ureg glob_id_head = glob_id_start;
        for (mdg_node** n = r->mdgs_begin; n != r->mdgs_end; n++) {
            aseglist_iterator it;
            aseglist_iterator_begin(&it, &(**n).module_frames);
            for (module_frame* mf = aseglist_iterator_next(&it); mf;
                 mf = aseglist_iterator_next(&it)) {
                adjust_body_ids(r, &glob_id_head, &mf->body);
            }
        }
        assert(glob_id_head - glob_id_start == r->public_sym_count);
        r->glob_id_start = glob_id_start;
    }
    int res =
        mdg_nodes_resolved(r->mdgs_begin, r->mdgs_end, r->tc, r->error_occured);
    if (res) return RE_FATAL;
    if (r->error_occured) tauc_error_occured(r->tc->t, ERR);
    return RE_OK;
}
resolve_error resolver_resolve(resolver* r)
{
    resolve_error re = resolver_resolve_raw(r);
    if (re == RE_SUSPENDED) return re;
    if (re == RE_FATAL) r->error_occured = true;
    free_pprns(r, re != RE_OK);
    if (re && r->tc->t->trap_on_error) debugbreak();
    resolve_error re2 = resolver_finalize_resolution(r);
    if (re2) return re2;
    return re;
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
        r, &mod->body, NULL, false, &mod->body, name, result_1, result_2);
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
                r, &mf->body, NULL, false, &mf->body, name, result_1, result_2);
            if (re) return re;
            // since we check multiple frames we might hit the same main
            // twice
            if (ambiguity == res) ambiguity = ambiguity2;
            if (ambiguity) break;
        }
    }
    if (ambiguity) {
        src_range_large srl1, srl2;
        ast_node_get_src_range((ast_node*)res, res->declaring_body, &srl1);
        ast_node_get_src_range(
            (ast_node*)ambiguity, ambiguity->declaring_body, &srl2);
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
    // gen entrypoint in case if we are the root module
    mdg_node* root = r->tc->t->mdg.root_node;
    symbol* mainfn = NULL;
    symbol* startfn = NULL;
    if (*r->mdgs_begin == root) {
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
    }
    if (!tauc_success_so_far(r->tc->t) && r->tc->t->needs_emit_stage) {
        *module = NULL;
        return OK;
    }
    llvm_error lle;
    int res = llvm_backend_init_module(
        r->backend, r->mdgs_begin, r->mdgs_end, module);
    if (res) return res;
    if (*r->mdgs_begin == root) {
        lle = llvm_backend_generate_entrypoint(
            r->backend, (sc_func*)mainfn, (sc_func*)startfn,
            &r->tc->t->module_ctors, &r->tc->t->module_dtors, r->glob_id_start,
            r->glob_id_start + r->public_sym_count,
            r->private_sym_count + r->public_sym_count);
        if (lle) return RE_ERROR;
    }
    lle = llvm_backend_emit_module(
        r->backend, r->glob_id_start, r->glob_id_start + r->public_sym_count,
        r->private_sym_count + r->public_sym_count);
    if (lle == LLE_FATAL) return RE_FATAL;
    if (lle) return RE_ERROR;
    return OK;
}
resolve_error
resolver_resolve_and_emit_post_setup(resolver* r, llvm_module** module)
{
    ureg curr_max_glob_id = atomic_ureg_load(&r->tc->t->node_ids);
    llvm_error lle =
        llvm_backend_reserve_symbols(r->backend, 0, curr_max_glob_id);
    if (lle) return RE_ERROR;
    resolve_error re = RE_OK;
    TAU_TIME_STAGE_CTX(r->tc->t, re = resolver_resolve(r), {
        const char* msg;
        if (!re) {
            msg = "resolving";
        }
        else if (re == RE_SUSPENDED) {
            msg = "suspended resolving";
        }
        else {
            msg = "failed resolving";
        }
        print_debug_info(r, msg);
    });
    if (re) return re;
    prp_error pre;
    TAU_TIME_STAGE_CTX(
        r->tc->t, pre = prp_run_modules(&r->prp, r->mdgs_begin, r->mdgs_end);
        , print_debug_info(r, "running prp for"));
    if (pre) return ERR;
    if (resolver_resolving_root(r)) {
        if (tauc_request_finalize(r->tc->t)) return ERR;
    }
    if (r->error_occured) return RE_OK;
    return resolver_emit(r, module);
}
void resolver_setup_blank_resolve(resolver* r)
{
    // reserve global symbol slots so that the pprn doesn't overrun the
    // global ids of other modules it might use
    r->public_sym_count = 0;
    r->private_sym_count = 0;
    r->id_space = PRIV_SYMBOL_OFFSET;
    r->deps_required_for_pp = false;
    r->committed_waiters = 0;
    r->error_occured = false;
}
// this can't return an error since we also use it in an aborted suspend
void resolver_unpack_partial_resolution_data(
    resolver* r, partial_resolution_data* prd)
{
    // this undoes the dummy allocation of this partial_resolution_data
    // freeing the space for more pprns. since no allocs come
    // during the run of this methods the memory stays valid long enough
    pool_undo_last_alloc(&prd->pprn_mem, sizeof(partial_resolution_data));
    pool_steal_all(&r->pprn_mem, &prd->pprn_mem);
    pool_fin(&prd->pprn_mem);
    sbuffer_take_and_invalidate(
        &r->pp_resolve_nodes_pending, &prd->pprns_pending);
    sbuffer_take_and_invalidate(
        &r->pp_resolve_nodes_waiting, &prd->pprns_waiting);
    sbuffer_take_and_invalidate(
        &r->import_module_data_nodes, &prd->import_module_data_nodes);
#if DEBUG
    assert(r->pp_resolve_nodes.alloc_count == 0);
    r->pp_resolve_nodes.alloc_count = prd->pprn_count;
#endif
    r->id_space = prd->id_space;
    r->error_occured = prd->error_occured;
    r->deps_required_for_pp = prd->deps_required_for_pp;
    r->committed_waiters = prd->committed_waiters;
    r->public_sym_count = prd->public_sym_count;
    r->private_sym_count = prd->private_sym_count;
}
resolve_error resolver_resolve_and_emit(
    resolver* r, mdg_node** start, mdg_node** end, partial_resolution_data* prd,
    llvm_module** module)
{
    r->mdgs_begin = start;
    r->mdgs_end = end;
    if (prd) {
        resolver_unpack_partial_resolution_data(r, prd);
    }
    else {
        resolver_setup_blank_resolve(r);
    }
    r->resumed = (prd != NULL);
    return resolver_resolve_and_emit_post_setup(r, module);
}
resolve_error resolver_suspend(resolver* r)
{
    // this alloc will be undone by the continue
    partial_resolution_data* p =
        pool_alloc(&r->pprn_mem, sizeof(partial_resolution_data));
    int res = sbuffer_steal_used(
        &p->pprns_pending, &r->pp_resolve_nodes_pending, false);
    if (res) {
        pool_undo_last_alloc(&r->pprn_mem, sizeof(partial_resolution_data));
        return RE_FATAL;
    }
    res = sbuffer_steal_used(
        &p->pprns_waiting, &r->pp_resolve_nodes_waiting, false);
    if (res) {
        sbuffer_take_and_invalidate(
            &r->pp_resolve_nodes_pending, &p->pprns_pending);
        pool_undo_last_alloc(&r->pprn_mem, sizeof(partial_resolution_data));
        return RE_FATAL;
    }
    res = sbuffer_steal_used(
        &p->import_module_data_nodes, &r->import_module_data_nodes, false);
    if (res) {
        sbuffer_take_and_invalidate(
            &r->pp_resolve_nodes_waiting, &p->pprns_waiting);
        sbuffer_take_and_invalidate(
            &r->pp_resolve_nodes_pending, &p->pprns_pending);
        pool_undo_last_alloc(&r->pprn_mem, sizeof(partial_resolution_data));
        return RE_FATAL;
    }
    pool_init_dummy(&p->pprn_mem);
    pool_steal_used(&p->pprn_mem, &r->pprn_mem);
    assert(sbuffer_get_used_size(&r->pp_resolve_nodes_ready) == 0);
    ptrlist_clear(&r->pp_resolve_nodes_pending);
    ptrlist_clear(&r->pp_resolve_nodes_waiting);
#if DEBUG
    p->pprn_count = r->pp_resolve_nodes.alloc_count;
#endif
    freelist_clear(&r->pp_resolve_nodes);
    p->deps_required_for_pp = r->deps_required_for_pp;
    p->error_occured = r->error_occured;
    p->id_space = r->id_space;
    p->committed_waiters = r->committed_waiters;
    p->private_sym_count = r->private_sym_count;
    p->public_sym_count = r->public_sym_count;
    // put partial res data in scc. no need for a lock since nobody accesses
    // this while we are in the resolving stage
    assert(!(**r->mdgs_begin).partial_res_data);
    (**r->mdgs_begin).partial_res_data = p;
    // 'unblock' the mdg in the scc and decrement the dummy ungenerated pps
    // since we can't do all at once we need to check afterwards
    // if everybody is done already (using scc to avoid double fire)
    for (mdg_node** n = r->mdgs_begin; n != r->mdgs_end; n++) {
        atomic_ureg_dec(&(**n).ungenerated_pp_deps);
        // we can do this unrwlock'ed read since we are sure that whille
        // we are resolving nobody may change the state but us
        module_stage ms = ((**n).stage == MS_RESOLVING_EXPLORATION)
                              ? MS_AWAITING_PP_DEPENDENCIES_EXPLORATION
                              : MS_AWAITING_PP_DEPENDENCIES;
        rwlock_write(&(**n).lock);
        (**n).stage = ms;
        rwlock_end_write(&(**n).lock);
    }
    if (thread_context_preorder_job(r->tc)) {
        resolver_unpack_partial_resolution_data(r, p);
        return RE_FATAL;
    }
    // we might already be ready by this time but since the stages are
    // updated one after the other we need to do one final check
    if (sccd_run(&r->tc->sccd, *r->mdgs_begin, SCCD_CHECK_PP_DEPS_GENERATED)) {
        // this might be ready but unnoticed otherwise
        bool undo_required = false;
        rwlock_write(&(**r->mdgs_begin).lock);
        if ((**r->mdgs_begin).partial_res_data == p) {
            undo_required = true;
            (**r->mdgs_begin).partial_res_data = NULL;
        }
        rwlock_end_write(&(**r->mdgs_begin).lock);
        if (undo_required) {
            resolver_unpack_partial_resolution_data(r, p);
        }
        return RE_FATAL;
    }
    return RE_SUSPENDED;
}
int resolver_partial_fin(resolver* r, int i, int res)
{
    switch (i) {
        case -1:
        case 12: llvm_backend_delete(r->backend); // fallthrough
        case 11: ppdct_fin(&r->ppdct); // fallthrough
        case 10: ptr_map_fin(&r->pm); // fallthrough
        case 9: prp_fin(&r->prp); // fallthrough
        case 8: ptrlist_fin(&r->import_module_data_nodes); // fallthrough
        case 7: ptrlist_fin(&r->pp_resolve_nodes_ready); // fallthrough
        case 6: ptrlist_fin(&r->pp_resolve_nodes_pending); // fallthrough
        case 5: ptrlist_fin(&r->pp_resolve_nodes_waiting); // fallthrough
        case 4: freelist_fin(&r->pp_resolve_nodes); // fallthrough
        case 3: pool_fin(&r->pprn_mem); // fallthrough
        case 2: sbuffer_fin(&r->temp_buffer); // fallthrough
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
    e = sbuffer_init(&r->temp_buffer, sizeof(ast_node*) * 32);
    if (e) return resolver_partial_fin(r, 1, e);
    e = pool_init(&r->pprn_mem);
    if (e) return resolver_partial_fin(r, 2, e);
    e = freelist_init(
        &r->pp_resolve_nodes, &r->pprn_mem, sizeof(pp_resolve_node));
    if (e) return resolver_partial_fin(r, 3, e);
    e = ptrlist_init(&r->pp_resolve_nodes_waiting, 16);
    if (e) return resolver_partial_fin(r, 4, e);
    e = ptrlist_init(&r->pp_resolve_nodes_pending, 16);
    if (e) return resolver_partial_fin(r, 5, e);
    e = ptrlist_init(&r->pp_resolve_nodes_ready, 16);
    if (e) return resolver_partial_fin(r, 6, e);
    e = ptrlist_init(&r->import_module_data_nodes, 16);
    if (e) return resolver_partial_fin(r, 7, e);
    e = prp_init(&r->prp, r->tc);
    if (e) return resolver_partial_fin(r, 8, e);
    e = ptr_map_init(&r->pm, &r->tc->t->gpm);
    if (e) return resolver_partial_fin(r, 9, e);
    e = ppdct_init(&r->ppdct, r);
    if (e) return resolver_partial_fin(r, 10, e);
    r->backend = llvm_backend_new(r->tc);
    if (!r->backend) return resolver_partial_fin(r, 11, ERR);
    r->allow_type_loops = false;
    r->type_loop_start = NULL;
    return OK;
}
ast_elem* get_resolved_ast_node_ctype(ast_node* n)
{
    // we could optimize this but it's currently not worth it
    ast_elem* ctype;
    resolve_ast_node_raw(NULL, n, NULL, NULL, &ctype);
    return ctype;
}
bool ast_body_pastes_done(resolver* r, ast_body* b)
{
    if (!ast_elem_has_unordered_body((ast_elem*)b->owning_node)) return true;
    return ast_node_get_pp_done(b->owning_node);
}
