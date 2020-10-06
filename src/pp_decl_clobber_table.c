#include "pp_decl_clobber_table.h"
#include "utils/fnv_hash.h"
#include "utils/error.h"
#include "utils/zero.h"
#include "resolver.h"
#include "utils/debug_utils.h"
#include "thread_context.h"
ast_body* TOMBSTONE = (ast_body*)&NULL_BYTES;
int ppdct_init(pp_decl_clobber_table* t, resolver* r)
{
    int res = freelist_init(
        &t->waiting_users_mem, &r->tc->permmem, sizeof(ppdct_waiting_users));
    if (res) return ERR;
    t->clobber_count = 0;
    t->hash_bits = 4;
    ureg cap = 1 << t->hash_bits;
    t->table = tmallocz(cap * sizeof(pp_decl_clobber));
    if (!t->table) {
        freelist_fin(&t->waiting_users_mem);
        return ERR;
    }
    t->hash_mask = cap - 1;
    t->max_fill = cap - (cap >> 2); // 75%
    t->r = r;
    return OK;
}
void ppdct_remove(pp_decl_clobber_table* t, pp_decl_clobber* ppdc)
{
    pp_decl_clobber* next =
        (ppdc == t->table + t->hash_mask) ? t->table : ppdc + 1;
    if (!next->body && next->body != TOMBSTONE) {
        ppdc->body = NULL;
    }
    else {
        ppdc->body = TOMBSTONE;
    }
    t->clobber_count--;
}
resolve_error
waiting_users_sym_found(pp_decl_clobber_table* t, ppdct_waiting_users* wu_node)
{
    sbuffer* waiting_users = &t->r->tc->temp_buffer;
    sbuffer* pprns = &t->r->temp_buffer;
    sbuffer_iterator wu_begin = sbuffer_iterator_begin_at_end(waiting_users);
    sbuffer_iterator pprns_begin = sbuffer_iterator_begin_at_end(pprns);
    sbuffer_iterator wu_it = wu_begin;
    list_it it;
    void* v;
    ppdct_waiting_users* wu = wu_node;
    ppdct_waiting_users** wup;
    resolve_error re = RE_OK;
    void** append_pos;
    // collect and separate all wus and pprns
    while (true) {
        list_it_begin(&it, &wu->waiting_users);
        while ((v = list_it_next(&it, &wu->waiting_users))) {
            if ((ureg)v & (ureg)1) {
                v = (void*)((ureg)v ^ (ureg)1);
                append_pos = (void**)sbuffer_append(pprns, sizeof(void*));
                list_remove_swap(&wu->waiting_users, &it);
            }
            else {
                append_pos =
                    (void**)sbuffer_append(waiting_users, sizeof(void*));
            }
            if (!append_pos) {
                re = RE_FATAL;
                break;
            }
            *append_pos = v;
        }
        if (re) break;
        wup = sbuffer_iterator_next(&wu_it, sizeof(void*));
        if (!wup) break;
        wu = *wup;
    }
    if (re) {
        sbuffer_set_end(waiting_users, &wu_begin);
        sbuffer_set_end(pprns, &pprns_begin);
        return re;
    }
    sbuffer_iterator pp_it = pprns_begin;
    while (true) {
        pp_resolve_node** ppp =
            (pp_resolve_node**)sbuffer_iterator_next(&pp_it, sizeof(void*));
        if (!ppp) break;
        resolve_error re2 = pp_resolve_node_dep_done(t->r, *ppp, NULL);
        re = add_resolve_error(re, re2);
    }
    if (re) {
        sbuffer_set_end(waiting_users, &wu_begin);
        sbuffer_set_end(pprns, &pprns_begin);
        return re;
    }
    wu_it = sbuffer_iterator_begin_at_end(waiting_users);
    bool root_alive;
    while (true) {
        if (wu_it.pos == wu_begin.pos) {
            wu = wu_node;
        }
        else {
            wup = sbuffer_iterator_previous(&wu_it, sizeof(void*));
            assert(wup);
            wu = *wup;
        }
        assert(wu->has_parent || wu == wu_node);
        list_it_begin(&it, &wu->waiting_users);
        bool alive = false;
        while ((v = list_it_next(&it, &wu->waiting_users))) {
            if ((ureg)v & (ureg)1) {
                alive = true;
            }
            else {
                ppdct_waiting_users* child = (ppdct_waiting_users*)v;
                if (child->has_parent) {
                    alive = true;
                }
                else {
                    list_remove_swap(&wu->waiting_users, &it);
                }
            }
        }
        if (wu == wu_node) {
            root_alive = alive;
            break;
        }
        if (!alive) {
            wu->has_parent = false;
        }
    }
    wu_it = sbuffer_iterator_begin_at_end(waiting_users);
    wu = NULL;
    while (true) {
        if (wu_it.pos == wu_begin.pos) {
            if (wu == wu_node || root_alive) break;
            wu = wu_node;
        }
        else {
            wup = sbuffer_iterator_previous(&wu_it, sizeof(void*));
            assert(wup);
            wu = *wup;
        }
        if (!wu->has_parent && !wu->owner) {
            list_fin(&wu->waiting_users, true);
            freelist_free(&t->waiting_users_mem, wu);
        }
    }
    sbuffer_set_end(waiting_users, &wu_begin);
    sbuffer_set_end(pprns, &pprns_begin);
    return RE_OK;
}
resolve_error
waiting_users_scope_done(pp_decl_clobber_table* t, ppdct_waiting_users* wu)
{
    assert(wu->owner);
    if (wu->has_parent) return RE_OK;
    resolve_error re = RE_OK;
    list_it it;
    void* v;
    list_it_begin(&it, &wu->waiting_users);

    stack* s = &t->r->tc->temp_stack;
    stack_iter s_begin;
    stack_iter_begin(&s_begin, s);
    while (true) {
        while ((v = list_it_next(&it, &wu->waiting_users))) {
            if ((ureg)v & (ureg)1) {
                resolve_error re2 = pp_resolve_node_dep_done(
                    t->r, (pp_resolve_node*)((ureg)v ^ (ureg)1), NULL);
                re = add_resolve_error(re, re2);
            }
            else {
                ppdct_waiting_users* child = (ppdct_waiting_users*)v;
                child->has_parent = false;
                if (!child->owner) {
                    if (stack_push(s, child)) {
                        re = RE_FATAL;
                        break;
                    }
                }
            }
        }
        if (re == RE_FATAL) break;
        list_fin(&wu->waiting_users, true);
        freelist_free(&t->waiting_users_mem, wu);
        if (s->head == s_begin.head) break;
        wu = stack_pop(s);
    }
    stack_set_end(s, &s_begin);
    return re;
}
void ppdct_fin(pp_decl_clobber_table* t)
{
    pp_decl_clobber* end = t->table + t->hash_mask + 1;
    for (pp_decl_clobber* i = t->table; i != end; i++) {
        if (!i->body || i->body == TOMBSTONE) continue;
        t->clobber_count--;
        if (i->conflicting_symbol || !i->name) continue;
        list_fin(&i->waiting_users->waiting_users, true);
        freelist_free(&t->waiting_users_mem, i->waiting_users);
    }
    assert(t->clobber_count == 0);
    tfree(t->table);
    freelist_fin(&t->waiting_users_mem);
}

ureg ppdct_prehash_name(const char* name)
{
    return fnv_hash_str(FNV_START_HASH, name);
}

static inline pp_decl_clobber* ppdct_lookup_raw(
    pp_decl_clobber_table* t, ast_body* body, ast_elem* associtated_type,
    const char* name, ureg name_prehash)
{
    ureg hash =
        fnv_hash_ureg(name_prehash, (ureg)body ^ (ureg)associtated_type);
    ureg idx = fnv_fold(hash, t->hash_bits, t->hash_mask);
    pp_decl_clobber* last = t->table + t->hash_mask;
    pp_decl_clobber* ppdc = &t->table[idx];
    while (true) {
        if (!ppdc->body || ppdc->body == TOMBSTONE) return ppdc;
        if (ppdc->body == body && ppdc->associated_type == associtated_type) {
            if (name) {
                if (ppdc->name) {
                    if (cstr_eq(ppdc->name, name)) return ppdc;
                }
            }
            else {
                if (!ppdc->name) return ppdc;
            }
        }
        if (ppdc == last) {
            ppdc = t->table;
            continue;
        }
        ppdc++;
    }
}

int ppdct_grow(pp_decl_clobber_table* t)
{
    // size times two but 0 becomes 2 :)
    ureg cap_new = (t->hash_mask + 1) << 1;
    pp_decl_clobber* tn = tmallocz(cap_new * sizeof(pp_decl_clobber));
    if (!tn) return ERR;
    pp_decl_clobber* t_old = t->table;
    pp_decl_clobber* end = t->table + cap_new;
    t->table = tn;
    t->hash_bits++;
    t->hash_mask = cap_new - 1;
    t->max_fill = cap_new - (cap_new >> 2);
    ureg elem_count = 0;
    UNUSED(elem_count);
    for (pp_decl_clobber* i = t_old; i != end; i++) {
        // we have to fixup the scope elem linked lists
        // so we skip past non scope lists
        // and handle all scope list elems once we hit the scope list
        if (i->body && i->body != TOMBSTONE && i->name == NULL) {
            pp_decl_clobber* old_ppdc = i;
            pp_decl_clobber* prev_new_ppdc = NULL;
            ureg name_prehash = FNV_START_HASH;
            while (true) {
                pp_decl_clobber* new_ppdc = ppdct_lookup_raw(
                    t, old_ppdc->body, old_ppdc->associated_type,
                    old_ppdc->name, name_prehash);
                *new_ppdc = *old_ppdc;
                if (!new_ppdc->conflicting_symbol) {
                    assert(new_ppdc->waiting_users->owner);
                    new_ppdc->waiting_users->owner = new_ppdc;
                }
                elem_count++;
                if (prev_new_ppdc) {
                    prev_new_ppdc->prev = new_ppdc;
                }
                old_ppdc = old_ppdc->prev;
                if (!old_ppdc) break;
                prev_new_ppdc = new_ppdc;
                name_prehash = ppdct_prehash_name(old_ppdc->name);
            }
        }
    }
    assert(elem_count == t->clobber_count);
    tfree(t_old);
    return OK;
}
int ppdct_add_symbol(
    pp_decl_clobber_table* t, symbol* s, ast_body* target_body,
    ast_elem* associtated_type)
{
    // we could reuse the hash from the symbol lookup, but meh
    ureg name_hash = ppdct_prehash_name(s->name);
    pp_decl_clobber* ppdc =
        ppdct_lookup_raw(t, target_body, associtated_type, s->name, name_hash);
    if (!ppdc->body || ppdc->body == TOMBSTONE) return RE_OK;
    if (ppdc->conflicting_symbol) {
        ast_node_set_poisoned(&s->node);
        src_range_large srl_conflicting_sym;
        src_range_large srl_new_decl;
        src_range_large srl_conflicting_use;
        ast_node_get_src_range(
            (ast_node*)ppdc->conflicting_symbol,
            ppdc->conflicting_symbol->declaring_body, &srl_conflicting_sym);
        ast_node_get_src_range((ast_node*)s, s->declaring_body, &srl_new_decl);
        ast_node_get_src_range(
            (ast_node*)ppdc->conflicting_use.user,
            ppdc->conflicting_use.user_body, &srl_conflicting_use);
        error_log_report_annotated_thrice(
            t->r->tc->err_log, ES_RESOLVER, false,
            "cannot shadow previously used symbol during the preprocessor",
            srl_new_decl.smap, srl_new_decl.start, srl_new_decl.end,
            "illegal shadowing here", srl_conflicting_sym.smap,
            srl_conflicting_sym.start, srl_conflicting_sym.end,
            "symbol that would be shadowed", srl_conflicting_use.smap,
            srl_conflicting_use.start, srl_conflicting_use.end,
            "use of the to be shadowed symbol here");
        t->r->error_occured = true;
        if (curr_body_propagate_error(t->r, target_body)) return ERR;
        return RE_OK;
    }
    else {
        ppdc->waiting_users->owner = NULL;
        ppdc->conflicting_symbol = s; // TODO: add user from waiter list
    }
    assert(ppdc->waiting_users);
    resolve_error re = waiting_users_sym_found(t, ppdc->waiting_users);
    return re;
}
static inline pp_decl_clobber*
ppdct_get_scope_list(pp_decl_clobber_table* t, ast_body* body)
{
    return ppdct_lookup_raw(
        t, body, NULL, NULL, fnv_hash_pointer(FNV_START_HASH, body));
}
static inline int ppdct_block_symbol(
    pp_decl_clobber_table* t, ast_body* lower_body, ast_body* upper_body,
    ast_elem* associtated_type, const char* name, symbol* conflicting_symbol,
    ast_node* user, pp_resolve_node* dependency, bool* notifier_added)
{
    ureg name_hash = ppdct_prehash_name(name);
    bool continue_up = true;
    ppdct_waiting_users* lower_waiting_users = NULL;
    for (ast_body* b = lower_body; continue_up; b = b->parent) {
        b = ast_body_get_non_paste_parent(b);
        if (b == upper_body) break;
        if (b->owning_node->kind == ELEM_MDG_NODE) continue_up = false;
        if (ast_body_pastes_done(t->r, b)) continue;
        pp_decl_clobber* ppdc =
            ppdct_lookup_raw(t, b, associtated_type, name, name_hash);
        if (ppdc->body && ppdc->body != TOMBSTONE) {
            if (conflicting_symbol) {
                // we are using that parent, so it exists
                if (!ppdc->conflicting_symbol) {
                    // the used symbol was pasted in a scope below
                    // all users here must have been satisfied,
                    // since we can no longer paste in this scope ore above
                    // since that would trigger the ambiguity error.
                    // therefore we can cut off the users here
                    ppdc->conflicting_symbol = conflicting_symbol;
                    ppdc->waiting_users->owner = NULL;
                    resolve_error re =
                        waiting_users_sym_found(t, ppdc->waiting_users);
                    if (re) return re;
                }
                assert(!notifier_added);
                // somebody else already added a clobber. good for us
                continue;
            }
            else {
                // the parent scope might already be handled
                continue_up = !ppdc->waiting_users->has_parent;
            }
        }
        else {
            // we have to add a clobber. resize if needed
            // we say count +1 to avoid having to grow for the scope list
            // which would leak this entry on error
            if (t->clobber_count + 1 >= t->max_fill) {
                int res = ppdct_grow(t);
                if (res) return ERR;
                ppdc =
                    ppdct_lookup_raw(t, b, associtated_type, name, name_hash);
                assert(!ppdc->body);
            }
            t->clobber_count++;
            if (conflicting_symbol) {
                ppdc->conflicting_use.user = user;
                ppdc->conflicting_use.user_body = lower_body;
                assert(!dependency);
            }
            else {
                ppdct_waiting_users* wu = freelist_alloc(&t->waiting_users_mem);
                int r = list_init(&wu->waiting_users);
                if (r) {
                    freelist_free(&t->waiting_users_mem, wu);
                    return r;
                }
                ppdc->waiting_users = wu;
                wu->owner = ppdc;
                wu->has_parent = false;
            }
            ppdc->body = b;
            ppdc->name = name;
            ppdc->associated_type = associtated_type;
            ppdc->conflicting_symbol = conflicting_symbol;
        }
        if (!conflicting_symbol) {
            assert(notifier_added);
            int r;
            if (lower_waiting_users) {
                r = list_append(
                    &ppdc->waiting_users->waiting_users, NULL,
                    lower_waiting_users);
                lower_waiting_users->has_parent = true;
            }
            else {
                if (!dependency) {
                    resolve_error re =
                        get_curr_pprn(t->r, lower_body, &dependency);
                    if (re) return re;
                }
                assert(!*notifier_added);
                dependency->dep_count++;
                void* dep_mangled = (void*)((ureg)dependency | 1);
                r = list_append(
                    &ppdc->waiting_users->waiting_users, NULL, dep_mangled);
            }
            if (r) return r;
            *notifier_added = true;
            lower_waiting_users = ppdc->waiting_users;
        }
        pp_decl_clobber* scope_list = ppdct_get_scope_list(t, b);
        if (!scope_list->body || scope_list->body == TOMBSTONE) {
            if (t->clobber_count == t->max_fill) {
                int res = ppdct_grow(t);
                if (res) return ERR;
                scope_list = ppdct_get_scope_list(t, b);
                assert(!scope_list->body || scope_list->body == TOMBSTONE);
            }
            scope_list->body = b;
            scope_list->name = NULL;
            scope_list->conflicting_symbol = NULL;
            scope_list->prev = ppdc;
            ppdc->prev = NULL;
            t->clobber_count++;
        }
        else {
            ppdc->prev = scope_list->prev;
            scope_list->prev = ppdc;
        }
    }
    return OK;
}
int ppdct_use_symbol(
    pp_decl_clobber_table* t, symbol* s, ast_body* user_body,
    ast_elem* associtated_type, ast_node* user)
{
    return ppdct_block_symbol(
        t, user_body, s->declaring_body, associtated_type, s->name, s, user,
        NULL, NULL);
}
int ppdct_curr_pprn_require_symbol(
    pp_decl_clobber_table* t, ast_body* body, ast_elem* associated_type,
    const char* name, bool* notifier_added)
{
    return ppdct_block_symbol(
        t, body, NULL, associated_type, name, NULL, NULL, NULL, notifier_added);
}
int ppdct_require_symbol(
    pp_decl_clobber_table* t, ast_body* body, ast_elem* associated_type,
    const char* name, pp_resolve_node* dep, bool* notifier_added)
{
    dep->dep_count++;
    return ppdct_block_symbol(
        t, body, NULL, associated_type, name, NULL, NULL, dep, notifier_added);
}
int ppdct_seal_body(pp_decl_clobber_table* t, ast_body* body)
{
    assert(ast_body_pastes_done(t->r, body));
    pp_decl_clobber* scope_list = ppdct_get_scope_list(t, body);
    if (!scope_list->body || scope_list->body == TOMBSTONE) return OK;
    resolve_error re;
    resolve_error re1 = RE_OK;
    pp_decl_clobber* ppdc = scope_list->prev;
    ppdct_remove(t, scope_list);
    while (ppdc != NULL) {
        assert(ppdc->body == body);
        if (!ppdc->conflicting_symbol) {
            assert(ppdc->name);
            re = waiting_users_scope_done(t, ppdc->waiting_users);
            re1 = add_resolve_error(re1, re);
        }
        ppdct_remove(t, ppdc);
        ppdc = ppdc->prev;
    }
    return re1;
}
