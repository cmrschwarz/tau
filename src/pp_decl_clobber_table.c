#include "pp_decl_clobber_table.h"
#include "utils/fnv_hash.h"
#include "utils/error.h"
#include "utils/zero.h"
#include "resolver.h"
#include "thread_context.h"
ast_body* TOMBSTONE = (ast_body*)&NULL_BYTES;
int ppdct_init(pp_decl_clobber_table* t, resolver* r)
{
    int res = freelist_init(
        &t->waiting_users_mem, &r->tc->permmem, sizeof(ppdct_waiting_users));
    if (res) return ERR;
    t->clobber_count = 0;
    ureg is = plattform_get_page_size();
    ureg rem = is % sizeof(pp_decl_clobber);
    if (rem) is = is * rem;
    t->table = tmallocz(is);
    if (!t->table) {
        freelist_fin(&t->waiting_users_mem);
        return ERR;
    }
    ureg cap = is / sizeof(pp_decl_clobber);
    t->hash_bits = ulog2(cap);
    t->hash_mask = cap - 1;
    t->max_fill = cap - (cap >> 2); // 75%
    t->r = r;
    return OK;
}
void ppdct_fin(pp_decl_clobber_table* t)
{
    pp_decl_clobber* end = t->table + t->hash_mask + 1;
    for (pp_decl_clobber* i = t->table; i != end; i++) {
        if (!i->body || i->parent_sym || !i->name) continue;
        list_fin(&i->waiting_users->waiting_users, true);
        freelist_free(&t->waiting_users_mem, i->waiting_users);
    }
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
                if (cstr_eq(ppdc->name, name)) return ppdc;
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
    ureg cap_new = (t->hash_mask + 1) << 2;
    pp_decl_clobber* tn = tmallocz(cap_new * sizeof(pp_decl_clobber));
    if (!tn) return ERR;
    pp_decl_clobber* i = t->table;
    pp_decl_clobber* end = t->table + cap_new;
    t->table = tn;
    t->hash_bits++;
    t->hash_mask = cap_new - 1;
    t->max_fill = cap_new - (cap_new >> 2);
    while (i != end) {
        if (i->body && i->body != TOMBSTONE) {
            *ppdct_lookup_raw(
                t, i->body, i->associated_type, i->name,
                ppdct_prehash_name(i->name)) = *i;
        }
        i++;
    }
    return OK;
}
void ppdct_remove(pp_decl_clobber_table* t, pp_decl_clobber* ppdc)
{
    pp_decl_clobber* next =
        (ppdc == t->table + t->hash_mask) ? t->table : ppdc + 1;
    if (!next->body && next->body != TOMBSTONE) {
        ppdc->body = NULL;
        t->clobber_count--;
    }
    else {
        ppdc->body = TOMBSTONE;
    }
}

resolve_error
notify_users(pp_decl_clobber_table* t, ppdct_waiting_users* wu, bool found)
{
    wu->refcount--;
    bool rc0 = (wu->refcount == 0);
    list_it it;
    list_it_begin(&it, &wu->waiting_users);
    void* v;
    resolve_error re;
    resolve_error re1 = RE_OK;
    while ((v = list_it_next(&it, &wu->waiting_users))) {
        if ((ureg)v & (ureg)1) {
            if (found || rc0) {
                re = pp_resolve_node_dep_done(
                    t->r, (pp_resolve_node*)((ureg)v ^ (ureg)1), NULL);
            }
        }
        else {
            re = notify_users(t, (ppdct_waiting_users*)v, found);
        }
        re1 = add_resolve_error(re1, re);
    }
    if (found || rc0) list_fin(&wu->waiting_users, true);
    if (found && !rc0) {
        int r = list_init(&wu->waiting_users);
        if (r) re1 = RE_FATAL;
    }
    if (rc0) {
        freelist_free(&t->waiting_users_mem, wu);
    }
    return re1;
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
    if (ppdc->parent_sym) {
        ast_node_set_poisoned(&s->node);
        src_range_large srl_parent_sym;
        src_range_large srl_shadowing_decl;
        src_range_large srl_parent_use;
        ast_node_get_src_range(
            (ast_node*)ppdc->parent_sym, ppdc->parent_sym->declaring_body,
            &srl_parent_sym);
        ast_node_get_src_range(
            (ast_node*)s, s->declaring_body, &srl_shadowing_decl);
        ast_node_get_src_range(
            (ast_node*)ppdc->parent_use.user, ppdc->parent_use.user_body,
            &srl_parent_use);
        error_log_report_annotated_thrice(
            t->r->tc->err_log, ES_RESOLVER, false,
            "cannot shadow previously used symbol during the preprocessor",
            srl_shadowing_decl.smap, srl_shadowing_decl.start,
            srl_shadowing_decl.end, "illegal shadowing here",
            srl_parent_sym.smap, srl_parent_sym.start, srl_parent_sym.end,
            "symbol that would be shadowed", srl_parent_use.smap,
            srl_parent_use.start, srl_parent_use.end,
            "use of the to be shadowed symbol here");
        t->r->error_occured = true;
        if (curr_body_propagate_error(t->r, target_body)) return ERR;
        ppdct_remove(t, ppdc);
        return RE_OK;
    }
    assert(ppdc->waiting_users);
    return notify_users(t, ppdc->waiting_users, true);
}
static inline pp_decl_clobber*
ppdct_get_scope_list(pp_decl_clobber_table* t, ast_body* body)
{
    return ppdct_lookup_raw(
        t, body, NULL, NULL, fnv_hash_pointer(FNV_START_HASH, body));
}
static inline int ppdct_block_symbol(
    pp_decl_clobber_table* t, ast_body* lower_body, ast_body* upper_body,
    ast_elem* associtated_type, const char* name, symbol* parent_sym,
    ast_node* user, pp_resolve_node* dependency, bool* notifier_added)
{
    ureg name_hash = ppdct_prehash_name(name);
    bool continue_up = true;
    ppdct_waiting_users* lower_waiting_users = NULL;
    void* dep_mangled = (void*)((ureg)dependency | 1);
    for (ast_body* b = lower_body; continue_up; b = b->parent) {
        b = ast_body_get_non_paste_parent(b);
        if (b == upper_body) break;
        if (b->owning_node->kind == ELEM_MDG_NODE) continue_up = false;
        if (ast_body_is_pp_done(t->r, b)) continue;
        pp_decl_clobber* ppdc =
            ppdct_lookup_raw(t, b, associtated_type, name, name_hash);
        if (ppdc->body && ppdc->body != TOMBSTONE) {
            if (parent_sym) {
                // we are using that parent, so it exists
                assert(ppdc->parent_sym);
                assert(!notifier_added);
                // somebody else already added a clobber. good for us
                continue;
            }
        }
        else {
            // we have to add a clobber. resize if needed
            if (t->clobber_count == t->max_fill) {
                int res = ppdct_grow(t);
                if (res) return ERR;
                ppdc =
                    ppdct_lookup_raw(t, b, associtated_type, name, name_hash);
                assert(!ppdc->body);
            }
            if (parent_sym) {
                ppdc->parent_use.user = user;
                ppdc->parent_use.user_body = lower_body;
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
                wu->refcount = 1;
            }
            ppdc->body = b;
            ppdc->associated_type = associtated_type;
            ppdc->name = name;
            ppdc->parent_sym = parent_sym;
        }
        if (!parent_sym) {
            int r;
            if (lower_waiting_users) {
                r = list_append(
                    &ppdc->waiting_users->waiting_users, NULL,
                    lower_waiting_users);
                lower_waiting_users->refcount++;
            }
            else {
                lower_waiting_users = ppdc->waiting_users;
                r = list_append(
                    &ppdc->waiting_users->waiting_users, NULL, dep_mangled);
            }
            if (r) return r;
            assert(notifier_added);
            *notifier_added = true;
        }
        pp_decl_clobber* scope_list = ppdct_get_scope_list(t, b);
        if (!scope_list->body || scope_list->body == TOMBSTONE) {
            scope_list->body = b;
            scope_list->name = NULL;
            scope_list->parent_sym = NULL;
            scope_list->prev = ppdc;
            ppdc->prev = NULL;
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
    assert(body->pprn == NULL);
    pp_decl_clobber* scope_list = ppdct_get_scope_list(t, body);
    if (!scope_list->body) return OK;
    resolve_error re;
    resolve_error re1 = RE_OK;
    for (pp_decl_clobber* ppdc = scope_list->prev; ppdc; ppdc = ppdc->prev) {
        if (ppdc->parent_sym) {
            ppdct_remove(t, ppdc);
            continue;
        }
        re = notify_users(t, ppdc->waiting_users, false);
        re1 = add_resolve_error(re1, re);
    }
    return re;
}