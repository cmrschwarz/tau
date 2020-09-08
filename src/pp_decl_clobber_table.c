#include "pp_decl_clobber_table.h"
#include "utils/fnv_hash.h"
#include "utils/error.h"
#include "utils/zero.h"
#include "resolver.h"
#include "thread_context.h"

int ppdct_init(pp_decl_clobber_table* t, resolver* r)
{
    t->clobber_count = 0;
    ureg is = plattform_get_page_size();
    is = is * (is % sizeof(pp_decl_clobber));
    t->table = tmallocz(is);
    if (!t->table) return ERR;
    ureg cap = is / sizeof(pp_decl_clobber);
    t->hash_bits = ulog2(cap);
    t->hash_mask = cap - 1;
    t->max_fill = cap - (cap >> 2); // 75%
    t->r = r;
    return OK;
}
void ppdct_fin(pp_decl_clobber_table* t)
{
    tfree(t->table);
}

ureg ppdct_prehash_name(char* name)
{
    return fnv_hash_str(FNV_START_HASH, name);
}

pp_decl_clobber* ppdct_lookup_raw(
    pp_decl_clobber_table* t, ast_body* body, char* name, ureg name_prehash)
{
    ureg idx = fnv_fold(
        fnv_hash_pointer(name_prehash, body), t->hash_bits, t->hash_mask);
    pp_decl_clobber* last = t->table + t->hash_mask;
    pp_decl_clobber* ppdc = &t->table[idx];
    while (true) {
        if (!ppdc->body) return ppdc;
        if (ppdc->body == body) {
            if (cstr_eq(ppdc->name, name)) return ppdc;
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
        if (i->body) {
            *ppdct_lookup_raw(
                t, i->body, i->name, ppdct_prehash_name(i->name)) = *i;
        }
        i++;
    }
    return OK;
}

int ppdct_add_symbol(pp_decl_clobber_table* t, symbol* s, ast_body* target_body)
{
    // we could reuse the hash from the symbol lookup, but meh
    ureg name_hash = ppdct_prehash_name(s->name);
    pp_decl_clobber* ppdc =
        ppdct_lookup_raw(t, target_body, s->name, name_hash);
    if (ppdc->body) {
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
    }
    return OK;
}

int ppdct_use_symbol(
    pp_decl_clobber_table* t, symbol* s, ast_node* user, ast_body* user_body)
{
    ureg name_hash = ppdct_prehash_name(s->name);
    bool continue_up = true;
    for (ast_body* b = user_body; continue_up; b = b->parent) {
        if (b == s->declaring_body) break;
        if (b->owning_node->kind == ELEM_MDG_NODE) continue_up = false;
        if (ast_body_is_pp_done(t->r, b)) continue;
        pp_decl_clobber* ppdc = ppdct_lookup_raw(t, b, s->name, name_hash);
        if (ppdc->body) {
            // somebody else already added a clobber. good for us
            assert(ppdc->parent_sym); // we are using that parent, so it exists
            continue;
        }
        // we have to add a clobber. resize if needed
        if (t->clobber_count == t->max_fill) {
            int res = ppdct_grow(t);
            if (res) return ERR;
            ppdc = ppdct_lookup_raw(t, b, s->name, name_hash);
            assert(!ppdc->body);
        }
        ppdc->body = b;
        ppdc->name = s->name;
        ppdc->parent_sym = s;
        ppdc->parent_use.user = user;
        ppdc->parent_use.user_body = user_body;
    }
    return OK;
}