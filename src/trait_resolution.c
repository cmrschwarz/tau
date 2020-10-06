#include "trait_resolution.h"
#include "ast.h"
#include "error_log.h"
#include "thread_context.h"

resolve_error resolve_trait_impl(resolver* r, trait_impl* ti, ast_body* body)
{
    resolve_error re;
    if (ti->tib.impl_of) {
        ast_elem* impl_of_ctype;
        re = resolve_ast_node(r, ti->tib.impl_of, body, &impl_of_ctype, NULL);
        if (re) return re;
        if (impl_of_ctype != ERROR_ELEM && impl_of_ctype->kind != SC_TRAIT) {
            src_range_large ti_srl, impl_of_srl;
            ast_node_get_src_range((ast_node*)ti, body, &ti_srl);
            ast_node_get_src_range(ti->tib.impl_of, body, &impl_of_srl);
            error_log_report_annotated_twice(
                r->tc->err_log, ES_RESOLVER, false,
                "impl must be of a trait type or directly for a type",
                impl_of_srl.smap, impl_of_srl.start, impl_of_srl.end,
                "is not a trait", ti_srl.smap, ti_srl.start, ti_srl.end, NULL);
            r->error_occured = true;
            ti->impl_of_trait = (sc_trait*)ERROR_ELEM;
        }
        else {
            ti->impl_of_trait = (sc_trait*)impl_of_ctype;
        }
    }
    else {
        ti->impl_of_trait = NULL;
    }
    ast_elem* impl_for_ctype_ctype;
    re = resolve_ast_node(
        r, ti->tib.impl_for, body, (ast_elem**)&ti->impl_for_ctype,
        &impl_for_ctype_ctype);
    if (re) return re;
    if (ti->impl_for_ctype == ERROR_ELEM) {
        ast_node_set_poisoned((ast_node*)ti);
        return RE_OK;
    }
    if (impl_for_ctype_ctype != TYPE_ELEM &&
        impl_for_ctype_ctype != TRAIT_ELEM) {
        src_range_large ti_srl, impl_for_srl;
        ast_node_get_src_range((ast_node*)ti, body, &ti_srl);
        ast_node_get_src_range(ti->tib.impl_for, body, &impl_for_srl);
        error_log_report_annotated_twice(
            r->tc->err_log, ES_RESOLVER, false,
            "impl must be for a type or trait", impl_for_srl.smap,
            impl_for_srl.start, impl_for_srl.end, "is not a type or trait",
            ti_srl.smap, ti_srl.start, ti_srl.end, NULL);
        r->error_occured = true;
        ast_node_set_poisoned((ast_node*)ti);
        return RE_OK;
    }

    if (ti->impl_for_ctype == ERROR_ELEM) {
        ast_node_set_poisoned((ast_node*)ti);
        return RE_OK;
    }
    if (ti->impl_of_trait != (sc_trait*)ERROR_ELEM &&
        ti->impl_of_trait != NULL) {
        assert(body->symtab && body->symtab->tt);
        impl_status_for_type* is;
        is = trait_table_get_impl_status_for_type(
            body->symtab->tt, ti->impl_for_ctype, ti->impl_of_trait);
        if (!is) return RE_FATAL;
        if (is->type) {
            assert(is->impl != ti);
            assert(is->impl); // TODO: handle pp trashing our state
            src_range_large prev_srl, ti_srl;
            ast_node_get_src_range((ast_node*)is->impl, body, &prev_srl);
            ast_node_get_src_range((ast_node*)ti, body, &ti_srl);
            error_log_report_annotated_twice(
                r->tc->err_log, ES_RESOLVER, false,
                "conflicting impls found in the same scope", ti_srl.smap,
                ti_srl.start, ti_srl.end, "conflicting impl here",
                prev_srl.smap, prev_srl.start, prev_srl.end,
                "previous impl here");
            r->error_occured = true;
            ast_node_set_poisoned((ast_node*)ti);
            return RE_OK; // no point adding both to impl list
        }
        is->type = ti->impl_for_ctype;
        is->trait = ti->impl_of_trait;
        is->impl = ti;
    }
    impl_list_for_type* il = trait_table_get_impl_list_for_type(
        body->symtab->tt, ti->impl_for_ctype);
    if (!il) return RE_FATAL;
    if (!il->type) {
        if (list_init(&il->impls)) {
            body->symtab->tt->impl_lists_count--;
            return RE_FATAL;
        }
        il->type = ti->impl_for_ctype;
        il->generics_done = false;
    }
    if (list_append(&il->impls, NULL, ti)) return RE_FATAL;
    return add_body_decls(
        r, &ti->tib.body, NULL, is_body_public_st(&ti->tib.body));
    // TODO: check if all traits are present
}
static inline resolve_error unordered_body_add_trait_decls_raw(
    resolver* r, ast_body* body, bool* progress, resolve_error* holdup)
{
    dbuffer_iterator it;
    resolve_error re;
    if (!body->symtab) return RE_OK;
    if (!body->symtab->tt) return RE_OK;
    dbuffer* ui = &body->symtab->tt->unresolved_impls;
    if (dbuffer_is_invalid(ui)) return RE_OK;
    dbuffer_iterator_init(&it, ui);
    while (true) {
        trait_impl** ti = dbuffer_iterator_get(&it, sizeof(trait_impl*));
        if (!ti) break;
        re = resolve_trait_impl(r, *ti, body);
        if (re == RE_UNKNOWN_SYMBOL || re == RE_UNREALIZED_COMPTIME) {
            if (*holdup != RE_UNREALIZED_COMPTIME) {
                *holdup = re;
            }
            dbuffer_iterator_next(&it, sizeof(trait_impl*));
            continue;
        }
        else {
            if (re) return re;
        }
        void* new_end = ptrsub(it.end, sizeof(trait_impl*));
        dbuffer_swap(ui, it.pos, new_end, sizeof(trait_impl*));
        dbuffer_pop(ui, sizeof(trait_impl*));
        // hacky way of fixing the iterator
        it.end = new_end;
        *progress = true;
    }
    return RE_OK;
}
resolve_error unordered_body_add_trait_decls(resolver* r, ast_body* body)
{
    bool x;
    resolve_error holdup = RE_OK;
    resolve_error re = unordered_body_add_trait_decls_raw(r, body, &x, &holdup);
    if (re) return re;
    return holdup;
}
resolve_error add_mf_trait_decls(resolver* r)
{
    bool progress;
    resolve_error holdup;
    do {
        holdup = RE_OK;
        progress = false;
        for (mdg_node** i = r->mdgs_begin; i != r->mdgs_end; i++) {
            resolve_error re = unordered_body_add_trait_decls_raw(
                r, &(**i).body, &progress, &holdup);
            if (re) return re;
            aseglist_iterator asi;
            aseglist_iterator_begin(&asi, &(**i).module_frames);
            for (module_frame* mf = aseglist_iterator_next(&asi); mf != NULL;
                 mf = aseglist_iterator_next(&asi)) {
                re = unordered_body_add_trait_decls_raw(
                    r, &mf->body, &progress, &holdup);
                if (re) return re;
            }
        }
        if (holdup) {
            resolve_error re = resolver_run_pp_resolve_nodes(r, &progress);
            if (re) return re;
        }
    } while (progress);
    return RE_OK;
}

resolve_error ast_node_add_trait_decls(resolver* r, ast_body* body, ast_node* n)
{
    if (n->kind == TRAIT_IMPL) {
        resolve_error re = resolve_trait_impl(r, (trait_impl*)n, body);
        if (re) return re;
    }
    return RE_OK;
}

resolve_error resolve_type(resolver* r, ast_body* b, ast_elem* type)
{
    while (!b->symtab || !b->symtab->tt) {
        b = b->parent;
        if (!b) return RE_OK;
    }
    trait_table* tt = b->symtab->tt;
    impl_list_for_type* il = trait_table_try_get_impl_list_for_type(tt, type);
    if (il && il->generics_done) return RE_OK;
    dbuffer_iterator it;
    dbuffer_iterator_init(&it, &tt->generic_impls);
    trait_impl_generic** ti;
    while ((ti = dbuffer_iterator_next(&it, sizeof(trait_impl_generic*)))) {
    }
    return RE_OK;
}
