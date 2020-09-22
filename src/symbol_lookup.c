#include "symbol_lookup.h"
#include "resolver.h"

static inline symbol* ast_body_lookup(ast_body* b, ureg hash, const char* name)
{
    if (!b->symtab) return NULL;
    return *symbol_table_lookup_raw(b->symtab, hash, name);
}

static inline bool
check_visible_within(ast_body* looking_body, ast_body* visible_within_body)
{
    if (visible_within_body == NULL) return true;
    for (ast_body* t = looking_body; t != NULL; t = t->parent) {
        if (visible_within_body == t) return true;
    }
    return false;
}

static inline access_modifier resolver_get_am_start(
    ast_body* lookup_body, ast_body* looking_mf_body,
    ast_body* looking_mod_body)
{
    for (ast_body* t = lookup_body; t != NULL; t = t->parent) {
        if (t == looking_mf_body) {
            return AM_LOCAL;
        }
        if (t == looking_mod_body) {
            return AM_INTERNAL;
        }
    }
    return AM_PUBLIC;
}

static inline resolve_error update_ams(
    resolver* r, ast_body* lookup_body, ast_body* looking_body,
    sc_struct* looking_struct, ast_body* looking_mf_body,
    ast_body* looking_mod_body, ast_body* visible_within_body,
    bool* visible_within, access_modifier* am_start, access_modifier* am_end)
{
    bool vis_within_check_required = false;
    if (visible_within) {
        if (visible_within_body) {
            vis_within_check_required = true;
            *visible_within = false;
        }
        else {
            *visible_within = true;
        }
    }
    if (*am_start == AM_NONE) {
        *am_start = resolver_get_am_start(
            lookup_body, looking_mf_body, looking_mod_body);
    }

    if (*am_end == AM_NONE || vis_within_check_required) {
        for (ast_body* t = looking_body; t != NULL; t = t->parent) {
            if (t == lookup_body) {
                *am_end = AM_PRIVATE;
                if (!vis_within_check_required) break;
            }
            if (vis_within_check_required && visible_within_body == t) {
                *visible_within = true;
                vis_within_check_required = false;
                if (*am_end != AM_NONE) break;
            }
        }
    }
    if (*am_end == AM_NONE) {
        *am_end = AM_PUBLIC;
        if (lookup_body->owning_node &&
            ast_elem_is_struct((ast_elem*)lookup_body->owning_node)) {
            if (looking_struct) {
                sc_struct* lookup_struct = (sc_struct*)lookup_body->owning_node;
                while (true) {
                    if (looking_struct == lookup_struct) {
                        *am_end = AM_PROTECTED;
                        break;
                    }
                    if (!looking_struct->sb.extends_spec) break;
                    if (!looking_struct->sb.extends) {
                        // PERF: meh
                        ast_body* decl_body = ast_elem_get_body(
                            (ast_elem*)lookup_body->owning_node);
                        resolve_error re = resolve_ast_node(
                            r, looking_struct->sb.extends_spec, decl_body,
                            (ast_elem**)&looking_struct->sb.extends, NULL);
                        if (re) return re;
                        assert(ast_elem_is_struct(
                            (ast_elem*)looking_struct->sb.extends));
                    }
                    looking_struct = looking_struct->sb.extends;
                }
            }
        }
    }
    return RE_OK;
}
void pop_symbol_lookup_level(symbol_lookup_iterator* sli)
{
    if (sli->stack_height > 1) {
        sbuffer_remove_back(&sli->r->temp_stack, sizeof(symbol_lookup_level));
    }
    sli->stack_height--;
    if (sli->stack_height > 1) {
        sli->head =
            sbuffer_back(&sli->r->temp_stack, sizeof(symbol_lookup_level));
    }
    else if (sli->stack_height == 1) {
        sli->head = &sli->sll_prealloc;
    }
    else {
        sli->head = NULL;
    }
}
resolve_error push_symbol_lookup_level(
    symbol_lookup_iterator* sli, list_rit trait_impls, ast_body** usings_head,
    ast_body** usings_end, ast_body* extends_body,
    open_symbol* overloaded_sym_head, access_modifier am_start,
    access_modifier am_end, bool look_for_members)
{
    symbol_lookup_level* sll;
    if (sli->head == NULL) {
        sll = &sli->sll_prealloc;
    }
    else {
        sll = (symbol_lookup_level*)sbuffer_append(
            &sli->r->temp_stack, sizeof(symbol_lookup_level));
        if (!sll) return RE_FATAL;
    }
    sll->trait_impls = trait_impls;
    sll->usings_head = usings_head;
    sll->usings_end = usings_end;
    sll->extends_body = extends_body;
    sll->overloaded_sym_head = overloaded_sym_head;
    sll->am_start = am_start;
    sll->am_end = am_end;
    sll->look_for_members = look_for_members;
    sli->head = sll;
    sli->stack_height++;
    return RE_OK;
}
resolve_error symbol_lookup_level_run(
    symbol_lookup_iterator* sli, ast_body* lookup_body, bool look_for_members,
    symbol** res)
{
    // access modifiers that would be visible to
    access_modifier am_start = AM_NONE;
    access_modifier am_end = AM_NONE;
    resolve_error re;
    symbol* sym = NULL;
    list_rit impls;
    int responsibility_count = 0;
    if (!sli->lhs_ctype || look_for_members) {
        sym = ast_body_lookup(lookup_body, sli->hash, sli->tgt_name);
    }
    if (sym != NULL) {
        access_modifier am = ast_node_get_access_mod(&sym->node);
        bool vis_within = false;
        if (symbol_is_open_symbol(sym)) {
            open_symbol* osym = (open_symbol*)sym;
            re = update_ams(
                sli->r, lookup_body, sli->looking_body, sli->looking_struct,
                sli->looking_mf_body, sli->looking_mod_body,
                osym->visible_within_body, &vis_within, &am_start, &am_end);
            if (re) return re;
        }
        else {
            re = update_ams(
                sli->r, lookup_body, sli->looking_body, sli->looking_struct,
                sli->looking_mf_body, sli->looking_mod_body, NULL, NULL,
                &am_start, &am_end);
            if (re) return re;
            vis_within = true;
        }
        if (!vis_within || am < am_start || am > am_end) {
            if (!sli->first_hidden_match) sli->first_hidden_match = sym;
            sym = NULL;
        }
    }

    sym_import_symbol* overloaded_import_symbol = NULL;
    if (sym && sli->deref_aliases) {
        if (sym->node.kind == SYM_IMPORT_SYMBOL) {
            sym_import_symbol* is = (sym_import_symbol*)sym;
            if (!ast_node_get_resolved(&sym->node)) {
                re = resolve_import_symbol(
                    sli->r, (sym_import_symbol*)sym, sli->looking_body);
                if (re) return re;
            }
            if (is->target_body) {
                if (sli->enable_shadowing) {
                    return symbol_lookup_level_run(
                        sli, is->target_body, look_for_members, res);
                }
                overloaded_import_symbol = is;
                responsibility_count++;
                sym = NULL;
            }
            else {
                sym = is->target.sym;
            }
        }
    }

    open_symbol* overloaded_sym_head = NULL;
    if (sym && sym->node.kind == SYM_FUNC_OVERLOADED) {
        open_symbol* ols = (open_symbol*)((sym_func_overloaded*)sym)->overloads;
        sym = NULL;
        while (ols) {
            open_symbol* s = ols;
            ols = (open_symbol*)s->sym.next;
            access_modifier am = ast_node_get_access_mod(&s->sym.node);
            if (am < am_start || am > am_end) continue;
            if (!check_visible_within(
                    sli->looking_body, s->visible_within_body)) {
                continue;
            }
            if (sym) {
                overloaded_sym_head = s;
                responsibility_count++;
                break;
            }
            sym = (symbol*)s;
        }
    }
    if (sli->lhs_ctype && lookup_body->symtab && lookup_body->symtab->tt) {
        trait_table* tt = lookup_body->symtab->tt;
        impl_list_for_type* il =
            trait_table_try_get_impl_list_for_type(tt, sli->lhs_ctype);
        // TODO: we need to generate this here :/
        if (il && !list_empty(&il->impls)) {
            list_rit_begin_at_end(&impls, &il->impls);
            responsibility_count++;
            if (list_length(&il->impls) > 1) responsibility_count++;
        }
        else {
            list_rit_empty(&impls);
        }
    }
    ast_body* extends_body = NULL;
    ast_body** usings_end = NULL;
    ast_body** usings_start = NULL;
    if (!sym || !sli->enable_shadowing) {
        if (lookup_body->owning_node &&
            ast_elem_is_struct((ast_elem*)lookup_body->owning_node)) {
            sc_struct* st = (sc_struct*)lookup_body->owning_node;
            // TODO: respect extends visibility
            if (st->sb.extends_spec) {
                if (!st->sb.extends) {
                    re = resolve_ast_node(
                        sli->r, st->sb.extends_spec, lookup_body,
                        (ast_elem**)&st->sb.extends, NULL);
                    if (re) return re;
                    assert(ast_elem_is_struct((ast_elem*)st->sb.extends));
                }
                extends_body = &st->sb.extends->sb.sc.body;
                responsibility_count++;
            }
        }

        if (lookup_body->symtab &&
            symbol_table_has_usings(lookup_body->symtab)) {
            re = update_ams(
                sli->r, lookup_body, sli->looking_body, sli->looking_struct,
                sli->looking_mf_body, sli->looking_mod_body, NULL, NULL,
                &am_start, &am_end);
            if (re) return re;
            usings_start =
                symbol_table_get_uses_start(lookup_body->symtab, am_start);
            usings_end = symbol_table_get_uses_end(lookup_body->symtab, am_end);
        }
        if (usings_start != usings_end) {
            responsibility_count++;
            if (usings_start + 1 != usings_end) responsibility_count++;
        }
    }
    if (sym) responsibility_count++;
    if (responsibility_count == 0) {
        *res = NULL;
        return RE_OK;
    }
    bool second_resp = (responsibility_count >= 2);
    if (second_resp) {
        re = push_symbol_lookup_level(
            sli, impls, usings_start, usings_end, extends_body,
            overloaded_sym_head, am_start, am_end, look_for_members);
        if (re) return re;
        sli->last_match_loc = MATCH_LOCATION_PUSHED;
    }
    if (sym) {
        *res = sym;
        return RE_OK;
    }
    // if overloaded_symbol_head was set than sym was set
    if (overloaded_import_symbol) { // if this is set than sym was NULL
        sli->last_match_loc = MATCH_LOCATION_OVERLOAD;
        return symbol_lookup_level_run(
            sli, overloaded_import_symbol->target_body, look_for_members, res);
    }
    trait_impl* ti = list_rit_prev(&impls);
    if (ti) {
        if (second_resp) sli->head->trait_impls = impls;
        sli->last_match_loc = MATCH_LOCATION_TRAIT_IMPLS;
        return symbol_lookup_level_run(sli, &ti->tib.body, true, res);
    }
    if (usings_end != usings_start) {
        if (second_resp) sli->head->usings_head++;
        sli->last_match_loc = MATCH_LOCATION_USING;
        return symbol_lookup_level_run(
            sli, *usings_start, look_for_members, res);
    }
    // otherwise some condition above would have fired
    assert(extends_body && !second_resp);
    return symbol_lookup_level_run(sli, extends_body, look_for_members, res);
}

resolve_error symbol_lookup_iterator_init(
    symbol_lookup_iterator* sli, resolver* r, ast_body* lookup_body,
    ast_elem* lhs_ctype, ast_body* looking_body, const char* tgt_name,
    bool enable_shadowing, bool deref_aliases)
{
    bool indirect_lookup = (looking_body != lookup_body);
    sli->explore_parents = !indirect_lookup;
    lookup_body = ast_body_get_non_paste_parent(lookup_body);
    looking_body = indirect_lookup ? ast_body_get_non_paste_parent(looking_body)
                                   : lookup_body;
    sc_struct* looking_struct = NULL;
    ast_body* looking_mf_body;
    ast_body* looking_mod_body;
    ast_body* i = looking_body;
    while (true) {
        assert(i && i->owning_node);
        if (!looking_struct && ast_elem_is_struct((ast_elem*)i->owning_node)) {
            looking_struct = (sc_struct*)i->owning_node;
        }
        if (ast_elem_is_module_frame((ast_elem*)i->owning_node)) {
            looking_mf_body = i;
            if (looking_mf_body->parent->owning_node->kind == ELEM_MDG_NODE) {
                looking_mod_body = looking_mf_body->parent;
            }
            else {
                looking_mod_body = looking_mf_body;
            }
            break;
        }
        if (i->owning_node->kind == ELEM_MDG_NODE) {
            looking_mf_body = NULL;
            looking_mod_body = i;
            break;
        }
        i = i->parent;
    }
    sli->explore_type = (lhs_ctype && ast_elem_is_scope((ast_elem*)lhs_ctype));
    sli->r = r;
    sli->first_hidden_match = NULL;
    sli->hash = symbol_table_prehash(tgt_name);
    sli->tgt_name = tgt_name;
    sli->looking_body = looking_body;
    sli->looking_struct = looking_struct;
    sli->looking_mf_body = looking_mf_body;
    sli->looking_mod_body = looking_mod_body;
    sli->enable_shadowing = enable_shadowing;
    sli->deref_aliases = deref_aliases;
    sli->head = NULL;
    sli->last_match_loc = MATCH_LOCATION_POPPED;
    sli->stack_height = 0;
    sli->lhs_ctype = lhs_ctype;
    sli->next_lookup_body = lookup_body;
    sli->exploring_members = false;
    return RE_OK;
}
static inline resolve_error symbol_lookup_level_continue(
    symbol_lookup_iterator* sli, symbol_lookup_level* sll, symbol** res)
{
    while (sll->overloaded_sym_head) {
        open_symbol* s = sll->overloaded_sym_head;
        sll->overloaded_sym_head = (open_symbol*)s->sym.next;
        access_modifier am = ast_node_get_access_mod(&s->sym.node);
        if (am < sll->am_start || am > sll->am_end) continue;
        if (check_visible_within(sli->looking_body, s->visible_within_body)) {
            *res = (symbol*)s;
            sli->last_match_loc = MATCH_LOCATION_OVERLOAD;
            return RE_OK;
        }
    }
    while (true) {
        trait_impl* ti = list_rit_prev(&sll->trait_impls);
        if (!ti) break;
        resolve_error re =
            symbol_lookup_level_run(sli, &ti->tib.body, true, res);
        if (re) return re;
        if (*res) {
            sli->last_match_loc = MATCH_LOCATION_TRAIT_IMPLS;
            return RE_OK;
        }
    }
    while (sll->usings_head != sll->usings_end) {
        ast_body* t = *sll->usings_head;
        sll->usings_head++;
        resolve_error re =
            symbol_lookup_level_run(sli, t, sll->look_for_members, res);
        if (re) return re;
        if (*res) {
            sli->last_match_loc = MATCH_LOCATION_USING;
            return RE_OK;
        }
    }
    if (sll->extends_body) {
        ast_body* body = sll->extends_body;
        sll->extends_body = NULL;
        pop_symbol_lookup_level(sli);
        sli->last_match_loc = MATCH_LOCATION_POPPED;
        return symbol_lookup_level_run(sli, body, sll->look_for_members, res);
    }
    pop_symbol_lookup_level(sli);
    *res = NULL;
    return RE_OK;
}

resolve_error
symbol_lookup_iterator_next(symbol_lookup_iterator* sli, symbol** res)
{
    resolve_error re;
    symbol* sym = NULL;
    while (true) {
        if (sli->head) {
            while (sli->head) {
                re = symbol_lookup_level_continue(sli, sli->head, &sym);
                if (re) return re;
                if (sym) break;
            }
            if (sym) break;
        }
        ast_body* nlb = sli->next_lookup_body;
        if (!nlb) break;
        if (!sli->explore_parents) {
            sli->next_lookup_body = NULL;
        }
        else if (nlb) {
            sli->next_lookup_body = nlb->parent;
        }
        if (!sli->next_lookup_body && sli->explore_type) {
            sli->explore_type = false;
            sli->explore_parents = true;
            sli->next_lookup_body = &((scope*)sli->lhs_ctype)->body;
            sli->exploring_members = true;
        }
        re = symbol_lookup_level_run(sli, nlb, sli->exploring_members, &sym);
        if (sym && sli->enable_shadowing) {
            sli->next_lookup_body = NULL;
        }
        if (re) return re;
        if (sym) break;
    }
    *res = sym;
    return RE_OK;
}

void symbol_lookup_iterator_cut_off_shadowed(symbol_lookup_iterator* sli)
{
    assert(!sli->enable_shadowing); // always cut off in that case
    switch (sli->last_match_loc) {
        case MATCH_LOCATION_PUSHED: {
            pop_symbol_lookup_level(sli);
        } break;
        case MATCH_LOCATION_OVERLOAD: {
            assert(sli->head);
            sli->head->usings_head = sli->head->usings_end;
            list_rit_empty(&sli->head->trait_impls);
        } // fallthrough
        case MATCH_LOCATION_TRAIT_IMPLS:
        case MATCH_LOCATION_USING: {
            assert(sli->head);
            sli->head->extends_body = NULL;
        } break;
        case MATCH_LOCATION_POPPED: break;
    }
    sli->next_lookup_body = NULL;
}
