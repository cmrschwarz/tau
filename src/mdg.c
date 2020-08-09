#include "mdg.h"
#include "error_log.h"
#include "mdght.h"
#include "tauc.h"
#include "utils/allocator.h"
#include "utils/panic.h"
#include "utils/threading.h"
#include "utils/zero.h"
#include "scc_detector.h"
#include "print_ast.h"
#include <assert.h>

int mdg_fin_partial(module_dependency_graph* m, int i, int r)
{
    switch (i) {
        case -1:
            mdght_fin_contained_nodes(mdg_start_write(m));
            mdg_end_write(m);
            mdg_node_fin(m->invalid_node);
            // fallthrough
        case 6: mdg_node_fin(m->root_node); // fallthrough
        case 5: mdght_fin(&m->mdghts[1]); // fallthrough
        case 4: mdght_fin(&m->mdghts[0]); // fallthrough
        case 3: evmap2_fin(&m->evm); // fallthrough
        case 2: pool_fin(&m->node_pool); // fallthrough
        case 1: pool_fin(&m->ident_pool); // fallthrough
        case 0: break;
    }
    return r;
}
mdg_node* mdg_node_create(
    module_dependency_graph* m, string ident, mdg_node* parent,
    module_stage initial_stage);
int mdg_init(module_dependency_graph* m)
{
    int r = pool_init(&m->ident_pool);
    if (r) return mdg_fin_partial(m, 0, r);
    r = pool_init(&m->node_pool);
    if (r) return mdg_fin_partial(m, 1, r);
    r = evmap2_init(&m->evm, MDG_MAX_CHANGES);
    if (r) return mdg_fin_partial(m, 2, r);
    r = mdght_init(&m->mdghts[0]);
    if (r) return mdg_fin_partial(m, 3, r);
    r = mdght_init(&m->mdghts[1]);
    if (r) return mdg_fin_partial(m, 4, r);

    atomic_ureg_init(&m->node_ids, 0);

    m->root_node = mdg_node_create(m, string_from_cstr("_"), NULL, MS_PARSING);
    if (!m->root_node) return mdg_fin_partial(m, 5, ERR);

    m->invalid_node = mdg_node_create(
        m, string_from_cstr("_invalid_node_"), NULL, MS_PARSING);
    if (!m->root_node) return mdg_fin_partial(m, 6, ERR);
    m->invalid_node->error_occured = true;
    m->invalid_node->symtab = NULL;
    m->change_count = 0;
    return 0;
}
void mdg_fin(module_dependency_graph* m)
{
    mdg_fin_partial(m, -1, 0);
}

mdght* mdg_start_read(module_dependency_graph* m)
{
    ureg id = evmap2_start_read(&m->evm);
    return &m->mdghts[id];
}
void mdg_end_read(module_dependency_graph* m, mdght* h)
{
    evmap2_end_read(&m->evm, (h == &m->mdghts[0]) ? 0 : 1);
}

static int mdg_apply_changes(module_dependency_graph* m, mdght* tgt)
{
    for (ureg i = 0; i < m->change_count; i++) {
        void* p = mdght_insert_at(tgt, m->changes[i].pos, m->changes[i].node);
        if (!p) return ERR;
    }
    m->change_count = 0;
    return OK;
}

mdght* mdg_start_write(module_dependency_graph* m)
{
    ureg id;
    ureg changes = evmap2_start_write(&m->evm, &id);
    if (changes) {
        if (mdg_apply_changes(m, &m->mdghts[id])) return NULL;
    }
    m->change_count = 0;
    return &m->mdghts[id];
}
void mdg_end_write(module_dependency_graph* m)
{
    evmap2_end_write(&m->evm);
}
void* mdg_node_partial_fin(mdg_node* n, int i)
{
    switch (i) {
        case -1:
        case 4: list_fin(&n->notify, true); // fallthrough
        case 3: list_fin(&n->dependencies, true); // fallthrough
        case 2: rwlock_fin(&n->lock); // fallthrough
        case 1: aseglist_fin(&n->module_frames); // fallthrough
        case 0: break;
    }
    return NULL;
}
// since this is called while inside mdg_write, there is no race
// on the memory pools
mdg_node* mdg_node_create(
    module_dependency_graph* m, string ident, mdg_node* parent,
    module_stage initial_stage)
{
    mdg_node* n = pool_alloc(&m->node_pool, sizeof(mdg_node));
    if (!n) return NULL;
    n->elem.kind = ELEM_MDG_NODE;
    ureg identlen = string_len(ident);
    n->name = pool_alloc(&m->ident_pool, identlen + 1);
    memcpy(n->name, ident.start, identlen);
    if (!n->name) return NULL;
    n->name[identlen] = '\0';
    n->id = atomic_ureg_inc(&m->node_ids);
    n->parent = parent;
    int r = aseglist_init(&n->module_frames);
    if (r) return mdg_node_partial_fin(n, 0);
    r = rwlock_init(&n->lock);
    if (r) return mdg_node_partial_fin(n, 1);
    r = list_init(&n->dependencies);
    if (r) return mdg_node_partial_fin(n, 2);
    r = list_init(&n->notify);
    if (r) return mdg_node_partial_fin(n, 3);
    atomic_ureg_init(&n->unparsed_files, 0);
    atomic_ureg_init(&n->decl_count, 0);
    atomic_ureg_init(&n->using_count, 0);
    // we init this to 1 so we don't get notified while the mdg  is still
    // resolving itself. before its suspended this is decremented
    atomic_ureg_init(&n->ungenerated_pp_deps, 1);
    n->stage = initial_stage;
    n->symtab = NULL;
    n->notifier = NULL;
    n->ppe_stage = PPES_UNNEEDED;
    n->requested_for_pp = false;
    n->partial_res_data = NULL;
    n->error_occured = false;
    return n;
}
void free_body_symtabs(ast_node* node, ast_body* b);
void free_astn_symtabs(ast_node* n)
{
    if (!n) return;
    if (ast_elem_is_module_frame((ast_elem*)n)) {
        // these are parts of a module and therefore already handled
        return;
    }
    if (ast_elem_is_scope((ast_elem*)n)) {
        free_body_symtabs(n, &((scope*)n)->body);
        if (n->kind == SC_STRUCT_GENERIC) {
            for (sc_struct_generic_inst* sgi =
                     ((sc_struct_generic*)n)->instances;
                 sgi;
                 sgi = (sc_struct_generic_inst*)sgi->st.sb.sc.osym.sym.next) {
                free_astn_symtabs((ast_node*)sgi);
            }
        }
        return;
    }
    switch (n->kind) {
        case EXPR_BREAK:
        case EXPR_RETURN: free_astn_symtabs(((expr_break*)n)->value); break;

        case EXPR_BLOCK: {
            free_body_symtabs(n, &((expr_block*)n)->ebb.body);
        } break;

        case SYM_IMPORT_PARENT: {
            sym_import_parent* ip = (sym_import_parent*)n;
            if (!ast_flags_get_resolved(n->flags)) break;
            if (ip->children.symtab->owning_node != (ast_elem*)ip) break;
            symtab_it it = symtab_it_make(ip->children.symtab);
            for (symbol* s = symtab_it_next(&it); s != NULL;
                 s = symtab_it_next(&it)) {
                free_astn_symtabs((ast_node*)s);
            }
            symbol_table_fin(ip->children.symtab);
        } break;
        case SYM_IMPORT_GROUP: {
            sym_import_group* ig = (sym_import_group*)n;
            if (!ast_flags_get_resolved(n->flags)) break;
            if (ig->children.symtab->owning_node != (ast_elem*)ig) break;
            symtab_it it = symtab_it_make(ig->children.symtab);
            for (symbol* s = symtab_it_next(&it); s != NULL;
                 s = symtab_it_next(&it)) {
                free_astn_symtabs((ast_node*)s);
            }
            symbol_table_fin(ig->children.symtab);
        } break;
        case EXPR_IF: {
            expr_if* ei = (expr_if*)n;
            free_astn_symtabs(ei->condition);
            free_astn_symtabs(ei->if_body);
            free_astn_symtabs(ei->else_body);
        } break;
        case EXPR_LOOP: free_body_symtabs(n, &((expr_loop*)n)->ebb.body); break;

        case EXPR_MACRO_CALL: {
            expr_macro_call* emc = (expr_macro_call*)n;
            free_body_symtabs(n, &emc->body);
        } break;

        case EXPR_PP: free_astn_symtabs(((expr_pp*)n)->pp_expr); break;
        case EXPR_PASTE_STR: {
            free_astn_symtabs(((expr_paste_str*)n)->value);
        } break;
        case EXPR_MATCH: {
            expr_match* em = (expr_match*)n;
            free_astn_symtabs(em->match_expr);
            for (match_arm** ma = (match_arm**)em->body.elements; *ma != NULL;
                 ma++) {
                free_astn_symtabs((**ma).condition);
                free_astn_symtabs((**ma).value);
            }
        } break;

        case EXPR_OP_BINARY: {
            expr_op_binary* ob = (expr_op_binary*)n;
            free_astn_symtabs(ob->lhs);
            free_astn_symtabs(ob->rhs);
        } break;
        case EXPR_MACRO_STR_CALL: {
            free_astn_symtabs(((expr_macro_str_call*)n)->lhs);
        } break;
        case EXPR_OP_UNARY: {
            free_astn_symtabs(((expr_op_unary*)n)->child);
        } break;
        case EXPR_PARENTHESES: {
            free_astn_symtabs(((expr_op_unary*)n)->child);
        } break;
        case SYM_VAR_INITIALIZED: {
            free_astn_symtabs(((sym_var_initialized*)n)->initial_value);
        } // fallthrough
        case SYM_VAR: {
            free_astn_symtabs(((sym_var*)n)->type);
        } break;
        case STMT_USE: {
            free_astn_symtabs(((stmt_use*)n)->target);
        } break;
        case SYM_NAMED_USE: {
            free_astn_symtabs(((sym_named_use*)n)->target);
        } break;
        case EXPR_CALL: {
            expr_call* c = (expr_call*)n;
            for (ureg i = 0; i < c->arg_count; i++) {
                free_astn_symtabs(c->args[i]);
            }
            free_astn_symtabs(c->lhs);
        } break;
        case EXPR_MEMBER_ACCESS:
        case EXPR_SCOPE_ACCESS: {
            expr_scope_access* esa = (expr_scope_access*)n;
            free_astn_symtabs(esa->lhs);
        }
        case EXPR_IDENTIFIER:
        case SYM_IMPORT_MODULE:
        case SYM_IMPORT_SYMBOL:
        case EXPR_LITERAL: break;
        case EXPR_PASTE_EVALUATION: {
            expr_paste_evaluation* epe = (expr_paste_evaluation*)n;
            free_astn_symtabs(epe->expr);
            free_astn_symtabs(epe->pe.source_pp_expr);
        } break;
        case STMT_PASTE_EVALUATION: {
            stmt_paste_evaluation* spe = (stmt_paste_evaluation*)n;
            free_body_symtabs(n, &spe->body);
            free_astn_symtabs(spe->pe.source_pp_expr);
        } break;
        case EXPR_ARRAY_TYPE: {
            expr_array_type* eat = (expr_array_type*)n;
            free_astn_symtabs(eat->length_spec);
            free_astn_symtabs(eat->slice_type.base_type);
        } break;
        case EXPR_SLICE_TYPE: {
            expr_slice_type* est = (expr_slice_type*)n;
            free_astn_symtabs(est->base_type);
        } break;
        case EXPR_ARRAY: {
            expr_array* ea = (expr_array*)n;
            for (ureg i = 0; i < ea->elem_count; i++) {
                free_astn_symtabs(ea->elements[i]);
            }
            free_astn_symtabs((ast_node*)ea->explicit_decl);
        } break;
        case EXPR_CAST: {
            expr_cast* ec = (expr_cast*)n;
            free_astn_symtabs(ec->value);
            free_astn_symtabs(ec->target_type);
        } break;
        case EXPR_ACCESS: {
            expr_access* ea = (expr_access*)n;
            free_astn_symtabs(ea->lhs);
            for (ureg i = 0; i < ea->arg_count; i++) {
                free_astn_symtabs(ea->args[i]);
            }
        } break;
        default: assert(false);
    }
}
void free_body_symtabs(ast_node* node, ast_body* b)
{
    // delete children first since children might contain that symtab
    // pointer and check for it's owning node
    for (ast_node** n = b->elements; *n != NULL; n++) {
        free_astn_symtabs(*n);
    }
    if (b->symtab && b->symtab->owning_node == (ast_elem*)node) {
        symtab_it it = symtab_it_make(b->symtab);
        for (symbol* s = symtab_it_next(&it); s != NULL;
             s = symtab_it_next(&it)) {
            if (s->node.kind == SYM_IMPORT_PARENT) {
                free_astn_symtabs((ast_node*)s);
            }
        }
        symbol_table_fin(b->symtab);
    }
}
void mdg_node_fin(mdg_node* n)
{
    if (n->stage >= MS_PARSING) {
        aseglist_iterator it;
        aseglist_iterator_begin(&it, &n->module_frames);
        while (true) {
            module_frame* mf = aseglist_iterator_next(&it);
            if (!mf) break;
            free_body_symtabs((ast_node*)mf, &mf->body);
        }
    }
    if (n->stage >= MS_RESOLVING) {
        symbol_table_fin(n->symtab);
    }
    mdg_node_partial_fin(n, -1);
}

mdg_node* mdg_get_node(
    module_dependency_graph* m, mdg_node* parent, string ident,
    module_stage initial_stage)
{
    ureg hash = mdght_get_hash_str(parent, ident);
    mdght* h = mdg_start_read(m);
    mdg_node* n = mdght_get_str_ph(h, hash, parent, ident);
    mdg_end_read(m, h);
    if (n == NULL) {
        h = mdg_start_write(m);
        mdg_node** np = mdght_get_str_raw_ph(h, hash, parent, ident);
        n = *np;
        if (n != NULL) {
            mdg_end_write(m);
        }
        else {
            n = mdg_node_create(m, ident, parent, initial_stage);
            if (n == NULL) {
                mdg_end_write(m);
                return NULL;
            }
            *np = n;
            m->changes[m->change_count].pos = np - h->table_start;
            m->changes[m->change_count].node = n;
            m->change_count++;
            h->elem_count++;
            mdg_end_write(m);
        }
    }
    return n;
}
void report_module_redeclaration(
    thread_context* tc, module_frame* mod_1, src_map* mod2_smap,
    src_range mod2_sr)
{
    error_log_report_annotated_twice(
        tc->err_log, ES_RESOLVER, false, "module redeclaration", mod_1->smap,
        src_range_get_start(mod_1->node.srange),
        src_range_get_end(mod_1->node.srange), "one declaration here",
        mod2_smap, src_range_get_start(mod2_sr), src_range_get_end(mod2_sr),
        "another conflicting declaration here");
}
void report_unrequired_extend(thread_context* tc, src_map* smap, src_range sr)
{
    error_log_report_annotated(
        tc->err_log, ES_RESOLVER, false, "extend in non required file", smap,
        src_range_get_start(sr), src_range_get_end(sr),
        "the file containing this has not been required by this module");
}
// the source range and smap are in case we need to report an error
mdg_node* mdg_found_node(
    thread_context* tc, mdg_node* parent, string ident, bool extend,
    src_map* smap, src_range sr)
{
    mdg_node* n = mdg_get_node(&tc->t->mdg, parent, ident, MS_FOUND_UNNEEDED);
    if (!n) return NULL;
    rwlock_write(&n->lock);
    if (n->stage == MS_UNFOUND) {
        n->stage = MS_PARSING;
    }
    else if (n->stage == MS_UNFOUND_UNNEEDED) {
        n->stage = MS_FOUND_UNNEEDED;
    }
    else if (n->stage >= MS_PARSED_UNNEEDED) {
        // TODO: the resolver stage isn't really appropriate here
        rwlock_end_write(&n->lock);
        if (extend) {
            report_unrequired_extend(tc, smap, sr);
        }
        else {
            aseglist_iterator it;
            aseglist_iterator_begin(&it, &n->module_frames);
            module_frame* f;
            while ((f = aseglist_iterator_next(&it))) {
                if (f->node.kind != MF_MODULE) continue;
                report_module_redeclaration(tc, f, smap, sr);
                break;
            }
            assert(f && f->node.kind == MF_MODULE);
        }
        return tc->t->mdg.invalid_node;
    }
    rwlock_end_write(&n->lock);
    return n;
}

int mdg_node_parsed(module_dependency_graph* m, mdg_node* n, thread_context* tc)
{
    return sccd_run(&tc->sccd, n, SCCD_PARSED);
}
int mdg_node_file_parsed(
    module_dependency_graph* m, mdg_node* n, thread_context* tc,
    bool error_occured)
{
    ureg up = atomic_ureg_dec(&n->unparsed_files);
    assert(up != 0);
    if (error_occured) {
        rwlock_write(&n->lock);
        n->error_occured = true;
        rwlock_end_write(&n->lock);
    }
    if (up == 1) return mdg_node_parsed(m, n, tc);
    return OK;
}

bool module_import_group_find_import(
    sym_import_group* ig, mdg_node* import, sym_import_group** tgt_group,
    symbol** tgt_sym)
{
    if (ig->parent_im.module == import) {
        *tgt_sym = (symbol*)ig;
        *tgt_group = ig;
        return true;
    }
    for (symbol* c = ig->children.symbols; c != NULL; c = c->next) {
        if (c->node.kind == SYM_IMPORT_SYMBOL) continue;
        if (c->node.kind == SYM_IMPORT_GROUP) {
            if (module_import_group_find_import(
                    (sym_import_group*)c, import, tgt_group, tgt_sym)) {
                return true;
            }
        }
        else {
            assert(c->node.kind == SYM_IMPORT_MODULE);
            if (((sym_import_module*)c)->module == import) {
                *tgt_sym = (symbol*)c;
                *tgt_group = ig;
                return true;
            }
        }
    }
    return false;
}
bool ast_body_find_import(
    ast_body* b, mdg_node* import, sym_import_group** tgt_group,
    symbol** tgt_sym)
{
    for (ast_node** n = b->elements; *n; n++) {
        if (ast_elem_is_scope((ast_elem*)*n)) {
            if (ast_body_find_import(
                    &((scope*)*n)->body, import, tgt_group, tgt_sym)) {
                return true;
            }
        }
        else if (ast_elem_is_module_frame((ast_elem*)n)) {
            if (ast_body_find_import(
                    &((module_frame*)*n)->body, import, tgt_group, tgt_sym)) {
                return true;
            }
        }
        else if ((**n).kind == SYM_IMPORT_GROUP) {
            sym_import_group* ig = (sym_import_group*)*n;
            if (module_import_group_find_import(
                    ig, import, tgt_group, tgt_sym)) {
                return true;
            }
        }
        else if ((**n).kind == SYM_IMPORT_MODULE) {
            if (((sym_import_module*)*n)->module == import) {
                *tgt_group = NULL;
                *tgt_sym = (symbol*)*n;
                return true;
            }
        }
    }
    return false;
}
void mdg_node_find_import(
    mdg_node* m, mdg_node* import, sym_import_group** tgt_group,
    symbol** tgt_sym, src_map** smap)
{
    aseglist_iterator it;
    aseglist_iterator_begin(&it, &m->module_frames);
    while (true) {
        module_frame* mf = aseglist_iterator_next(&it);
        if (!mf) break;
        if (ast_body_find_import(&mf->body, import, tgt_group, tgt_sym)) {
            *smap = module_frame_get_smap(mf);
            return;
        }
    }
    panic("failed to find the source location of a missing import!");
}
void mdg_node_report_missing_import(
    thread_context* tc, mdg_node* m, mdg_node* import)
{
    symbol* tgt_sym;
    sym_import_group* tgt_group = NULL;
    src_map* smap;
    mdg_node_find_import(m, import, &tgt_group, &tgt_sym, &smap);
    src_range_large tgt_sym_srl;
    src_range_unpack(tgt_sym->node.srange, &tgt_sym_srl);
    if (!tgt_group) {
        error_log_report_annotated(
            tc->err_log, ES_RESOLVER, false,
            "missing definition for imported module", smap, tgt_sym_srl.start,
            tgt_sym_srl.end, "imported here");
        return;
    }
    src_range_large tgt_group_srl;
    src_range_unpack(tgt_group->parent_im.osym.sym.node.srange, &tgt_group_srl);
    error_log_report_annotated_twice(
        tc->err_log, ES_RESOLVER, false,
        "missing definition for imported module", smap, tgt_sym_srl.start,
        tgt_sym_srl.end, "imported here", smap, tgt_group_srl.start,
        tgt_group_srl.end, NULL);
}
int mdg_nodes_resolved(
    mdg_node** start, mdg_node** end, thread_context* tc, bool error_occured)
{
    for (mdg_node** i = start; i != end; i++) {
        rwlock_write(&(**i).lock);
        module_stage ms = (**i).stage;
        if (ms == MS_RESOLVING) {
            (**i).stage = MS_GENERATING;
        }
        else {
            assert(ms == MS_RESOLVING_EXPLORATION);
            (**i).stage = MS_RESOLVED_UNNEEDED;
        }
        if (error_occured) {
            (**i).error_occured = true;
        }
        else {
            assert(!(**i).error_occured);
        }
        list* l = sbuffer_append(&tc->temp_buffer, sizeof(list));
        if (!l) {
            // revert on error
            // TODO: figure out what to do with the remaining notifys
            rwlock_end_write(&(**i).lock);
            for (mdg_node** j = start; j != i; j++) {
                list_fin(
                    (list*)sbuffer_back(&tc->temp_buffer, sizeof(list)), true);
                sbuffer_remove_back(&tc->temp_buffer, sizeof(list));
            }
            return ERR;
        }
        *l = (**i).notify;
        list_init(&(**i).notify);
        rwlock_end_write(&(**i).lock);
    }
    bool ok = true;
    for (mdg_node** i = end - 1; i != start - 1; i--) {
        list* l = sbuffer_back(&tc->temp_buffer, sizeof(list));
        list_it it;
        list_it_begin(&it, l);
        while (ok) {
            mdg_node* dep = list_it_next(&it, l);
            if (!dep) break;
            if (sccd_run(&tc->sccd, dep, SCCD_NOTIFY_DEP_RESOLVED)) ok = false;
            // TODO: figure out what to do with the remaining notifys on err
        }
        // we just replace the old notify list with a new one and free the old
        // one without reusing any allocations. we have SSO anyways and a big
        // "notify on resolved" list doesn't imply a big "notify on generated"
        // list
        list_fin(l, true);
        sbuffer_remove_back(&tc->temp_buffer, sizeof(list));
    }
    return ok ? OK : ERR;
}
int mdg_nodes_generated(
    mdg_node** start, mdg_node** end, thread_context* tc, bool pp_generated)
{
    for (mdg_node** i = start; i != end; i++) {
        rwlock_write(&(**i).lock);
        (**i).stage = MS_GENERATED;
        (**i).ppe_stage = pp_generated ? PPES_DONE : PPES_SKIPPED;
        assert(pp_generated || (**i).parent == NULL); // TODO
        list* l = sbuffer_append(&tc->temp_buffer, sizeof(list));
        if (!l) {
            // revert on error
            // TODO: figure out what to do with the remaining notifys
            rwlock_end_write(&(**i).lock);
            for (mdg_node** j = start; j != i; j++) {
                list_fin(
                    (list*)sbuffer_back(&tc->temp_buffer, sizeof(list)), true);
                sbuffer_remove_back(&tc->temp_buffer, sizeof(list));
            }
            return ERR;
        }
        *l = (**i).notify;
        list_init(&(**i).notify);
        rwlock_end_write(&(**i).lock);
    }
    bool ok = true;
    for (mdg_node** i = end - 1; i != start - 1; i--) {
        list* l = sbuffer_back(&tc->temp_buffer, sizeof(list));
        list_it it;
        list_it_begin(&it, l);
        while (ok) {
            ast_elem* dep = list_it_next(&it, l);
            if (!dep) break;
            assert(ast_elem_is_import_module(dep));
            sym_import_module* im = (sym_import_module*)dep;
            atomic_boolean_store(&im->done, true);
            mdg_node* dep_mdg = (mdg_node*)symbol_table_get_module_table(
                                    im->osym.sym.declaring_st)
                                    ->owning_node;
            if (sccd_run(
                    &tc->sccd, (mdg_node*)dep_mdg, SCCD_PP_DEP_GENERATED)) {
                ok = false;
            }
        }
        // we just replace the old notify list with a new one and free the old
        // one without reusing any allocations. we have SSO anyways and a big
        // "notify on resolved" list doesn't imply a big "notify on generated"
        // list
        list_fin(l, true);
        sbuffer_remove_back(&tc->temp_buffer, sizeof(list));
    }
    return OK;
}
// FIXME: rename this to add_external_requirements
int module_frame_require_requirements(
    module_frame* mf, mdg_node* n, thread_context* tc, bool in_pp,
    bool module_requred)
{
    file_require* r = mf->requires;
    // TODO: rethink this handled thing and make it apply to the pp
    while (*(void**)r && (in_pp || !r->handled)) {
        if ((!in_pp && r->is_pp) || (r->runtime && in_pp)) {
            r++;
            continue;
        }
        int res = file_map_head_require(
            r->fmh, tc->t, module_frame_get_smap(mf), r->srange, n,
            r->is_pp || in_pp, module_requred);
        r++;
        if (res) return res;
    }
    return OK;
}
int mdg_node_add_frame(mdg_node* n, module_frame* mf, thread_context* tc)
{
    int r;
    bool needed;
    rwlock_read(&n->lock);
    // the aseglist_add must be inside the locked region!
    // otherwise we find unnedded, exit lock,
    // another thread updates to needed, and than we add mf
    // --> missing requires
    r = aseglist_add(&n->module_frames, mf);
    needed = module_stage_requirements_needed(n->stage, NULL);
    rwlock_end_read(&n->lock);
    if (r) return r;
    // the actual require already added the 'require entry'
    // so we only do this when we might trigger file parsing
    if (needed) {
        r = module_frame_require_requirements(mf, n, tc, false, needed);
        if (r) return r;
    }
    return r;
}

int mdg_node_require_requirements(mdg_node* n, thread_context* tc, bool in_pp)
{
    // TODO: do we have to update our module stage from unneeded to parsing
    // here?
    if (in_pp) {
        bool pplr;
        rwlock_write(&n->lock);
        pplr = n->requested_for_pp;
        if (!pplr) n->requested_for_pp = true;
        rwlock_end_write(&n->lock);
        if (pplr) return OK;
    }
    aseglist_iterator it;
    aseglist_iterator_begin(&it, &n->module_frames);
    while (true) {
        module_frame* mf = aseglist_iterator_next(&it);
        if (!mf) break;
        module_frame_require_requirements(mf, n, tc, in_pp, true);
    }
    if (in_pp) {
        list_it it;
        list_it_begin(&it, &n->dependencies);
        while (true) {
            mdg_node* dep = list_it_next(&it, &n->dependencies);
            if (!dep) break;
            int res = mdg_node_require_requirements(dep, tc, true);
            if (res) return res;
        }
    }
    return OK;
}
typedef struct mdg_require_stack_node_s {
    mdg_node* node;
    list_rit deps_rit;
    bool exploratory_requirement;
    bool run_scc;
} mdg_require_stack_node;

int mdg_node_add_dependency(
    mdg_node* n, mdg_node* dependency, thread_context* tc)
{
    rwlock_write(&n->lock);
    int r = list_append(&n->dependencies, NULL, dependency);
    bool exploring;
    bool needed = module_stage_deps_needed(n->stage, &exploring);
    rwlock_end_write(&n->lock);
    if (!r && needed) {
        r = sccd_run(
            &tc->sccd, dependency,
            exploring ? SCCD_NODE_REQUIRE_EXPLORATION : SCCD_NODE_REQUIRE);
    }
    return r;
}
// this just checks for double module m  and extend m without module m for now
int mdg_final_sanity_check(module_dependency_graph* m, thread_context* tc)
{
    // write is necessary since we aren't satisfied with eventual
    // consistency
    mdght* h = mdg_start_write(m);
    mdght_iterator mdg_it;
    mdght_iterator_begin(&mdg_it, h);
    int res = OK;
    while (true) {
        mdg_node* n = mdght_iterator_next(&mdg_it);
        if (!n) break;
        // we still need to lock the stages since some final resolving might
        // still be going on
        rwlock_read(&n->lock);
        // we used to only do this for unneeded and awaiting deps but i don't
        // see why that would suffice
        module_frame* mod_frame = NULL;
        aseglist_iterator it;
        aseglist_iterator_begin(&it, &n->module_frames);
        module_frame* first_target = NULL;
        module_frame* i = aseglist_iterator_next(&it);
        first_target = i;
        while (i) {
            if (i->node.kind == MF_MODULE ||
                i->node.kind == MF_MODULE_GENERIC) {
                if (mod_frame != NULL) {
                    src_range_large srl;
                    src_range_unpack(i->node.srange, &srl);
                    src_range_large srl_mod;
                    src_range_unpack(mod_frame->node.srange, &srl_mod);
                    // these should always have a smap
                    assert(srl.smap && srl_mod.smap);
                    // since aseglist iterates backwards we reverse, so
                    // if it's in the same file the redeclaration is always
                    // below
                    error_log_report_annotated_twice(
                        tc->err_log, ES_RESOLVER, false, "module redeclared",
                        srl_mod.smap, srl_mod.start, srl_mod.end,
                        "redeclaration here", srl.smap, srl.start, srl.end,
                        "already declared here");
                    res = ERR;
                    break;
                }
                mod_frame = i;
            }
            i = aseglist_iterator_next(&it);
        }
        if (mod_frame == NULL && first_target != NULL) {
            src_range_large srl;
            src_range_unpack(first_target->node.srange, &srl);
            // THINK: maybe report extend count here or report all
            error_log_report_annotated(
                tc->err_log, ES_RESOLVER, false,
                "extend without module declaration", srl.smap, srl.start,
                srl.end, "extend here");
            res = ERR;
        }
        rwlock_end_read(&n->lock);
    }
    mdg_end_write(m);
    return res | atomic_sreg_load(&tc->t->error_code);
}
