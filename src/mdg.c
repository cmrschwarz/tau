#include "mdg.h"
#include "error_log.h"
#include "mdght.h"
#include "tauc.h"
#include "utils/allocator.h"
#include "utils/panic.h"
#include "utils/threading.h"
#include "utils/zero.h"
#include <assert.h>

int mdg_fin_partial(module_dependency_graph* m, int i, int r)
{
    switch (i) {
        case 0:
            mdg_node_fin(m->root_node);
            mdght_fin_contained_nodes(mdg_start_write(m));
            mdg_end_write(m);
            // fallthrough
        case 1: mdght_fin(&m->mdghts[1]); // fallthrough
        case 2: mdght_fin(&m->mdghts[0]); // fallthrough
        case 3: evmap2_fin(&m->evm); // fallthrough
        case 4: pool_fin(&m->node_pool); // fallthrough
        case 5: pool_fin(&m->ident_pool); // fallthrough
        case 6: atomic_ureg_fin(&m->node_ids); // fallthrough
        default: break;
    }
    return r;
}
mdg_node* mdg_node_create(
    module_dependency_graph* m, string ident, mdg_node* parent,
    module_stage initial_stage);
int mdg_init(module_dependency_graph* m)
{
    int r = atomic_ureg_init(&m->node_ids, 0);
    if (r) return r;
    r = pool_init(&m->ident_pool);
    if (r) return mdg_fin_partial(m, 6, r);
    r = pool_init(&m->node_pool);
    if (r) return mdg_fin_partial(m, 5, r);
    r = evmap2_init(&m->evm, MDG_MAX_CHANGES);
    if (r) return mdg_fin_partial(m, 4, r);
    r = mdght_init(&m->mdghts[0]);
    if (r) return mdg_fin_partial(m, 3, r);
    r = mdght_init(&m->mdghts[1]);
    if (r) return mdg_fin_partial(m, 2, r);
    m->root_node = mdg_node_create(m, string_from_cstr("_"), NULL, MS_PARSING);
    if (!m->root_node) return mdg_fin_partial(m, 1, ERR);
    // this gets increased when we use src_file_require on the root file
    atomic_ureg_store(&m->root_node->unparsed_files, 0);
    m->change_count = 0;
    return 0;
}
void mdg_fin(module_dependency_graph* m)
{
    mdg_fin_partial(m, 0, 0);
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
        default:
        case 7: atomic_ureg_fin(&n->using_count); // fallthrough
        case 6: atomic_ureg_fin(&n->decl_count); // fallthrough
        case 5: atomic_ureg_fin(&n->unparsed_files); // fallthrough
        case 4: aseglist_fin(&n->notify); // fallthrough
        case 3: aseglist_fin(&n->dependencies); // fallthrough
        case 2: rwlock_fin(&n->lock); // fallthrough
        case 1: aseglist_fin(&n->open_scopes); // fallthrough
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
    int r = aseglist_init(&n->open_scopes);
    if (r) return NULL;
    r = rwlock_init(&n->lock);
    if (r) return mdg_node_partial_fin(n, 1);
    r = aseglist_init(&n->dependencies);
    if (r) return mdg_node_partial_fin(n, 2);
    r = aseglist_init(&n->notify);
    if (r) return mdg_node_partial_fin(n, 3);
    r = atomic_ureg_init(&n->unparsed_files, 0);
    if (r) return mdg_node_partial_fin(n, 4);
    r = atomic_ureg_init(&n->decl_count, 0);
    if (r) return mdg_node_partial_fin(n, 5);
    r = atomic_ureg_init(&n->using_count, 0);
    if (r) return mdg_node_partial_fin(n, 6);
    n->stage = initial_stage;
    n->symtab = NULL;
    n->ppe_stage = PPES_UNNEEDED;
    n->pp_libs_requested = false;
    return n;
}
void free_body_symtabs(ast_node* node, ast_body* b);
static void free_astn_symtabs(ast_node* n)
{
    if (!n) return;
    if (ast_elem_is_scope((ast_elem*)n)) {
        // these are parts of a module and therefore already handled
        if (!ast_elem_is_open_scope((ast_elem*)n)) {
            free_body_symtabs(n, &((scope*)n)->body);
            if (n->kind == SC_STRUCT_GENERIC) {
                for (sc_struct_generic_inst* sgi =
                         ((sc_struct_generic*)n)->instances;
                     sgi;
                     sgi = (sc_struct_generic_inst*)sgi->st.sb.sc.sym.next) {
                    free_astn_symtabs((ast_node*)sgi);
                }
            }
        }
        return;
    }
    switch (n->kind) {
        case EXPR_BREAK:
        case EXPR_RETURN: free_astn_symtabs(((expr_break*)n)->value); break;

        case EXPR_BLOCK: free_body_symtabs(n, &((expr_block*)n)->body); break;

        case SYM_IMPORT_PARENT:
        case SYM_IMPORT_GROUP: {
            // skip unnamed groups
            if (((symbol*)n)->name == NULL) break;
            // TODO: handle non resolved case
            symbol_table* st = (n->kind == SYM_IMPORT_PARENT)
                                   ? (((sym_import_parent*)n)->children.symtab)
                                   : (((sym_import_group*)n)->children.symtab);
            symtab_it it = symtab_it_make(st);
            for (symbol* s = symtab_it_next(&it); s != NULL;
                 s = symtab_it_next(&it)) {
                free_astn_symtabs((ast_node*)s);
            }
            symbol_table_fin(st);
        } break;

        case EXPR_IF: {
            expr_if* ei = (expr_if*)n;
            free_astn_symtabs(ei->condition);
            free_astn_symtabs(ei->if_body);
            free_astn_symtabs(ei->else_body);
        } break;
        case EXPR_LOOP: free_body_symtabs(n, &((expr_loop*)n)->body); break;

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
        case STMT_USING: {
            free_astn_symtabs(((stmt_using*)n)->target);
        } break;
        case SYM_NAMED_USING: {
            free_astn_symtabs(((sym_named_using*)n)->target);
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
        case EXPR_LITERAL: break;
        case EXPR_PASTE_EVALUATION: {
            expr_paste_evaluation* epe = (expr_paste_evaluation*)n;
            free_astn_symtabs(epe->expr);
            free_astn_symtabs(epe->pe.source_pp_expr);
        } break;
        case STMT_PASTE_EVALUATION: {
            stmt_paste_evaluation* spe = (stmt_paste_evaluation*)n;
            free_body_symtabs(n, &spe->body);
        } break;
        case ARRAY_DECL: {
            array_decl* ad = (array_decl*)n;
            free_astn_symtabs(ad->length_spec);
            free_astn_symtabs(ad->base_type);
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
        aseglist_iterator_begin(&it, &n->open_scopes);
        while (true) {
            open_scope* osc = aseglist_iterator_next(&it);
            if (!osc) break;
            free_body_symtabs((ast_node*)osc, &osc->sc.body);
        }
    }
    if (n->stage >= MS_RESOLVING) {
        symbol_table_fin(n->symtab);
    }
    mdg_node_partial_fin(n, 0);
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

mdg_node*
mdg_found_node(module_dependency_graph* m, mdg_node* parent, string ident)
{
    mdg_node* n = mdg_get_node(m, parent, ident, MS_UNNEEDED);
    if (!n) return NULL;
    atomic_ureg_inc(&n->unparsed_files);
    rwlock_read(&n->lock);
    bool found = (n->stage != MS_NOT_FOUND);
    rwlock_end_read(&n->lock);
    if (found) return n;
    rwlock_write(&n->lock);
    if (n->stage == MS_NOT_FOUND) n->stage = MS_PARSING;
    rwlock_end_write(&n->lock);
    return n;
}

int mdg_node_parsed(module_dependency_graph* m, mdg_node* n, thread_context* tc)
{
    bool run_scc = true;
    rwlock_write(&n->lock);
    if (n->stage == MS_PARSING) {
        n->stage = MS_AWAITING_DEPENDENCIES;
    }
    else if (n->stage == MS_UNNEEDED) {
        n->stage = MS_AWAITING_NEED;
        run_scc = false;
    }
    else {
        // this happens for example when a module is redeclared
        assert(n->stage == MS_AWAITING_NEED);
        run_scc = false;
    }
    rwlock_end_write(&n->lock);
    if (run_scc) return scc_detector_run(tc, n);
    return OK;
}
int mdg_node_file_parsed(
    module_dependency_graph* m, mdg_node* n, thread_context* tc)
{
    ureg up = atomic_ureg_dec(&n->unparsed_files);
    assert(up != 0);
    if (up == 1) return mdg_node_parsed(m, n, tc);
    return OK;
}

#define SCCD_BUCKET_CAP 16
#define SCCD_BUCKET_SIZE (SCCD_BUCKET_CAP * sizeof(sccd_node))
static inline int scc_detector_expand(scc_detector* d, ureg node_id)
{
    ureg node_count = node_id + 1;
    if (d->allocated_node_count > node_count) return OK;
    ureg bucket_count = d->allocated_node_count / SCCD_BUCKET_CAP;
    if (node_count > d->bucketable_node_capacity) {
        // figure out new size
        ureg bucketable_cap_new = d->bucketable_node_capacity;
        do {
            bucketable_cap_new *= 2;
        } while (node_count > bucketable_cap_new);
        // allocate new buffers buffer
        sccd_node** buckets_bucket_new = (sccd_node**)pool_alloc(
            d->mem_src,
            bucketable_cap_new / SCCD_BUCKET_CAP * sizeof(sccd_node*));
        if (!buckets_bucket_new) return ERR;
        memcpy(
            buckets_bucket_new, d->sccd_node_buckets,
            bucket_count * sizeof(sccd_node*));
        // reuse old buffers buffer for buffers
        ureg recyclable_bucket_cap =
            d->bucketable_node_capacity / SCCD_BUCKET_CAP;
        ureg recyclable_bucket_size =
            recyclable_bucket_cap * sizeof(sccd_node*);
        memset(d->sccd_node_buckets, 0, recyclable_bucket_size);
        for (ureg i = 0; i < recyclable_bucket_size; i += SCCD_BUCKET_SIZE) {
            buckets_bucket_new[bucket_count] = ptradd(d->sccd_node_buckets, i);
            bucket_count++;
        }
        d->allocated_node_count += recyclable_bucket_cap;
        // apply new buffers
        d->bucketable_node_capacity = bucketable_cap_new;
        d->sccd_node_buckets = buckets_bucket_new;
    }
    while (node_count > d->allocated_node_count) {
        sccd_node* b = pool_alloc(d->mem_src, SCCD_BUCKET_SIZE);
        if (!b) return ERR;
        memset(b, 0, SCCD_BUCKET_SIZE);
        d->sccd_node_buckets[bucket_count] = b;
        bucket_count++;
        d->allocated_node_count += SCCD_BUCKET_CAP;
    }
    return OK;
}
static inline sccd_node* scc_detector_get(scc_detector* d, ureg id)
{
    if (scc_detector_expand(d, id)) return NULL;
    return &d->sccd_node_buckets[id / SCCD_BUCKET_CAP][id % SCCD_BUCKET_CAP];
}
int scc_detector_init(scc_detector* d, pool* mem_src)
{
    d->sccd_node_buckets = (sccd_node**)pool_alloc(mem_src, SCCD_BUCKET_SIZE);
    d->bucketable_node_capacity =
        SCCD_BUCKET_SIZE / sizeof(sccd_node*) * SCCD_BUCKET_CAP;
    if (!d->sccd_node_buckets) return ERR;
    d->dfs_index = 1; // 0 means index remained from previous run
    d->allocated_node_count = 0;

    d->mem_src = mem_src;
    return OK;
}
int compare_mdg_nodes(const mdg_node* fst, const mdg_node* snd)
{
    return fst->id - snd->id;
}
#define SORT_NAME mdg_nodes
#define SORT_TYPE mdg_node*
#define SORT_CMP(x, y) compare_mdg_nodes(x, y)
#include "sort.h"

#define SCCD_ADDED_NOTIFICATION STATUS_1
#define SCCD_MISSING_IMPORT STATUS_2
#define SCCD_HANDLED STATUS_3

bool module_import_group_find_import(
    sym_import_group* ig, mdg_node* import, sym_import_group** tgt_group,
    symbol** tgt_sym)
{
    if (ig->parent_mdgn == import) {
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
            if (((sym_import_module*)c)->target == import) {
                *tgt_sym = (symbol*)c;
                *tgt_group = ig;
                return true;
            }
        }
    }
    return false;
}
bool scope_find_import(
    scope* s, mdg_node* import, sym_import_group** tgt_group, symbol** tgt_sym)
{
    for (ast_node** n = s->body.elements; *n; n++) {
        if (ast_elem_is_scope((ast_elem*)*n)) {
            if (scope_find_import((scope*)*n, import, tgt_group, tgt_sym)) {
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
            if (((sym_import_module*)*n)->target == import) {
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
    aseglist_iterator_begin(&it, &m->open_scopes);
    while (true) {
        open_scope* osc = aseglist_iterator_next(&it);
        if (!osc) break;
        if (scope_find_import(&osc->sc, import, tgt_group, tgt_sym)) {
            *smap = open_scope_get_smap(osc);
            return;
        }
        osc = (open_scope*)osc->sc.sym.next;
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
    src_range_unpack(tgt_group->sym.node.srange, &tgt_group_srl);
    error_log_report_annotated_twice(
        tc->err_log, ES_RESOLVER, false,
        "missing definition for imported module", smap, tgt_sym_srl.start,
        tgt_sym_srl.end, "imported here", smap, tgt_group_srl.start,
        tgt_group_srl.end, NULL);
}
int scc_detector_strongconnect(
    thread_context* tc, mdg_node* n, sccd_node* sn, mdg_node* caller)
{
    if (n->stage == MS_NOT_FOUND) {
        mdg_node* par = n->parent;
        while (true) {
            assert(par); // root is never NOT_FOUND
            rwlock_read(&par->lock);
            if (par->stage > MS_PARSING) {
                // if we reach here all children on n's way up
                // are not found. therefore the direct child of par
                // is also not found, but we are done parsing par,
                // so it can never be found. therefore we have an error
                rwlock_end_read(&par->lock);
                rwlock_end_read(&n->lock);
                return SCCD_MISSING_IMPORT;
            }
            if (par->stage == MS_NOT_FOUND) {
                rwlock_end_read(&par->lock);
                par = par->parent;
            }
            else {
                // if this parent gets done we are sure
                // that the entire unfound chain is invalid
                int r = aseglist_add(&par->notify, caller);
                rwlock_end_read(&par->lock);
                // if the one we actually need gets found
                // we can continue
                int r2 = aseglist_add(&n->notify, caller);
                rwlock_end_read(&n->lock);
                if (r || r2) return ERR;
                return SCCD_ADDED_NOTIFICATION;
            }
        }
    }
    else if (n->stage == MS_PARSING || n->stage == MS_RESOLVING) {
        int r = aseglist_add(&n->notify, caller);
        rwlock_end_read(&n->lock);
        if (r) return ERR;
        return SCCD_ADDED_NOTIFICATION;
    }

    // can't be in a circle with caller if it's not pending
    if (n->stage != MS_AWAITING_DEPENDENCIES) {
        sn->index = tc->sccd.dfs_start_index;
        sn->lowlink = UREG_MAX;
        rwlock_end_read(&n->lock);
        return OK;
    }
    sn->lowlink = tc->sccd.dfs_index;
    sn->index = tc->sccd.dfs_index;
    tc->sccd.dfs_index++;
    stack_push(&tc->tempstack, n);
    aseglist_iterator it;
    aseglist_iterator_begin(&it, &n->dependencies);
    while (true) {
        mdg_node* m = aseglist_iterator_next(&it);
        if (!m) break;
        sccd_node* mn = scc_detector_get(&tc->sccd, m->id);
        if (!mn) return ERR;
        if (mn->index < tc->sccd.dfs_start_index) {
            rwlock_read(&m->lock);
            int r = scc_detector_strongconnect(tc, m, mn, caller);
            if (r == SCCD_MISSING_IMPORT) {
                mdg_node_report_missing_import(tc, n, m);
                return SCCD_HANDLED;
            }
            if (r) {
                rwlock_end_read(&n->lock);
                return r;
            }
            if (mn->lowlink < sn->index) sn->lowlink = mn->lowlink;
        }
        else if (mn->index != tc->sccd.dfs_start_index) {
            if (mn->index < sn->lowlink) sn->lowlink = mn->index;
        }
    }
    if (sn->lowlink == sn->index) {
        // the caller isn't part of the cycle so it depends on it
        // we must wait for the cycle to be resolved
        int ret = OK;
        if (sn->lowlink != tc->sccd.dfs_start_index + 1) {
            int r = aseglist_add(&n->notify, caller);
            rwlock_end_read(&n->lock);
            if (r) return ERR;
            ret = SCCD_ADDED_NOTIFICATION;
        }
        else {
            rwlock_end_read(&n->lock);
        }

        bool success = false;

        if (stack_peek(&tc->tempstack) == n) {
            stack_pop(&tc->tempstack);
            rwlock_write(&n->lock);
            if (n->stage == MS_AWAITING_DEPENDENCIES) {
                n->stage = MS_RESOLVING;
                success = true;
            }
            rwlock_end_write(&n->lock);
            sn->index = tc->sccd.dfs_start_index;
            if (success) {
                tauc_request_resolve_single(tc->t, n);
            }
            return ret;
        }
        stack_state ss_end, ss_start;
        stack_state_save(&ss_end, &tc->tempstack);
        mdg_node* stack_mdgn = stack_pop(&tc->tempstack);
        ureg node_count = 1;
        do {
            stack_mdgn = stack_pop(&tc->tempstack);
            node_count++;
        } while (stack_mdgn != n);
        stack_state_save(&ss_start, &tc->tempstack);
        ureg list_size = node_count * sizeof(mdg_node*);
        mdg_node** node_list = tmalloc(list_size);
        if (!node_list) return ERR;
        stack_pop_to_list(
            &tc->tempstack, &ss_start, &ss_end, (void**)node_list);
        mdg_nodes_quick_sort(node_list, node_count);
        for (mdg_node** i = node_list; i != node_list + node_count; i++) {
            rwlock_write(&(**i).lock);
            if ((**i).stage == MS_AWAITING_DEPENDENCIES) {
                (**i).stage = MS_RESOLVING;
                success = true;
            }
            rwlock_end_write(&(**i).lock);
            sccd_node* in = scc_detector_get(&tc->sccd, (**i).id);
            if (!in) return ERR;
            in->index = tc->sccd.dfs_start_index;
            if (!success) break;
        }
        if (success) {
            tauc_request_resolve_multiple(
                tc->t, node_list, ptradd(node_list, list_size));
        }
        else {
            tfree(node_list);
        }
        return ret;
    }
    rwlock_end_read(&n->lock);
    return OK;
}
static inline void scc_detector_housekeep_ids(scc_detector* d)
{
    // reset the dfs_index sometimes to avoid running out of ids
    if (UREG_MAX - d->dfs_index < UREG_MAX / sizeof(mdg_node)) {
        d->dfs_index = 1;
        for (sccd_node** b = d->sccd_node_buckets;
             b < d->sccd_node_buckets +
                     d->bucketable_node_capacity / SCCD_BUCKET_CAP;
             b++) {
            for (sccd_node* n = *b; n < *b + SCCD_BUCKET_CAP; n++) {
                n->lowlink = 0;
            }
        }
    }
}
int scc_detector_run(thread_context* tc, mdg_node* n)
{
    scc_detector_housekeep_ids(&tc->sccd);
    rwlock_read(&n->lock);
    if (n->stage != MS_AWAITING_DEPENDENCIES) {
        // this happens when the dependency found out by itself that we are done
        // and started resolving
        rwlock_end_read(&n->lock);
        return OK;
    }
    tc->sccd.dfs_start_index = tc->sccd.dfs_index;
    // the start index is used to indicate the node is off the stack
    tc->sccd.dfs_index++;
    sccd_node* sn = scc_detector_get(&tc->sccd, n->id);
    if (!sn) return ERR;
    stack_state ss;
    stack_state_save(&ss, &tc->tempstack);
    int r = scc_detector_strongconnect(tc, n, sn, n);
    if (r) stack_state_apply(&ss, &tc->tempstack);
    if (r == SCCD_ADDED_NOTIFICATION) return OK;
    return r;
}

void scc_detector_fin(scc_detector* d)
{
}

int mdg_node_resolved(mdg_node* n, thread_context* tc)
{
    rwlock_write(&n->lock);
    n->stage = MS_GENERATING;
    rwlock_end_write(&n->lock);
    // TODO: clear notify before actually notifying, since necessary
    // notifications will be readded and we don't want this to explode
    aseglist_iterator it;
    aseglist_iterator_begin(&it, &n->notify);
    while (true) {
        mdg_node* dep = aseglist_iterator_next(&it);
        if (!dep) break;
        if (scc_detector_run(tc, dep)) return ERR;
    }
    return OK;
}
int mdg_nodes_resolved(mdg_node** start, mdg_node** end, thread_context* tc)
{
    for (mdg_node** i = start; i != end; i++) {
        rwlock_write(&(**i).lock);
        (**i).stage = MS_GENERATING;
        rwlock_end_write(&(**i).lock);
    }
    for (mdg_node** i = start; i != end; i++) {
        aseglist_iterator it;
        aseglist_iterator_begin(&it, &(**i).notify);
        while (true) {
            mdg_node* dep = aseglist_iterator_next(&it);
            if (!dep) break;
            if (scc_detector_run(tc, dep)) return ERR;
        }
    }
    return OK;
}
int mdg_node_generated(mdg_node* n, thread_context* tc, bool pp_generated)
{
    rwlock_write(&n->lock);
    n->stage = MS_DONE;
    n->ppe_stage = pp_generated ? PPES_DONE : PPES_SKIPPED;
    rwlock_end_write(&n->lock);
    assert(pp_generated || n->parent == NULL); // TODO
    // TODO: clear notify before actually notifying, since necessary
    // notifications will be readded and we don't want this to explode
    aseglist_iterator it;
    aseglist_iterator_begin(&it, &n->notify);
    while (true) {
        ast_elem* dep = aseglist_iterator_next(&it);
        if (!dep) break;
        if (dep->kind == ELEM_MDG_NODE) {
            if (scc_detector_run(tc, (mdg_node*)dep)) return ERR;
        }
        else if (dep->kind == SYM_IMPORT_GROUP) {
            sym_import_group* ig = (sym_import_group*)dep;
            atomic_boolean_store(&ig->done, true);
        }
        else if (dep->kind == SYM_IMPORT_MODULE) {
            sym_import_module* im = (sym_import_module*)dep;
            atomic_boolean_store(&im->done, true);
            // TODO: maybe some condition variable or similar to avoid
            // busy waiting here
        }
        else {
            assert(false);
        }
    }
    return OK;
}
int mdg_node_add_osc(mdg_node* n, open_scope* osc, tauc* t)
{
    int r;
    bool needed;
    rwlock_read(&n->lock);
    // the aseglist_add must be inside the locked region!
    // otherwise we find unnedded, exit lock,
    // another thread updates to needed, and than we add osc
    // --> missing requires
    r = aseglist_add(&n->open_scopes, osc);
    needed = module_stage_needed(n->stage);
    rwlock_end_read(&n->lock);
    if (r) return r;
    if (needed) {
        file_require* r = osc->requires;
        while (*(void**)r && !r->handled) {
            file_map_head_require(
                r->fmh, t, open_scope_get_smap(osc), r->srange, n, r->in_ppl);
            r++;
        }
    }
    return r;
}
// make sure the required libraries of an mdg node are loaded
int mdg_node_require_requirements(mdg_node* n, thread_context* tc, bool in_pp)
{
    if (in_pp) {
        bool pplr;
        rwlock_write(&n->lock);
        pplr = n->pp_libs_requested;
        if (!pplr) n->pp_libs_requested = true;
        rwlock_end_write(&n->lock);
        if (pplr) return OK;
    }
    aseglist_iterator it;
    aseglist_iterator_begin(&it, &n->open_scopes);
    while (true) {
        open_scope* osc = aseglist_iterator_next(&it);
        if (!osc) break;
        file_require* r = osc->requires;
        while (*(void**)r) {
            int res = file_map_head_require(
                r->fmh, tc->t, open_scope_get_smap(osc), r->srange, n,
                r->in_ppl || in_pp);
            r++;
            if (res) return res;
        }
    }
    if (in_pp) {
        aseglist_iterator_begin(&it, &n->dependencies);
        while (true) {
            mdg_node* dep = aseglist_iterator_next(&it);
            if (!dep) break;
            int res = mdg_node_require_requirements(dep, tc, true);
            if (res) return res;
        }
    }
    return OK;
}
int mdg_node_require(mdg_node* n, thread_context* tc)
{
    mdg_node* parent = n->parent;
    // TODO: evaluate this. we don't really want all the parents
    mdg_node* start_node = n;
    bool run_scc = false;
    while (true) {
        rwlock_read(&n->lock);
        bool needed = module_stage_needed(parent->stage);
        rwlock_end_read(&n->lock);
        if (needed) break;
        int r = mdg_node_require(parent, tc);
        if (r) return r;
        parent = parent->parent;
        if (parent == NULL) break;
    }
    scc_detector_housekeep_ids(&tc->sccd);
    tc->sccd.dfs_start_index = tc->sccd.dfs_index;
    tc->sccd.dfs_index++;
    aseglist* oscs;
    // since the stack is segmented there is no risk of this getting invalidated
    void** stack_head = tc->tempstack.head;
    while (true) {
        rwlock_write(&n->lock);
        if (n->stage == MS_UNNEEDED) {
            n->stage = MS_PARSING;
            oscs = &n->open_scopes;
        }
        else if (n->stage == MS_AWAITING_NEED) {
            n->stage = MS_AWAITING_DEPENDENCIES;
            oscs = &n->open_scopes;
            run_scc = true;
        }
        else {
            oscs = NULL;
        }

        if (oscs) {
            aseglist_iterator it;
            aseglist_iterator_begin(&it, &n->dependencies);
            while (true) {
                mdg_node* i = aseglist_iterator_next(&it);
                if (!i) break;
                sccd_node* sn = scc_detector_get(&tc->sccd, i->id);
                if (!sn) {
                    rwlock_end_write(&n->lock);
                    return ERR;
                }
                if (sn->index != tc->sccd.dfs_start_index) {
                    stack_push(&tc->tempstack, i);
                    sn->index = tc->sccd.dfs_start_index;
                }
            }
        }
        int r = OK;
        if (oscs) {
            r = mdg_node_require_requirements(n, tc, false);
        }
        rwlock_end_write(&n->lock);
        if (r) return r;
        while (true) {
            if (tc->tempstack.head == stack_head) {
                if (run_scc) return scc_detector_run(tc, start_node);
                return OK;
            }
            n = stack_pop(&tc->tempstack);
            rwlock_read(&n->lock);
            bool needed = module_stage_needed(n->stage);
            rwlock_end_read(&n->lock);
            if (!needed) break;
        }
    }
    assert(false);
}
int mdg_node_add_dependency(
    mdg_node* n, mdg_node* dependency, thread_context* tc)
{
    rwlock_read(&n->lock);
    int r = aseglist_add(&n->dependencies, dependency);
    bool needed = module_stage_needed(n->stage);
    rwlock_end_read(&n->lock);
    if (!r && needed) mdg_node_require(dependency, tc);
    return r;
}
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
        if (n->stage == MS_UNNEEDED || n->stage == MS_AWAITING_NEED) {
            open_scope* mod = NULL;
            aseglist_iterator it;
            aseglist_iterator_begin(&it, &n->open_scopes);
            open_scope* first_target = NULL;
            open_scope* i = aseglist_iterator_next(&it);
            first_target = i;
            while (i) {
                if (i->sc.sym.node.kind == OSC_MODULE ||
                    i->sc.sym.node.kind == OSC_MODULE_GENERIC) {
                    if (mod != NULL) {
                        src_range_large srl;
                        src_range_unpack(i->sc.sym.node.srange, &srl);
                        src_range_large srl_mod;
                        src_range_unpack(mod->sc.sym.node.srange, &srl_mod);
                        // these should always have a smap
                        assert(srl.smap && srl_mod.smap);
                        // since aseglist iterates backwards we reverse, so
                        // if it's in the same file the redeclaration is always
                        // below
                        error_log_report_annotated_twice(
                            tc->err_log, ES_RESOLVER, false,
                            "module redeclared", srl_mod.smap, srl_mod.start,
                            srl_mod.end, "redeclaration here", srl.smap,
                            srl.start, srl.end, "already declared here");
                        res = ERR;
                        break;
                    }
                    mod = i;
                }
                i = aseglist_iterator_next(&it);
            }
            if (mod == NULL && first_target != NULL) {
                src_range_large srl;
                src_range_unpack(first_target->sc.sym.node.srange, &srl);
                // THINK: maybe report extend count here or report all
                error_log_report_annotated(
                    tc->err_log, ES_RESOLVER, false,
                    "extend without module declaration", srl.smap, srl.start,
                    srl.end, "extend here");
                res = ERR;
            }
        }
        rwlock_end_read(&n->lock);
    }
    mdg_end_write(m);
    return res;
}
