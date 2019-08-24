#include "mdg.h"
#include "error_log.h"
#include "mdght.h"
#include "tauc.h"
#include "utils/allocator.h"
#include "utils/panic.h"
#include "utils/threading.h"
#include "utils/zero.h"

int mdg_fin_partial(mdg* m, int i, int r)
{
    switch (i) {
        case 0:
            mdg_node_fin(m->root_node);
            mdght_fin_contained_nodes(mdg_start_write(m));
            mdg_end_write(m);
        case 1: mdght_fin(&m->mdghts[1]);
        case 2: mdght_fin(&m->mdghts[0]);
        case 3: evmap2_fin(&m->evm);
        case 4: pool_fin(&m->node_pool);
        case 5: pool_fin(&m->ident_pool);
        case 6: atomic_ureg_fin(&m->node_ids);
    }
    return r;
}
mdg_node* mdg_node_create(mdg* m, string ident, mdg_node* parent);
int mdg_init(mdg* m)
{
    int r = atomic_ureg_init(&m->node_ids, 0);
    if (r) return r;
    r = pool_init(&m->ident_pool);
    if (r) return mdg_fin_partial(m, 6, r);
    r = pool_init(&m->node_pool);
    if (r) return mdg_fin_partial(m, 5, r);
    r = evmap2_init(&m->evm, MDG_MAX_CHANGES - MDG_MAX_CHANGES_PER_WRITE);
    if (r) return mdg_fin_partial(m, 4, r);
    r = mdght_init(&m->mdghts[0]);
    if (r) return mdg_fin_partial(m, 3, r);
    r = mdght_init(&m->mdghts[1]);
    if (r) return mdg_fin_partial(m, 2, r);
    m->root_node = mdg_node_create(m, string_from_cstr("_"), NULL);
    if (!m->root_node) return mdg_fin_partial(m, 1, ERR);
    m->root_node->stage = MS_PARSING;
    atomic_ureg_store(&m->root_node->unparsed_files, 1);
    m->change_count = 0;
    return 0;
}
void mdg_fin(mdg* m)
{
    mdg_fin_partial(m, 0, 0);
}

mdght* mdg_start_read(mdg* m)
{
    ureg id = evmap2_start_read(&m->evm);
    return &m->mdghts[id];
}
void mdg_end_read(mdg* m, mdght* h)
{
    evmap2_end_read(&m->evm, (h == &m->mdghts[0]) ? 0 : 1);
}

static int mdg_apply_changes(mdg* m, mdght* tgt)
{
    for (ureg i = 0; i < m->change_count; i++) {
        void* p = mdght_insert_at(tgt, m->changes[i].pos, m->changes[i].node);
        if (!p) return ERR;
    }
    m->change_count = 0;
    return OK;
}

mdght* mdg_start_write(mdg* m)
{
    ureg id;
    ureg changes = evmap2_start_write(&m->evm, &id);
    if (changes) {
        if (mdg_apply_changes(m, &m->mdghts[id])) return NULL;
    }
    m->change_count = 0;
    return &m->mdghts[id];
}
void mdg_end_write(mdg* m)
{
    evmap2_end_write(&m->evm);
}
void* mdg_node_partial_fin(mdg_node* n, int i)
{
    switch (i) {
        default:
        case 7: atomic_ureg_fin(&n->using_count);
        case 6: atomic_ureg_fin(&n->decl_count);
        case 5: atomic_ureg_fin(&n->unparsed_files);
        case 4: aseglist_fin(&n->notify);
        case 3: aseglist_fin(&n->dependencies);
        case 2: rwslock_fin(&n->stage_lock);
        case 1: aseglist_fin(&n->open_scopes);
    }
    return NULL;
}
// since this is called while inside mdg_write, there is no race
// on the memory pools
mdg_node* mdg_node_create(mdg* m, string ident, mdg_node* parent)
{
    mdg_node* n = pool_alloc(&m->node_pool, sizeof(mdg_node));
    if (!n) return NULL;
    n->elem.kind = ELEM_MDG_NODE;
    ureg identlen = string_len(ident);
    n->name = pool_alloc(&m->ident_pool, identlen + 1);
    n->id = atomic_ureg_inc(&m->node_ids);
    if (!n->name) return NULL;
    memcpy(n->name, ident.start, identlen);
    n->name[identlen] = '\0';
    n->parent = parent;
    int r = aseglist_init(&n->open_scopes);
    if (r) return NULL;
    r = rwslock_init(&n->stage_lock);
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
    n->stage = MS_UNNEEDED;
    n->symtab = NULL;
    return n;
}
static void free_body_symtabs(ast_node* node, body* b);
static void free_astn_symtabs(ast_node* n)
{
    if (!n) return;
    if (ast_elem_is_scope((ast_elem*)n)) {
        // these are parts of a module and therefore already handled
        if (!ast_elem_is_open_scope((ast_elem*)n)) {
            free_body_symtabs(n, &((scope*)n)->body);
        }
        else {
            return;
        }
    }
    switch (n->kind) {
        case EXPR_BREAK:
        case EXPR_RETURN: free_astn_symtabs(((expr_break*)n)->value); break;

        case EXPR_BLOCK: free_body_symtabs(n, &((expr_block*)n)->body); break;

        case EXPR_IF: {
            expr_if* ei = (expr_if*)n;
            free_astn_symtabs(ei->condition);
            free_astn_symtabs(ei->if_body);
            free_astn_symtabs(ei->else_body);
        } break;

        case EXPR_LOOP: free_body_symtabs(n, &((expr_loop*)n)->body); break;

        case EXPR_MACRO: {
            expr_macro* em = (expr_macro*)n;
            free_body_symtabs(n, &em->body);
            free_astn_symtabs((ast_node*)em->next);
        } break;

        case EXPR_PP: free_astn_symtabs(((expr_pp*)n)->pp_expr); break;

        case EXPR_MATCH: {
            expr_match* em = (expr_match*)n;
            free_astn_symtabs(em->match_expr);
            for (match_arm** ma = (match_arm**)em->body.elements; *ma != NULL;
                 ma++) {
                free_astn_symtabs((**ma).condition);
                free_astn_symtabs((**ma).value);
            }
        } break;

        default: break;
    }
}
static void free_body_symtabs(ast_node* node, body* b)
{
    // delete children first since children might contain parent symtab pointer
    for (ast_node** n = b->elements; *n != NULL; n++) {
        free_astn_symtabs(*n);
    }
    if (b->symtab && b->symtab->owning_node == (ast_elem*)node) {
        symbol_table_fin(b->symtab);
    }
}
void mdg_node_fin(mdg_node* n)
{
    symbol_table_fin(n->symtab);
    aseglist_iterator it;
    aseglist_iterator_begin(&it, &n->open_scopes);
    while (true) {
        open_scope* osc = aseglist_iterator_next(&it);
        if (!osc) break;
        free_body_symtabs((ast_node*)osc, &osc->scope.body);
    }
    mdg_node_partial_fin(n, 0);
}

mdg_node* mdg_get_node(mdg* m, mdg_node* parent, string ident)
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
            n = mdg_node_create(m, ident, parent);
            if (n == NULL) {
                mdg_end_write(m);
                return NULL;
            }
            *np = n;
            m->changes[m->change_count].pos = np - h->table_start;
            m->changes[m->change_count].node = n;
            m->change_count++;
            mdg_end_write(m);
        }
    }
    return n;
}

mdg_node*
mdg_add_open_scope(mdg* m, mdg_node* parent, open_scope* osc, string ident)
{
    mdg_node* n = mdg_get_node(m, parent, ident);
    if (!n) return n;
    rwslock_read(&n->stage_lock);
    bool unfound = (n->stage == MS_NOT_FOUND);
    rwslock_end_read(&n->stage_lock);
    if (unfound) {
        rwslock_write(&n->stage_lock);
        if (n->stage == MS_NOT_FOUND) n->stage = MS_PARSING;
        rwslock_end_write(&n->stage_lock);
    }
    aseglist_add(&n->open_scopes, osc);
    osc->scope.symbol.name = n->name;
    return n;
}

int mdg_node_parsed(mdg* m, mdg_node* n, thread_context* tc)
{
    bool run_scc = true;
    rwslock_write(&n->stage_lock);
    if (n->stage == MS_PARSING) {
        n->stage = MS_AWAITING_DEPENDENCIES;
    }
    else {
        assert(n->stage == MS_UNNEEDED);
        n->stage = MS_AWAITING_NEED;
        run_scc = false;
    }
    rwslock_end_write(&n->stage_lock);
    if (run_scc) return scc_detector_run(tc, n);
    return OK;
}
int mdg_node_file_parsed(mdg* m, mdg_node* n, thread_context* tc)
{
    ureg up = atomic_ureg_dec(&n->unparsed_files);
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
    return snd->id - fst->id;
}
#define SORT_NAME mdg_nodes
#define SORT_TYPE mdg_node*
#define SORT_CMP(x, y) compare_mdg_nodes(x, y)
#include "sort.h"

#define SCCD_ADDED_NOTIFICATION STATUS_1
#define SCCD_MISSING_IMPORT STATUS_2
#define SCCD_HANDLED STATUS_3

bool module_import_group_find_import(
    sym_import_group* ig, mdg_node* import, sym_import_module** tgt_sym)
{
    for (symbol* c = ig->children.symbols; c != NULL; c = c->next) {
        if (c->node.kind == SYM_IMPORT_SYMBOL) continue;
        if (c->node.kind == SYM_IMPORT_GROUP) {
            if (module_import_group_find_import(
                    (sym_import_group*)c, import, tgt_sym)) {
                return true;
            }
        }
        else {
            assert(c->node.kind == SYM_IMPORT_MODULE);
            if (((sym_import_module*)c)->target == import) {
                *tgt_sym = (sym_import_module*)c;
                return true;
            }
        }
    }
    return false;
}
bool scope_find_import(
    scope* s, mdg_node* import, sym_import_group** tgt_group,
    sym_import_module** tgt_sym)
{
    for (ast_node** n = s->body.elements; *n; n++) {
        if (ast_elem_is_scope((ast_elem*)*n)) {
            if (scope_find_import((scope*)*n, import, tgt_group, tgt_sym))
                return true;
        }
        else if ((**n).kind == SYM_IMPORT_GROUP) {
            sym_import_group* ig = (sym_import_group*)*n;
            if (module_import_group_find_import(ig, import, tgt_sym)) {
                *tgt_group = ig;
                return true;
            }
        }
        else if ((**n).kind == SYM_IMPORT_MODULE) {
            if (((sym_import_module*)*n)->target == import) {
                *tgt_group = NULL;
                *tgt_sym = (sym_import_module*)*n;
            }
        }
    }
    return false;
}
void mdg_node_find_import(
    mdg_node* m, mdg_node* import, sym_import_group** tgt_group,
    sym_import_module** tgt_sym, src_file** file)
{
    aseglist_iterator it;
    aseglist_iterator_begin(&it, &m->open_scopes);
    while (true) {
        open_scope* osc = aseglist_iterator_next(&it);
        if (!osc) break;
        if (scope_find_import(&osc->scope, import, tgt_group, tgt_sym)) {
            *file = open_scope_get_file(osc);
            return;
        }
        osc = (open_scope*)osc->scope.symbol.next;
    }
    panic("failed to find the source location of a missing import!");
}
void mdg_node_report_missing_import(
    thread_context* tc, mdg_node* m, mdg_node* import)
{
    sym_import_module* tgt_sym;
    sym_import_group* tgt_group;
    src_file* f;
    mdg_node_find_import(m, import, &tgt_group, &tgt_sym, &f);
    src_range_large tgt_group_srl, tgt_sym_srl;
    src_range_unpack(tgt_group->symbol.node.srange, &tgt_group_srl);
    src_range_unpack(tgt_sym->symbol.node.srange, &tgt_sym_srl);
    error_log_report_annotated_twice(
        &tc->error_log, ES_RESOLVER, false,
        "missing definition for imported module", f, tgt_sym_srl.start,
        tgt_sym_srl.end, "imported here", f, tgt_group_srl.start,
        tgt_group_srl.end, NULL);
}
int scc_detector_strongconnect(
    thread_context* tc, mdg_node* n, sccd_node* sn, mdg_node* caller)
{
    if (n->stage == MS_PARSING || n->stage == MS_RESOLVING ||
        n->stage == MS_NOT_FOUND) {
        if (n->stage == MS_NOT_FOUND) {
            mdg_node* par = n->parent;
            assert(par); // root is never NOT_FOUND
            while (true) {
                rwslock_read(&par->stage_lock);
                if (par->stage > MS_PARSING) {
                    // if we reach here all children on n's way up
                    // are not found. therefore the direct child of par
                    // is also not found, but we are done parsing par,
                    // so it can never be found. therefore we have an error
                    rwslock_end_read(&par->stage_lock);
                    rwslock_end_read(&n->stage_lock);
                    return SCCD_MISSING_IMPORT;
                }
                else if (n->parent->stage == MS_NOT_FOUND) {
                    rwslock_end_read(&par->stage_lock);
                    par = par->parent;
                }
                else {
                    // if this parent gets done we are sure
                    // that the entire unfound chain is invalid
                    int r = aseglist_add(&par->notify, caller);
                    rwslock_end_read(&par->stage_lock);
                    // if the one we actually need gets found
                    // we can continue
                    int r2 = aseglist_add(&n->notify, caller);
                    rwslock_end_read(&n->stage_lock);
                    if (r || r2) return ERR;
                    return SCCD_ADDED_NOTIFICATION;
                }
            }
        }
        else {
            int r = aseglist_add(&n->notify, caller);
            rwslock_end_read(&n->stage_lock);
            if (r) return ERR;
            return SCCD_ADDED_NOTIFICATION;
        }
    }
    // can't be in a circle with caller if it's not pending
    if (n->stage != MS_AWAITING_DEPENDENCIES) {
        sn->index = tc->sccd.dfs_start_index;
        sn->lowlink = UREG_MAX;
        rwslock_end_read(&n->stage_lock);
        return OK;
    }
    sn->lowlink = tc->sccd.dfs_index;
    sn->index = tc->sccd.dfs_index;
    tc->sccd.dfs_index++;
    stack_push(&tc->stack, n);
    aseglist_iterator it;
    aseglist_iterator_begin(&it, &n->dependencies);
    while (true) {
        mdg_node* m = aseglist_iterator_next(&it);
        if (!m) break;
        sccd_node* mn = scc_detector_get(&tc->sccd, m->id);
        if (!mn) return ERR;
        if (mn->index < tc->sccd.dfs_start_index) {
            rwslock_read(&m->stage_lock);
            int r = scc_detector_strongconnect(tc, m, mn, caller);
            if (r == SCCD_MISSING_IMPORT) {
                mdg_node_report_missing_import(tc, n, m);
                return SCCD_HANDLED;
            }
            if (r) {
                rwslock_end_read(&n->stage_lock);
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
        // and somebody else is already dealing with it
        // we must wait for the cycle to be resolved
        if (sn->lowlink != tc->sccd.dfs_start_index + 1) {
            int r = aseglist_add(&n->notify, caller);
            rwslock_end_read(&n->stage_lock);
            if (r) return ERR;
            return SCCD_ADDED_NOTIFICATION;
        }

        rwslock_end_read(&n->stage_lock);
        bool success = false;

        if (stack_peek(&tc->stack) == n) {
            stack_pop(&tc->stack);
            rwslock_write(&n->stage_lock);
            if (n->stage == MS_AWAITING_DEPENDENCIES) {
                n->stage = MS_RESOLVING;
                success = true;
            }
            rwslock_end_write(&n->stage_lock);
            sn->index = tc->sccd.dfs_start_index;
            if (success) {
                tauc_request_resolve_single(n);
            }
            return OK;
        }
        else {
            stack_state ss_end, ss_start;
            stack_state_save(&ss_end, &tc->stack);
            mdg_node* stack_mdgn = stack_pop(&tc->stack);
            ureg node_count = 1;
            do {
                stack_mdgn = stack_pop(&tc->stack);
                node_count++;
            } while (stack_mdgn != n);
            stack_state_save(&ss_start, &tc->stack);
            ureg list_size = node_count * sizeof(mdg_node*);
            mdg_node** node_list = tmalloc(list_size);
            if (!node_list) return ERR;
            stack_pop_to_list(
                &tc->stack, &ss_start, &ss_end, (void**)node_list);
            mdg_nodes_quick_sort(node_list, node_count);
            for (mdg_node** i = node_list; i != node_list + node_count; i++) {
                rwslock_write(&(**i).stage_lock);
                if ((**i).stage == MS_AWAITING_DEPENDENCIES) {
                    (**i).stage = MS_RESOLVING;
                    success = true;
                }
                rwslock_end_write(&(**i).stage_lock);
                sccd_node* in = scc_detector_get(&tc->sccd, (**i).id);
                if (!in) return ERR;
                in->index = tc->sccd.dfs_start_index;
                if (!success) break;
            }
            if (success) {
                tauc_request_resolve_multiple(
                    node_list, ptradd(node_list, list_size));
            }
            else {
                tfree(node_list);
            }
            return OK;
        }
    }
    rwslock_end_read(&n->stage_lock);
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
    rwslock_read(&n->stage_lock);
    if (n->stage != MS_AWAITING_DEPENDENCIES) { // is this really possible?
        rwslock_end_read(&n->stage_lock);
        return OK;
    }
    tc->sccd.dfs_start_index = tc->sccd.dfs_index;
    // the start index is used to indicate the node is off the stack
    tc->sccd.dfs_index++;
    sccd_node* sn = scc_detector_get(&tc->sccd, n->id);
    if (!sn) return ERR;
    stack_state ss;
    stack_state_save(&ss, &tc->stack);
    int r = scc_detector_strongconnect(tc, n, sn, n);
    if (r) stack_state_apply(&ss, &tc->stack);
    if (r == SCCD_ADDED_NOTIFICATION) return OK;
    return r;
}

void scc_detector_fin(scc_detector* d)
{
}

int mdg_node_resolved(mdg_node* n, thread_context* tc)
{
    rwslock_write(&n->stage_lock);
    n->stage = MS_GENERATING;
    rwslock_end_write(&n->stage_lock);
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
        rwslock_write(&(**i).stage_lock);
        (**i).stage = MS_GENERATING;
        rwslock_end_write(&(**i).stage_lock);
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

int mdg_node_require(mdg_node* n, thread_context* tc)
{
    mdg_node* parent = n->parent;
    // TODO: evaluate this. we don't really want all the parents
    mdg_node* start_node = n;
    bool run_scc = false;
    while (true) {
        rwslock_read(&n->stage_lock);
        bool needed = module_stage_needed(parent->stage);
        rwslock_end_read(&n->stage_lock);
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
    void** stack_head = tc->stack.head;
    while (true) {
        rwslock_write(&n->stage_lock);
        if (n->stage == MS_UNNEEDED) {
            ureg up = atomic_ureg_load(&n->unparsed_files);
            if (up == 0) {
                n->stage = MS_NOT_FOUND;
            }
            else {
                n->stage = MS_PARSING;
            }
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
                    rwslock_end_write(&n->stage_lock);
                    return ERR;
                }
                if (sn->index != tc->sccd.dfs_start_index) {
                    stack_push(&tc->stack, i);
                    sn->index = tc->sccd.dfs_start_index;
                }
            }
        }
        rwslock_end_write(&n->stage_lock);
        if (oscs) {
            aseglist_iterator oscs_it;
            aseglist_iterator_begin(&oscs_it, oscs);
            while (true) {
                open_scope* osc = aseglist_iterator_next(&oscs_it);
                if (!osc) break;
                file_require* r = osc->requires;
                while (*(void**)r) {
                    src_file_require(
                        r->file, open_scope_get_file(osc), r->srange, n);
                    r++;
                }
            }
        }
        while (true) {
            if (tc->stack.head == stack_head) {
                if (run_scc) return scc_detector_run(tc, start_node);
                return OK;
            }
            n = stack_pop(&tc->stack);
            rwslock_read(&n->stage_lock);
            bool needed = module_stage_needed(n->stage);
            rwslock_end_read(&n->stage_lock);
            if (!needed) break;
        }
    }
    assert(false);
}
int mdg_node_add_dependency(
    mdg_node* n, mdg_node* dependency, thread_context* tc)
{
    rwslock_read(&n->stage_lock);
    int r = aseglist_add(&n->dependencies, dependency);
    bool needed = module_stage_needed(n->stage);
    rwslock_end_read(&n->stage_lock);
    if (!r && needed) mdg_node_require(dependency, tc);
    return r;
}
int mdg_final_sanity_check(mdg* m, thread_context* tc)
{
    // write is necessary since we aren't satisfied with eventual consistency
    mdght* h = mdg_start_write(m);
    mdght_iterator it;
    mdght_iterator_begin(&it, h);
    int res = OK;
    while (true) {
        mdg_node* n = mdght_iterator_next(&it);
        if (!n) break;
        // we still need to lock the stages since some final resolving might
        // still be going on
        rwslock_read(&n->stage_lock);
        if (n->stage == MS_UNNEEDED || n->stage == MS_AWAITING_NEED) {
            open_scope* mod = NULL;
            aseglist_iterator it;
            aseglist_iterator_begin(&it, &n->open_scopes);
            open_scope* first_target = NULL;
            open_scope* i = aseglist_iterator_next(&it);
            first_target = i;
            while (i) {
                if (i->scope.symbol.node.kind == OSC_MODULE ||
                    i->scope.symbol.node.kind == OSC_MODULE_GENERIC) {
                    if (mod != NULL) {
                        src_range_large srl;
                        src_range_unpack(i->scope.symbol.node.srange, &srl);
                        src_range_large srl_mod;
                        src_range_unpack(
                            mod->scope.symbol.node.srange, &srl_mod);
                        // since aseglist iterates backwards we reverse, so if
                        // it's in the same file the redeclaration is always
                        // below
                        error_log_report_annotated_twice(
                            &tc->error_log, ES_RESOLVER, false,
                            "module redeclared", srl_mod.file, srl_mod.start,
                            srl_mod.end, "redeclaration here", srl.file,
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
                src_range_unpack(first_target->scope.symbol.node.srange, &srl);
                // THINK: maybe report extend count here or report all
                error_log_report_annotated(
                    &tc->error_log, ES_RESOLVER, false,
                    "extend without module declaration", srl.file, srl.start,
                    srl.end, "extend here");
                res = ERR;
            }
        }
        rwslock_end_read(&n->stage_lock);
    }
    mdg_end_write(m);
    return res;
}
