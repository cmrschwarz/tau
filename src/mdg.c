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
// since this is called while inside mdg_write, there is no race
// on the memory pools
mdg_node* mdg_node_create(mdg* m, string ident, mdg_node* parent)
{
    mdg_node* n = pool_alloc(&m->node_pool, sizeof(mdg_node));
    if (!n) return NULL;
    ureg identlen = string_len(ident);
    n->name = pool_alloc(&m->ident_pool, identlen + 1);
    n->id = atomic_ureg_inc(&m->node_ids);
    if (!n->name) return NULL;
    memcpy(n->name, ident.start, identlen);
    n->name[identlen] = '\0';
    n->parent = parent;
    int r = atomic_ptr_init(&n->targets, NULL);
    if (r) return NULL;
    r = rwslock_init(&n->stage_lock);
    if (r) {
        atomic_ptr_fin(&n->targets);
        return NULL;
    }
    r = aseglist_init(&n->dependencies);
    if (r) {
        rwslock_fin(&n->stage_lock);
        atomic_ptr_fin(&n->targets);
        return NULL;
    }
    r = atomic_ureg_init(&n->unparsed_files, 0);
    if (r) {
        aseglist_fin(&n->dependencies);
        rwslock_fin(&n->stage_lock);
        atomic_ptr_fin(&n->targets);
        return NULL;
    }
    r = aseglist_init(&n->notify);
    if (r) {
        atomic_ureg_fin(&n->unparsed_files);
        aseglist_fin(&n->dependencies);
        rwslock_fin(&n->stage_lock);
        atomic_ptr_fin(&n->targets);
        return NULL;
    }
    n->stage = MS_UNNEEDED;
    return n;
}
void mdg_node_fin(mdg_node* n)
{
    aseglist_fin(&n->notify);
    atomic_ureg_fin(&n->unparsed_files);
    aseglist_fin(&n->dependencies);
    rwslock_fin(&n->stage_lock);
    atomic_ptr_fin(&n->targets);
}
void mdg_node_add_target(mdg_node* n, scope* target)
{
    target->symbol.stmt.next = atomic_ptr_load(&n->targets);
    while (!atomic_ptr_cas(
        &n->targets, (void**)&target->symbol.stmt.next, (void*)target)) {
    }
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

    mdg_node_add_target(n, (scope*)osc);
    osc->scope.symbol.name = n->name;
    return n;
}

int mdg_node_parsed(mdg* m, mdg_node* n, thread_context* tc)
{
    rwslock_write(&n->stage_lock);
    n->stage = MS_AWAITING_DEPENDENCIES;
    rwslock_end_write(&n->stage_lock);
    return scc_detector_run(tc, n);
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
        // allocate new stack
        scc_stack_entry* stack_new = (scc_stack_entry*)pool_alloc(
            d->mem_src, bucketable_cap_new * sizeof(scc_stack_entry));
        if (!stack_new) return ERR;
        ureg stack_size = ptrdiff(d->stack_head, d->stack);
        memcpy(stack_new, d->stack, stack_size);
        d->stack_head = ptradd(stack_new, stack_size);
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
        // reuse old stack for buffers
        recyclable_bucket_cap = d->bucketable_node_capacity;
        recyclable_bucket_size = recyclable_bucket_cap * sizeof(mdg_node*);
        memset(d->stack, 0, recyclable_bucket_size);
        for (ureg i = 0; i < recyclable_bucket_size; i += SCCD_BUCKET_SIZE) {
            buckets_bucket_new[bucket_count] = ptradd(d->stack, i);
            bucket_count++;
        }
        d->allocated_node_count += recyclable_bucket_cap;
        // apply new buffers
        d->stack = stack_new;
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
    d->stack = (scc_stack_entry*)pool_alloc(
        mem_src, d->bucketable_node_capacity * sizeof(scc_stack_entry));

    if (!d->stack) return ERR;
    d->stack_head = d->stack;
    d->dfs_index = 1; // 0 means index remained from previous run
    d->allocated_node_count = 0;

    d->mem_src = mem_src;
    return OK;
}
int compare_mdg_nodes(const scc_stack_entry fst, const scc_stack_entry snd)
{
    return snd.mdgn->id - fst.mdgn->id;
}
#define SORT_NAME mdg_nodes
#define SORT_TYPE scc_stack_entry
#define SORT_CMP(x, y) compare_mdg_nodes(x, y)
#include "sort.h"

#define SCCD_ADDED_NOTIFICATION STATUS_1
#define SCCD_MISSING_IMPORT STATUS_2
#define SCCD_HANDLED STATUS_3

bool module_import_find_import(
    module_import* mi, mdg_node* import, module_import** tgt)
{
    if (mi->tgt == import) {
        *tgt = mi;
        return true;
    }
    module_import* ni = mi->nested_imports;
    if (!ni) return false;
    while (*(void**)ni) {
        if (module_import_find_import(ni, import, tgt)) return true;
        ni++;
    }
    return false;
}
bool scope_find_import(
    scope* s, mdg_node* import, stmt_import** tgt, module_import** tgt_sym)
{
    stmt* st = s->body.children;
    while (st) {
        switch (st->type) {
            case STMT_IMPORT: {
                stmt_import* i = (stmt_import*)st;
                if (module_import_find_import(
                        &i->module_import, import, tgt_sym)) {
                    *tgt = i;
                    return true;
                }
            } break;
            case SC_FUNC:
            case SC_FUNC_GENERIC:
            case SC_STRUCT:
            case SC_STRUCT_GENERIC:
            case SC_TRAIT:
            case SC_TRAIT_GENERIC:
            case OSC_EXTEND:
            case OSC_EXTEND_GENERIC:
            case OSC_MODULE:
            case OSC_MODULE_GENERIC: {
                if (scope_find_import((scope*)st, import, tgt, tgt_sym))
                    return true;
            } break;
            default: break;
        }
        st = st->next;
    }
    return false;
}
void mdg_node_find_import(
    mdg_node* m, mdg_node* import, stmt_import** tgt, module_import** tgt_sym,
    src_file** file)
{
    open_scope* osc = atomic_ptr_load(&m->targets);
    while (osc) {
        if (scope_find_import(&osc->scope, import, tgt, tgt_sym)) {
            *file = scope_get_file((scope*)osc);
            return;
        }
        osc = (open_scope*)osc->scope.symbol.stmt.next;
    }
    panic("failed to find import!");
}
void mdg_node_report_missing_import(
    thread_context* tc, mdg_node* m, mdg_node* import)
{
    stmt_import* tgt;
    module_import* tgt_sym;
    src_file* f;
    mdg_node_find_import(m, import, &tgt, &tgt_sym, &f);
    src_range_large tgt_srl, tgt_sym_srl;
    src_range_unpack(tgt->stmt.srange, &tgt_srl);
    src_range_unpack(tgt_sym->srange, &tgt_sym_srl);
    error_log_report_annotated_twice(
        &tc->error_log, ES_RESOLVER, false,
        "missing definition for imported module", f, tgt_sym_srl.start,
        tgt_sym_srl.end, "imported here", tgt_srl.start, tgt_srl.end, NULL);
}
int scc_detector_strongconnect(
    thread_context* tc, mdg_node* n, sccd_node* sn, mdg_node* caller)
{
    if (n->stage == MS_PARSING || n->stage == MS_RESOLVING ||
        n->stage == MS_NOT_FOUND) {
        if (n->stage == MS_NOT_FOUND) {
            rwslock_read(&n->parent->stage_lock);
            if (n->parent->stage > MS_PARSING) {
                rwslock_end_read(&n->parent->stage_lock);
                rwslock_end_read(&n->stage_lock);
                return SCCD_MISSING_IMPORT;
            }
            rwslock_end_read(&n->parent->stage_lock);
        }
        int r = aseglist_add(&n->notify, caller);
        rwslock_end_read(&n->stage_lock);
        if (r) return ERR;
        return SCCD_ADDED_NOTIFICATION;
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
    tc->sccd.stack_head->mdgn = n;
    tc->sccd.stack_head++;
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
        tc->sccd.stack_head--;
        rwslock_end_read(&n->stage_lock);
        bool success = false;
        if (tc->sccd.stack_head->mdgn == n) {
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
            scc_stack_entry* end = tc->sccd.stack_head + 1;
            do {
                tc->sccd.stack_head--;
            } while (tc->sccd.stack_head->mdgn != n);
            scc_stack_entry* start = tc->sccd.stack_head;
            mdg_nodes_quick_sort(start, end - start);
            for (scc_stack_entry* i = start; i != end; i++) {
                rwslock_write(&i->mdgn->stage_lock);
                if (i->mdgn->stage == MS_AWAITING_DEPENDENCIES) {
                    i->mdgn->stage = MS_RESOLVING;
                    success = true;
                }
                rwslock_end_write(&i->mdgn->stage_lock);
                sccd_node* in = scc_detector_get(&tc->sccd, i->mdgn->id);
                if (!in) return ERR;
                in->index = tc->sccd.dfs_start_index;
                if (!success) break;
            }
            if (success) {
                ureg list_size = ptrdiff(end, start);
                mdg_node** node_list = tmalloc(list_size);
                if (!node_list) return ERR;
                memcpy(node_list, start, list_size);
                tauc_request_resolve_multiple(
                    node_list, ptradd(node_list, list_size));
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
    if (n->stage != MS_AWAITING_DEPENDENCIES) {
        rwslock_end_read(&n->stage_lock);
        return OK;
    }
    tc->sccd.dfs_start_index = tc->sccd.dfs_index;
    // the start index is used to indicate the node is off the stack
    tc->sccd.dfs_index++;
    sccd_node* sn = scc_detector_get(&tc->sccd, n->id);
    if (!sn) return ERR;
    int r = scc_detector_strongconnect(tc, n, sn, n);
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
    while (true) {
        rwslock_read(&n->stage_lock);
        bool unneded = (parent->stage == MS_UNNEEDED);
        rwslock_end_read(&n->stage_lock);
        if (!unneded) break;
        int r = mdg_node_require(parent, tc);
        if (r) return r;
        parent = parent->parent;
        if (parent == NULL) break;
    }
    scc_detector_housekeep_ids(&tc->sccd);
    tc->sccd.dfs_start_index = tc->sccd.dfs_index;
    tc->sccd.dfs_index++;
    open_scope* tgts;
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
            tgts = atomic_ptr_load(&n->targets);
            aseglist_iterator it;
            aseglist_iterator_begin(&it, &n->dependencies);
            while (true) {
                mdg_node* i = aseglist_iterator_next(&it);
                if (!i) break;
                sccd_node* sn = scc_detector_get(&tc->sccd, i->id);
                if (!sn) {
                    rwslock_end_write(&n->stage_lock);
                    tc->sccd.stack_head = tc->sccd.stack;
                    return ERR;
                }
                if (sn->index != tc->sccd.dfs_start_index) {
                    tc->sccd.stack_head->mdgn = i;
                    tc->sccd.stack_head++;
                    sn->index = tc->sccd.dfs_start_index;
                }
            }
        }
        else {
            tgts = NULL;
        }
        rwslock_end_write(&n->stage_lock);
        while (tgts) {
            file_require* r = tgts->requires;
            while (*(void**)r) {
                src_file_require(
                    r->file, scope_get_file((scope*)tgts), r->srange, n);
                r++;
            }
            tgts++;
        }
        while (true) {
            if ((tc->sccd.stack_head == tc->sccd.stack)) return OK;
            tc->sccd.stack_head--;
            n = tc->sccd.stack_head->mdgn;
            rwslock_read(&n->stage_lock);
            bool unneded = (n->stage == MS_UNNEEDED);
            rwslock_end_read(&n->stage_lock);
            if (unneded) break;
        }
    }
    return OK;
}
int mdg_node_add_dependency(
    mdg_node* n, mdg_node* dependency, thread_context* tc)
{
    rwslock_read(&n->stage_lock);
    int r = aseglist_add(&n->dependencies, dependency);
    bool needed = (n->stage != MS_UNNEEDED);
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
        if (n->stage == MS_UNNEEDED) {
            open_scope* mod = NULL;
            open_scope* tgt = (open_scope*)atomic_ptr_load(&n->targets);
            open_scope* i = tgt;
            while (i) {
                if (i->scope.symbol.stmt.type == OSC_MODULE ||
                    i->scope.symbol.stmt.type == OSC_MODULE_GENERIC) {
                    if (mod != NULL) {
                        src_range_large srl;
                        src_range_unpack(i->scope.symbol.stmt.srange, &srl);
                        // TODO: implement errors with annotations in multiple
                        // files so we can annotate the first declaration
                        error_log_report_annotated(
                            &tc->error_log, ES_RESOLVER, false,
                            "module redeclared", scope_get_file(&i->scope),
                            srl.start, srl.end, "second declaration here");
                        res = ERR;
                        break;
                    }
                    mod = i;
                }
                i = (open_scope*)tgt->scope.symbol.stmt.next;
            }
            if (mod == NULL && tgt != NULL) {
                src_range_large srl;
                src_range_unpack(tgt->scope.symbol.stmt.srange, &srl);
                error_log_report_annotated(
                    &tc->error_log, ES_RESOLVER, false,
                    "extend without module declaration",
                    scope_get_file(&tgt->scope), srl.start, srl.end,
                    "extend here");
                res = ERR;
            }
        }
        rwslock_end_read(&n->stage_lock);
    }
    mdg_end_write(m);
    return res;
}
