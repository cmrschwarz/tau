#include "mdg.h"
#include "error_log.h"
#include "mdght.h"
#include "utils/threading.h"
#include "utils/zero.h"
int mdg_fin_partial(mdg* m, int i, int r)
{
    switch (i) {
        case 0: mdght_fin(&m->mdghts[1]);
        case 1: mdght_fin(&m->mdghts[0]);
        case 2: evmap2_fin(&m->evm);
        case 3: pool_fin(&m->node_pool);
        case 4: pool_fin(&m->ident_pool);
    }
    return r;
}

int mdg_init(mdg* m)
{
    int r = pool_init(&m->ident_pool);
    if (r) return r;
    r = pool_init(&m->node_pool);
    if (r) return mdg_fin_partial(m, 4, r);
    r = evmap2_init(&m->evm, MDG_MAX_CHANGES - MDG_MAX_CHANGES_PER_WRITE);
    if (r) return mdg_fin_partial(m, 3, r);
    r = mdght_init(&m->mdghts[0]);
    if (r) return mdg_fin_partial(m, 2, r);
    r = mdght_init(&m->mdghts[1]);
    if (r) return mdg_fin_partial(m, 1, r);
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

static int mdg_apply_changes(mdg* m, mdght* tgt, ureg change_count)
{
    for (ureg i = 0; i < change_count; i++) {
        void* p = mdght_insert_at(tgt, m->changes[i].pos, m->changes[i].node);
        if (!p) return ERR;
    }
    return OK;
}

mdght* mdg_start_write(mdg* m)
{
    ureg id;
    ureg changes = evmap2_start_write(&m->evm, &id);
    if (changes) {
        if (mdg_apply_changes(m, &m->mdghts[id], changes)) return NULL;
    }
    return &m->mdghts[id];
}
void mdg_end_write(mdg* m)
{
    evmap2_end_write(&m->evm);
}
// since this is called while inside mdg_write, there is no race
// on the memory pools
mdg_node* mdg_node_create(mdg* m, string ident, mdg_node* parent, scope* tgt)
{
    mdg_node* n = pool_alloc(&m->node_pool, sizeof(mdg_node));
    if (!n) return NULL;
    ureg identlen = string_len(ident);
    n->name = pool_alloc(&m->ident_pool, identlen + 1);
    if (!n->name) return NULL;
    memcpy(n->name, ident.start, identlen);
    n->name[identlen] = '\0';
    n->parent = parent;
    int r = atomic_ptr_init(&n->targets, tgt);
    if (r) return NULL;
    r = rwslock_init(&n->lock);
    if (r) {
        atomic_ptr_fin(&n->targets);
        return NULL;
    }
    r = aseglist_init(&n->dependencies);
    if (r) {
        rwslock_fin(&n->lock);
        atomic_ptr_fin(&n->targets);
        return NULL;
    }
    r = atomic_ureg_init(&n->unparsed_files, 1);
    if (r) {
        aseglist_fin(&n->dependencies);
        rwslock_fin(&n->lock);
        atomic_ptr_fin(&n->targets);
        return NULL;
    }
    r = atomic_ureg_init(&n->stage, MS_GENERATING);
    if (r) {
        atomic_ureg_fin(&n->unparsed_files);
        aseglist_fin(&n->dependencies);
        rwslock_fin(&n->lock);
        atomic_ptr_fin(&n->targets);
        return NULL;
    }
    return n;
}
void mdg_node_add_target(mdg_node* n, scope* target)
{
    target->symbol.stmt.next = atomic_ptr_load(n->targets);
    while (!atomic_ptr_cas(
        &n->targets, (void**)&target->symbol.stmt.next, (void*)target)) {
    }
}
mdg_node*
mdg_add_module(mdg* m, mdg_node* parent, osc_module* mod, string ident)
{
    ureg hash = mdght_get_hash_str(parent, ident);
    mdght* h = mdg_start_read(m);
    mdg_node* n = mdght_get_str_ph(h, hash, parent, ident);
    mdg_end_read(m, h);
    if (n != NULL) {
        mdg_node_add_target(n, (scope*)mod);
    }
    else {
        h = mdg_start_write(m);
        mdg_node** np = mdght_get_str_raw_ph(h, hash, parent, ident);
        mdg_node* n = *np;
        if (n != NULL) {
            mdg_end_write(m);
            mdg_node_add_target(n, &mod->oscope.scope);
        }
        else {
            mod->oscope.scope.symbol.stmt.next = NULL;
            n = mdg_node_create(m, ident, parent, (scope*)mod);
            if (n == NULL) {
                mdg_end_write(m);
                return NULL;
            }
            *np = n;
            mdg_end_write(m);
        }
    }
    mod->oscope.scope.symbol.name = n->name;
    if (atomic_ureg_load(&n->stage) != MS_UNNEEDED) {
        stmt_flags_set_osc_required(&mod->oscope.scope.symbol.stmt.flags);
    }
    return n;
}
int mdg_add_dependency(mdg* m, mdg_node* n, mdg_node* dependency)
{
    rwslock_read(&n->lock);
    int r = aseglist_add(&n->dependencies, dependency);
    rwslock_end_read(&n->lock);
    return r;
}
mdg_group* mdg_group_new()
{
    mdg_group* g = tmalloc(sizeof(mdg_group));
    if (!g) return g;
    if (aseglist_init(&g->members)) {
        tfree(g);
        return NULL;
    }
    g->tarjan_node.index = 0;
    return g;
}
void mdg_group_fin(mdg_group* g)
{
    aseglist_fin(&g->members);
    tfree(g);
    tfree(g);
}
static int replace_group(mdg_group* old, mdg_group* new)
{
    int r = 0;
    aseglist_iterator it;
    aseglist_iterator_begin(&it, &old->members);
    while (true) {
        mdg_node* next = aseglist_iterator_next(&it);
        if (!next) break;
        r = aseglist_add(&new->members, next);
        if (r) break;
        next->group = new;
    }
    aseglist_iterator_fin(&it);
    mdg_group_fin(old);
    return r;
}
typedef enum tarjan_res {
    TARJ_RES_SUCCESS = 0,
    TARJ_RES_UNMET_DEPENDENCY,
    TARJ_RES_ERROR,
} tarjan_res;

static inline void reset_tarjan(mdg_node* n)
{
    n->tarjan_node.index = 0;
}

// UGLY: avoid reset by decrementing not on stack value instead
static void reset_tarjan_rec(mdg_node* n)
{
    if (n->tarjan_node.index == 0) return;
    reset_tarjan(n);
    rwslock_read(&n->lock);
    aseglist_iterator it;
    aseglist_iterator_begin(&it, &n->dependencies);
    while (true) {
        mdg_node* next = aseglist_iterator_next(&it);
        if (!next) break;
        reset_tarjan_rec(next);
    }
    rwslock_end_read(&n->lock);
}
static tarjan_res
tarjan_step(ureg* index, mdg_node* node, mdg_group** group, ureg* group_size);

static tarjan_res tarjan_iterate(
    aseglist_iterator* it, ureg* index, mdg_node* node, mdg_group** group,
    ureg* group_size)
{
    mdg_node* next = aseglist_iterator_next(it);
    if (!next) {
        if (*group_size > 1 && !*group) {
            *group = mdg_group_new();
            if (!*group) return TARJ_RES_ERROR;
        }
        return TARJ_RES_SUCCESS;
    }
    else {
        rwslock_read(&next->lock);
        module_stage stage = atomic_ureg_load(&next->stage);
        if (stage <= MS_PARSING) {
            int r = aseglist_add(&next->notifiy, node);
            rwslock_end_read(&next->lock);
            if (r) return TARJ_RES_ERROR;
            return TARJ_RES_UNMET_DEPENDENCY;
        }
        ureg next_index = next->tarjan_node.index;
        if (next_index == 0) {
            tarjan_res r = tarjan_step(index, next, group, group_size);
            if (r) {
                reset_tarjan_rec(next);
                rwslock_end_read(&next->lock);
                return r;
            }
            if (next->tarjan_node.lowlink < node->tarjan_node.lowlink) {
                node->tarjan_node.lowlink = next->tarjan_node.lowlink;
            }
        }
        else if (next_index != UREG_MAX) {
            if (next_index < node->tarjan_node.lowlink) {
                node->tarjan_node.lowlink = next_index;
            }
        }
        tarjan_res r = tarjan_iterate(it, index, node, group, group_size);
        if (r) {
            reset_tarjan(next);
            rwslock_end_read(&next->lock);
            return r;
        }
        if (*group_size > 1 && next_index != UREG_MAX) {
            int r = 0;
            if (next->group == NULL) {
                next->group = *group;
                r = (aseglist_add(&(**group).members, next));
            }
            else if (next->group != *group) {
                r = (replace_group(next->group, *group));
            }
            if (r) {
                reset_tarjan_rec(next);
                rwslock_end_read(&next->lock);
                return TARJ_RES_ERROR;
            }
        }
        rwslock_end_read(&next->lock);
        return TARJ_RES_SUCCESS;
    }
}
static tarjan_res
tarjan_step(ureg* index, mdg_node* node, mdg_group** group, ureg* group_size)
{
    node->tarjan_node.index = *index;
    node->tarjan_node.lowlink = *index;
    (*index)++;
    aseglist_iterator it;
    aseglist_iterator_begin(&it, &node->dependencies);
    mdg_group* gprev = *group;
    ureg group_size_old = *group_size;
    tarjan_res r = tarjan_iterate(&it, index, node, group, group_size);
    aseglist_iterator_fin(&it);
    if (r || node->tarjan_node.lowlink == node->tarjan_node.index) {
        node->tarjan_node.index = UREG_MAX;
        if (*group) {
            node->group = *group;
            aseglist_iterator it;
            aseglist_iterator_begin(&it, &(**group).members);
            while (true) {
                mdg_node* i = aseglist_iterator_next(&it);
                if (!i) break;
                i->tarjan_node.index = UREG_MAX;
            }
            aseglist_iterator_fin(&it);
            if (aseglist_add(&(**group).members, node)) return TARJ_RES_ERROR;
            *group_size = group_size_old;
            *group = gprev;
        }
    }
    else {
        *group_size += 1;
    }
    return r;
}

int mdg_node_file_parsed(mdg* m, mdg_node* n)
{
    rwslock_read(&n->lock);
    ureg up = atomic_ureg_dec(&n->unparsed_files) == 1;
    if (up == 1) {
        ureg idx = 1;
        ureg group_size = 1;
        tarjan_res r = tarjan_step(&idx, n, &n->group, &group_size);
        rwslock_end_read(&n->lock);
        if (!r) reset_tarjan_rec(n);
    }
    else {
        rwslock_end_read(&n->lock);
    }
    return OK;
}