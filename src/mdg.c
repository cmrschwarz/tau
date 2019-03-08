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
        case 3: atomic_pool_fin(&m->node_pool);
        case 4: pool_fin(&m->ident_pool);
    }
    return r;
}

int mdg_init(mdg* m, thread_allocator* tal)
{
    int r = pool_init(&m->ident_pool, tal);
    if (r) return r;
    r = atomic_pool_init(&m->node_pool, tal);
    if (r) return mdg_fin_partial(m, 4, r);
    r = evmap2_init(&m->evm, MDG_MAX_CHANGES - MDG_MAX_CHANGES_PER_WRITE);
    if (r) return mdg_fin_partial(m, 3, r);
    r = mdght_init(&m->mdghts[0], tal);
    if (r) return mdg_fin_partial(m, 2, r);
    r = mdght_init(&m->mdghts[1], tal);
    if (r) return mdg_fin_partial(m, 1, r);
    m->tal = tal;
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
mdg_node* mdg_node_create(mdg* m, string ident, mdg_node* parent, scope* tgt)
{
    mdg_node* n = atomic_pool_alloc(&m->node_pool, sizeof(mdg_node));
    if (!n) return NULL;
    n->name = pool_alloc(&m->ident_pool, sizeof(mdg_node));
    if (!n->name) return NULL;
    n->parent = parent;
    int r = atomic_ptr_init(n->targets, tgt);
    if (r) return NULL;
    r = rwslock_init(&n->lock);
    if (r) {
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
mdg_node* mdg_add_scope(mdg* m, mdg_node* parent, sc_module* mod, string ident)
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
            mdg_node_add_target(n, &mod->scope);
        }
        else {
            mod->scope.symbol.stmt.next = NULL;
            n = mdg_node_create(m, ident, parent, (scope*)mod);
            if (n == NULL) {
                mdg_end_write(m);
                return NULL;
            }
            *np = n;
            mdg_end_write(m);
        }
    }
    mod->scope.symbol.name = n->name;
    return n;
}

int mdg_node_file_parsed(mdg* m, mdg_node* n)
{
    return OK;
}
int mdg_node_import_parsed(mdg* m, mdg_node* n)
{
    return OK;
}
int mdg_node_import_imports_parsed(mdg* m, mdg_node* n)
{
    return OK;
}