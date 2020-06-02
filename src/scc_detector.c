#include "scc_detector.h"
#include "thread_context.h"
#include "tauc.h"

int sccd_run(scc_detector* sccd, mdg_node* origin);
#define SCCD_BUCKET_CAP 16
#define SCCD_BUCKET_SIZE (SCCD_BUCKET_CAP * sizeof(sccd_node))

static inline int compare_mdg_nodes(const mdg_node* fst, const mdg_node* snd)
{
    return (fst->id < snd->id) ? -1 : (fst->id == snd->id) ? 0 : 1;
}
#define SORT_NAME mdg_nodes
#define SORT_TYPE mdg_node*
#define SORT_CMP(x, y) compare_mdg_nodes(x, y)
#include "sort.h"

#define SCCD_ADDED_NOTIFICATION STATUS_1
#define SCCD_MISSING_IMPORT STATUS_2
#define SCCD_HANDLED STATUS_3

int sccd_init(scc_detector* sccd, thread_context* tc)
{
    sccd->tc = tc;
    sccd->sccd_node_buckets =
        (sccd_node**)pool_alloc(&sccd->tc->permmem, SCCD_BUCKET_SIZE);
    if (!sccd->sccd_node_buckets) return ERR;
    sccd->bucketable_node_capacity =
        SCCD_BUCKET_SIZE / sizeof(sccd_node*) * SCCD_BUCKET_CAP;
    sccd->dfs_index = 1; // 0 means index remained from previous run
    sccd->allocated_node_count = 0;
    return OK;
}
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
            &d->tc->permmem,
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
        sccd_node* b = pool_alloc(&d->tc->permmem, SCCD_BUCKET_SIZE);
        if (!b) return ERR;
        memset(b, 0, SCCD_BUCKET_SIZE);
        d->sccd_node_buckets[bucket_count] = b;
        bucket_count++;
        d->allocated_node_count += SCCD_BUCKET_CAP;
    }
    return OK;
}
sccd_node* sccd_get(scc_detector* d, ureg id)
{
    if (scc_detector_expand(d, id)) return NULL;
    return &d->sccd_node_buckets[id / SCCD_BUCKET_CAP][id % SCCD_BUCKET_CAP];
}
void sccd_housekeep_ids(scc_detector* d)
{
    // reset the dfs_index when the remaining id space is smaller then the
    // maximum number of possible nodes (crude heuristik: *could fit in
    // memory*). this should almost never happen
    if (d->dfs_index > UREG_MAX - UREG_MAX / sizeof(mdg_node)) {
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
void sccd_release_buffer(scc_detector* sccd)
{
    while (true) {
        sccd_stack_entry* se =
            sbuffer_back(&sccd->tc->temp_buffer, sizeof(sccd_stack_entry));
        bool done = (se == sccd->origin_se);
        sbuffer_remove_back(&sccd->tc->temp_buffer, sizeof(sccd_stack_entry));
        if (done) break;
    }
}
void sccd_release_stack(scc_detector* sccd)
{
    while (true) {
        mdg_node* n = stack_pop(&sccd->tc->temp_stack);
        if (n == sccd->origin_se->mdgn) break;
    }
}
void sccd_release(scc_detector* sccd)
{
    sccd_release_buffer(sccd);
    sccd_release_stack(sccd);
}
int sccd_handle_not_found(scc_detector* sccd, mdg_node* n)
{
    mdg_node* origin = sccd->origin_se->mdgn;
    sccd_release(sccd);
    mdg_node* par = n->parent;
    while (true) {
        assert(par); // root is never NOT_FOUND
        rwlock_read(&par->lock);
        // TODO: what do we do if the parent had an error?
        if (par->stage == MS_DONE) {
            // if we reach here all children on n's way up
            // are not found. therefore the direct child of par
            // is also not found, but we are done with generating par,
            // so it can never be found. therefore we have an error
            rwlock_end_read(&par->lock);
            rwlock_read(&n->lock);
            if (n->stage == MS_NOT_FOUND) {
                rwlock_end_read(&n->lock);
                return SCCD_MISSING_IMPORT;
            }
            else {
                return sccd_run(sccd, origin);
            }
        }
        if (par->stage == MS_NOT_FOUND) {
            rwlock_end_read(&par->lock);
            par = par->parent;
        }
        else {
            assert(par->stage == MS_GENERATING || par->stage == MS_PARSING);
            rwlock_end_read(&par->lock);
            rwlock_write(&par->lock);
            int r;
            if (par->stage == MS_GENERATING || par->stage == MS_PARSING) {
                r = list_append(&par->notify, NULL, n);
                rwlock_end_write(&par->lock);
                if (r) return ERR;
                rwlock_write(&n->lock);
                if (n->stage != MS_NOT_FOUND) {
                    rwlock_end_write(&n->lock);
                    return sccd_run(sccd, origin);
                }
                r = list_append(&n->notify, NULL, origin);
                rwlock_end_write(&n->lock);
                if (r) return ERR;
                return SCCD_ADDED_NOTIFICATION;
            }
            else {
                return sccd_run(sccd, origin);
            }
        }
    }
}
int sccd_emit_lone_mdgn(scc_detector* sccd, sccd_stack_entry* se_curr)
{
    mdg_node* n = stack_pop(&sccd->tc->temp_stack);
    assert(n == se_curr->mdgn);
    bool outdated = false;
    rwlock_write(&n->lock);
    if (list_length(&n->dependencies) != se_curr->deps_count) {
        outdated = true;
    }
    else if (n->stage == MS_AWAITING_DEPENDENCIES) {
        n->stage = MS_RESOLVING;
    }
    else {
        outdated = true;
    }
    rwlock_end_write(&n->lock);
    sbuffer_remove_back(&sccd->tc->temp_buffer, sizeof(sccd_stack_entry));
    if (outdated) return OK;
    return tauc_request_resolve_single(sccd->tc->t, n);
}
int sccd_emit_mdg(scc_detector* sccd, sccd_stack_entry* se_curr)
{
    ureg node_count = se_curr->scc_elem_count;
    if (node_count == 1) return sccd_emit_lone_mdgn(sccd, se_curr);
    ureg deps_count_now = 0;
    ureg deps_count_expected = se_curr->deps_count;
    mdg_node* lowlink = se_curr->mdgn;
    sbuffer_remove_back(&sccd->tc->temp_buffer, sizeof(sccd_stack_entry));
    mdg_node** nodes = tmalloc(node_count * sizeof(mdg_node*));
    mdg_node** nodes_end = nodes + node_count;
    mdg_node** ni = nodes;
    mdg_node* n;
    do {
        assert(ni < nodes_end);
        n = stack_pop(&sccd->tc->temp_stack);
        *(ni++) = n;
    } while (n != lowlink);
    assert(ni == nodes_end);
    mdg_nodes_quick_sort(nodes, node_count);

    bool outdated = false;
    for (ni = nodes; ni != nodes_end; ni++) {
        n = *ni;
        rwlock_write(&n->lock);
        if (n->stage == MS_AWAITING_DEPENDENCIES) {
            n->stage = MS_RESOLVING;
        }
        else {
            outdated = true;
            break;
        }
        deps_count_now += list_length(&n->dependencies);
        if (n == lowlink) break;
    }
    if (outdated || deps_count_now != deps_count_expected) {
        // rollback. somebody else is doing our job. this is NOT an error
        for (mdg_node** ni_redo = nodes; ni_redo != ni; ni_redo++) {
            n = *ni_redo;
            n->stage = MS_AWAITING_DEPENDENCIES;
            rwlock_end_write(&n->lock);
        }
        tfree(nodes);
        return OK;
    }
    for (ni = nodes; ni != nodes_end; ni++) {
        rwlock_end_write(&(**ni).lock);
    }
    return tauc_request_resolve_multiple(sccd->tc->t, nodes, nodes_end);
}
void sccd_new_ctx(scc_detector* sccd)
{
    sccd_housekeep_ids(sccd);
    sccd->dfs_start_index = sccd->dfs_index;
    // the start index is used to indicate the node is off the stack
    sccd->dfs_index++;
    // always push origin so we know when to stop the pop(TM) in release
}
int sccd_prepare(
    scc_detector* sccd, mdg_node* n, bool propagate_required, bool exploratory)
{
    rwlock_read(&n->lock);
    if (n->stage != MS_AWAITING_DEPENDENCIES) {
        // this happens when the dependency found out by itself that we are
        // done and started resolving
        rwlock_end_read(&n->lock);
        return OK;
    }
    list_rit rit;
    list_rit_begin_at_end(&rit, &n->dependencies);
    rwlock_end_read(&n->lock);
    sccd_stack_entry* se =
        sbuffer_append(&sccd->tc->temp_buffer, sizeof(sccd_stack_entry));
    if (!se) return ERR;
    sccd_node* sn = sccd_get(sccd, n->id);
    int r = stack_push(&sccd->tc->temp_stack, n);
    if (!sn || r) {
        sbuffer_remove_back(&sccd->tc->temp_buffer, sizeof(sccd_stack_entry));
        return ERR;
    }
    se->sn = sn;
    se->children_rit = rit;
    se->mdgn = n;
    se->deps_count = 0;
    se->scc_elem_count = 1;
    se->propagate_required = propagate_required;
    se->exploratory = false;
    se->dependant_to_notify = NULL;
    sccd->origin_se = se;
    sccd->propagate_required = propagate_required;
    sccd->exploratory = exploratory;
    sccd->awaiting_node = n;
    return OK;
}
int sccd_handle_in_progress(scc_detector* sccd, mdg_node* n)
{
    mdg_node* origin = sccd->origin_se->mdgn;
    rwlock_end_read(&n->lock);
    sccd_release_buffer(sccd);
    mdg_node* prev_mdgn = n;
    while (true) {
        mdg_node* n = stack_pop(&sccd->tc->temp_stack);
        if (n == sccd->origin_se->mdgn) break;
        prev_mdgn = n;
    }
    assert(prev_mdgn != sccd->origin_se->mdgn); // can't be in progress
    rwlock_write(&prev_mdgn->lock);
    if (prev_mdgn->stage != MS_PARSING && prev_mdgn->stage != MS_RESOLVING) {
        // the thing changed underneath us, redo. TODO: avoid complete redo
        rwlock_end_write(&prev_mdgn->lock);
        return sccd_run(sccd, origin);
    }
    // we get notified once our direct dependency updates
    // either that dep is in a mdg with us, than it doesn't matter if we
    // don't get updated until it does or it isn't in which case we will get
    // updates
    int r = list_append(&prev_mdgn->notify, NULL, origin);
    rwlock_end_write(&n->lock);
    if (r) return ERR;
    return SCCD_ADDED_NOTIFICATION;
}
sccd_stack_entry*
sccd_push_node(scc_detector* sccd, mdg_node* n, sccd_node* sn, list_rit* rit)
{
    sn->lowlink = sccd->dfs_index;
    sn->index = sccd->dfs_index;
    sccd->dfs_index++;
    sccd_stack_entry* se =
        sbuffer_append(&sccd->tc->temp_buffer, sizeof(sccd_stack_entry));
    int r = stack_push(&sccd->tc->temp_stack, n);
    if (!se || r) {
        // release cleans up correctly no matter which one failed
        sccd_release(sccd);
        return NULL;
    }
    // if it's awaiting dependencies it always HAS dependencies, so there's no
    // need for some kind of leaf node optimization
    se->children_rit = rit;
    se->mdgn = n;
    se->sn = sn;
    se->deps_count = 0;
    se->scc_elem_count = 1;
    se->propagate_required = sccd->propagate_required;
    se->exploratory = sccd->exploratory;
    se->awaiting_parent = NULL;
    return se;
}
int sccd_handle_node_propagating_required(
    scc_detector* sccd, mdg_node* n, sccd_node* sn, sccd_stack_entry** next_se)
{

    bool notified_people = false;
    const bool explore = sccd->exploratory;
    bool explore_parents = false;
    bool propagate_required = false;
    bool awaiting = false;
    bool available = false;
    aseglist* mfs;
    mdg_node* notifier;
    list_rit deps_iter;
    bool rwlock_write(&n->lock);
    int r = OK;
    switch (n->stage) {
        case MS_UNFOUND_UNNEEDED: {
            n->stage = explore ? MS_UNFOUND_EXPLORATION : MS_UNFOUND;
            explore_parents = true;
        } break;
        case MS_UNFOUND_EXPLORATION: {
            if (!explore) {
                n->stage = MS_UNFOUND;
            }
        } break;
        case MS_UNFOUND: break;
        case MS_FOUND_UNNEEDED: {
            n->stage = explore ? MS_PARSING_EXPLORATION : MS_PARSING;
            propagate_required = true;
        } break;
        case MS_PARSING: break;
        case MS_PARSING_EXPLORATION: {
            if (!explore) {
                n->stage = MS_PARSING;
                propagate_required = true;
            }
        } break;
        case MS_PARSED_EXPLORATION: {
            if (!explore) {
                n->stage = MS_AWAITING_DEPENDENCIES;
                propagate_required = true;
                awaiting = true;
            }
        } break;
        case MS_AWAITING_DEPENDENCIES: {
            awaiting = true;
        } break;
        case MS_RESOLVING_EXPLORATION: {
            if (!explore) {
                n->stage = MS_RESOLVING;
                propagate_required = true;
            }
        } break;
        case MS_RESOLVING: break;
        case MS_GENERATING:
        case MS_DONE: {
            available = true;
        } break;
        default: assert(false); break;
    }
    if (propagate_required) {
        mfs = &curr->module_frames;
        list_rit_begin_at_end(&deps_iter, &curr->dependencies);
        notifier = n->notifier;
    }
    if (!available) {
        sccd_stack_entry* dtn = sccd->dependant_to_notify;
        while (dtn && !r) {
            sccd_stack_entry* cd = dtn->curr_dep;
            if (!cd || !cd->curr_dep || cd->notifier != cd->curr_dep->mdgn) {
                r = list_append(&n->notify, NULL, dtn->mdgn);
            }
            dtn = dtn->dependant_to_notify;
        }
    }
    rwlock_end_write(&curr->lock);
    if (available) {
        assert(!propagate_required && !explore_parents && !awaiting);
        return OK; // nothing to do, handled already
    }
    if (explore_parents) {
        // TODO: push this on some sort of stack?
        assert(false);
    }
    if (propagate_required) {
        if (parent_se->dependant_to_notify) {
            parent_se->notifier_of_current_child = n;
        }
        sccd->propagate_required = true;
        *next_se = sccd_push_node(sccd, n, sn, &deps_iter);
        if (!*next_se) return ERR;
    }
    assert(!available); // we found someone that's still working to latch on.
    sccd_stack_entry* dtn = sccd->dependant_to_notify;
    if (dtn) {
        if (r) {
            sccd_release(sccd);
            return ERR;
        }
        sccd_stack_entry* dtn = sccd->dependant_to_notify;
        while (dtn && !r) {
            sccd_stack_entry* cd = dtn->curr_dep;
            bool success = true;
            mdg_node* notifier = n;
            if (cd && cd->curr_dep && cd->notifier == cd->curr_dep->mdgn) {
                rwlock_write(&cd->mdgn->lock);
                if (cd->notifier == cd->mdgn->notifier) {
                    r = list_append(&cd->mdgn->notify, NULL, n);
                    if (r) {
                        dtn->mdgn->notifier = n;
                    }
                }
                else {
                    success = false;
                }
                rwlock_end_write(&dtn->mdgn->lock);
                if (r) {
                    sccd_release(sccd);
                    return ERR;
                }
                if (!success) {
                    // TODO: push this on some sort of stack for redo
                    // similar to the parents to expore
                    assert(false);
                }
                notifier = cd->mdgn;
            }
            rwlock_write(&dtn->mdgn->lock);
            // in case somebody else was raced us we back off.
            // this will leave one uneccessary notification but whatever
            if (dtn->notifier == dtn->mdgn->notifier) {
                dtn->mdgn->notifier = notifier;
            }
            rwlock_end_write(&dtn->mdgn->lock);
            dtn = dtn->dependant_to_notify;
        }
        sccd->dependant_to_notify = NULL;
    }
}
int sccd_handle_node(
    scc_detector* sccd, mdg_node* n, sccd_node* sn, sccd_stack_entry** next_se)
{
    rwlock_read(&n->lock);
    if (n->stage == MS_NOT_FOUND) {
        return sccd_handle_not_found(sccd, n);
    }
    if (n->stage == MS_PARSING || n->stage == MS_RESOLVING) {
        return sccd_handle_in_progress(sccd, n);
    }
    if (n->stage == MS_DONE) {
        // can't be in a circle with caller if it's not pending
        sn->index = sccd->dfs_start_index;
        sn->lowlink = UREG_MAX;
        rwlock_end_read(&n->lock);
        return OK;
    }
    // TODO: handle error stages
    assert(n->stage == MS_AWAITING_DEPENDENCIES);
    list_bounded_it it;
    list_bounded_it_begin(&it, &n->dependencies);
    rwlock_end_read(&n->lock);
    *next_se = sccd_push_node(sccd, n, sn, &it);
    return *next_se ? OK : ERR;
}

int sccd_run(
    scc_detector* sccd, mdg_node* origin, bool propagate_required,
    bool exploratory)
{
    int r = sccd_prepare(sccd, origin, propagate_required);
    if (r) return r;
    // we do this after 'prepare' to save the ids in case prepare fails
    sccd_new_ctx(sccd);
    sccd_stack_entry* se = sccd->origin_se;
    while (true) {
        mdg_node* dep_mdg;
        sccd_node* dep_sn;
        while (true) {
            dep_mdg = list_bounded_it_next(&se->children_rit);
            if (!dep_mdg) break;
            se->deps_count++;
            dep_sn = sccd_get(sccd, dep_mdg->id);
            if (dep_sn->index < sccd->dfs_start_index) {
                // node is new
                break;
            }
            if (dep_sn->index == sccd->dfs_start_index) {
                // node is off the stack already
                continue;
            }
            // node is on the stack already
            // apply lowlink of the dependency if it is lower
            // otherwise ignore it
            if (dep_sn->index < sn->lowlink) sn->lowlink = dep_sn->index;
        }
        if (dep_mdg) {
            // it's a new node
            r = sccd_handle_node(sccd, dep_mdg, dep_sn, &se);
            if (r) return r;
            continue;
        }
        // we are done with the deps for the current node
        if (sn->lowlink != sn->index) {
            // part of an scc, but not the lowlink
            assert(sn->lowlink < sn->index);
            ureg dep_deps_count = se->deps_count;
            ureg dep_scc_elem_count = se->scc_elem_count;
            sbuffer_remove_back(
                &sccd->tc->temp_buffer, sizeof(sccd_stack_entry));
            sn_dep = sn;
            se = sbuffer_back(&sccd->tc->temp_buffer, sizeof(sccd_stack_entry));
            sn = se->sn;
            // if the parent has a higher lowlink apply this one
            if (sn->lowlink > sn_dep->lowlink) sn->lowlink = sn_dep->lowlink;
            se->deps_count += dep_deps_count;
            se->scc_elem_count += dep_scc_elem_count;
            sccd->propagate_required = se->propagate_required;
            sccd->exploratory = se->exploratory;
            continue;
        }
        // we are the lowlink. assemble a list and initiate resolving
        r = sccd_emit_mdg(sccd, se);
        if (r) return r;
        if (se == sccd->origin_se) return OK;
        se = sbuffer_back(&sccd->tc->temp_buffer, sizeof(sccd_stack_entry));
    }
    return OK;
}

void sccd_fin(scc_detector* d)
{
}
