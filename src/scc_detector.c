#include "scc_detector.h"
#include "thread_context.h"
#include "tauc.h"

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
    else if (n->stage == MS_PARSED_EXPLORATION) {
        n->stage = MS_RESOLVING_EXPLORATION;
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
        else if (n->stage == MS_PARSED_EXPLORATION) {
            n->stage = MS_RESOLVING_EXPLORATION;
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
int sccd_prepare(scc_detector* sccd, mdg_node* n, sccd_run_reason sccdrr)
{
    bool exploratory = false;
    bool exploratory_resolve = false;
    bool propagate_required = false;
    bool awaiting = false;
    int r = OK;
    rwlock_write(&n->lock);
    switch (sccdrr) {
        case SCCD_NODE_PARSED: {
            switch (n->stage) {
                case MS_PARSING: {
                    n->stage = MS_AWAITING_DEPENDENCIES;
                    awaiting = true;
                } break;
                case MS_PARSING_EXPLORATION: {
                    exploratory = true;
                    assert(false); // TODO
                } break;
                // TODO: propagate errors?
                default: assert(false); return ERR;
            }
        } break;
        case SCCD_NOTIFY_DEP_PARSED:
        case SCCD_NOTIFY_DEP_RESOLVED: {
            switch (n->stage) {
                case MS_AWAITING_DEPENDENCIES: {
                    awaiting = true;
                } break;
                case MS_AWAITING_DEPENDENCIES_EXPLORATION: {
                    exploratory = true;
                    awaiting = true;
                } break;
                // we currently don't care as we are not awaiting
                // TODO: is this correct?
                default: return SCCD_HANDLED;
            }
        } break;
        case SCCD_NOTIFY_DEP_GENERATED: assert(false); return ERR; // TODO
        case SCCD_NOTIFY_DEP_ERROR: assert(false); return ERR; // TODO
        case SCCD_NODE_REQUIRE: {
            switch (n->stage) {
                case MS_UNFOUND_UNNEEDED:
                case MS_UNFOUND_EXPLORATION: {
                    n->stage = MS_UNFOUND;
                    propagate_required = true;
                } break;
                case MS_FOUND_UNNEEDED: {
                    n->stage = MS_PARSING;
                    propagate_required = true;
                } break;
                case MS_PARSED_EXPLORATION: {
                    n->stage = MS_AWAITING_DEPENDENCIES;
                    awaiting = true;
                    propagate_required = true;
                } break;
                case MS_PARSING_EXPLORATION: {
                    n->stage = MS_PARSING;
                    propagate_required = true;
                } break;
                case MS_RESOLVED_UNNEEDED: {
                    // TODO: emit generate job
                    assert(false);
                    return ERR;
                } break;
                default: r = SCCD_HANDLED;
            }
        } break;
        case SCCD_NODE_REQUIRE_EXPLORATION:
            propagate_required = true;
            exploratory = true;
            exploratory_resolve = true;
            assert(false); // TODO: implement this
    }
    mdg_node* notifier = n->notifier;
    list_bounded_it it;
    list_bounded_it_begin(&it, &n->dependencies);
    rwlock_end_read(&n->lock);
    if (r) return r;
    sccd_stack_entry* se =
        sbuffer_append(&sccd->tc->temp_buffer, sizeof(sccd_stack_entry));
    if (!se) return ERR;
    sccd_node* sn = sccd_get(sccd, n->id);
    r = stack_push(&sccd->tc->temp_stack, n);
    if (!sn || r) {
        sbuffer_remove_back(&sccd->tc->temp_buffer, sizeof(sccd_stack_entry));
        return ERR;
    }

    se->sn = sn;
    se->children_it = it;
    se->mdgn = n;
    se->deps_count = 0;
    se->scc_elem_count = 1;
    se->propagate_required = sccd;
    se->exploratory = false;
    se->dependant_to_notify = NULL;
    se->notifier = notifier;

    sccd->origin_se = se;
    sccd->propagate_required = propagate_required;
    sccd->exploratory = exploratory;
    sccd->exploratory_resolve = exploratory_resolve;
    if (awaiting) {
        se->dependant_to_notify = NULL;
        sccd->dependant_to_notify = se;
    }
    else {
        sccd->dependant_to_notify = NULL;
    }
    return OK;
}
sccd_stack_entry* sccd_push_node(
    scc_detector* sccd, mdg_node* n, sccd_node* sn, list_bounded_it* it,
    mdg_node* notifier, bool newly_awaiting)
{
    sccd_stack_entry* dependant =
        sbuffer_back(&sccd->tc->temp_buffer, sizeof(sccd_stack_entry));
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
    se->children_it = *it;
    se->mdgn = n;
    se->sn = sn;
    se->deps_count = 0;
    se->scc_elem_count = 1;
    se->propagate_required = sccd->propagate_required;
    se->exploratory = sccd->exploratory;
    se->notifier = notifier;
    if (newly_awaiting) {
        se->dependant_to_notify = sccd->dependant_to_notify;
        sccd->dependant_to_notify = se;
    }
    dependant->curr_dep = se;
    return se;
}
int sccd_handle_node(
    scc_detector* sccd, mdg_node* n, sccd_node* sn, sccd_stack_entry** next_se)
{
    // PERF: maybe introduce a read first mode when not propagating
    const bool explore = sccd->exploratory;
    bool explore_parents = false;
    bool propagate_required = false;
    bool awaiting = false;
    bool newly_awaiting = false;
    bool ready = false;
    mdg_node* notifier;
    list_bounded_it deps_iter;
    rwlock_write(&n->lock);
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
                newly_awaiting = true;
            }
        } break;
        case MS_AWAITING_DEPENDENCIES_EXPLORATION: {
            awaiting = true;
            if (!explore) {
                n->stage = MS_AWAITING_DEPENDENCIES;
                propagate_required = true;
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
        case MS_GENERATED: {
            ready = true;
        } break;
        default: assert(false); break;
    }
    if (propagate_required) {
        list_bounded_it_begin(&deps_iter, &n->dependencies);
        notifier = n->notifier;
    }
    if (!ready && !awaiting) {
        sccd_stack_entry* dtn = sccd->dependant_to_notify;
        while (dtn && !r) {
            sccd_stack_entry* cd = dtn->curr_dep;
            if (!cd || !cd->curr_dep || cd->notifier != cd->curr_dep->mdgn) {
                r = list_append(&n->notify, NULL, dtn->mdgn);
            }
            dtn = dtn->dependant_to_notify;
        }
    }
    rwlock_end_write(&n->lock);
    if (ready) {
        assert(!propagate_required && !explore_parents && !awaiting);
        return OK; // nothing to do, handled already
    }
    if (explore_parents) {
        // TODO: push this on some sort of stack?
        assert(false);
    }
    if (propagate_required || awaiting) {
        *next_se =
            sccd_push_node(sccd, n, sn, &deps_iter, notifier, newly_awaiting);
        if (!*next_se) return ERR;
    }
    if (awaiting) return OK;
    if (propagate_required && !explore) {
        r = mdg_node_require_requirements(n, sccd->tc, false);
        if (r) {
            sccd_release(sccd);
            return ERR;
        }
    }
    // we found someone that's still working to latch on.
    assert(!ready && !awaiting);
    sccd_stack_entry* dtn = sccd->dependant_to_notify;
    if (dtn) {
        if (r) {
            sccd_release(sccd);
            return ERR;
        }
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
    if (!propagate_required) {
        sccd_stack_entry* se;
        do {
            se = sbuffer_back(&sccd->tc->temp_buffer, sizeof(sccd_stack_entry));
            if (se->propagate_required) {
                *next_se = se;
                return OK;
            }
            sbuffer_remove_back(
                &sccd->tc->temp_buffer, sizeof(sccd_stack_entry));
        } while (se != sccd->origin_se);
        return SCCD_ADDED_NOTIFICATION; // we're done
    }
    return RE_OK;
}
int sccd_run(scc_detector* sccd, mdg_node* n, sccd_run_reason sccdrr)
{
    int r = sccd_prepare(sccd, n, sccdrr);
    if (r == SCCD_HANDLED) return OK;
    if (r) return r;
    // we do this after 'prepare' to save the ids in case prepare fails
    sccd_new_ctx(sccd);
    sccd_stack_entry* se = sccd->origin_se;
    while (true) {
        mdg_node* dep_mdg;
        sccd_node* dep_sn;
        while (true) {
            dep_mdg =
                list_bounded_it_next(&se->children_it, &se->mdgn->dependencies);
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
            if (dep_sn->index < se->sn->lowlink) {
                se->sn->lowlink = dep_sn->index;
            }
        }
        if (dep_mdg) {
            // it's a new node
            r = sccd_handle_node(sccd, dep_mdg, dep_sn, &se);
            if (r) return r;
            continue;
        }
        // we are done with the deps for the current node
        if (se->sn->lowlink != se->sn->index) {
            // part of an scc, but not the lowlink
            assert(se->sn->lowlink < se->sn->index);
            ureg dep_deps_count = se->deps_count;
            ureg dep_scc_elem_count = se->scc_elem_count;
            sbuffer_remove_back(
                &sccd->tc->temp_buffer, sizeof(sccd_stack_entry));
            dep_sn = se->sn;
            se = sbuffer_back(&sccd->tc->temp_buffer, sizeof(sccd_stack_entry));
            // if the parent has a higher lowlink apply this one
            if (se->sn->lowlink > dep_sn->lowlink)
                se->sn->lowlink = dep_sn->lowlink;
            se->deps_count += dep_deps_count;
            se->scc_elem_count += dep_scc_elem_count;
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
