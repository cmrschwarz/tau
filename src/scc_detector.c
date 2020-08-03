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

#define sccd_tprintf(sccd, ...)                                                \
    {                                                                          \
        if ((sccd->tc->t->verbosity_flags & VERBOSITY_FLAGS_SCCD) != 0)        \
            tprintf(__VA_ARGS__);                                              \
        tflush();                                                              \
    }
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
void sccd_release_dependants_including(scc_detector* sccd, sccd_stack_entry* se)
{
    if (sccd->dependant_to_notify == se) {
        sccd->dependant_to_notify = se->dependant_to_notify;
        se->dependant_to_notify = NULL;
        return;
    }
    if (!se->dependant_to_notify) return;
    while (sccd->dependant_to_notify != se->dependant_to_notify) {
        sccd_stack_entry* dep_prev =
            sccd->dependant_to_notify->dependant_to_notify;
        sccd->dependant_to_notify->dependant_to_notify = NULL;
        sccd->dependant_to_notify = dep_prev;
    }
}
void sccd_release(scc_detector* sccd)
{
    sccd_release_buffer(sccd);
    sccd_release_stack(sccd);
}
// curr must be under write lock!
int notify_dependants(scc_detector* sccd, mdg_node* curr)
{
    int r = OK;
    sccd_stack_entry* dtn = sccd->dependant_to_notify;
    while (dtn) {
        sccd_stack_entry* cd = dtn->curr_dep;
        bool add_note = false;
        if (!cd) {
            add_note = true;
        }
        else {
            assert(!cd->note_added);
            mdg_node* curr_dep_mdgn = cd->curr_dep ? cd->curr_dep->mdgn : curr;
            if (curr_dep_mdgn != cd->notifier) {
                add_note = true;
                sccd_tprintf(
                    sccd, "avoiding tree notification: %s <- %s present\n",
                    cd->notifier->name, dtn->curr_dep->mdgn->name);
            }
        }
        if (add_note) {
            r = list_append(&curr->notify, NULL, dtn->mdgn);
            if (r) break;
            dtn->note_added = true;
            sccd_tprintf(
                sccd, "notification added: %s <- %s\n", dtn->mdgn->name,
                curr->name);
            break;
        }
        dtn = dtn->dependant_to_notify;
    }
    return r;
}
int update_dependant_notifiers(
    scc_detector* sccd, mdg_node* notifier, bool error_occured)

{
    sccd_stack_entry* dtn = sccd->dependant_to_notify;
    if (dtn) {
        if (error_occured) {
            sccd_release(sccd);
            return ERR;
        }
        int r = OK;
        bool carried_lock = false;
        while (dtn && !r) {
            sccd_stack_entry* cd = dtn->curr_dep;
            bool success = true;
            if (!dtn->note_added) {
                if (!carried_lock) rwlock_write(&cd->mdgn->lock);
                if (cd->notifier == cd->mdgn->notifier) {
                    sccd_tprintf(
                        sccd, "tree notification optimization: %s <- %s\n",
                        dtn->mdgn->name, cd->mdgn->name);
                    r = list_append(&cd->mdgn->notify, NULL, dtn->mdgn);
                    if (r) {
                        dtn->mdgn->notifier = cd->mdgn;
                        dtn->notifier = cd->mdgn;
                    }
                }
                else {
                    success = false;
                }
                rwlock_end_write(&cd->mdgn->lock);
                if (r) {
                    sccd_release(sccd);
                    return ERR;
                }
                if (!success) {
                    // TODO: push this on some sort of stack for redo
                    // similar to the parents to expore
                    assert(false);
                }
                dtn->note_added = true;
                notifier = cd->mdgn;
            }
            else if (carried_lock) {
                rwlock_end_write(&cd->mdgn->lock);
            }
            rwlock_write(&dtn->mdgn->lock);
            // in case somebody else raced us we back off.
            // this will leave one uneccessary notification but whatever
            if (dtn->notifier == dtn->mdgn->notifier) {
                dtn->mdgn->notifier = notifier;
                dtn->notifier = notifier;
            }
            if (dtn->dependant_to_notify &&
                dtn->dependant_to_notify->curr_dep == dtn) {
                carried_lock = true;
            }
            else {
                rwlock_end_write(&dtn->mdgn->lock);
                carried_lock = false;
            }
            sccd_stack_entry* se = dtn;
            dtn = dtn->dependant_to_notify;
            se->dependant_to_notify = NULL;
        }
        sccd->dependant_to_notify = NULL;
    }
    return OK;
}
int sccd_emit_lone_mdgn(scc_detector* sccd, sccd_stack_entry* se_curr)
{
    mdg_node* n = stack_pop(&sccd->tc->temp_stack);
    assert(n == se_curr->mdgn);
    bool outdated = false;
    module_stage stage;
    rwlock_write(&n->lock);
    stage = n->stage;
    if (list_length(&n->dependencies) != se_curr->deps_count) {
        outdated = true;
    }
    else if (stage == MS_AWAITING_DEPENDENCIES) {
        n->stage = MS_RESOLVING;
    }
    else if (stage == MS_AWAITING_DEPENDENCIES_EXPLORATION) {
        n->stage = MS_RESOLVING_EXPLORATION;
    }
    else {
        outdated = true;
    }
    // we must do this even if we are outdated since we might have picked
    // up a new dependant
    int r = OK;
    if (n->stage < MS_RESOLVED_UNNEEDED) r = notify_dependants(sccd, n);
    if (r) {
        // we failed to add notifications, so we can't launch the update
        // abort and hope that somebody else will take care of it
        n->stage = stage;
        rwlock_end_write(&n->lock);
        sccd_release(sccd);
        return ERR;
    }
    rwlock_end_write(&n->lock);
    if (outdated) {
        sccd_tprintf(sccd, "outdated sccd run for %s!\n");
        return OK;
    }
    sccd_tprintf(sccd, "requesting emission of %s\n", n->name);
    partial_resolution_data* prd = n->partial_res_data;
    n->partial_res_data = NULL;
    r = update_dependant_notifiers(sccd, n, r != OK);
    // intentionaly continuing despite error in previous step
    return r | tauc_request_resolve_single(sccd->tc->t, n, prd);
}
int sccd_emit_mdg(scc_detector* sccd, sccd_stack_entry* se_curr)
{
    sccd_release_dependants_including(sccd, se_curr);
    // TODO: if we are not the origin, update the dependants to notify
    // to let them know that they need to wait on us being resolved
    ureg node_count = se_curr->scc_elem_count;
    if (node_count == 1) return sccd_emit_lone_mdgn(sccd, se_curr);
    ureg deps_count_now = 0;
    ureg deps_count_expected = se_curr->deps_count;
    mdg_node* lowlink = se_curr->mdgn;
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
    // used for notify dependants so that it uses the
    // correct 'current node' for the tree opt check
    mdg_node* stack_top = *nodes;
    mdg_nodes_quick_sort(nodes, node_count);
    bool outdated = false;
    bool notify = true;
    for (ni = nodes; ni != nodes_end; ni++) {
        n = *ni;
        rwlock_write(&n->lock);
        module_stage stage = n->stage;
        if (stage == MS_AWAITING_DEPENDENCIES) {
            n->stage = MS_RESOLVING;
        }
        else if (stage == MS_AWAITING_DEPENDENCIES_EXPLORATION) {
            n->stage = MS_RESOLVING_EXPLORATION;
        }
        else {
            outdated = true;
            // we must do this even if we are outdated because we might have
            // pickedup a new dependant
            // if it will still send out notifications that works for us
            // but if it's done we can't expect updates from it
            notify = n->stage < MS_RESOLVED_UNNEEDED;
            break;
        }
        deps_count_now += list_length(&n->dependencies);
    }
    int r = OK;
    if (notify) r = notify_dependants(sccd, stack_top);
    if (r || outdated || deps_count_now != deps_count_expected) {
        // rollback. we failed or somebody else is already doing our job
        for (mdg_node** ni_redo = nodes; ni_redo != ni; ni_redo++) {
            n = *ni_redo;
            if (n->stage == MS_RESOLVING) {
                n->stage = MS_AWAITING_DEPENDENCIES;
            }
            else if (n->stage == MS_RESOLVING_EXPLORATION) {
                n->stage = MS_AWAITING_DEPENDENCIES_EXPLORATION;
            }
            rwlock_end_write(&n->lock);
        }
        tfree(nodes);
        return r;
    }
    for (ni = nodes; ni != nodes_end; ni++) {
        rwlock_end_write(&(**ni).lock);
    }
    partial_resolution_data* prd = NULL;
    for (ni = nodes; ni != nodes_end; ni++) {
        if ((**ni).partial_res_data) {
            assert(!prd); // TODO: prd merge
            prd = (**ni).partial_res_data;
            (**ni).partial_res_data = NULL;
        }
    }
    r = update_dependant_notifiers(sccd, *(nodes_end - 1), r != OK);
    if (sccd->tc->t->verbosity_flags & VERBOSITY_FLAGS_SCCD) {
        tprintf("requesting resolve for {");
        for (mdg_node** n = nodes; n < nodes_end - 1; n++) {
            tprintf("%s, ", (**n).name);
        }
        tprintf("%s}\n", (**(nodes_end - 1)).name);
    }
    // intentionaly continuing despite error in previous step
    return r |
           tauc_request_resolve_multiple(sccd->tc->t, nodes, nodes_end, prd);
}
void sccd_new_ctx(scc_detector* sccd)
{
    sccd_housekeep_ids(sccd);
    sccd->dfs_start_index = sccd->dfs_index;
    // the start index is used to indicate the node is off the stack
    sccd->dfs_index++;
    // always push origin so we know when to stop the pop(TM) in release
}
sccd_stack_entry* sccd_push_node(
    scc_detector* sccd, mdg_node* n, sccd_node* sn, list_bounded_it* it,
    mdg_node* notifier, bool awaiting, bool newly_awaiting, bool is_root)
{
    sccd_stack_entry* dependant;
    if (!is_root) {
        dependant =
            sbuffer_back(&sccd->tc->temp_buffer, sizeof(sccd_stack_entry));
    }
    sn->index = sccd->dfs_index;
    sn->lowlink = sccd->dfs_index;
    sccd->dfs_index++;
    sccd_stack_entry* se =
        sbuffer_append(&sccd->tc->temp_buffer, sizeof(sccd_stack_entry));
    int r = OK;
    if (awaiting) r = stack_push(&sccd->tc->temp_stack, n);
    if (!se || r) {
        // release cleans up correctly no matter which one failed
        sccd_release(sccd);
        return NULL;
    }
    // if it's awaiting dependencies it always HAS dependencies, so there's
    // no need for some kind of leaf node optimization
    se->children_it = *it;
    se->mdgn = n;
    se->sn = sn;
    se->deps_count = 0;
    se->scc_elem_count = 1;
    se->propagate_required = sccd->propagate_required;
    se->exploratory = sccd->exploratory;
    se->notifier = notifier;
    se->awaiting = awaiting;
    se->note_added = false;
    if (newly_awaiting) {
        se->dependant_to_notify = sccd->dependant_to_notify;
        sccd->dependant_to_notify = se;
    }
    else {
        se->dependant_to_notify = NULL;
        if (is_root) sccd->dependant_to_notify = NULL;
    }
    if (!is_root) dependant->curr_dep = se;
    se->curr_dep = NULL;
    return se;
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
        case SCCD_PARSED: {
            switch (n->stage) {
                case MS_PARSING: {
                    n->stage = MS_AWAITING_DEPENDENCIES;
                    awaiting = true;
                } break;
                case MS_PARSING_EXPLORATION: {
                    n->stage = MS_AWAITING_DEPENDENCIES_EXPLORATION;
                    exploratory = true;
                    awaiting = true;
                } break;
                case MS_FOUND_UNNEEDED: {
                    n->stage = MS_PARSED_UNNEEDED;
                    r = SCCD_HANDLED;
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
                default: r = SCCD_HANDLED;
            }
        } break;
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
                case MS_PARSED_UNNEEDED: {
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
                    r = ERR;
                } break;
                default: r = SCCD_HANDLED;
            }
        } break;
        case SCCD_NODE_REQUIRE_EXPLORATION: {
            propagate_required = true;
            exploratory = true;
            exploratory_resolve = true;
            assert(false); // TODO: implement this
        } break;
        case SCCD_PP_DEPS_GENERATED: {
            assert(false); // TODO
        } break;
    }
    mdg_node* notifier = n->notifier;
    list_bounded_it it;
    list_bounded_it_begin(&it, &n->dependencies);
    rwlock_end_write(&n->lock);
    if (r) return r;
    sccd_node* sn = sccd_get(sccd, n->id);
    if (!sn) return ERR;
    sccd->propagate_required = propagate_required;
    sccd->exploratory = exploratory;
    sccd->exploratory_resolve = exploratory_resolve;
    sccd->dependant_to_notify = NULL;
    sccd->origin_se =
        sccd_push_node(sccd, n, sn, &it, notifier, awaiting, true, true);
    if (!sccd->origin_se) return ERR;
    return OK;
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
    bool require_deps = false;
    bool resolved = false;
    mdg_node* notifier;
    list_bounded_it deps_iter;
    sccd_tprintf(sccd, "handling %s \n", n->name);
    rwlock_write(&n->lock);
    int r = OK;
    switch (n->stage) {
        case MS_UNFOUND_UNNEEDED: {
            n->stage = explore ? MS_UNFOUND_EXPLORATION : MS_UNFOUND;
            explore_parents = true;
            // no require deps since no deps yet
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
            require_deps = true;
        } break;
        case MS_PARSING: break;
        case MS_PARSING_EXPLORATION: {
            if (!explore) {
                n->stage = MS_PARSING;
                propagate_required = true;
            }
        } break;
        case MS_PARSED_UNNEEDED: {
            n->stage = explore ? MS_AWAITING_DEPENDENCIES_EXPLORATION
                               : MS_AWAITING_DEPENDENCIES;
            propagate_required = true;
            awaiting = true;
            newly_awaiting = true;
            require_deps = true;
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
            resolved = true;
        } break;
        default: assert(false); break;
    }
    if (propagate_required || awaiting) {
        list_bounded_it_begin(&deps_iter, &n->dependencies);
        notifier = n->notifier;
    }
    if (!resolved && !awaiting) {
        r = notify_dependants(sccd, n);
    }
    rwlock_end_write(&n->lock);
    if (resolved) {
        assert(!propagate_required && !explore_parents && !awaiting);
        return OK; // nothing to do, handled already
    }
    if (explore_parents) {
        // TODO: push this on some sort of stack?
        assert(false);
    }
    if (propagate_required || awaiting) {
        sccd->propagate_required = propagate_required;
        *next_se = sccd_push_node(
            sccd, n, sn, &deps_iter, notifier, awaiting, newly_awaiting, false);
        if (!*next_se) return ERR;
    }
    if (require_deps) {
        r = mdg_node_require_requirements(n, sccd->tc, false);
        if (r) {
            sccd_release(sccd);
            return ERR;
        }
    }
    if (awaiting) return OK;
    // we found someone that's still working to latch on.
    r = update_dependant_notifiers(sccd, n, r != OK);
    if (r) return r;
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
    sccd_tprintf(sccd, "running sccd for %s\n", n->name);
    sccd_new_ctx(sccd);
    int r = sccd_prepare(sccd, n, sccdrr);
    if (r == SCCD_HANDLED) return OK;
    if (r) return r;
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
            if (r == SCCD_ADDED_NOTIFICATION) return OK;
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
        if (se->awaiting && se->note_added == false) {
            r = sccd_emit_mdg(sccd, se);
            if (r) return r;
        }
        do {
            sbuffer_remove_back(
                &sccd->tc->temp_buffer, sizeof(sccd_stack_entry));
            if (se == sccd->origin_se) return OK;
            se = sbuffer_back(&sccd->tc->temp_buffer, sizeof(sccd_stack_entry));
        } while (se->note_added && !se->exploratory);
    }
    return OK;
}

void sccd_fin(scc_detector* d)
{
}
