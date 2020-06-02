#pragma once
#include "utils/types.h"
#include "utils/list.h"
typedef struct thread_context_s thread_context;
typedef struct mdg_node_s mdg_node;

typedef struct sccd_node_s {
    ureg index;
    ureg lowlink;
} sccd_node;

typedef struct sccd_stack_entry_s {
    mdg_node* mdgn;
    sccd_node* sn;
    list_bounded_it children_it;
    // we have to prevent a scenario where two threads build an mdg but
    // while one gets scheduled out the other already begins resolving the mdg
    // which aquires a new dependency and goes back to waiting. now our sleeper
    // could come in and create an outdated task missing the newly required
    // module(s).
    // to prevent this we count the combined deps count of the scc
    // to detect the invalidation
    ureg deps_count;
    ureg scc_elem_count; // so we can alloc the mdg without counting again
    sccd_stack_entry* awaiting_parent;
    mdg_node* notifier;
    bool propagate_required;
    bool exploratory;
} sccd_stack_entry;

typedef struct scc_detector_s {
    thread_context* tc;
    ureg allocated_node_count;
    ureg bucketable_node_capacity;
    sccd_node** sccd_node_buckets;
    ureg dfs_start_index;
    ureg dfs_index;
    sccd_stack_entry* origin_se;
    sccd_stack_entry* awaiting_parent;
    bool propagate_required;
    bool exploratory;
} scc_detector;

int sccd_init(scc_detector* d, thread_context* tc);
int sccd_run(scc_detector* d, mdg_node* n, bool make_required);
void sccd_fin(scc_detector* d);
// we need this in mdg to abuse sccds data structure fore cycle prevention
// the whole thing is ... debatable
void sccd_housekeep_ids(scc_detector* d);
sccd_node* sccd_get(scc_detector* d, ureg id);
