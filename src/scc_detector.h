#pragma once
#include "utils/types.h"
#include "utils/list.h"
typedef struct thread_context_s thread_context;
typedef struct mdg_node_s mdg_node;

typedef enum sccd_run_reason_e {
    SCCD_NODE_PARSED,
    SCCD_NOTIFY_DEP_PARSED,
    SCCD_NOTIFY_DEP_RESOLVED,
    SCCD_NOTIFY_DEP_GENERATED,
    SCCD_NOTIFY_DEP_ERROR,
    SCCD_NODE_REQUIRE,
    SCCD_NODE_REQUIRE_EXPLORATION,
} sccd_run_reason;

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
    struct sccd_stack_entry_s* dependant_to_notify;
    mdg_node* notifier;
    struct sccd_stack_entry_s* curr_dep;
    bool propagate_required;
    bool exploratory;
    bool exploratory_resolve;
} sccd_stack_entry;

typedef struct scc_detector_s {
    thread_context* tc;
    ureg allocated_node_count;
    ureg bucketable_node_capacity;
    sccd_node** sccd_node_buckets;
    ureg dfs_start_index;
    ureg dfs_index;
    sccd_stack_entry* origin_se;
    sccd_stack_entry* dependant_to_notify;
    bool propagate_required;
    bool exploratory;
    bool exploratory_resolve;
} scc_detector;

int sccd_init(scc_detector* d, thread_context* tc);
int sccd_run(scc_detector* d, mdg_node* n, sccd_run_reason sccdrr);
void sccd_fin(scc_detector* d);
// we need this in mdg to abuse sccds data structure fore cycle prevention
// the whole thing is ... debatable
void sccd_housekeep_ids(scc_detector* d);
sccd_node* sccd_get(scc_detector* d, ureg id);
