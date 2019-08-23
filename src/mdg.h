#pragma once
#include "ast.h"
#include "mdght.h"
#include "symbol_table.h"
#include "utils/aseglist.h"
#include "utils/atomic_pool.h"
#include "utils/evmap2.h"
#include "utils/rwslock.h"
#include "utils/threading.h"
// mdg: module dependency graph
typedef struct mdg_deps_list mdg_deps_list;
typedef struct mdg_node mdg_node;

typedef enum module_stage {
    MS_UNNEEDED, // partially parsed, but unneded
    MS_AWAITING_NEED, // fully parsed, but unneded
    MS_NOT_FOUND, // required but not found in src
    MS_PARSING, // required and partially parsed
    MS_AWAITING_DEPENDENCIES, // required, parsed
    MS_RESOLVING, // required, parsed, deps resolved or in same resolve group
    MS_GENERATING, // resolved, generating IR
    MS_DONE, // IR generated
} module_stage;

static inline bool module_stage_needed(module_stage ms)
{
    return ms != MS_UNNEEDED && ms != MS_AWAITING_NEED;
}
typedef struct mdg_new_node {
    ureg pos;
    mdg_node* node;
} mdg_new_node;

#define MDG_DEPS_LIST_CAPACITY                                                 \
    ((sizeof(mdg_new_node) - sizeof(mdg_deps_list*)) / sizeof(mdg_node*))

typedef struct mdg_deps_list {
    struct mdg_deps_list* next;
    atomic_ureg count;
    mdg_node* deps[MDG_DEPS_LIST_CAPACITY];
} mdg_deps_list;

typedef struct mdg_node {
    // TODO: make this an ast node/element for the symtab owning node to work
    mdg_node* parent;
    char* name;
    atomic_ureg unparsed_files;
    aseglist dependencies;
    aseglist open_scopes;
    aseglist notify;
    ureg id;
    atomic_ureg decl_count;
    atomic_ureg using_count;
    symbol_table* symtab;
    rwslock stage_lock;
    module_stage stage;
} mdg_node;

#define MDG_MAX_CHANGES 16
#define MDG_MAX_CHANGES_PER_WRITE 2
typedef struct mdg {
    evmap2 evm;
    pool node_pool;
    pool ident_pool;
    mdght mdghts[2]; // PERF: might cause some false sharing
    mdg_new_node changes[MDG_MAX_CHANGES];
    ureg change_count;
    mdg_node* root_node;
    atomic_ureg node_ids;
} mdg;

int mdg_init(mdg* m);
void mdg_fin(mdg* m);
void mdg_node_fin(mdg_node* n);

mdght* mdg_start_read(mdg* m);
void mdg_end_read(mdg* m, mdght* h);

mdght* mdg_start_write(mdg* m);
void mdg_end_write(mdg* m);

mdg_node*
mdg_add_open_scope(mdg* m, mdg_node* parent, open_scope* osc, string ident);
mdg_node* mdg_get_node(mdg* m, mdg_node* parent, string ident);

typedef struct sccd_node {
    ureg index;
    ureg lowlink;
} sccd_node;

typedef struct scc_detector {
    ureg allocated_node_count;
    ureg dfs_index;
    ureg dfs_start_index;
    ureg bucketable_node_capacity;
    sccd_node** sccd_node_buckets;
    pool* mem_src;
} scc_detector;

int mdg_node_parsed(mdg* m, mdg_node* n, thread_context* tc);
int mdg_node_file_parsed(mdg* m, mdg_node* n, thread_context* tc);
int mdg_node_resolved(mdg_node* n, thread_context* tc);
int mdg_nodes_resolved(mdg_node** start, mdg_node** end, thread_context* tc);
int mdg_node_add_dependency(
    mdg_node* n, mdg_node* dependency, thread_context* tc);

int scc_detector_init(scc_detector* d, pool* mem_src);
int scc_detector_run(thread_context* tc, mdg_node* n);
void scc_detector_fin(scc_detector* d);

int mdg_final_sanity_check(mdg* m, thread_context* tc);
