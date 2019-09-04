#ifndef TAUC_MDG_H
#define TAUC_MDG_H

#include "ast.h"
#include "mdght.h"
#include "symbol_table.h"
#include "utils/aseglist.h"
#include "utils/atomic_pool.h"
#include "utils/evmap2.h"
#include "utils/rwslock.h"
#include "utils/threading.h"
// mdg: module dependency graph
typedef struct mdg_deps_list_s mdg_deps_list;
typedef struct thread_context_s thread_context;

typedef enum module_stage_e {
    MS_NOT_FOUND, // required but not found in src
    MS_UNNEEDED, //  partially parsed, but unneded
    MS_PARSING, // required and partially parsed
    MS_AWAITING_NEED, // fully parsed, but unneded
    MS_AWAITING_DEPENDENCIES, // required, parsed
    MS_RESOLVING, // required, parsed, deps resolved or in same resolve group
    MS_GENERATING, // resolved, generating IR
    // since linking must be done in single threadedly we don't
    // update the stage to done_generating or smth but track a single
    // atomic in TAUC.linking_holdups
} module_stage;

static inline bool module_stage_needed(module_stage ms)
{
    return ms != MS_UNNEEDED && ms != MS_AWAITING_NEED;
}

typedef struct mdg_node_s {
    // TODO: make this an ast node/element for the symtab owning node to work
    ast_elem elem;
    struct mdg_node_s* parent;
    char* name;
    atomic_ureg unparsed_files;
    aseglist dependencies;
    aseglist open_scopes;
    aseglist notify;
    // these are used in the scc detector. don't confuse these with
    // the symbol ids used in the llvm backend, they don't share an "id space"
    ureg id;

    atomic_ureg decl_count;
    atomic_ureg using_count;
    symbol_table* symtab;
    rwslock stage_lock;
    module_stage stage;
} mdg_node;

typedef struct mdg_new_node_s {
    ureg pos;
    mdg_node* node;
} mdg_new_node;

#define MDG_DEPS_LIST_CAPACITY                                                 \
    ((sizeof(mdg_new_node) - sizeof(mdg_deps_list*)) / sizeof(mdg_node*))

typedef struct mdg_deps_list_s {
    mdg_deps_list* next;
    atomic_ureg count;
    mdg_node* deps[MDG_DEPS_LIST_CAPACITY];
} mdg_deps_list;

#define MDG_MAX_CHANGES 16
#define MDG_MAX_CHANGES_PER_WRITE 2
typedef struct module_dependency_graph_s {
    evmap2 evm;
    pool node_pool;
    pool ident_pool;
    mdght mdghts[2]; // PERF: might cause some false sharing
    mdg_new_node changes[MDG_MAX_CHANGES];
    ureg change_count;
    mdg_node* root_node;
    atomic_ureg node_ids; // stores max used id for modules
} module_dependency_graph;

int mdg_init(module_dependency_graph* m);
void mdg_fin(module_dependency_graph* m);
void mdg_node_fin(mdg_node* n);

mdght* mdg_start_read(module_dependency_graph* m);
void mdg_end_read(module_dependency_graph* m, mdght* h);

mdght* mdg_start_write(module_dependency_graph* m);
void mdg_end_write(module_dependency_graph* m);

mdg_node*
mdg_found_node(module_dependency_graph* m, mdg_node* parent, string ident);
mdg_node*
mdg_get_node(module_dependency_graph* m, mdg_node* parent, string ident);

typedef struct sccd_node_s {
    ureg index;
    ureg lowlink;
} sccd_node;

typedef struct scc_detector_s {
    ureg allocated_node_count;
    ureg dfs_index;
    ureg dfs_start_index;
    ureg bucketable_node_capacity;
    sccd_node** sccd_node_buckets;
    pool* mem_src;
} scc_detector;

int mdg_node_parsed(
    module_dependency_graph* m, mdg_node* n, thread_context* tc);
int mdg_node_file_parsed(
    module_dependency_graph* m, mdg_node* n, thread_context* tc);
int mdg_node_resolved(mdg_node* n, thread_context* tc);
int mdg_nodes_resolved(mdg_node** start, mdg_node** end, thread_context* tc);
int mdg_node_add_dependency(
    mdg_node* n, mdg_node* dependency, thread_context* tc);

int scc_detector_init(scc_detector* d, pool* mem_src);
int scc_detector_run(thread_context* tc, mdg_node* n);
void scc_detector_fin(scc_detector* d);

int mdg_final_sanity_check(module_dependency_graph* m, thread_context* tc);

#endif