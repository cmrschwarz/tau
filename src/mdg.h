#pragma once
#include "ast.h"
#include "mdght.h"
#include "utils/aseglist.h"
#include "utils/atomic_pool.h"
#include "utils/evmap2.h"
#include "utils/rwslock.h"
#include "utils/threading.h"
// mdg: module dependency graph
typedef struct mdg_deps_list mdg_deps_list;
typedef struct mdg_node mdg_node;

typedef enum module_stage {
    MS_UNNEEDED,
    MS_PARSING,
    MS_AWAITING_DEPENDENCIES,
    MS_RESOLVING,
    MS_GENERATING,
    MS_DONE,
} module_stage;

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

typedef struct tarjan_node {
    ureg index;
    ureg lowlink;
} tarjan_node;

typedef struct mdg_group {
    aseglist members; // TODO: this doesn't really need to be atomic
    tarjan_node tarjan_node;
} mdg_group;

typedef struct mdg_node {
    mdg_node* parent;
    char* name;
    atomic_ureg unparsed_files;
    atomic_ureg stage;
    atomic_ptr targets;
    aseglist dependencies;
    aseglist notifiy;
    mdg_group* group;
    tarjan_node tarjan_node;
    //'read' can also write dependencies and targets
    rwslock lock;
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
} mdg;

int mdg_init(mdg* m);
void mdg_fin(mdg* m);

mdght* mdg_start_read(mdg* m);
void mdg_end_read(mdg* m, mdght* h);

mdght* mdg_start_write(mdg* m);
void mdg_end_write(mdg* m);

mdg_node*
mdg_add_open_scope(mdg* m, mdg_node* parent, open_scope* osc, string ident);
mdg_node* mdg_get_node(mdg* m, mdg_node* parent, string ident);

int mdg_add_dependency(mdg* m, mdg_node* n, mdg_node* dependency);
int mdg_node_file_parsed(mdg* m, mdg_node* n);