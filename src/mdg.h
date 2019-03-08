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
    MS_UNNECESSARY,
    MS_PARSING,
    MS_IMPORTS_PARSING,
    MS_IMPORTS_OF_IMPORTS_PARSING,
    MS_GROUP_BUILDING,
    MS_PREPROCESSING,
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

typedef struct mdg_node {
    mdg_node* parent;
    char* name;
    atomic_ureg unparsed_files;
    atomic_ureg stage;
    atomic_ureg unparsed_imports;
    atomic_ureg imports_with_unparsed_imports;
    atomic_ptr targets;
    aseglist depending;
    aseglist dependencies;
    //'read' is allowed change the dependency lists, 'write' to change the stage
    rwslock lock;
} mdg_node;

#define MDG_MAX_CHANGES 16
#define MDG_MAX_CHANGES_PER_WRITE 2
typedef struct mdg {
    evmap2 evm;
    atomic_pool node_pool;
    pool ident_pool;
    thread_allocator* tal;
    mdght mdghts[2]; // PERF: might cause some false sharing
    mdg_new_node changes[MDG_MAX_CHANGES];
    ureg change_count;
} mdg;

int mdg_init(mdg* m, thread_allocator* tal);
void mdg_fin(mdg* m);

mdght* mdg_start_read(mdg* m);
void mdg_end_read(mdg* m, mdght* h);

mdght* mdg_start_write(mdg* m);
void mdg_end_write(mdg* m);

mdg_node*
mdg_add_module(mdg* m, mdg_node* parent, sc_module* mod, string ident);

int mdg_add_dependency(mdg* m, mdg_node* n, mdg_node* dependency);
int mdg_node_file_parsed(mdg* m, mdg_node* n);
int mdg_node_import_parsed(mdg* m, mdg_node* n);
int mdg_node_import_imports_parsed(mdg* m, mdg_node* n);