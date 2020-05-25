#pragma once

#include "ast.h"
#include "mdght.h"
#include "symbol_table.h"
#include "utils/aseglist.h"
#include "utils/atomic_pool.h"
#include "utils/evmap2.h"
#include "utils/rwslock.h"
#include "utils/threading.h"
#include "utils/list.h"
// mdg: module dependency graph
typedef struct thread_context_s thread_context;
typedef struct tauc_s tauc;

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
    MS_DONE,
    // can happen e.g. when host int is out of bounds for arch int
    MS_GENERATION_ERROR, // TODO: actually integrate this
    MS_RESOLVING_ERROR,
} module_stage;

typedef enum pp_emission_stage_e {
    PPES_UNNEEDED,
    PPES_SKIPPED,
    PPES_REQUESTED,
    PPES_RUNNING,
    PPES_DONE,
} pp_emission_stage;

static inline bool module_stage_needed(module_stage ms)
{
    return ms != MS_UNNEEDED && ms != MS_AWAITING_NEED;
}
typedef struct partial_resolution_data_s partial_resolution_data;
typedef struct mdg_node_s {
    // TODO: make this an ast node/element for the symtab owning node to work
    ast_elem elem;
    struct mdg_node_s* parent;
    char* name;
    atomic_ureg unparsed_files;
    // these ids are used in the scc detector. don't confuse these with
    // the symbol ids used in the llvm backend, they don't share an "id space"
    ureg id;
    atomic_ureg decl_count;
    atomic_ureg using_count;
    symbol_table* symtab;
    partial_resolution_data* partial_res_data;
    aseglist module_frames; // doesn't require any locks

    rwlock lock; // everything below here is under the stage lock
    module_stage stage;
    pp_emission_stage ppe_stage;
    // whether some module (maybe itself) was found to use this in the pp
    // in that case all deps of this need to be recursively loaded in the pp
    // when we set this to true (initially false) we do this for all known ones
    bool requested_for_pp;
    list notify;
    list dependencies;
} mdg_node;

typedef struct mdg_new_node_s {
    ureg pos;
    mdg_node* node;
} mdg_new_node;

#define MDG_MAX_CHANGES 16
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
mdg_node* mdg_get_node(
    module_dependency_graph* m, mdg_node* parent, string ident,
    module_stage initial_stage);

int mdg_node_parsed(
    module_dependency_graph* m, mdg_node* n, thread_context* tc);
int mdg_node_file_parsed(
    module_dependency_graph* m, mdg_node* n, thread_context* tc);
int mdg_node_resolved(mdg_node* n, thread_context* tc);
int mdg_node_generated(mdg_node* n, thread_context* tc, bool pp_generated);
int mdg_nodes_resolved(mdg_node** start, mdg_node** end, thread_context* tc);
int mdg_node_add_dependency(
    mdg_node* n, mdg_node* dependency, thread_context* tc);
int mdg_node_add_frame(mdg_node* n, module_frame* mf, thread_context* tc);
int mdg_node_require_requirements(mdg_node* n, thread_context* tc, bool in_pp);

int mdg_final_sanity_check(module_dependency_graph* m, thread_context* tc);

void free_body_symtabs(ast_node* node, ast_body* b);
