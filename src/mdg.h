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

// it might be worth putting needed as a separate flag
// but since not all combinations make sense we keep it like this for now
typedef enum module_stage_e {
    MS_UNFOUND_UNNEEDED, // was imported by a MS_FOUND_BUT_UNNEEDED module
    MS_UNFOUND_EXPLORATION, // unneded but we would like it for exploration
    MS_UNFOUND, // unfound but we need need it. we are exploring the parents
    MS_FOUND_UNNEEDED, // was in a file together with something we needed
    MS_PARSING_EXPLORATION, // we attempt to find a child by parsing this
    MS_PARSING, // found, needed, but not fully parsed yet
    MS_PARSED_UNNEEDED, // fully parsed for exploration, but unneded
    MS_AWAITING_PP_DEPENDENCIES_EXPLORATION, // pp dep gen needed to explore
    MS_AWAITING_PP_DEPENDENCIES, // pp dep gen needed to resolve this
    MS_AWAITING_DEPENDENCIES_EXPLORATION, // we want to explore gnerating this
    MS_AWAITING_DEPENDENCIES, // needed, parsed, but deps missing for resolved
    MS_RESOLVING_EXPLORATION, // we attempt to find a child by resolving this
    MS_RESOLVING, // standard resolve in hope of generating it later
    MS_RESOLVED_UNNEEDED, // we don't generated it since it's not needed
    MS_GENERATING, // resolved, generating IR
    MS_GENERATED, // generation completed successfully and written to disk
    // since linking must be done in one unit we don't track it here
} module_stage;

typedef enum pp_emission_stage_e {
    PPES_UNNEEDED,
    PPES_REQUESTED,
    PPES_RUNNING,
    PPES_DONE,
    PPES_SKIPPED,
} pp_emission_stage;

static inline bool module_stage_deps_needed(module_stage ms, bool* exploration)
{
    switch (ms) {
        case MS_UNFOUND:
        case MS_FOUND_UNNEEDED:
        case MS_PARSING:
        case MS_AWAITING_DEPENDENCIES:
            if (exploration) *exploration = false;
            return true;
        case MS_AWAITING_DEPENDENCIES_EXPLORATION:
            if (exploration) *exploration = true;
            return true;
        case MS_RESOLVING:
        case MS_GENERATING:
        case MS_GENERATED:
            assert(false); // TODO: should be an error
        // TODO: well we could speculate on this one if we have absolutely
        // nothing better to do
        case MS_PARSING_EXPLORATION:
        default: assert(false); return false;
    }
}
static inline bool
module_stage_requirements_needed(module_stage ms, bool* exploring)
{
    // why would we be handling requires otherwise?
    assert(ms < MS_PARSED_UNNEEDED);
    switch (ms) {
        case MS_UNFOUND:
        case MS_PARSING:
            if (exploring) *exploring = false;
            return true;
        case MS_UNFOUND_EXPLORATION:
        case MS_PARSING_EXPLORATION:
            if (exploring) *exploring = true;
            return true;
        default: return false;
    }
}
static inline bool module_stage_is_found(module_stage ms)
{
    return ms != MS_UNFOUND && ms != MS_UNFOUND_UNNEEDED &&
           ms != MS_UNFOUND_EXPLORATION;
}
static inline bool module_stage_is_exploring(module_stage ms)
{
    return ms == MS_UNFOUND_EXPLORATION || ms == MS_PARSING_EXPLORATION ||
           ms == MS_RESOLVING_EXPLORATION;
}
typedef struct partial_resolution_data_s partial_resolution_data;
typedef struct mdg_node_s {
    ast_elem elem;
    ast_body body;
    struct mdg_node_s* parent;
    char* name;
    atomic_ureg unparsed_files;
    // relevant for mdgs in the MS_AWAITING_PP_DEPENDENCIES state
    // used to wake them up when the deps are ready
    atomic_ureg ungenerated_pp_deps;
    // these ids are used in the scc detector. don't confuse these with
    // the symbol ids used in the llvm backend, they don't share an "id space"
    ureg id;
    atomic_ureg decl_count;
    atomic_ureg using_count;
    partial_resolution_data* partial_res_data;
    aseglist module_frames; // doesn't require any locks (?)

    rwlock lock; // everything below here is under the stage lock
    module_stage stage;
    pp_emission_stage ppe_stage;

    // list of mdg_nodes that want to be informed if this changes stage
    list notify;
    // list of mdg_nodes that this node imports (directly)
    list dependencies;

    // the guy that notifies us when he's ready. used for reducing scc
    // notification overhead by piggybacking on others with the same notifier
    mdg_node* notifier;

    // whether some module (maybe itself) was found to use this in the pp
    // in that case all deps of this need to be recursively loaded in the pp
    // when we set this to true (initially false) we do this for all known ones
    bool requested_for_pp;

    // we set unfound_children in the parent
    bool exploring_parent;

    bool error_occured;

    // we are currently (unnecessarily) parsing this node in hopes of
    // finding a missing child. once it is found we should stop working on
    // this.
    // TODO: implement this
    ureg unfound_children;
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
    mdg_node* invalid_node; // used as a dump non required extends
    atomic_ureg node_ids; // stores max used id for modules
} module_dependency_graph;

int mdg_init(module_dependency_graph* m);
void mdg_fin(module_dependency_graph* m);
void mdg_node_fin(mdg_node* n);

mdght* mdg_start_read(module_dependency_graph* m);
void mdg_end_read(module_dependency_graph* m, mdght* h);

mdght* mdg_start_write(module_dependency_graph* m);
void mdg_end_write(module_dependency_graph* m);

mdg_node* mdg_found_node(
    thread_context* tc, mdg_node* parent, string ident, bool extend,
    src_map* smap, src_range sr);
mdg_node* mdg_get_node(
    module_dependency_graph* m, mdg_node* parent, string ident,
    module_stage initial_stage);

int report_unrequired_extend(
    thread_context* tc, mdg_node* mod, src_map* smap, src_range sr);

int mdg_node_parsed(
    module_dependency_graph* m, mdg_node* n, thread_context* tc);
int mdg_node_file_parsed(
    module_dependency_graph* m, mdg_node* n, thread_context* tc,
    bool error_occured);
int mdg_nodes_resolved(
    mdg_node** start, mdg_node** end, thread_context* tc, bool error_occured);
int mdg_nodes_generated(
    mdg_node** start, mdg_node** end, thread_context* tc, bool pp_generated);
int mdg_node_add_dependency(
    mdg_node* n, mdg_node* dependency, thread_context* tc);
int mdg_node_add_frame(mdg_node* n, module_frame* mf, thread_context* tc);

int mdg_node_require_requirements(mdg_node* n, thread_context* tc, bool in_pp);

int mdg_final_sanity_check(module_dependency_graph* m, thread_context* tc);

void free_body_symtabs(ast_body* b);
void free_astn_symtabs(ast_node* n);
