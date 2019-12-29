#include "ast.h"
#include "src_map.h"
#ifndef TAUC_FILE_MAP_H
#define TAUC_FILE_MAP_H

#include "utils/aseglist.h"
#include "utils/pool.h"
#include "utils/rwslock.h"
#include "utils/string.h"
#include "utils/threading.h"
#include "utils/ptrlist.h"
#include <stdio.h>

typedef struct tauc_s tauc;
typedef struct src_dir_s src_dir;
typedef struct thread_context_s thread_context;
typedef struct open_scope_s open_scope;
typedef struct file_require_s file_require;
typedef struct mdg_node_s mdg_node;

typedef struct file_map_head_s {
    ast_elem elem;
    src_dir* parent;
    string name;
    struct file_map_head_s* next;
} file_map_head;

typedef struct src_dir_s {
    file_map_head head;
} src_dir;

typedef enum src_file_stage_s {
    SFS_UNNEEDED,
    SFS_UNPARSED,
    // the distinction between unparsed and parsing is needed
    // because only once parsing the src_map is initialized
    SFS_PARSING,
    SFS_PARSED,
} src_file_stage;

typedef struct src_file_s {
    file_map_head head;
    aseglist requiring_modules;
    rwslock stage_lock;
    src_file_stage stage;
    src_map smap;
    osc_extend root;
    FILE* file_stream;
} src_file;

typedef struct src_lib_s {
    file_map_head head;
    aseglist requiring_modules;
    atomic_boolean loaded;
    atomic_boolean loaded_for_pp;
    bool dynamic;
} src_lib;

void file_map_head_print_path(file_map_head* f, bool to_stderr);
ureg file_map_head_get_path_len(file_map_head* h);
void file_map_head_write_path(file_map_head* h, char* tgt);
char* file_map_head_tmalloc_path(file_map_head* h);

int src_file_start_parse(src_file* f, thread_context* tc);
int src_file_done_parsing(src_file* f, thread_context* tc);

// requiring file and srange are purely for error reporting
int src_file_require(
    src_file* f, tauc* t, src_map* requiring_smap, src_range requiring_srange,
    mdg_node* n);

int src_lib_require(
    src_lib* l, tauc* t, src_map* requiring_smap, src_range requiring_srange,
    bool in_pp);

typedef struct file_map_s {
    file_map_head** table_start;
    file_map_head** table_end;
    ureg elem_count;
    ureg grow_on_elem_count;
    ureg hash_mask;
    ureg size_bits;
    mutex lock;
    pool file_mem_pool;
    pool string_mem_pool;
    ptrlist rt_src_libs;
} file_map;

typedef struct file_map_iterator_s {
    file_map_head** head;
    file_map_head** end;
} file_map_iterator;

void file_map_iterator_begin(file_map_iterator* it, file_map* fm);
src_file* file_map_iterator_next_file(file_map_iterator* it);
src_dir* file_map_iterator_next_dir(file_map_iterator* it);
file_map_head* file_map_iterator_next(file_map_iterator* it);

int file_map_init(file_map* fm);
void file_map_fin(file_map* fm);

src_file*
file_map_get_file_from_path(file_map* fm, src_dir* parent, string path);
src_dir* file_map_get_dir_from_path(file_map* fm, src_dir* parent, string path);
src_lib* file_map_get_lib_from_path(
    file_map* fm, src_dir* parent, string path, bool is_dynamic);

int file_map_head_require(
    file_map_head* h, tauc* t, src_map* requiring_smap,
    src_range requiring_srange, mdg_node* requiring_mdgn, bool in_pp);
#endif
