#pragma once
#include "ast.h"
#include "src_map.h"
#include "utils/aseglist.h"
#include "utils/pool.h"
#include "utils/rwslock.h"
#include "utils/string.h"
#include "utils/threading.h"
#include <stdio.h>

typedef struct src_dir src_dir;

typedef struct file_map_head {
    src_dir* parent;
    string name;
    struct file_map_head* next;
    bool is_directory;
} file_map_head;

typedef struct src_dir {
    file_map_head head;
} src_dir;

typedef enum src_file_stage {
    SFS_UNNEDED,
    SFS_UNPARSED,
    // the distinction between unparsed and parsing is needed
    // because only once parsing the src_map is initialized
    SFS_PARSING,
    SFS_PARSED,
} src_file_stage;

typedef struct src_file {
    file_map_head head;
    aseglist requiring_modules;
    rwslock stage_lock;
    src_file_stage stage;
    src_map src_map;
    osc_extend root;
    FILE* file_stream;
} source_file;

void src_file_print_path(src_file* f, bool to_stderr);
ureg src_file_get_path_len(src_file* f);
void src_file_write_path(src_file* f, char* tgt);

int src_file_start_parse(src_file* f, thread_context* tc);
int src_file_done_parsing(src_file* f, thread_context* tc);
typedef struct open_scope open_scope;
typedef struct file_require file_require;
typedef struct mdg_node mdg_node;
#define SF_ALREADY_PARSED STATUS_1
int src_file_require(
    src_file* f, src_file* requiring_file, src_range requiring_srange,
    mdg_node* n);

typedef struct file_map {
    file_map_head** table_start;
    file_map_head** table_end;
    ureg elem_count;
    ureg grow_on_elem_count;
    ureg hash_mask;
    ureg size_bits;
    mutex lock;
    pool file_mem_pool;
    pool string_mem_pool;
} file_map;

int file_map_init(file_map* fm);
void file_map_fin(file_map* fm);

src_file* file_map_get_file(file_map* fm, src_dir* parent, string name);
src_file* file_map_get_file_from_path(file_map* fm, string path);
src_file* file_map_get_file_from_relative_path(
    file_map* fm, src_dir* parent_dir, string path);

src_dir* file_map_get_dir(file_map* fm, src_dir* parent, string name);
