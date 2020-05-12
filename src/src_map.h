#pragma once
#include "utils/allocator.h"
#include "utils/dbuffer.h"
#include "utils/string.h"
// LAYOUT EXTERNAL: [1, change src_map(1 bit), pointer to external
// src_range shifted right by 2 bits ] LAYOUT INTERNAL: [0, start
// (UREG_BITS - 8 bits), length (7 bits)]
// TODO: better would be :
// LAYOUT EXTERNAL: [pointer to external src_range_large, change src_map(1 bit),
// 1] LAYOUT INTERNAL: [start (UREG_BITS - 8 bits), length (7 bits), 1]

typedef ureg src_range;
#define SRC_RANGE_INVALID (((ureg)0x1) << (REG_BITS - 1))

typedef struct ast_elem_s ast_elem;
typedef struct thread_context_s thread_context;
typedef struct src_file_s src_file;

typedef struct line_store_s {
    struct line_store_s* prev;
    ureg* end;
    ureg first_line;
} line_store;

typedef struct src_map_s {
    ast_elem* source;
    ureg* last_line;
    line_store* last_line_store;
    struct src_map_s* next;
} src_map;

typedef struct src_range_large_s {
    src_map* smap;
    ureg start;
    ureg end;
} src_range_large;

typedef struct paste_area_s {
    src_map smap;
    src_range_large pasted_from;
} paste_area;

typedef struct src_pos_s {
    ureg line;
    ureg column;
} src_pos;

int src_map_init(src_map* m, ast_elem* source, thread_context* tc);
src_map* src_map_create_child(src_map* m, ast_elem* source, thread_context* tc);
void src_map_fin(src_map* m);
src_file* src_map_get_file(src_map* smap);
int src_map_add_line(src_map* m, ureg line_start);
// this can't fail without programmer's error as the storage is already
// allocated
src_pos src_map_get_pos(src_map* m, ureg pos);
int src_pos_get_line_bounds(
    src_map* m, ureg line, ureg* start_pos, ureg* length);

// TODO: find a better name for this
src_range src_range_pack_lines(thread_context* tc, ureg start, ureg end);
src_range
src_range_pack(thread_context* tc, ureg start, ureg end, src_map* smap);
src_range src_range_large_pack(thread_context* tc, src_range_large* d);
void src_range_unpack(src_range r, src_range_large* d);
void src_range_unpack_lines(src_range r, ureg* start, ureg* end);
ureg src_range_get_start(src_range r);
src_map* src_range_get_smap(src_range r);
ureg src_range_get_end(src_range r);
void src_range_set_end(thread_context* tc, src_range* old, ureg end);
void src_range_set_smap(thread_context* tc, src_range* old, src_map* smap);

void src_map_print_path(src_map* smap, bool to_stderr);
bool src_map_is_opened(src_map* smap);
int src_map_open(src_map* smap);
int src_map_read(src_map* smap, ureg size, ureg* read_size, char* tgt);
int src_map_seek_set(src_map* smap, ureg pos);
void src_map_close(src_map* smap);

