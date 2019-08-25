#ifndef TAUC_SRC_MAP_H
#define TAUC_SRC_MAP_H
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

#ifndef TAUC_FILE_MAP_H
typedef struct src_file_s src_file;
#endif
#ifndef TAUC_THREAD_CONTEXT_H
typedef struct thread_context_s thread_context;
#endif

typedef struct line_store_s {
    struct line_store_s* prev;
    ureg* end;
    ureg first_line;
} line_store;

typedef struct source_map_s {
    ureg* last_line;
    line_store* last_line_store;
} source_map;

typedef struct src_range_large_s {
    src_file* file;
    ureg start;
    ureg end;
} src_range_large;

typedef struct paste_area_s {
    source_map src_map;
    src_range_large pasted_from;
} paste_area;

typedef struct src_pos_s {
    ureg line;
    ureg column;
} src_pos;

int src_map_init(src_map* m, thread_context* tc);
int src_map_fin(src_map* m);
int src_map_add_line(src_map* m, thread_context* tc, ureg line_start);
// this can't fail without programmer's error as the storage is already
// allocated
src_pos src_map_get_pos(src_map* m, ureg pos);
int src_pos_get_line_bounds(
    src_map* m, ureg line, ureg* start_pos, ureg* length);

// TODO: find a better name for this
src_range src_range_pack_lines(thread_context* tc, ureg start, ureg end);
src_range src_range_pack(thread_context* tc, ureg start, ureg end, src_file* f);
src_range src_range_large_pack(thread_context* tc, src_range_large* d);
void src_range_unpack(src_range r, src_range_large* d);
void src_range_unpack_lines(src_range r, ureg* start, ureg* end);
ureg src_range_get_start(src_range r);
src_file* src_range_get_file(src_range r);
ureg src_range_get_end(src_range r);
void src_range_set_end(thread_context* tc, src_range* old, ureg end);

#endif