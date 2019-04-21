#pragma once
#include "utils/allocator.h"
#include "utils/dbuffer.h"
#include "utils/string.h"
typedef struct thread_context thread_context;
// LAYOUT EXTERNAL: [1, change src_map(1 bit), pointer to external
// src_range shifted right by 2 bits ] LAYOUT INTERNAL: [0, start
// (UREG_BITS - 8 bits), length (7 bits)]
// TODO: better would be :
// LAYOUT EXTERNAL: [pointer to external src_range_large, change src_map(1 bit),
// 1] LAYOUT INTERNAL: [start (UREG_BITS - 8 bits), length (7 bits), 1]
typedef ureg src_range;
static const ureg SRC_RANGE_INVALID = ((ureg)0x1) << (REG_BITS - 1);

typedef struct src_file src_file;

typedef struct line_store {
    struct line_store* prev;
    ureg* end;
    ureg first_line;
} line_store;

typedef struct src_map {
    ureg* last_line;
    line_store* last_line_store;
} src_map;

typedef struct src_range_large {
    src_file* file;
    ureg start;
    ureg end;
} src_range_large;

typedef struct paste_area {
    src_map src_map;
    src_range_large pasted_from;
} paste_area;

typedef struct src_pos {
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
src_range src_range_pack(thread_context* tc, src_range_large* d);
void src_range_unpack(src_range r, src_range_large* d);
ureg src_range_get_start(src_range r);
ureg src_range_get_end(src_range r);
void src_range_set_end(thread_context* tc, src_range* old, ureg end);