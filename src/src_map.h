#pragma once
#include "utils/string.h"
#include "utils/allocator.h"
#include "utils/dbuffer.h"
typedef struct thread_context thread_context;
//LAYOUT EXTERNAL: [1, change src_map(1 bit), pointer to external src_range_packed shifted right by 2 bits ]
//LAYOUT INTERNAL: [0, start (UREG_BITS - 8 bits), length (7 bits)]
//TODO: better would be :
//LAYOUT EXTERNAL: [pointer to external src_range, change src_map(1 bit), 1]
//LAYOUT INTERNAL: [start (UREG_BITS - 8 bits), length (7 bits), 1]
typedef ureg src_range_packed;
static const ureg SRC_RANGE_INVALID = ((ureg)0x1) << (REG_BITS - 1);

typedef struct line_store{
    struct line_store* prev;
    ureg* end;
    ureg first_line;
}line_store;

typedef struct src_map{
    ureg* last_line;
    line_store* last_line_store;
    bool is_paste_area;
}src_map;

typedef struct file{
    src_map src_map;
    char* path;
}file;


typedef struct src_range{
    src_map* map; //this is NULL if the src_range_packed doesn't change it
    ureg start;
    ureg end;
}src_range;

typedef struct paste_area{
    src_map src_map;
    struct file* origin_file;
    src_range pasted_from;
}paste_area;

typedef struct src_pos{
    ureg line;
    ureg column;
}src_pos;

//THINK: who manages memory of the string here
int file_init(file* f, thread_context* tc, char* path); 
void file_fin(file* f);

int src_map_init(src_map* m, thread_context* tc, bool is_paste_area);
int src_map_fin(src_map* m);
int src_map_add_line(src_map* m, thread_context* tc, ureg line_start);
src_pos src_map_get_pos(src_map* m, ureg pos);
int src_pos_get_line_bounds(src_map* m, ureg line, ureg* start_pos, ureg* length);

//TODO: find a better name for this
src_range_packed src_range_pack_lines(thread_context* tc, ureg start, ureg end);
src_range_packed src_range_pack(thread_context* tc, src_range* d);
void src_range_unpack(src_range_packed r, src_range* d);