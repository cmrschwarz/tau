#pragma once
#include "utils/string.h"
#include "utils/allocator.h"
#include "utils/dbuffer.h"
typedef struct thread_context thread_context;
//LAYOUT EXTERNAL: [1, change src_map(1 bit), pointer to external src_range shifted right by 2 bits ]
//LAYOUT INTERNAL: [0, start (UREG_BITS - 8 bits), length (7 bits)]
typedef ureg src_range;
static const ureg SRC_RANGE_INVALID = ((ureg)0x1) << (REG_BITS - 1);

typedef struct line_store{
    struct lines_store* prev;
    ureg* end;
}line_store;

typedef struct src_map{
    ureg* last_line;
    line_store* last_line_store;
    thread_context* tc;
    bool is_paste_area;
}src_map;

typedef struct src_range_data{
    src_map* map;
    ureg start;
    ureg end;
}src_range_data;

typedef struct paste_area{
    src_map src_map;
    file* origin_file;
    src_range pasted_from;
}paste_area;

typedef struct file{
    src_map src_map;
    string directory;
    string name;
}file;

int src_map_init(src_map* m, thread_context* tc, bool is_paste_area);
int src_map_fin(src_map* m);
int src_map_add_line(src_map* m, ureg line_start);

src_range src_map_create_src_range(thread_context* tc, src_range_data d);
src_range_data src_range_get_data(src_range r);