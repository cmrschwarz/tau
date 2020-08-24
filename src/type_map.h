#pragma once
#include "ast.h"
#include "utils/types.h"
#include "utils/dbuffer.h"
#include "utils/rwslock.h"
#include "utils/pool.h"

// chosen by a fair dice roll
#define TYPE_MAP_INITIAL_SEGMENT_COUNT 4
#define TYPE_MAP_SEGMENT_CAPACITY REG_BITS
#define TYPE_MAP_SEGMENT_SIZE                                                  \
    ((ureg)(                                                                   \
        sizeof(type_map_segment) +                                             \
        TYPE_MAP_SEGMENT_CAPACITY * sizeof(ast_elem*)))

typedef struct tauc_s tauc;
typedef struct ast_elem_s ast_elem;

typedef struct type_map_segment_s {
    ureg filled_bits;
    rwlock lock;
    // joint alloc: followed by TYPE_MAP_SEGMENT_SIZE type** 's
} type_map_segment;

typedef struct type_map_segment_ref_s {
    ureg filled_bits;
    type_map_segment* segment;
} type_map_segment_ref;

typedef ureg type_id;

typedef struct type_map_s {
    global_type_map* gtm;
    type_map_segment_ref* segment_refs;
    ureg segment_capacity;
} type_map;

typedef struct global_type_map_s {
    rwlock lock;
    type_map_segment** segments;
    pool segment_mem;
    ureg segment_capacity;
    ureg type_ids;
} global_type_map;

int type_map_init(type_map* m, global_type_map* gtm);
int global_type_map_init(global_type_map* gtm);
void type_map_fin(type_map* m);
void global_type_map_fin(global_type_map* gtm);

type_pointer* type_map_get_pointer(
    type_map* tm, ast_elem* base_type, ureg ptr_id, bool is_const,
    pool* type_mem);