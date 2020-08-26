#pragma once
#include "utils/threading.h"
typedef struct type_array_s type_array;
typedef struct type_tuple_s type_tuple;
typedef struct ast_elem_s ast_elem;
// PERF: put the rwlock behind another layer of indirection
// and only alloc it for public nodes. that way local nodes are smaller
typedef struct type_map_s {
    rwlock lock;
    ureg capactiy;
    ureg count;
    ast_elem** map;
} type_map;

int type_map_init(type_map* tm);
void type_map_fin(type_map* tm);

type_array* type_map_get_array(type_map* tm, ureg elem_count);
type_tuple* type_map_get_tuple_follower(type_map* tm, ast_elem* following_type);
