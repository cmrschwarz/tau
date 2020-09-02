#pragma once
#include "ast.h"
#include "utils/types.h"
#include "utils/dbuffer.h"
#include "utils/threading.h"
#include "utils/pool.h"

// chosen by a fair dice roll
#define PTR_MAP_INITIAL_SEGMENT_COUNT 4
#define PTR_MAP_SEGMENT_CAPACITY REG_BITS
#define PTR_MAP_SEGMENT_SIZE                                                   \
    ((ureg)(                                                                   \
        sizeof(ptr_map_segment) +                                              \
        PTR_MAP_SEGMENT_CAPACITY * sizeof(ast_elem*)))

typedef struct tauc_s tauc;
typedef struct ast_elem_s ast_elem;

typedef struct ptr_map_segment_s {
    ureg filled_bits;
    rwlock lock;
    // joint alloc: followed by PTR_MAP_SEGMENT_SIZE type** 's
} ptr_map_segment;

typedef struct ptr_map_segment_ref_s {
    ureg filled_bits;
    ptr_map_segment* segment;
} ptr_map_segment_ref;

typedef struct global_ptr_map_s {
    rwlock lock;
    ptr_map_segment** segments;
    pool segment_mem;
    ureg segment_capacity;
    atomic_ureg type_ids;
    tauc* t;
} global_ptr_map;

typedef struct ptr_map_s {
    global_ptr_map* gpm;
    ptr_map_segment_ref* segment_refs;
    ureg segment_capacity;
    ureg free_type_ids_start;
    ureg free_type_ids_end;
    ureg free_backend_ids_start;
    ureg free_backend_ids_end;
} ptr_map;

int ptr_map_init(ptr_map* m, global_ptr_map* gtm);
int global_ptr_map_init(global_ptr_map* gtm, tauc* t);
void ptr_map_fin(ptr_map* m);
void global_ptr_map_fin(global_ptr_map* gtm);

type_pointer* ptr_map_get_pointer(
    ptr_map* pm, ast_elem* base_type, ureg ptr_id, bool is_const,
    ureg non_const_id, pool* type_mem);
type_slice* ptr_map_get_slice(
    ptr_map* pm, ast_elem* ctype_members, ureg slice_id, bool is_const,
    ureg non_const_id, pool* type_mem);

ureg ptr_map_claim_id(ptr_map* tm);
ureg ptr_map_claim_backend_id(ptr_map* tm);