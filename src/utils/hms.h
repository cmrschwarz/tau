#ifndef TAUC_UTILS_HMS_H
#define TAUC_UTILS_HMS_H

#include "allocator.h"
#include "types.h"

// hms: hash map for strings
typedef struct hms_node_s {
    const char* key;
    void* value;
} hms_node;

typedef struct hms_s {
    hms_node* map;
    hms_node* map_end;
    ureg elem_count;
    ureg grow_on_elem_count;
    ureg hash_mask;
    ureg size_bits;
} hms;

ureg hms_hashpos(hms* h, const char* key);
int hms_init(hms* h);
int hms_init_with_capacity(hms* h, ureg n);
void hms_fin(hms* h);
int hms_set(hms* h, const char* key, void* value);
void* hms_get(hms* h, const char* key);
hms_node* hms_get_node(hms* h, const char* key);
void* hms_remove(hms* h, const char* key);
int hms_grow(hms* h);

#endif