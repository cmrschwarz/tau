#ifndef TAUC_UTILS_DBUFFER_H
#define TAUC_UTILS_DBUFFER_H

#include "allocator.h"
#include "types.h"

typedef struct dbuffer_s {
    u8* start;
    u8* end;
    u8* head;
} dbuffer;

// 0: success, -1: allocation failiure
int dbuffer_init_with_capacity(dbuffer* db, ureg size);
int dbuffer_init(dbuffer* db);

void dbuffer_fin(dbuffer* db);

ureg dbuffer_get_capacity(dbuffer* db);
ureg dbuffer_get_size(dbuffer* db);
ureg dbuffer_get_free_space(dbuffer* db);

void dbuffer_get(dbuffer* db, void* target, void* pos, ureg size);

bool dbuffer_is_emtpy(dbuffer* db);

// 0: success, -1: allocation failiure
int dbuffer_set_capacity(dbuffer* db, ureg new_size);
int dbuffer_set_bigger_capacity(dbuffer* db, ureg new_size);

bool dbuffer_can_fit(dbuffer* db, ureg required_space);

// 0: success, -1: allocation failiure
int dbuffer_grow(dbuffer* db);
int dbuffer_reserve(dbuffer* db, ureg required_space);

// valid_address: success, NULL: allocation failiure
void* dbuffer_claim(dbuffer* db, ureg required_space);

// 0: success, -1: allocation failiure
int dbuffer_insert_at(dbuffer* db, const void* data, void* pos, ureg size);

void dbuffer_remove_at(dbuffer* db, void* pos, ureg size);
void dbuffer_swap(dbuffer* db, void* posa, void* posb, ureg size);

void dbuffer_pop(dbuffer* db, ureg size);
void dbuffer_clear(dbuffer* db);

// 0: success, -1: allocation failiure
int dbuffer_append(dbuffer* db, const void* data, ureg size);

#define dbuffer_append_val(db, val)                                            \
    do {                                                                       \
        (*(typeof(val)*)dbuffer_claim(db, sizeof(val))) = (val);               \
    } while (0)

#endif
