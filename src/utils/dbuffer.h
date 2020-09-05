#pragma once

#include "allocator.h"
#include "types.h"
#include "math_utils.h"
#include <memory.h>

typedef struct dbuffer_s {
    u8* start;
    u8* end;
    u8* head;
} dbuffer;

typedef struct dbuffer_iterator_s {
    u8* pos;
    u8* end;
} dbuffer_iterator;

// 0: success, -1: allocation failiure
int dbuffer_init_with_capacity(dbuffer* db, ureg size);
int dbuffer_init(dbuffer* db);

void dbuffer_set_invalid(dbuffer* db);
bool dbuffer_is_invalid(dbuffer* db);

void dbuffer_fin(dbuffer* db);

ureg dbuffer_get_capacity(dbuffer* db);
ureg dbuffer_get_size(dbuffer* db);
ureg dbuffer_get_free_space(dbuffer* db);

bool dbuffer_is_emtpy(dbuffer* db);

// 0: success, -1: allocation failiure
int dbuffer_set_capacity(dbuffer* db, ureg new_size);

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
void* dbuffer_back(dbuffer* db, ureg size);
void dbuffer_clear(dbuffer* db);

// 0: success, -1: allocation failiure
int dbuffer_append(dbuffer* db, const void* data, ureg size);

static inline void
dbuffer_get_raw(dbuffer* db, void* target, void* pos, ureg size)
{
    memcpy(target, pos, size);
}
static inline void
dbuffer_get(dbuffer* db, void* target, ureg offset, ureg size)
{
    dbuffer_get_raw(db, target, ptradd(db->start, offset), size);
}
static inline void
dbuffer_get_element(dbuffer* db, void* target, ureg idx, ureg size)
{
    dbuffer_get(db, target, idx * size, size);
}
static inline void* dbuffer_get_element_ptr(dbuffer* db, ureg idx, ureg size)
{
    return ptradd(db->start, idx * size);
}

#define dbuffer_append_val(db, val)                                            \
    do {                                                                       \
        (*(typeof(val)*)dbuffer_claim(db, sizeof(val))) = (val);               \
    } while (0)

static inline void dbuffer_iterator_init(dbuffer_iterator* it, dbuffer* db)
{
    it->pos = db->start;
    it->end = db->head;
}

static inline void* dbuffer_iterator_next(dbuffer_iterator* it, ureg size)
{
    if (it->pos == it->end) return NULL;
    void* res = it->pos;
    it->pos += size;
    return res;
}

static inline void* dbuffer_iterator_get(dbuffer_iterator* it, ureg size)
{
    if (it->pos == it->end) return NULL;
    return it->pos;
}