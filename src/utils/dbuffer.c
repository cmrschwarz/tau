#include "dbuffer.h"
#include "allocator.h"
#include "math_utils.h"
#include <memory.h>

int dbuffer_init_with_capacity(
    dbuffer* db, thread_allocator* tal, ureg capacity)
{
    db->tal = tal;
    memblock b;
    if (tal_alloc(tal, capacity, &b)) return -1;
    db->start = b.start;
    db->head = b.start;
    db->end = b.end;
    return 0;
}

int dbuffer_init(dbuffer* db, thread_allocator* tal)
{
    return dbuffer_init_with_capacity(db, tal, PAGE_SIZE);
}
bool dbuffer_is_emtpy(dbuffer* db)
{
    return (db->start == db->head);
}
void dbuffer_fin(dbuffer* db)
{
    memblock b;
    b.start = db->start;
    b.end = db->end;
    tal_free(db->tal, &b);
}
ureg dbuffer_get_size(dbuffer* db)
{
    return db->head - db->start;
}

ureg dbuffer_get_capacity(dbuffer* db)
{
    return ptrdiff(db->end, db->start);
}

ureg dbuffer_get_free_space(dbuffer* db)
{
    return ptrdiff(db->end, db->head);
}

int dbuffer_set_capacity(dbuffer* db, ureg capacity)
{
    memblock b;
    b.start = db->start;
    b.end = db->end;
    ureg filled_space = dbuffer_get_size(db);
    if (tal_realloc(db->tal, filled_space, capacity, &b)) return -1;
    if (ptrdiff(b.end, b.start) >= filled_space) {
        db->head = (u8*)b.start + filled_space;
    }
    else {
        db->head = b.end;
    }
    db->end = b.end;
    db->start = b.start;
    return 0;
}

int dbuffer_grow(dbuffer* db)
{
    return dbuffer_set_capacity(db, dbuffer_get_capacity(db) << 1);
}

bool dbuffer_can_fit(dbuffer* db, ureg required_space)
{
    return (db->head + required_space <= db->end);
}

int dbuffer_reserve(dbuffer* db, ureg space)
{
    if (dbuffer_can_fit(db, space)) {
        ureg capacity = dbuffer_get_capacity(db);
        if (capacity > space) {
            capacity = capacity * 2;
        }
        else {
            capacity = dbuffer_get_size(db) + space;
        }
        return dbuffer_set_capacity(db, capacity);
    }
    return 0;
}
void* dbuffer_claim(dbuffer* db, ureg space)
{
    if (dbuffer_reserve(db, space)) return NULL;
    void* r = db->head;
    db->head += space;
    return r;
}
void dbuffer_pop(dbuffer* db, ureg size)
{
    db->head -= size;
}
void dbuffer_clear(dbuffer* db)
{
    db->head = db->start;
}

void dbuffer_remove_at(dbuffer* db, void* pos, ureg size)
{
    memmove(pos, (u8*)pos + size, db->head - (u8*)pos - size);
    db->head -= size;
}
int dbuffer_insert_at(dbuffer* db, const void* data, void* pos, ureg size)
{
    if (!dbuffer_can_fit(db, size)) {
        int r = dbuffer_reserve(db, size);
        if (r) return r;
        ureg offs = ptrdiff(pos, db->start);
        pos = db->start + offs;
    }
    memmove((u8*)pos + size, pos, ptrdiff(db->head, pos));
    memcpy(pos, data, size);
    db->head += size;
    return 0;
}

void dbuffer_swap(dbuffer* db, void* posa, void* posb, ureg size)
{
    u8* a = posa;
    u8* b = posb;
    if (size < dbuffer_get_free_space(db)) {
        // if we have a big enough temp buffer after head anyway, use it
        memcpy(db->head, b, size);
        memcpy(b, a, size);
        memcpy(a, db->head, size);
    }
    else {
        // otherwise do manual swapping
        // if both pos's are aligned on a sizeof(ureg) boundary
        if (!(((ureg)a) & (sizeof(ureg) - 1)) &&
            !(((ureg)b) & (sizeof(ureg) - 1))) {
            void* aend = a + (size - size % sizeof(ureg));
            ureg temp;
            while (a != aend) {
                temp = *(ureg*)a;
                *(ureg*)a = *(ureg*)b;
                *(ureg*)b = temp;
                a += sizeof(ureg);
                b += sizeof(ureg);
            }
        }
        void* aend = a + size;
        uint8_t temp;
        while (a != aend) {
            temp = *a;
            *a = *b;
            *b = temp;
            a++;
            b++;
        }
    }
}

int dbuffer_append(dbuffer* db, const void* data, ureg size)
{
    if (dbuffer_reserve(db, size)) return -1;
    memcpy(db->head, data, size);
    db->head += size;
    return 0;
}

void dbuffer_get(dbuffer* db, void* target, void* pos, ureg size)
{
    memcpy(target, pos, size);
}
