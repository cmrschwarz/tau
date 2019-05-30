#pragma once
#include "allocator.h"
#include "math_utils.h"
#include "types.h"

typedef struct sbuffer_segment {
    u8* start; // needed for efficient removal
    u8* head;
    u8* end;
    struct sbuffer_segment* next;
    struct sbuffer_segment* prev;
} sbuffer_segment;

typedef struct sbuffer {
    sbuffer_segment* first;
    sbuffer_segment* last;
} sbuffer;

typedef struct sbi {
    sbuffer_segment* seg;
    u8* pos;
} sbi;

int sbuffer_init(sbuffer* sb, ureg initial_capacity);
void sbuffer_fin(sbuffer* sb);
void* sbuffer_append(sbuffer* sb, ureg size);
void sbuffer_remove(sbuffer* sb, sbi* sbi, ureg size);
void* sbuffer_insert(sbuffer* sb, sbi* sbi, ureg size);

static inline void sbi_begin(sbi* sbi, sbuffer* sb)
{
    sbi->seg = sb->first;
    sbi->pos = sbi->seg->start;
}
static inline void sbi_begin_at_end(sbi* sbi, sbuffer* sb, ureg elem_size)
{
    sbi->seg = sb->last;
    while (ptradd(sbi->seg->start, elem_size) > (void*)sbi->seg->head) {
        sbi->seg = sbi->seg->prev;
    }
    sbi->pos = ptrsub(sb->last->head, elem_size);
}
static inline void* sbi_get(sbi* sbi, ureg size)
{
    if (sbi->seg->head >= sbi->pos + size) return sbi->pos;
    return NULL;
}
static inline void* sbi_next_vs(sbi* sbi, ureg step, ureg expected_item_size)
{
    sbi->pos += step;
    if (sbi->seg->head >= sbi->pos + expected_item_size) return sbi->pos;
    if (sbi->seg->next != NULL) {
        sbi->seg = sbi->seg->next;
        sbi->pos = sbi->seg->start;
        if (sbi->pos + expected_item_size <= sbi->seg->head) return sbi->pos;
        // we either reached the end or an invalid item size was requested
        return NULL;
    }
    else {
        // restore prev. state and return as we reached the end
        sbi->pos -= step;
        return NULL;
    }
}
static inline void* sbi_next(sbi* sbi, ureg step)
{
    return sbi_next_vs(sbi, step, step);
}
static inline void* sbi_previous(sbi* sbi, ureg step)
{
    sbi->pos -= step;
    if (sbi->pos >= sbi->seg->start) return sbi->pos;
    if (sbi->seg->prev != NULL) {
        sbi->seg = sbi->seg->prev;
        sbi->pos = sbi->seg->start;
        if (sbi->seg->start != sbi->seg->head) return sbi->pos;
        return NULL;
    }
    else {
        sbi->pos += step;
        return NULL;
    }
}
