#pragma once
#include "allocator.h"
#include "types.h"


typedef struct sbuffer_segment {
    u8* start;
    u8* head;
    u8* end;
    struct sbuffer_segment* next;
    struct sbuffer_segment* prev;
} sbuffer_segment;

typedef struct sbuffer {
    sbuffer_segment* first;
    sbuffer_segment* last;
    thread_allocator* tal;
} sbuffer;

int sbuffer_init(sbuffer* sb, ureg pages_per_segment);
void sbuffer_fin(sbuffer* sb);
sbuffer_segment* sbuffer_segment_create(sbuffer* sb, ureg size);
int sbuffer_segment_append(sbuffer* sb, ureg size);

// Segmented Buffer Iterator -->sbi
typedef struct sbi {
    sbuffer_segment* seg;
    u8* pos;
} sbi;

void* sbuffer_insert(sbuffer* sb, sbi* sbi, ureg size);
static inline void* sbuffer_append(sbuffer* sb, ureg size)
{
    if (sb->last->head + size <= sb->last->end) {
        void* ret_val = sb->last->head;
        sb->last->head += size;
        return ret_val;
    }
    if (sbuffer_segment_append(sb, (sb->last->end - sb->last->start) * 2)) {
        return NULL;
    }
    sb->last->head += size;
    return sb->last->start;
}

static inline void sbi_init(sbi* sbi, sbuffer* sb)
{
    sbi->seg = sb->first;
    sbi->pos = sbi->seg->start;
}
static inline void* sbi_get(sbi* sbi, ureg size)
{
    if (sbi->seg->head >= sbi->pos + size) return sbi->pos;
    return NULL;
}
static inline void* sbi_next(sbi* sbi, ureg step, ureg expected_item_size)
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