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
void sbuffer_clear(sbuffer* sb);

static inline void sbi_begin(sbi* sbi, sbuffer* sb)
{
    sbi->seg = sb->first;
    sbi->pos = sbi->seg->start;
}
static inline void sbi_begin_at_end(sbi* sbi, sbuffer* sb)
{
    sbi->seg = sb->last;
    sbi->pos = sb->last->head;
}
static inline void* sbi_get(sbi* sbi, ureg size)
{
    while (true) {
        if (ptrdiff(sbi->seg->head, sbi->pos) >= size) return sbi->pos;
        if (sbi->seg->next == NULL) return NULL;
        sbi->seg = sbi->seg->next;
        sbi->pos = sbi->seg->start;
    }
}
static inline void* sbi_next(sbi* sbi, ureg size)
{
    while (true) {
        if (ptrdiff(sbi->seg->head, sbi->pos) >= size) {
            void* res = sbi->pos;
            sbi->pos += size;
            return res;
        }
        if (sbi->seg->next == NULL) return NULL;
        sbi->seg = sbi->seg->next;
        sbi->pos = sbi->seg->start;
    }
}
static inline void* sbi_previous(sbi* sbi, ureg size)
{
    while (true) {
        if (ptrdiff(sbi->pos, sbi->seg->start) >= size) {
            sbi->pos -= size;
            return sbi->pos;
        }
        if (sbi->seg->prev == NULL) return NULL;
        sbi->seg = sbi->seg->prev;
        sbi->pos = sbi->seg->head;
    }
}
