#include "sbuffer.h"
#include "allocator.h"
#include "math_utils.h"
#include <memory.h>

int sbuffer_init(sbuffer* sb, ureg pages_per_segment)
{
    sb->first = sbuffer_segment_create(sb, PAGE_SIZE);
    if (!sb->first) return -1;
    sb->last = sb->first;
    sb->first->next = NULL;
    sb->first->prev = NULL;
    return 0;
}
void sbuffer_fin(sbuffer* sb)
{
    sbuffer_segment* d;
    do {
        d = sb->last;
        sb->last = d->prev;
        tfree(d);
    } while (sb->last != NULL);
}

sbuffer_segment* sbuffer_segment_create(sbuffer* sb, ureg size)
{
    sbuffer_segment* seg = (sbuffer_segment*)tmalloc(size);
    if (!seg) return NULL;
    seg->end = (u8*)ptradd(seg, size);
    seg->start = (u8*)(seg + 1);
    seg->head = seg->start;
    return seg;
}
int sbuffer_segment_append(sbuffer* sb, ureg size)
{
    // we always double the segment size
    sbuffer_segment* sn = sbuffer_segment_create(sb, size);
    if (!sn) return -1;
    sn->next = NULL;
    sn->prev = sb->last;
    sb->last->next = sn;
    sb->last = sn;
    return 0;
}

void* sbuffer_insert(sbuffer* sb, sbi* sbi, ureg size)
{
    sbuffer_segment* cs = sbi->seg;
    ureg free_space_after = ptrdiff(cs->end, cs->head);
    ureg used_space_after = ptrdiff(cs->head, sbi->pos);
    if (free_space_after >= size) {
        // enough space at the end of the segment
        memmove(sbi->pos + size, sbi->pos, used_space_after);
        cs->head += size;
        return sbi->pos;
    }
    ureg free_space_before =
        ptrdiff(cs->start, (u8*)cs + sizeof(sbuffer_segment));
    ureg used_space_before = ptrdiff(sbi->pos, cs->start);
    if (free_space_before >= size) {
        // enough space at the beginning of the segment
        memmove(cs->start - size, cs->start, used_space_before);
        cs->start -= size;
        sbi->pos -= size;
        return sbi->pos;
    }
    if (free_space_before + free_space_after >= size) {
        memmove(cs->start - free_space_before, cs->start, used_space_before);
        cs->start -= free_space_before;
        memmove(
            cs->start + used_space_before + size, sbi->pos, used_space_after);
        sbi->pos = cs->start + used_space_before;
        cs->head = sbi->pos + size + used_space_after;
        return sbi->pos;
    }
    if (cs->prev != NULL && used_space_before + free_space_before >= size) {
        ureg prev_free_space_after = ptrdiff(cs->prev->end, cs->prev->head);
        if (prev_free_space_after >= used_space_before) {
            memcpy(cs->prev->head, cs->start, used_space_before);
            cs->prev->head += used_space_before;
            sbi->pos -= size;
            cs->start = sbi->pos;
            return sbi->pos;
        }
        ureg prev_free_space_before =
            ptrdiff(cs->prev->start, (u8*)(cs->prev) + sizeof(sbuffer_segment));
        if (prev_free_space_after + prev_free_space_before >=
            used_space_before) {
            ureg mov_delta = used_space_before - prev_free_space_after;
            memmove(
                cs->prev->start - mov_delta, cs->prev->start,
                ptrdiff(cs->prev->head, cs->prev->start));
            cs->prev->start -= mov_delta;
            cs->prev->head -= mov_delta;
            memcpy(cs->prev->head, cs->start, used_space_before);
            cs->prev->head += used_space_before;
            sbi->pos -= size;
            cs->start = sbi->pos;
            return sbi->pos;
        }
    }

    if (cs->next != NULL && used_space_after + free_space_after >= size) {
        ureg next_free_space_before =
            ptrdiff(cs->next->start, (u8*)cs->next + sizeof(sbuffer_segment));
        if (next_free_space_before >= used_space_after) {
            cs->next->start -= used_space_after;
            memcpy(cs->next->start, sbi->pos, used_space_after);
            cs->head = sbi->pos + size;
            return sbi->pos;
        }
        ureg next_free_space_after = cs->next->end - cs->next->head;
        if (next_free_space_before + next_free_space_after >=
            used_space_after) {
            ureg mov_delta = used_space_after - next_free_space_before;
            memmove(
                cs->next->start + mov_delta, cs->next->start,
                ptrdiff(cs->next->head, cs->next->start));
            cs->next->start += mov_delta;
            cs->next->head += mov_delta;
            memcpy(
                cs->next->start - used_space_after, sbi->pos, used_space_after);
            cs->next->start -= used_space_after;
            cs->head = sbi->pos + size;
            return sbi->pos;
        }
    }
    ureg seg_size = ptrdiff(cs->end, cs);
    if (used_space_before >= used_space_after) {
        bool move_ins_too = false;
        if (used_space_after + free_space_after < size) {
            move_ins_too = true;
            used_space_after += size;
        }
        if (seg_size < used_space_after) {
            seg_size = ((used_space_after / seg_size) + 1) * seg_size;
        }
        sbuffer_segment* s;
        if (cs->next == NULL) {
            if (sbuffer_segment_append(sb, seg_size)) return NULL;
            s = sb->last;
        }
        else {
            s = sbuffer_segment_create(sb, seg_size);
            s->next = cs->next;
            s->prev = cs;
            s->next->prev = s;
            cs->next = s;
        }
        if (move_ins_too) {
            used_space_after -= size;
            memcpy(s->start + size, sbi->pos, used_space_after);
            cs->head = sbi->pos;
            sbi->pos = s->start;
            sbi->seg = s;
            return sbi->pos;
        }
        else {
            memcpy(s->start, sbi->pos, used_space_after);
            s->head += used_space_after;
            cs->head = sbi->pos + size;
            return sbi->pos;
        }
    }
    else {
        bool move_ins_too = false;
        if (used_space_before + free_space_before < size) {
            move_ins_too = true;
            used_space_before += size;
        }
        if (seg_size < used_space_before) {
            seg_size = ((used_space_before / seg_size) + 1) * seg_size;
        }
        sbuffer_segment* s;
        if (cs->prev == NULL) {
            s = sbuffer_segment_create(sb, seg_size);
            if (!s) return NULL;
            sb->first = s;
            s->next = cs;
            cs->prev = s;
            s->prev = NULL;
        }
        else {
            s = sbuffer_segment_create(sb, seg_size);
            if (!s) return NULL;
            s->next = cs;
            s->prev = cs->prev;
            cs->prev->next = s;
            cs->prev = s;
        }
        if (move_ins_too) {
            s->head += used_space_before;
            used_space_before -= size;
            memcpy(s->start, cs->start, used_space_before);
            cs->start = sbi->pos;
            sbi->pos = s->start + used_space_before;
            sbi->seg = s;
            return sbi->pos;
        }
        else {
            memcpy(s->start, cs->start, used_space_before);
            s->head += used_space_before;
            sbi->pos -= size;
            cs->start = sbi->pos;
            return sbi->pos;
        }
    }
}
