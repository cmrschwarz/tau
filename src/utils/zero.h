#ifndef TAUC_UTILS_ZERO_H
#define TAUC_UTILS_ZERO_H

#define NULL_PTR_PTR_CAPACITY 4
#define NULL_PTR_BYTES (NULL_PTR_PTR_CAPACITY * sizeof(void*))
// a bit of zeroed space to point to, e.g. as a linked list terminator
extern void* NULL_BYTES[NULL_PTR_PTR_CAPACITY];
extern void** NULL_PTR_PTR;
#endif
