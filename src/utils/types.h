#pragma once

#ifndef __cplusplus
#include <float.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>
#else
#include <cstdint>
#include <cstdlib>
#endif

typedef uint8_t u8;
typedef int8_t s8;
typedef uint16_t u16;
typedef int16_t s16;
typedef uint32_t u32;
typedef int32_t s32;
typedef uint64_t u64;
typedef int64_t s64;

#define U8_MAX 255U
#define U16_MAX 65536U
#define U32_MAX 4294967295U
#define U64_MAX 18446744073709551615ULL

#define S8_MAX 127U
#define S16_MAX 32767U
#define S32_MAX 2147483647U
#define S64_MAX 9223372036854775807L

#define S8_MIN -128
#define S16_MIN -32768
#define S32_MIN -2147483648
#define S64_MIN -9223372036854775808L

#define UREG_MAX UINTPTR_MAX
#define SREG_MAX INTPTR_MAX
#define SREG_MIN INTPTR_MAX

// EXPLORE: on which plattforms is this correct?
typedef float f32;
typedef double f64;
#define F32_MAX FLT_MAX
#define F64_MAX DBL_MAX

#if INTPTR_MAX == INT32_MAX
typedef size_t ureg;
typedef int32_t sreg;
typedef uint16_t uregh;
typedef int16_t sregh;
typedef uint8_t uregq;
typedef int8_t sregq;
#define UREGH_MAX U16_MAX
#define UREGQ_MAX U8_MAX
#define SREGH_MAX S16_MAX
#define SREGQ_MAX S8_MAX
#define SREGH_MIN S16_MIN
#define SREGQ_MIN S8_MIN

typedef f32 freg;
#define FREG_MAX F32_MAX

#define REG_BITS 32
#define REG_BYTES 4
#define REG_WIDTH_32 1
#define REG_WIDTH_64 0

#elif INTPTR_MAX == INT64_MAX
typedef unsigned long long int ureg;
typedef int64_t sreg;
typedef uint32_t uregh;
typedef int32_t sregh;
typedef uint16_t uregq;
typedef int16_t sregq;
#define UREGH_MAX U32_MAX
#define UREGQ_MAX U16_MAX
#define SREGH_MAX S32_MAX
#define SREGQ_MAX S16_MAX
#define SREGH_MIN S32_MIN
#define SREGQ_MIN S16_MIN

typedef f64 freg;
#define FREG_MAX F64_MAX

#define REG_BITS 64
#define REG_BYTES 8
#define REG_WIDTH_32 0
#define REG_WIDTH_64 1
#else
#error "Environment not 32 or 64-bit."
#endif
