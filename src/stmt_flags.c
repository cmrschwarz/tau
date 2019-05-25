#include "stmt_flags.h"

#define USED_IN_PP_OFFSET 11
#define FROM_PP_OFFSET 10
#define COMPUND_DECL_OFFSET 9
#define REDECL_OFFSET 8
#define PE_OFFSET 7
#define AM_OFFSET 5
#define AM_MASK (3 << AM_OFFSET)
#define CONST_OFFSET 4
#define SEALED_OFFSET 3
#define VIRTUAL_OFFSET 2
#define STATIC_OFFSET 1
#define RESOLVED_OFFSET 0

static inline void bitmask_set_bit(u16* data, ureg offs)
{
    *data = *data | 1 << offs;
}
static inline void bitmask_clear_bit(u16* data, ureg offs)
{
    *data = *data & ~((u16)1 << offs);
}
static inline bool bitmask_get_bit(u16 data, ureg offs)
{
    return (data >> offs) & 0x1;
}
static inline void bitmask_set_range(u16* data, ureg offs, u16 value)
{
    *data = *data | (value << offs);
}
static inline void bitmask_clear_range(u16* data, ureg mask)
{
    *data = *data & ~mask;
}
static inline u16 bitmask_get_range(u16 data, ureg offs, ureg mask)
{
    return (data >> offs) & mask;
}

void stmt_flags_set_access_mod(stmt_flags* f, access_modifier m)
{
    bitmask_set_range(f, AM_OFFSET, m);
}
access_modifier stmt_flags_get_access_mod(stmt_flags f)
{
    return (access_modifier)(bitmask_get_range(f, AM_OFFSET, AM_MASK));
}

void stmt_flags_set_const(stmt_flags* f)
{
    bitmask_set_bit(f, CONST_OFFSET);
}
bool stmt_flags_get_const(stmt_flags f)
{
    return bitmask_get_bit(f, CONST_OFFSET);
}

void stmt_flags_set_sealed(stmt_flags* f)
{
    bitmask_set_bit(f, SEALED_OFFSET);
}
bool stmt_flags_get_sealed(stmt_flags f)
{
    return bitmask_get_bit(f, SEALED_OFFSET);
}

void stmt_flags_set_virtual(stmt_flags* f)
{
    bitmask_set_bit(f, VIRTUAL_OFFSET);
}
bool stmt_flags_get_virtual(stmt_flags f)
{
    return bitmask_get_bit(f, VIRTUAL_OFFSET);
}

void stmt_flags_set_static(stmt_flags* f)
{
    bitmask_set_bit(f, STATIC_OFFSET);
}
bool stmt_flags_get_static(stmt_flags f)
{
    return bitmask_get_bit(f, STATIC_OFFSET);
}

void stmt_flags_set_compound_decl(stmt_flags* f)
{
    bitmask_set_bit(f, COMPUND_DECL_OFFSET);
}
bool stmt_flags_get_compound_decl(stmt_flags f)
{
    return bitmask_get_bit(f, COMPUND_DECL_OFFSET);
}
void err_flags_set_parse_error(stmt_flags* f)
{
    bitmask_set_bit(f, PE_OFFSET);
}
bool err_flags_get_parse_error(stmt_flags f)
{
    return bitmask_get_bit(f, PE_OFFSET);
}
void err_flags_set_redeclared(stmt_flags* f)
{
    bitmask_set_bit(f, REDECL_OFFSET);
}
bool err_flags_get_redeclared(stmt_flags f)
{
    return bitmask_get_bit(f, REDECL_OFFSET);
}

void stmt_flags_set_resolved(stmt_flags* f)
{
    bitmask_set_bit(f, RESOLVED_OFFSET);
}
bool stmt_flags_get_resolved(stmt_flags f)
{
    return bitmask_get_bit(f, RESOLVED_OFFSET);
}
void stmt_flags_set_from_pp(stmt_flags* f)
{
    bitmask_set_bit(f, FROM_PP_OFFSET);
}
bool stmt_flags_get_from_pp(stmt_flags f)
{
    return bitmask_get_bit(f, FROM_PP_OFFSET);
}
void stmt_flags_set_used_in_pp(stmt_flags* f)
{
    bitmask_set_bit(f, USED_IN_PP_OFFSET);
}
bool stmt_flags_get_used_in_pp(stmt_flags f)
{
    return bitmask_get_bit(f, USED_IN_PP_OFFSET);
}
