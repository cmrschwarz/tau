#include "ast_flags.h"

static inline void bitmask_set_bit(u16* data, ureg offs)
{
    *data = *data | (1 << offs);
}
static inline void bitmask_clear_bit(u16* data, ureg offs)
{
    *data = *data & ~((u16)1 << offs);
}
static inline bool bitmask_get_bit(u16 data, ureg offs)
{
    return data & (1 << offs);
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
    return (data & mask) >> offs;
}

void ast_flags_set_access_mod(ast_flags* f, access_modifier m)
{
    bitmask_set_range(f, ASTF_ACCESS_MODIFIER_OFFSET, m);
}
access_modifier ast_flags_get_access_mod(ast_flags f)
{
    return (access_modifier)(bitmask_get_range(
        f, ASTF_ACCESS_MODIFIER_OFFSET, ASTF_ACCESS_MODIFIER_MASK));
}

void ast_flags_set_const(ast_flags* f)
{
    bitmask_set_bit(f, ASTF_CONST_OFFSET);
}
bool ast_flags_get_const(ast_flags f)
{
    return bitmask_get_bit(f, ASTF_CONST_OFFSET);
}

void ast_flags_set_sealed(ast_flags* f)
{
    bitmask_set_bit(f, ASTF_SEALED_OFFSET);
}
bool ast_flags_get_sealed(ast_flags f)
{
    return bitmask_get_bit(f, ASTF_SEALED_OFFSET);
}

void ast_flags_set_virtual(ast_flags* f)
{
    bitmask_set_bit(f, ASTF_VIRTUAL_OFFSET);
}
bool ast_flags_get_virtual(ast_flags f)
{
    return bitmask_get_bit(f, ASTF_VIRTUAL_OFFSET);
}

void ast_flags_set_static(ast_flags* f)
{
    bitmask_set_bit(f, ASTF_STATIC_OFFSET);
}
bool ast_flags_get_static(ast_flags f)
{
    return bitmask_get_bit(f, ASTF_STATIC_OFFSET);
}

void ast_flags_set_compound_decl(ast_flags* f)
{
    bitmask_set_bit(f, ASTF_COMPUND_DECL_OFFSET);
}
bool ast_flags_get_compound_decl(ast_flags f)
{
    return bitmask_get_bit(f, ASTF_COMPUND_DECL_OFFSET);
}
void ast_flags_set_relative_import(ast_flags* f)
{
    bitmask_set_bit(f, ASTF_RELATIVE_IMPORT_OFFSET);
}
bool ast_flags_get_relative_import(ast_flags f)
{
    return bitmask_get_bit(f, ASTF_RELATIVE_IMPORT_OFFSET);
}

void ast_flags_set_error(ast_flags* f)
{
    bitmask_set_bit(f, ASTF_ERROR_OFFSET);
}
bool ast_flags_get_error(ast_flags f)
{
    return bitmask_get_bit(f, ASTF_ERROR_OFFSET);
}

void ast_flags_set_resolved(ast_flags* f)
{
    bitmask_set_bit(f, ASTF_RESOLVED_OFFSET);
}
bool ast_flags_get_resolved(ast_flags f)
{
    return bitmask_get_bit(f, ASTF_RESOLVED_OFFSET);
}

void ast_flags_set_resolving(ast_flags* f)
{
    bitmask_set_bit(f, ASTF_RESOLVING_OFFSET);
}
void ast_flags_clear_resolving(ast_flags* f)
{
    bitmask_clear_bit(f, ASTF_RESOLVING_OFFSET);
}
bool ast_flags_get_resolving(ast_flags f)
{
    return bitmask_get_bit(f, ASTF_RESOLVING_OFFSET);
}

void ast_flags_set_overloaded_in_pp(ast_flags* f)
{
    bitmask_set_bit(f, ASTF_OVERLOADED_IN_PP_OFFSET);
}
bool ast_flags_get_overloaded_in_pp(ast_flags f)
{
    return bitmask_get_bit(f, ASTF_OVERLOADED_IN_PP_OFFSET);
}
void ast_flags_set_used_in_pp(ast_flags* f)
{
    bitmask_set_bit(f, ASTF_USED_IN_PP_OFFSET);
}
bool ast_flags_get_used_in_pp(ast_flags f)
{
    return bitmask_get_bit(f, ASTF_USED_IN_PP_OFFSET);
}
