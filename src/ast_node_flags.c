#include "ast_node_flags.h"

#define USED_IN_PP_OFFSET 12
#define DEFINED_IN_PP_OFFSET 11
#define COMPUND_DECL_OFFSET 10
#define REDECLARATION_OFFSET 9
#define PARSE_ERROR_OFFSET 8
#define ACCESS_MODIFIER_OFFSET 6
#define ACCESS_MODIFIER_MASK (3 << ACCESS_MODIFIER_OFFSET)
#define CONST_OFFSET 5
#define SEALED_OFFSET 4
#define VIRTUAL_OFFSET 3
#define STATIC_OFFSET 2
#define RESOLVED_OFFSET 1
#define RESOLVING_OFFSET 0

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

void ast_node_flags_set_access_mod(ast_node_flags* f, access_modifier m)
{
    bitmask_set_range(f, ACCESS_MODIFIER_OFFSET, m);
}
access_modifier ast_node_flags_get_access_mod(ast_node_flags f)
{
    return (access_modifier)(
        bitmask_get_range(f, ACCESS_MODIFIER_OFFSET, ACCESS_MODIFIER_MASK));
}

void ast_node_flags_set_const(ast_node_flags* f)
{
    bitmask_set_bit(f, CONST_OFFSET);
}
bool ast_node_flags_get_const(ast_node_flags f)
{
    return bitmask_get_bit(f, CONST_OFFSET);
}

void ast_node_flags_set_sealed(ast_node_flags* f)
{
    bitmask_set_bit(f, SEALED_OFFSET);
}
bool ast_node_flags_get_sealed(ast_node_flags f)
{
    return bitmask_get_bit(f, SEALED_OFFSET);
}

void ast_node_flags_set_virtual(ast_node_flags* f)
{
    bitmask_set_bit(f, VIRTUAL_OFFSET);
}
bool ast_node_flags_get_virtual(ast_node_flags f)
{
    return bitmask_get_bit(f, VIRTUAL_OFFSET);
}

void ast_node_flags_set_static(ast_node_flags* f)
{
    bitmask_set_bit(f, STATIC_OFFSET);
}
bool ast_node_flags_get_static(ast_node_flags f)
{
    return bitmask_get_bit(f, STATIC_OFFSET);
}

void ast_node_flags_set_compound_decl(ast_node_flags* f)
{
    bitmask_set_bit(f, COMPUND_DECL_OFFSET);
}
bool ast_node_flags_get_compound_decl(ast_node_flags f)
{
    return bitmask_get_bit(f, COMPUND_DECL_OFFSET);
}
void err_flags_set_parse_error(ast_node_flags* f)
{
    bitmask_set_bit(f, PARSE_ERROR_OFFSET);
}
bool err_flags_get_parse_error(ast_node_flags f)
{
    return bitmask_get_bit(f, PARSE_ERROR_OFFSET);
}
void err_flags_set_redeclared(ast_node_flags* f)
{
    bitmask_set_bit(f, REDECLARATION_OFFSET);
}
bool err_flags_get_redeclared(ast_node_flags f)
{
    return bitmask_get_bit(f, REDECLARATION_OFFSET);
}

void ast_node_flags_set_resolved(ast_node_flags* f)
{
    bitmask_set_bit(f, RESOLVED_OFFSET);
}
bool ast_node_flags_get_resolved(ast_node_flags f)
{
    return bitmask_get_bit(f, RESOLVED_OFFSET);
}

void ast_node_flags_set_resolving(ast_node_flags* f)
{
    bitmask_set_bit(f, RESOLVING_OFFSET);
}
void ast_node_flags_clear_resolving(ast_node_flags* f)
{
    bitmask_clear_bit(f, RESOLVING_OFFSET);
}
bool ast_node_flags_get_resolving(ast_node_flags f)
{
    return bitmask_get_bit(f, RESOLVING_OFFSET);
}

void ast_node_flags_set_defined_in_pp(ast_node_flags* f)
{
    bitmask_set_bit(f, DEFINED_IN_PP_OFFSET);
}
bool ast_node_flags_get_defined_in_pp(ast_node_flags f)
{
    return bitmask_get_bit(f, DEFINED_IN_PP_OFFSET);
}
void ast_node_flags_set_used_in_pp(ast_node_flags* f)
{
    bitmask_set_bit(f, USED_IN_PP_OFFSET);
}
bool ast_node_flags_get_used_in_pp(ast_node_flags f)
{
    return bitmask_get_bit(f, USED_IN_PP_OFFSET);
}
