#include "ast.h"
#define AM_OFFSET 6
#define AM_MASK (3 << AM_OFFSET)
#define CONST_OFFSET 4
#define CONST_MASK (1 << CONST_OFFSET)
#define SEALED_OFFSET 3
#define SEALED_MASK (1 << SEALED_OFFSET)
#define VIRTUAL_OFFSET 2
#define VIRTUAL_MASK (1 << VIRTUAL_OFFSET)
#define STATIC_OFFSET 1
#define STATIC_MASK (1 << STATIC_OFFSET)

void astn_flags_set_access_mod(astn_flags* f, access_modifier m)
{
    *f = *f | (m << AM_OFFSET);
}
access_modifier astn_flags_get_access_mod(astn_flags f)
{
    return (access_modifier)((f & AM_MASK) >> AM_OFFSET);
}

void astn_flags_set_const(astn_flags* f, bool cnst)
{
    *f = *f | (cnst << CONST_OFFSET);
}
bool astn_flags_get_const(astn_flags f)
{
    return (f & CONST_MASK) >> CONST_OFFSET;
}

void astn_flags_set_sealed(astn_flags* f, bool sld)
{
    *f = *f | (sld << SEALED_OFFSET);
}
bool astn_flags_get_sealed(astn_flags f)
{
    return (f & SEALED_MASK) >> SEALED_OFFSET;
}

void astn_flags_set_virtual(astn_flags* f, bool virt)
{
    *f = *f | (virt << VIRTUAL_OFFSET);
}
bool astn_flags_get_virtual(astn_flags f)
{
    return (f & VIRTUAL_MASK) >> VIRTUAL_OFFSET;
}

void astn_flags_set_static(astn_flags* f, bool stat)
{
    *f = *f | (stat << STATIC_OFFSET);
}
bool astn_flags_get_static(astn_flags f)
{
    return (f & STATIC_MASK) >> STATIC_OFFSET;
}