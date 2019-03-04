#include "../../threading.h"
#ifdef USE_LIBC_ATOMICS
// ptr
int atomic_ptr_init(atomic_ptr* a, void* value)
{
    atomic_init(a, value);
    return 0;
}
void* atomic_ptr_load(atomic_ptr* a)
{
    return atomic_load(a);
}
void atomic_ptr_store(atomic_ptr* a, void* value)
{
    atomic_store(a, value);
}
bool atomic_ptr_cas(atomic_ptr* a, void** oldval, void* newval)
{
    return atomic_compare_exchange_weak(a, oldval, newval);
}
void atomic_ptr_fin(atomic_ptr* a)
{
}

// bool
int atomic_bool_init(atomic_bool* a, bool value)
{
    atomic_init(a, value);
    return 0;
}
bool atomic_bool_load(atomic_bool* a)
{
    return atomic_load(a);
}
void atomic_bool_store(atomic_bool* a, bool value)
{
    atomic_store(a, value);
}
bool atomic_bool_cas(atomic_bool* a, bool* oldval, bool newval)
{
    return atomic_compare_exchange_weak(a, oldval, newval);
}
void atomic_bool_fin(atomic_bool* a)
{
}

// ureg
int atomic_ureg_init(atomic_ureg* a, ureg value)
{
    atomic_init(a, value);
    return 0;
}
ureg atomic_ureg_load(atomic_ureg* a)
{
    return atomic_load(a);
}
void atomic_ureg_store(atomic_ureg* a, ureg value)
{
    atomic_store(a, value);
}
ureg atomic_ureg_inc(atomic_ureg* a)
{
    return atomic_fetch_add(a, 1);
}
ureg atomic_ureg_dec(atomic_ureg* a)
{
    return atomic_fetch_sub(a, 1);
}
ureg atomic_ureg_add(atomic_ureg* a, ureg v)
{
    return atomic_fetch_add(a, v);
}
ureg atomic_ureg_sub(atomic_ureg* a, ureg v)
{
    return atomic_fetch_sub(a, v);
}
bool atomic_ureg_cas(atomic_ureg* a, ureg* oldval, ureg newval)
{
    return atomic_compare_exchange_weak(a, oldval, newval);
}
void atomic_ureg_fin(atomic_ureg* a)
{
}

// sreg
int atomic_sreg_init(atomic_sreg* a, sreg value)
{
    atomic_init(a, value);
    return 0;
}
sreg atomic_sreg_load(atomic_sreg* a)
{
    return atomic_load(a);
}
void atomic_sreg_store(atomic_sreg* a, sreg value)
{
    atomic_store(a, value);
}
sreg atomic_sreg_inc(atomic_sreg* a)
{
    return atomic_fetch_add(a, 1);
}
sreg atomic_sreg_dec(atomic_sreg* a)
{
    return atomic_fetch_sub(a, 1);
}
sreg atomic_sreg_add(atomic_sreg* a, sreg v)
{
    return atomic_fetch_add(a, v);
}
sreg atomic_sreg_sub(atomic_sreg* a, sreg v)
{
    return atomic_fetch_sub(a, v);
}
bool atomic_sreg_cas(atomic_sreg* a, sreg* oldval, sreg newval)
{
    return atomic_compare_exchange_weak(a, oldval, newval);
}
void atomic_sreg_fin(atomic_sreg* a)
{
}

#endif