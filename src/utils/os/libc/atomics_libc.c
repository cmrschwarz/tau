#include "../../threading.h"
#ifdef USE_LIBC_ATOMICS
#include <stdatomic.h>
// ptr
void atomic_ptr_init(atomic_ptr* a, void* value)
{
    atomic_init(&a->val, value);
}
void* atomic_ptr_load(atomic_ptr* a)
{
    return atomic_load(&a->val);
}
void* atomic_ptr_load_flat(atomic_ptr* a)
{
    return a->val;
}
void atomic_ptr_store(atomic_ptr* a, void* value)
{
    atomic_store(&a->val, value);
}
void atomic_ptr_store_flat(atomic_ptr* a, void* value)
{
    a->val = value;
}
bool atomic_ptr_cas(atomic_ptr* a, void** oldval, void* newval)
{
    return atomic_compare_exchange_weak(&a->val, oldval, newval);
}

// bool
void atomic_boolean_init(atomic_boolean* a, bool value)
{
    atomic_init(&a->val, value);
}
bool atomic_boolean_load(atomic_boolean* a)
{
    return atomic_load(&a->val);
}
bool atomic_boolean_load_flat(atomic_boolean* a)
{
    return a->val;
}
void atomic_boolean_store(atomic_boolean* a, bool value)
{
    atomic_store(&a->val, value);
}
void atomic_boolean_store_flat(atomic_boolean* a, bool value)
{
    a->val = value;
}
bool atomic_boolean_cas(atomic_boolean* a, bool* oldval, bool newval)
{
    return atomic_compare_exchange_weak(&a->val, oldval, newval);
}
bool atomic_boolean_swap(atomic_boolean* a, bool newval)
{
    return atomic_exchange(&a->val, newval);
}

// ureg
void atomic_ureg_init(atomic_ureg* a, ureg value)
{
    atomic_init(&a->val, value);
}
ureg atomic_ureg_load(atomic_ureg* a)
{
    return atomic_load(&a->val);
}
ureg atomic_ureg_load_flat(atomic_ureg* a)
{
    return a->val;
}
void atomic_ureg_store(atomic_ureg* a, ureg value)
{
    atomic_store(&a->val, value);
}
void atomic_ureg_store_flat(atomic_ureg* a, ureg value)
{
    a->val = value;
}
ureg atomic_ureg_inc(atomic_ureg* a)
{
    return atomic_fetch_add(&a->val, 1);
}
ureg atomic_ureg_dec(atomic_ureg* a)
{
    return atomic_fetch_sub(&a->val, 1);
}
ureg atomic_ureg_add(atomic_ureg* a, ureg v)
{
    return atomic_fetch_add(&a->val, v);
}
ureg atomic_ureg_sub(atomic_ureg* a, ureg v)
{
    return atomic_fetch_sub(&a->val, v);
}
bool atomic_ureg_cas(atomic_ureg* a, ureg* oldval, ureg newval)
{
    return atomic_compare_exchange_weak(&a->val, oldval, newval);
}

// sreg
void atomic_sreg_init(atomic_sreg* a, sreg value)
{
    atomic_init(&a->val, value);
}
sreg atomic_sreg_load(atomic_sreg* a)
{
    return atomic_load(&a->val);
}
sreg atomic_sreg_load_flat(atomic_sreg* a)
{
    return a->val;
}
void atomic_sreg_store(atomic_sreg* a, sreg value)
{
    atomic_store(&a->val, value);
}
void atomic_sreg_store__flat(atomic_sreg* a, sreg value)
{
    a->val = value;
}
sreg atomic_sreg_inc(atomic_sreg* a)
{
    return atomic_fetch_add(&a->val, 1);
}
sreg atomic_sreg_dec(atomic_sreg* a)
{
    return atomic_fetch_sub(&a->val, 1);
}
sreg atomic_sreg_add(atomic_sreg* a, sreg v)
{
    return atomic_fetch_add(&a->val, v);
}
sreg atomic_sreg_sub(atomic_sreg* a, sreg v)
{
    return atomic_fetch_sub(&a->val, v);
}
bool atomic_sreg_cas(atomic_sreg* a, sreg* oldval, sreg newval)
{
    return atomic_compare_exchange_weak(&a->val, oldval, newval);
}

#endif
