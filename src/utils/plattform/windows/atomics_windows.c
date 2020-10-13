#include "../../atomics.h"
#ifdef HOST_OS_WINDOWS
#include "../../c_extensions.h"
#include "sane_windows.h"

#if REG_WIDTH_64
#define InterlockedRegCall(call, ...) CONCAT(call, 64)(__VA_ARGS__)
#elif REG_WIDTH_32
#define InterlockedRegCall(call, ...) call(__VA_ARGS__)
#else
#error unsupported register width
#endif

// ptr
void atomic_ptr_init(atomic_ptr* a, void* value)
{
    a->val = value;
}
void* atomic_ptr_load(atomic_ptr* a)
{
    return (void*)a->val;
}
void* atomic_ptr_load_flat(atomic_ptr* a)
{
    return (void*)a->val;
}
void atomic_ptr_store(atomic_ptr* a, void* value)
{
    a->val = value;
}
void atomic_ptr_store_flat(atomic_ptr* a, void* value)
{
    a->val = value;
}
bool atomic_ptr_cas(atomic_ptr* a, void** oldval, void* newval)
{
    void* found =
        (void*)InterlockedCompareExchangePointer(&a->val, newval, *oldval);
    if (found != *oldval) {
        *oldval = found;
        return false;
    }
    return true;
}

// bool
void atomic_boolean_init(atomic_boolean* a, bool value)
{
    a->val = value;
}
bool atomic_boolean_load(atomic_boolean* a)
{
    return (bool)a->val;
}
bool atomic_boolean_load_flat(atomic_boolean* a)
{
    return (bool)a->val;
}
void atomic_boolean_store(atomic_boolean* a, bool value)
{
    a->val = value;
}
void atomic_boolean_store_flat(atomic_boolean* a, bool value)
{
    a->val = value;
}
bool atomic_boolean_cas(atomic_boolean* a, bool* oldval, bool newval)
{
    bool found = (bool)_InterlockedCompareExchange8(&a->val, newval, *oldval);
    if (found != *oldval) {
        *oldval = found;
        return false;
    }
    return true;
}
bool atomic_boolean_swap(atomic_boolean* a, bool newval)
{
    return InterlockedExchange8(&a->val, (u8)newval);
}

// ureg
void atomic_ureg_init(atomic_ureg* a, ureg value)
{
    a->val = value;
}
ureg atomic_ureg_load(atomic_ureg* a)
{
    return a->val;
}
ureg atomic_ureg_load_flat(atomic_ureg* a)
{
    return a->val;
}
void atomic_ureg_store(atomic_ureg* a, ureg value)
{
    a->val = value;
}
void atomic_ureg_store_flat(atomic_ureg* a, ureg value)
{
    a->val = value;
}
ureg atomic_ureg_inc(atomic_ureg* a)
{
    return InterlockedRegCall(InterlockedIncrement, &a->val) - 1;
}
ureg atomic_ureg_dec(atomic_ureg* a)
{
    return InterlockedRegCall(InterlockedDecrement, &a->val) + 1;
}
ureg atomic_ureg_add(atomic_ureg* a, ureg v)
{
    return InterlockedRegCall(InterlockedExchangeAdd, &a->val, v);
}
ureg atomic_ureg_sub(atomic_ureg* a, ureg v)
{
    return InterlockedRegCall(InterlockedExchangeAdd, &a->val, -(sreg)v);
}
bool atomic_ureg_cas(atomic_ureg* a, ureg* oldval, ureg newval)
{
    ureg found = (ureg)InterlockedRegCall(
        InterlockedCompareExchange, &a->val, newval, *oldval);
    if (found != *oldval) {
        *oldval = found;
        return false;
    }
    return true;
}

// sreg
void atomic_sreg_init(atomic_sreg* a, sreg value)
{
    a->val = value;
}
sreg atomic_sreg_load(atomic_sreg* a)
{
    return a->val;
}
sreg atomic_sreg_load_flat(atomic_sreg* a)
{
    return a->val;
}
void atomic_sreg_store(atomic_sreg* a, sreg value)
{
    a->val = value;
}
void atomic_sreg_store_flat(atomic_sreg* a, sreg value)
{
    a->val = value;
}
sreg atomic_sreg_inc(atomic_sreg* a)
{
    return InterlockedRegCall(InterlockedIncrement, &a->val) - 1;
}
sreg atomic_sreg_dec(atomic_sreg* a)
{
    return InterlockedRegCall(InterlockedDecrement, &a->val) + 1;
}
sreg atomic_sreg_add(atomic_sreg* a, sreg v)
{
    return InterlockedRegCall(InterlockedExchangeAdd, &a->val, v);
}
sreg atomic_sreg_sub(atomic_sreg* a, sreg v)
{
    return InterlockedRegCall(InterlockedExchangeAdd, &a->val, -v);
}
bool atomic_sreg_cas(atomic_sreg* a, sreg* oldval, sreg newval)
{
    sreg found = InterlockedRegCall(
        InterlockedCompareExchange, &a->val, newval, *oldval);
    if (found != *oldval) {
        *oldval = found;
        return false;
    }
    return true;
}

#endif
