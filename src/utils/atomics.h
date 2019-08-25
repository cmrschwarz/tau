#ifndef TAUC_UTILS_ATOMICS_H
#define TAUC_UTILS_ATOMICS_H

#include "plattform.h"

#if USE_LIBC
#include "os/libc/atomics_libc.h"
#endif

// Atomics
// cas oldval gets updated only if the cas was not successful
// add,sub,inc and dec return the previously stored value
// load_flat and store_flat are non atomic, for when you have unique access

int atomic_ptr_init(atomic_ptr* a, void* value);
void* atomic_ptr_load(atomic_ptr* a);
void* atomic_ptr_load_flat(atomic_ptr* a);
void atomic_ptr_store(atomic_ptr* a, void* value);
void atomic_ptr_store_flat(atomic_ptr* a, void* value);
bool atomic_ptr_cas(atomic_ptr* a, void** oldval, void* newval);
void atomic_ptr_fin(atomic_ptr* a);

int atomic_bool_init(atomic_bool* a, bool value);
bool atomic_bool_load(atomic_bool* a);
bool atomic_bool_load_flat(atomic_bool* a);
void atomic_bool_store(atomic_bool* a, bool value);
void atomic_bool_store_flat(atomic_bool* a, bool value);
bool atomic_bool_cas(atomic_bool* a, bool* oldval, bool newval);
void atomic_bool_fin(atomic_bool* a);

int atomic_ureg_init(atomic_ureg* a, ureg value);
ureg atomic_ureg_load(atomic_ureg* a);
ureg atomic_ureg_load_flat(atomic_ureg* a);
void atomic_ureg_store(atomic_ureg* a, ureg value);
void atomic_ureg_store_flat(atomic_ureg* a, ureg value);
ureg atomic_ureg_inc(atomic_ureg* a);
ureg atomic_ureg_dec(atomic_ureg* a);
ureg atomic_ureg_add(atomic_ureg* a, ureg v);
ureg atomic_ureg_sub(atomic_ureg* a, ureg v);
bool atomic_ureg_cas(atomic_ureg* a, ureg* oldval, ureg newval);
void atomic_ureg_fin(atomic_ureg* a);

int atomic_sreg_init(atomic_sreg* a, sreg value);
sreg atomic_sreg_load(atomic_sreg* a);
sreg atomic_sreg_load_flat(atomic_sreg* a);
void atomic_sreg_store(atomic_sreg* a, sreg value);
void atomic_sreg_store_flat(atomic_sreg* a, sreg value);
sreg atomic_sreg_inc(atomic_sreg* a);
sreg atomic_sreg_dec(atomic_sreg* a);
sreg atomic_sreg_add(atomic_sreg* a, sreg v);
sreg atomic_sreg_sub(atomic_sreg* a, sreg v);
bool atomic_sreg_cas(atomic_sreg* a, sreg* oldval, sreg newval);
void atomic_sreg_fin(atomic_sreg* a);

#endif