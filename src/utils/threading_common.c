#include "threading.h"
void atomic_ureg_init(atomic_ureg* a, ureg value){
    atomic_init(a, value);
}
void atomic_ureg_store(atomic_ureg* a, ureg value){
    atomic_store(a, value);
}
ureg atomic_ureg_load(atomic_ureg* a, ureg value){
    return atomic_load(a);
}
void atomic_ureg_fin(atomic_ureg* a, ureg value){

}

void atomic_sreg_init(atomic_ureg* a, sreg value){
    atomic_init(a, value);
}
void atomic_sreg_store(atomic_ureg* a, sreg value){
    atomic_store(a, value);
}
sreg atomic_sreg_load(atomic_ureg* a, sreg value){
    return atomic_load(a);
}
void atomic_sreg_fin(atomic_ureg* a, sreg value){

}

void atomic_bool_init(atomic_ureg* a, bool value){
    atomic_init(a, value);
}
void atomic_bool_store(atomic_ureg* a, bool value){
    atomic_store(a, value);
}
bool atomic_bool_load(atomic_ureg* a, bool value){
    return atomic_load(a);
}
void atomic_bool_fin(atomic_ureg* a, bool value){

}