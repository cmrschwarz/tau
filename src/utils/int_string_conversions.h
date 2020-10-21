#pragma once
#include "types.h"
ureg ureg_get_decimal_digits_count(ureg integer);
ureg sreg_get_decimal_digits_count(sreg integer);

// returns the number of chars written
ureg ureg_to_decimal_string(ureg interger, char* str);
// returns the number of chars written
ureg sreg_to_decimal_string(sreg interger, char* str);
