#include "int_string_conversions.h"
#include "math_utils.h"
#include "stdio.h"

ureg ureg_get_decimal_digits_count(ureg integer)
{
    if (integer < 10) return 1;
    return floor_double_to_ureg(log10(integer)) + 1;
}
ureg sreg_get_decimal_digits_count(sreg integer)
{
    bool neg = false;
    if (integer == SREG_MIN) {
        integer = SREG_MAX;
        neg = true;
    }
    else if (integer < 0) {
        integer = -integer;
        neg = true;
    }
    if (integer < 10) return 1 + neg;
    return floor_double_to_ureg(log10(integer)) + 1 + neg;
}

ureg ureg_to_decimal_string(ureg integer, char* str)
{
    ureg len = ureg_get_decimal_digits_count(integer);
    char* msg;
#if REG_WIDTH_64
    msg = "%llu";
#elif REG_WIDTH_32
    msg = "%lu";
#else
#error
#endif
    ureg res = snprintf(str, len + 1, msg, integer);
    assert(res == len);
    return len;
}

ureg sreg_to_decimal_string(sreg integer, char* str)
{
    ureg len = sreg_get_decimal_digits_count(integer);
    char* msg;
#if REG_WIDTH_64
    msg = "%lli";
#elif REG_WIDTH_32
    msg = "%li";
#else
#error
#endif
    ureg res = snprintf(str, len, msg, integer);
    assert(res == len);
    return len;
}
