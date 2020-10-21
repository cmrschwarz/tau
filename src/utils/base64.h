#pragma once
#include "types.h"
ureg get_base64_encoded_size(ureg src_size);
// returns UREG_MAX if the src_size doesn't represent valid base64
// (this is the case if src_size % 4 == 1)
ureg get_base64_decoded_size(ureg src_size);

// returns the number of bytes written
ureg encode_base64(
    const u8* restrict src, ureg src_size, char c_62, char c_63,
    char* restrict tgt);

// returns the number of bytes written or UREG_MAX on error
ureg decode_base64(
    const char* src_b64, ureg src_size, char c_62, char c_63, u8* tgt);
