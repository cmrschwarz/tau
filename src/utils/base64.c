#include "base64.h"
#include "assert.h"

ureg get_base64_encoded_size(ureg src_size)
{
    ureg rem = src_size % 3;
    return (src_size - rem) << 2 | !rem ? 0 : (rem == 1) ? 2 : 3;
}
ureg get_base64_decoded_size(ureg src_size)
{
    ureg rem = src_size % 4;
    if (rem == 1) return UREG_MAX;
    return (src_size - rem) / 4 * 3 + !rem ? 0 : (rem == 2) ? 1 : 2;
}

static inline char encode_6bits_base64(u8 src, char c_62, char c_63)
{
    if (src < 26) {
        return src + 'A';
    }
    else if (src < 52) {
        return src - 26 + 'a';
    }
    else if (src < 62) {
        return src - 52 + '0';
    }
    else if (src == 62) {
        return c_62;
    }
    else {
        return c_63;
    }
}
static inline u8 decode_byte_base64(char src, char c_62, char c_63)
{
    if (src == c_62) return 62;
    if (src == c_63) return 63;
    if (src < '0') return U8_MAX;
    if (src <= '9') return src - '0' + 52;
    if (src < 'A') return U8_MAX;
    if (src <= 'Z') return src - 'A';
    if (src < 'a') return U8_MAX;
    if (src <= 'z') return src - 'a' + 26;
    return U8_MAX;
}

ureg encode_base64(
    const u8* restrict src, ureg src_size, char c_62, char c_63,
    char* restrict tgt)
{
    char* tgt_start = tgt;
    ureg src_size_whole = (src_size / 3) * 3;
    const u8* end = src + src_size_whole;
    while (src != end) {
        u32 data = src[0] << 16 | src[1] << 8 | src[2];
        for (int i = 3 * 6; i != -6; i -= 6) {
            *tgt = encode_6bits_base64(data >> i & 0x3F, c_62, c_63);
            tgt++;
        }
        src += 3;
    }
    ureg src_size_rem = src_size - src_size_whole;
    switch (src_size_rem) {
        case 0: break;
        case 1: {
            *tgt++ = encode_6bits_base64(*src >> 2, c_62, c_63);
            *tgt++ = encode_6bits_base64((*src & 0x3) << 4, c_62, c_63);
            break;
        }
        case 2: {
            u8 s0 = src[0];
            u8 s1 = src[1];
            *tgt++ = encode_6bits_base64(s0 >> 2, c_62, c_63);
            *tgt++ = encode_6bits_base64((s0 & 0x3) << 4 | s1 >> 4, c_62, c_63);
            *tgt++ = encode_6bits_base64((s1 & 0xF) << 2, c_62, c_63);
            break;
        }
        default: assert(false);
    }
    return tgt - tgt_start;
}
ureg decode_base64(
    const char* src, ureg src_size, char c_62, char c_63, u8* tgt)
{
    u8* tgt_start = tgt;
    ureg src_size_whole = (src_size / 4) * 4;
    const char* end = src + src_size_whole;
    while (src != end) {
        u32 data = 0;
        for (int i = 0; i < 4; i++) {
            u8 dec = decode_byte_base64(*src, c_62, c_63);
            if (dec == U8_MAX) return UREG_MAX;
            data = data << 6 | dec;
            src++;
        }
        tgt[0] = data >> 16;
        tgt[1] = data >> 8 & 0xFF;
        tgt[2] = data & 0xFF;
        tgt += 3;
    }
    ureg src_size_rem = src_size - src_size_whole;
    switch (src_size_rem) {
        case 0: break;
        case 1: {
            return UREG_MAX;
            break;
        }
        case 2: {
            u8 s0 = decode_byte_base64(src[0], c_62, c_63);
            if (s0 == U8_MAX) return UREG_MAX;
            u8 s1 = decode_byte_base64(src[1], c_62, c_63);
            if (s1 == U8_MAX || s1 & 0xF) return UREG_MAX;
            *tgt++ = (s0 << 2) | (s1 >> 4);
            break;
        }
        case 3: {
            u8 s0 = decode_byte_base64(src[0], c_62, c_63);
            if (s0 == U8_MAX) return UREG_MAX;
            u8 s1 = decode_byte_base64(src[1], c_62, c_63);
            if (s0 == U8_MAX) return UREG_MAX;
            u8 s2 = decode_byte_base64(src[2], c_62, c_63);
            if (s0 == U8_MAX || s0 & 0x03) return src + 2;
            *tgt++ = s0 << 2 | s1 >> 4;
            *tgt++ = (s1 & 0xF) << 4 | s2 >> 2;
            break;
        }
        default: assert(false);
    }
    return tgt - tgt_start;
    return NULL;
}
