/*
# Copyright (c) 2025 Reed A. Cartwright <reed@cartwright.ht>
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.
*/

#define R_NO_REMAP

#include <string.h>
#include "binary.h"

#include "byteswap.h"

static
const unsigned char* decode_ushort_l(uint16_t* val, const unsigned char* ptr) {
    memcpy(val, ptr, sizeof(*val));
    *val = ltoh(*val);
    return ptr + sizeof(*val);
}

static
const unsigned char* decode_uint_l(uint32_t* val, const unsigned char* ptr) {
    memcpy(val, ptr, sizeof(*val));
    *val = ltoh(*val);
    return ptr + sizeof(*val);
}

static
const unsigned char* decode_ulong_l(uint64_t* val, const unsigned char* ptr) {
    memcpy(val, ptr, sizeof(*val));
    *val = ltoh(*val);
    return ptr + sizeof(*val);
}

static
const unsigned char* decode_float_l(float* val, const unsigned char* ptr) {
    uint32_t u;
    memcpy(&u, ptr, sizeof(u));
    u = ltoh(u);
    memcpy(val, &u, sizeof(*val));
    return ptr + sizeof(*val);
}

static
const unsigned char* decode_double_l(double* val, const unsigned char* ptr) {
    uint64_t u;
    memcpy(&u, ptr, sizeof(u));
    u = ltoh(u);
    memcpy(val, &u, sizeof(*val));
    return ptr + sizeof(*val);
}

static
const unsigned char* decode_ushort_b(uint16_t* val, const unsigned char* ptr) {
    memcpy(val, ptr, sizeof(*val));
    *val = btoh(*val);
    return ptr + sizeof(*val);
}

static
const unsigned char* decode_uint_b(uint32_t* val, const unsigned char* ptr) {
    memcpy(val, ptr, sizeof(*val));
    *val = btoh(*val);
    return ptr + sizeof(*val);
}

static
const unsigned char* decode_ulong_b(uint64_t* val, const unsigned char* ptr) {
    memcpy(val, ptr, sizeof(*val));
    *val = btoh(*val);
    return ptr + sizeof(*val);
}

static
const unsigned char* decode_float_b(float* val, const unsigned char* ptr) {
    uint32_t u;
    memcpy(&u, ptr, sizeof(u));
    u = btoh(u);
    memcpy(val, &u, sizeof(*val));
    return ptr + sizeof(*val);
}

static
const unsigned char* decode_double_b(double* val, const unsigned char* ptr) {
    uint64_t u;
    memcpy(&u, ptr, sizeof(u));
    u = btoh(u);
    memcpy(val, &u, sizeof(*val));
    return ptr + sizeof(*val);
}

static
const unsigned char* decode_ushort_v(uint16_t* val, const unsigned char* ptr) {
    uint16_t b = (uint8_t)*ptr;
    if((b & 0x80) == 0) {
        *val = b;
        return ptr + 1;
    }
    uint16_t u = b;
    for(int i = 1; i < 3; ++i) {
        b = (uint8_t)ptr[i];
        u += (b - 1) << (i * 7);
        if((b & 0x80) == 0) {
            *val = u;
            return ptr + i + 1;
        }
    }
    return NULL;
}

static
const unsigned char* decode_uint_v(uint32_t* val, const unsigned char* ptr) {
    uint32_t b = (uint8_t)*ptr;
    if((b & 0x80) == 0) {
        *val = b;
        return ptr + 1;
    }
    uint32_t u = b;
    for(int i = 1; i < 5; ++i) {
        b = (uint8_t)ptr[i];
        u += (b - 1) << (i * 7);
        if((b & 0x80) == 0) {
            *val = u;
            return ptr + i + 1;
        }
    }
    return NULL;
}

static
const unsigned char* decode_ulong_v(uint64_t* val, const unsigned char* ptr) {
    uint64_t b = (uint8_t)*ptr;
    if((b & 0x80) == 0) {
        *val = b;
        return ptr + 1;
    }
    uint64_t u = b;
    for(int i = 1; i < 10; ++i) {
        b = (uint8_t)ptr[i];
        u += (b - 1) << (i * 7);
        if((b & 0x80) == 0) {
            *val = u;
            return ptr + i + 1;
        }
    }
    return NULL;
}

const unsigned char *decode_ubyte(uint8_t* val, const unsigned char* ptr, size_t n, char fmt) {
    (void)fmt; // silence warning about unused parameter
    if(n > 0) {
        *val = *ptr;
        return ptr + 1;
    }
    *val = 0;
    return NULL;
}

const unsigned char* decode_sbyte(int8_t* val, const unsigned char* ptr, size_t n, char fmt) {
    return decode_ubyte((uint8_t*)val, ptr, n, fmt);
}

const unsigned char* decode_ushort(uint16_t* val, const unsigned char* ptr, size_t n, char fmt) {
    if(fmt == 'v' || fmt == 'V') {
        // We have enough space to decode a varint if there are at least 3
        // characters or there is a stopping character between here and the
        // end.
        if(n < 3) {
            for(; n > 0; --n) {
                if((ptr[n - 1] & 0x80) == 0) {
                    break;
                }     
            }
            if(n == 0) {
                *val = 0;
                return NULL;
            }
        }
        return decode_ushort_v(val, ptr);
    }
    if(n < sizeof(*val)) {
        *val = 0;
        return NULL;
    }
#ifdef WORDS_BIGENDIAN
    if(fmt == 'l' || fmt == 'L') {
        return decode_ushort_l(val, ptr);
    }
    return decode_ushort_b(val, ptr);
#else
    if(fmt == 'b' || fmt == 'B') {
        return decode_ushort_b(val, ptr);
    }
    return decode_ushort_l(val, ptr);
#endif
}

const unsigned char* decode_uint(uint32_t* val, const unsigned char* ptr, size_t n, char fmt) {
    if(fmt == 'v' || fmt == 'V') {
        // We have enough space to decode a varint if there are at least 5
        // characters or there is a stopping character between here and the
        // end.
        if(n < 5) {
            for(; n > 0; --n) {
                if((ptr[n - 1] & 0x80) == 0) {
                    break;
                }     
            }
            if(n == 0) {
                *val = 0;
                return NULL;
            }
        }
        return decode_uint_v(val, ptr);
    }
    if(n < sizeof(*val)) {
        *val = 0;
        return NULL;
    }
#ifdef WORDS_BIGENDIAN
    if(fmt == 'l' || fmt == 'L') {
        return decode_uint_l(val, ptr);
    }
    return decode_uint_b(val, ptr);
#else
    if(fmt == 'b' || fmt == 'B') {
        return decode_uint_b(val, ptr);
    }
    return decode_uint_l(val, ptr);
#endif
}

const unsigned char* decode_ulong(uint64_t* val, const unsigned char* ptr, size_t n, char fmt) {
    if(fmt == 'v' || fmt == 'V') {
        // We have enough space to decode a varint if there are at least 10
        // characters or there is a stopping character between here and the
        // end.
        if(n < 10) {
            for(; n > 0; --n) {
                if((ptr[n - 1] & 0x80) == 0) {
                    break;
                }     
            }
            if(n == 0) {
                *val = 0;
                return NULL;
            }
        }
        return decode_ulong_v(val, ptr);
    }
    if(n < sizeof(*val)) {
        *val = 0;
        return NULL;
    }
#ifdef WORDS_BIGENDIAN
    if(fmt == 'l' || fmt == 'L') {
        return decode_ulong_l(val, ptr);
    }
    return decode_ulong_b(val, ptr);
#else
    if(fmt == 'b' || fmt == 'B') {
        return decode_ulong_b(val, ptr);
    }
    return decode_ulong_l(val, ptr);
#endif
}

const unsigned char *decode_float(float* val, const unsigned char* ptr, size_t n, char fmt) {
    if(n < sizeof(*val)) {
        *val = 0.0f;
        return NULL;
    }
#ifdef WORDS_BIGENDIAN
    if(fmt == 'l' || fmt == 'L') {
        return decode_float_l(val, ptr);
    }
    return decode_float_b(val, ptr);
#else
    if(fmt == 'b' || fmt == 'B') {
        return decode_float_b(val, ptr);
    }
    return decode_float_l(val, ptr);
#endif
}

const unsigned char *decode_double(double* val, const unsigned char* ptr, size_t n, char fmt) {
    if(n < sizeof(*val)) {
        *val = 0.0;
        return NULL;
    }
#ifdef WORDS_BIGENDIAN
    if(fmt == 'l' || fmt == 'L') {
        return decode_double_l(val, ptr);
    }
    return decode_double_b(val, ptr);
#else
    if(fmt == 'b' || fmt == 'B') {
        return decode_double_b(val, ptr);
    }
    return decode_double_l(val, ptr);
#endif
}

const unsigned char* decode_sshort(int16_t* val, const unsigned char* ptr, size_t n, char fmt) {
    uint16_t u;
    const unsigned char* ret = decode_ushort(&u, ptr, n, fmt);
    if((fmt == 'v' || fmt == 'V')) {
        // decode zig-zag encoding
        *val = (u >> 1) ^ (-(u & 1));
    } else {
        *val = u;
    }
    return ret;
}

const unsigned char* decode_sint(int32_t* val, const unsigned char* ptr, size_t n, char fmt) {
    uint32_t u;
    const unsigned char* ret = decode_uint(&u, ptr, n, fmt);
    if(fmt == 'v' || fmt == 'V') {
        // decode zig-zag encoding
        *val = (u >> 1) ^ (-(u & 1));
    } else {
        *val = u;
    }
    return ret;
}

const unsigned char* decode_slong(int64_t* val, const unsigned char* ptr, size_t n, char fmt) {
    uint64_t u;
    const unsigned char* ret = decode_ulong(&u, ptr, n, fmt);
    if(fmt == 'v' || fmt == 'V') {
        // decode zig-zag encoding
        *val = (u >> 1) ^ (-(u & 1));
    } else {
        *val = u;
    }
    return ret;
}