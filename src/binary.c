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

#include "binary.h"

const char* decode_ubyte(uint32_t* val, const char* ptr) {
    *val = *ptr;
    return ptr + 1;
}

const char* decode_ushort_l(uint32_t* val, const char* ptr) {
    memcpy(val, ptr, sizeof(*val));
#ifdef WORDS_BIGENDIAN
    *val = __builtin_bswap16(*val);
#endif
    return ptr + sizeof(*val);
}

const char* decode_uint_l(uint32_t* val, const char* ptr) {
    memcpy(val, ptr, sizeof(*val));
#ifdef WORDS_BIGENDIAN
    *val = __builtin_bswap32(*val);
#endif
    return ptr + sizeof(*val);
}

const char* decode_ulong_l(uint32_t* val, const char* ptr) {
    memcpy(val, ptr, sizeof(*val));
#ifdef WORDS_BIGENDIAN
    *val = __builtin_bswap64(*val);
#endif
    return ptr + sizeof(*val);
}

const char* decode_ushort_b(uint32_t* val, const char* ptr) {
    memcpy(val, ptr, sizeof(*val));
#ifndef WORDS_BIGENDIAN
    *val = __builtin_bswap16(*val);
#endif
    return ptr + sizeof(*val);
}

const char* decode_uint_b(uint32_t* val, const char* ptr) {
    memcpy(val, ptr, sizeof(*val));
#ifndef WORDS_BIGENDIAN
    *val = __builtin_bswap32(*val);
#endif
    return ptr + sizeof(*val);
}

const char* decode_ulong_b(uint32_t* val, const char* ptr) {
    memcpy(val, ptr, sizeof(*val));
#ifndef WORDS_BIGENDIAN
    *val = __builtin_bswap64(*val);
#endif
    return ptr + sizeof(*val);
}

const char* decode_uint_v(uint32_t* val, const char* ptr) {
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

const char* decode_uint(uint32_t* val, const char* ptr, size_t n, char fmt) {
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
            if(n == 0){
                return NULL;
            }
        }
        return decode_uint_v(val, ptr);
    }
    if(n < sizeof(*val)) {
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
