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
size_t encode_ushort_l(uint16_t val, unsigned char* ptr) {
    val = htol(val);
    memcpy(ptr, &val, sizeof(val));
    return sizeof(val);
}

static
size_t encode_uint_l(uint32_t val, unsigned char* ptr) {
    val = htol(val);
    memcpy(ptr, &val, sizeof(val));
    return sizeof(val);
}

static
size_t encode_ulong_l(uint64_t val, unsigned char* ptr) {
    val = htol(val);
    memcpy(ptr, &val, sizeof(val));
    return sizeof(val);
}

static
size_t encode_float_l(float val, unsigned char* ptr) {
    uint32_t u;
    memcpy(&u, &val, sizeof(val));
    u = htol(u);
    memcpy(ptr, &u, sizeof(u));
    return sizeof(val);
}

static
const size_t encode_double_l(double val, unsigned char* ptr) {
    uint64_t u;
    memcpy(&u, &val, sizeof(val));
    u = htol(u);
    memcpy(ptr, &u, sizeof(u));
    return sizeof(val);
}

static
size_t encode_ushort_b(uint16_t val, unsigned char* ptr) {
    val = htob(val);
    memcpy(ptr, &val, sizeof(val));
    return sizeof(val);
}

static
size_t encode_uint_b(uint32_t val, unsigned char* ptr) {
    val = htob(val);
    memcpy(ptr, &val, sizeof(val));
    return sizeof(val);
}

static
size_t encode_ulong_b(uint64_t val, unsigned char* ptr) {
    val = htob(val);
    memcpy(ptr, &val, sizeof(val));
    return sizeof(val);
}

static
size_t encode_float_b(float val, unsigned char* ptr) {
    uint32_t u;
    memcpy(&u, &val, sizeof(val));
    u = htob(u);
    memcpy(ptr, &u, sizeof(u));
    return sizeof(val);
}

static
size_t encode_double_b(double val, unsigned char* ptr) {
    uint64_t u;
    memcpy(&u, &val, sizeof(val));
    u = htob(u);
    memcpy(ptr, &u, sizeof(u));
    return sizeof(val);
}

static
size_t encode_ulong_v(uint64_t val, unsigned char* ptr) {
    size_t n = 0;
    do {
        uint8_t b = val & 0x7F;
        val = val >> 7;
        if(val) {
            b = b | 0x80;
        }
        ptr[n++] = b;
    } while (val);
    return n;
}

static
size_t encode_ushort_v(uint16_t val, unsigned char* ptr) {
    return encode_ulong_v(val, ptr);
}

static
size_t encode_uint_v(uint32_t val, unsigned char* ptr) {
    return encode_ulong_v(val, ptr);
}

size_t encode_ushort(uint16_t val, unsigned char* ptr, size_t n, char fmt) {
    if(fmt == 'v' || fmt == 'V') {
        if(n < 3 || ptr == NULL) {
            // use a buffer to measure how much space we need
            unsigned char buffer[3];
            size_t k = encode_ushort_v(val, buffer);
            if(k <= n && ptr != NULL) {
                memcpy(ptr, buffer, k);
            }
            return k;
        }
        return encode_ushort_v(val, ptr);
    }
    if(ptr == NULL || n < sizeof(val)) {
        return sizeof(val);
    }
#ifdef WORDS_BIGENDIAN
    if(fmt == 'l' || fmt == 'L') {
        return encode_ushort_l(val, ptr);
    }
    return encode_ushort_b(val, ptr);
#else
    if(fmt == 'b' || fmt == 'B') {
        return encode_ushort_b(val, ptr);
    }
    return encode_ushort_l(val, ptr);
#endif
}

size_t encode_uint(uint32_t val, unsigned char* ptr, size_t n, char fmt) {
    if(fmt == 'v' || fmt == 'V') {
        if(n < 5 || ptr == NULL) {
            // use a buffer to measure how much space we need
            unsigned char buffer[5];
            size_t k = encode_uint_v(val, buffer);
            if(k <= n && ptr != NULL) {
                memcpy(ptr, buffer, k);
            }
            return k;
        }
        return encode_uint_v(val, ptr);
    }
    if(ptr == NULL || n < sizeof(val)) {
        return sizeof(val);
    }
#ifdef WORDS_BIGENDIAN
    if(fmt == 'l' || fmt == 'L') {
        return encode_uint_l(val, ptr);
    }
    return encode_uint_b(val, ptr);
#else
    if(fmt == 'b' || fmt == 'B') {
        return encode_uint_b(val, ptr);
    }
    return encode_uint_l(val, ptr);
#endif
}

size_t encode_ulong(uint32_t val, unsigned char* ptr, size_t n, char fmt) {
    if(fmt == 'v' || fmt == 'V') {
        if(n < 10 || ptr == NULL) {
            // use a buffer to measure how much space we need
            unsigned char buffer[10];
            size_t k = encode_ulong_v(val, buffer);
            if(k <= n && ptr != NULL) {
                memcpy(ptr, buffer, k);
            }
            return k;
        }
        return encode_ulong_v(val, ptr);
    }
    if(ptr == NULL || n < sizeof(val)) {
        return sizeof(val);
    }
#ifdef WORDS_BIGENDIAN
    if(fmt == 'l' || fmt == 'L') {
        return encode_ulong_l(val, ptr);
    }
    return encode_ulong_b(val, ptr);
#else
    if(fmt == 'b' || fmt == 'B') {
        return encode_ulong_b(val, ptr);
    }
    return encode_ulong_l(val, ptr);
#endif
}

size_t encode_float(float val, unsigned char* ptr, size_t n, char fmt) {
    if(ptr == NULL || n < sizeof(val)) {
        return sizeof(val);
    }
#ifdef WORDS_BIGENDIAN
    if(fmt == 'l' || fmt == 'L') {
        return encode_float_l(val, ptr);
    }
    return encode_float_b(val, ptr);
#else
    if(fmt == 'b' || fmt == 'B') {
        return encode_float_b(val, ptr);
    }
    return encode_float_l(val, ptr);
#endif
}

size_t encode_double(double val, unsigned char* ptr, size_t n, char fmt) {
    if(ptr == NULL || n < sizeof(val)) {
        return sizeof(val);
    }
#ifdef WORDS_BIGENDIAN
    if(fmt == 'l' || fmt == 'L') {
        return encode_double_l(val, ptr);
    }
    return encode_double_b(val, ptr);
#else
    if(fmt == 'b' || fmt == 'B') {
        return encode_double_b(val, ptr);
    }
    return encode_double_l(val, ptr);
#endif
}

size_t encode_sshort(int16_t val, unsigned char* ptr, size_t n, char fmt) {
    uint16_t u;
    if((fmt == 'v' || fmt == 'V')) {
        u = (2 * val) ^ (val >> 15);
    } else {
        u = val;
    }
    return encode_ushort(u, ptr, n, fmt);
}

size_t encode_sint(int32_t val, unsigned char* ptr, size_t n, char fmt) {
    uint32_t u;
    if((fmt == 'v' || fmt == 'V')) {
        u = (2 * val) ^ (val >> 31);
    } else {
        u = val;
    }
    return encode_uint(u, ptr, n, fmt);
}

size_t encode_slong(int64_t val, unsigned char* ptr, size_t n, char fmt) {
    uint64_t u;
    if((fmt == 'v' || fmt == 'V')) {
        u = (2 * val) ^ (val >> 63);
    } else {
        u = val;
    }
    return encode_ulong(u, ptr, n, fmt);
}
