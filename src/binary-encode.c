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
#include <assert.h>
#include <stdbool.h>

#include "binary.h"

#include "byteswap.h"

static
unsigned char* encode_ushort_l(uint16_t val, unsigned char* ptr, ptrdiff_t *k) {
    val = htol(val);
    memcpy(ptr, &val, sizeof(val));
    *k += sizeof(val);
    return ptr + sizeof(val);
}

static
unsigned char* encode_uint_l(uint32_t val, unsigned char* ptr, ptrdiff_t *k) {
    val = htol(val);
    memcpy(ptr, &val, sizeof(val));
    *k += sizeof(val);
    return ptr + sizeof(val);
}

static
unsigned char* encode_ulong_l(uint64_t val, unsigned char* ptr, ptrdiff_t *k) {
    val = htol(val);
    memcpy(ptr, &val, sizeof(val));
    *k += sizeof(val);
    return ptr + sizeof(val);
}

static
unsigned char* encode_float_l(float val, unsigned char* ptr, ptrdiff_t *k) {
    uint32_t u;
    memcpy(&u, &val, sizeof(val));
    u = htol(u);
    memcpy(ptr, &u, sizeof(u));
    *k += sizeof(val);
    return ptr + sizeof(val);
}

static
unsigned char* encode_double_l(double val, unsigned char* ptr, ptrdiff_t *k) {
    uint64_t u;
    memcpy(&u, &val, sizeof(val));
    u = htol(u);
    memcpy(ptr, &u, sizeof(u));
    *k += sizeof(val);
    return ptr + sizeof(val);
}

static
unsigned char* encode_ushort_b(uint16_t val, unsigned char* ptr, ptrdiff_t *k) {
    val = htob(val);
    memcpy(ptr, &val, sizeof(val));
    *k += sizeof(val);
    return ptr + sizeof(val);
}

static
unsigned char* encode_uint_b(uint32_t val, unsigned char* ptr, ptrdiff_t *k) {
    val = htob(val);
    memcpy(ptr, &val, sizeof(val));
    *k += sizeof(val);
    return ptr + sizeof(val);
}

static
unsigned char* encode_ulong_b(uint64_t val, unsigned char* ptr, ptrdiff_t *k) {
    val = htob(val);
    memcpy(ptr, &val, sizeof(val));
    *k += sizeof(val);
    return ptr + sizeof(val);
}

static
unsigned char* encode_float_b(float val, unsigned char* ptr, ptrdiff_t *k) {
    uint32_t u;
    memcpy(&u, &val, sizeof(val));
    u = htob(u);
    memcpy(ptr, &u, sizeof(u));
    *k += sizeof(val);
    return ptr + sizeof(val);
}

static
unsigned char* encode_double_b(double val, unsigned char* ptr, ptrdiff_t *k) {
    uint64_t u;
    memcpy(&u, &val, sizeof(val));
    u = htob(u);
    memcpy(ptr, &u, sizeof(u));
    *k += sizeof(val);
    return ptr + sizeof(val);
}

static
size_t encode_length_v(uint64_t val) {
    size_t n = 0;
    do {
        val = val >> 7;
        ++n;
    } while (val);
    return n;
}

static
unsigned char* encode_ulong_v(uint64_t val, unsigned char* ptr, ptrdiff_t *k) {
    size_t n = 0;
    do {
        uint8_t b = val & 0x7F;
        val = val >> 7;
        if(val) {
            b = b | 0x80;
        }
        ptr[n++] = b;
    } while (val);
    *k += n;
    return ptr + n;
}


static
unsigned char* encode_ushort_v(uint16_t val, unsigned char* ptr, ptrdiff_t *k) {
    return encode_ulong_v(val, ptr, k);
}

static
unsigned char* encode_uint_v(uint32_t val, unsigned char* ptr, ptrdiff_t *k) {
    return encode_ulong_v(val, ptr, k);
}

static
size_t encode_ushort_length_approx(uint16_t val, size_t n, char fmt) {
    if(fmt == 'v' || fmt == 'V') {
        return n >= 3 ? 3 : encode_length_v(val);
    }
    return sizeof(val);
}

static
size_t encode_uint_length_approx(uint32_t val, size_t n, char fmt) {
    if(fmt == 'v' || fmt == 'V') {
        return n >= 5 ? 5 : encode_length_v(val);
    }
    return sizeof(val);
}

static
size_t encode_ulong_length_approx(uint64_t val, size_t n, char fmt) {
    if(fmt == 'v' || fmt == 'V') {
        return n >= 10 ? 10 : encode_length_v(val);
    }
    return sizeof(val);
}

unsigned char* encode_ubyte(uint8_t val, unsigned char* ptr, size_t n,
    char fmt, ptrdiff_t *k) {
    assert(ptr != NULL);
    assert(k != NULL);

    *k += sizeof(val);
    if(n < sizeof(val)) {
        return ptr + n;
    }
    *ptr = val;
    return ptr + sizeof(val);
}

unsigned char* encode_ushort(uint16_t val, unsigned char* ptr, size_t n,
    char fmt, ptrdiff_t *k) {
    assert(ptr != NULL);
    assert(k != NULL);

    size_t z = encode_ushort_length_approx(val, n, fmt);
    if(n < z) {
        *k += z;
        return ptr + n;
    }
    if(fmt == 'v' || fmt == 'V') {
        return encode_ushort_v(val, ptr, k);
    } else if(fmt == 'b' || fmt == 'B') {
        return encode_ushort_b(val, ptr, k);
    }
    return encode_ushort_l(val, ptr, k);
}

unsigned char* encode_uint(uint32_t val, unsigned char* ptr, size_t n,
    char fmt, ptrdiff_t *k) {
    assert(ptr != NULL);
    assert(k != NULL);

    size_t z = encode_uint_length_approx(val, n, fmt);
    if(n < z) {
        *k += z;
        return ptr + n;
    }
    if(fmt == 'v' || fmt == 'V') {
        return encode_uint_v(val, ptr, k);
    } else if(fmt == 'b' || fmt == 'B') {
        return encode_uint_b(val, ptr, k);
    }
    return encode_uint_l(val, ptr, k);
}

unsigned char* encode_ulong(uint64_t val, unsigned char* ptr, size_t n,
    char fmt, ptrdiff_t *k) {
    assert(ptr != NULL);
    assert(k != NULL);

    size_t z = encode_ulong_length_approx(val, n, fmt);
    if(n < z) {
        *k += z;
        return ptr + n;
    }
    if(fmt == 'v' || fmt == 'V') {
        return encode_ulong_v(val, ptr, k);
    } else if(fmt == 'b' || fmt == 'B') {
        return encode_ulong_b(val, ptr, k);
    }
    return encode_ulong_l(val, ptr, k);
}

unsigned char* encode_float(float val, unsigned char* ptr, size_t n,
    char fmt, ptrdiff_t *k) {
    assert(ptr != NULL);
    assert(k != NULL);

    if(n < sizeof(val)) {
        *k += sizeof(val);
        return ptr + n;
    }
    if(fmt == 'b' || fmt == 'B') {
        return encode_float_b(val, ptr, k);
    }
    return encode_float_l(val, ptr, k);
}

unsigned char* encode_double(double val, unsigned char* ptr, size_t n,
    char fmt, ptrdiff_t *k) {
    assert(ptr != NULL);
    assert(k != NULL);

    if(n < sizeof(val)) {
        *k += sizeof(val);
        return ptr + n;
    }
    if(fmt == 'b' || fmt == 'B') {
        return encode_double_b(val, ptr, k);
    }
    return encode_double_l(val, ptr, k);
}

unsigned char* encode_sbyte(int8_t val, unsigned char* ptr, size_t n,
    char fmt, ptrdiff_t *k) {
    return encode_ubyte(val, ptr, n, fmt, k);
}

unsigned char* encode_sshort(int16_t val, unsigned char* ptr, size_t n,
    char fmt, ptrdiff_t *k) {
    uint16_t u;
    if((fmt == 'v' || fmt == 'V')) {
        u = (2 * val) ^ (val >> 15);
    } else {
        u = val;
    }
    return encode_ushort(u, ptr, n, fmt, k);
}

unsigned char* encode_sint(int32_t val, unsigned char* ptr, size_t n,
    char fmt, ptrdiff_t *k) {
    uint32_t u;
    if((fmt == 'v' || fmt == 'V')) {
        u = (2 * val) ^ (val >> 31);
    } else {
        u = val;
    }
    return encode_uint(u, ptr, n, fmt, k);
}

unsigned char* encode_slong(int64_t val, unsigned char* ptr, size_t n,
    char fmt, ptrdiff_t *k) {
    uint64_t u;
    if((fmt == 'v' || fmt == 'V')) {
        u = (2 * val) ^ (val >> 63);
    } else {
        u = val;
    }
    return encode_ulong(u, ptr, n, fmt, k);
}
