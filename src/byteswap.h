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

#ifndef FRAGMITES_BYTESWAP_H
#define FRAGMITES_BYTESWAP_H

#include <Rconfig.h> // WORDS_BIGENDIAN

#define byteswap16(x) __builtin_bswap16(x)
#define byteswap32(x) __builtin_bswap32(x)
#define byteswap64(x) __builtin_bswap64(x)

#ifndef WORDS_BIGENDIAN

// big-endian to host/native
#define btoh16(x) byteswap16(x)
#define btoh32(x) byteswap32(x)
#define btoh64(x) byteswap64(x)

// little-endian to host/native
#define ltoh16(x) (x)
#define ltoh32(x) (x)
#define ltoh64(x) (x)

#else //ifndef WORDS_BIGENDIAN

// big-endian to host/native
#define btoh16(x) (x)
#define btoh32(x) (x)
#define btoh64(x) (x)

// little-endian to host/native
#define ltoh16(x) byteswap16(x)
#define ltoh32(x) byteswap32(x)
#define ltoh64(x) byteswap64(x)

#endif

#define btoh(x) _Generic((x), \
    uint16_t: btoh16(x), \
    uint32_t: btoh32(x), \
    uint64_t: btoh64(x) \
    )

#define ltoh(x) _Generic((x), \
    uint16_t: ltoh16(x), \
    uint32_t: ltoh32(x), \
    uint64_t: ltoh64(x) \
    )

#define htob(x) btoh(x)
#define htol(x) ltoh(x)

#endif
