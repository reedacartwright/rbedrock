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

#ifndef FRAGMITES_BINARY_H
#define FRAGMITES_BINARY_H

#include <stddef.h>
#include <stdint.h>

const unsigned char* decode_ubyte(uint8_t* val, const unsigned char* ptr, size_t n, char fmt);
const unsigned char* decode_ushort(uint16_t* val, const unsigned char* ptr, size_t n, char fmt);
const unsigned char* decode_uint(uint32_t* val, const unsigned char* ptr, size_t n, char fmt);
const unsigned char* decode_ulong(uint64_t* val, const unsigned char* ptr, size_t n, char fmt);

const unsigned char* decode_sbyte(int8_t* val, const unsigned char* ptr, size_t n, char fmt);
const unsigned char* decode_sshort(int16_t* val, const unsigned char* ptr, size_t n, char fmt);
const unsigned char* decode_sint(int32_t* val, const unsigned char* ptr, size_t n, char fmt);
const unsigned char* decode_slong(int64_t* val, const unsigned char* ptr, size_t n, char fmt);

const unsigned char* decode_float(float* val, const unsigned char* ptr, size_t n, char fmt);
const unsigned char* decode_double(double* val, const unsigned char* ptr, size_t n, char fmt);

#endif
