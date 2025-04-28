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

#include <R.h>
#include <Rinternals.h>
#include <stdint.h>

uint8_t read_ubyte(const void *src);

uint16_t read_ushort_l(const void *src);
uint32_t read_uint_l(const void *src);
uint64_t read_ulong_l(const void *src);

uint16_t read_ushort_b(const void *src);
uint32_t read_uint_b(const void *src);
uint64_t read_ulong_b(const void *src);

int16_t read_short(const unsigned char* src, size_t n, unsigned char** end, char fmt);
int32_t read_int(const unsigned char* src, size_t n, unsigned char** end, char fmt);
int64_t read_long(const unsigned char* src, size_t n, unsigned char** end, char fmt);
uint16_t read_ushort(const unsigned char* src, size_t n, unsigned char** end, char fmt);
uint32_t read_uint(const unsigned char* src, size_t n, unsigned char** end, char fmt);
uint64_t read_ulong(const unsigned char* src, size_t n, unsigned char** end, char fmt);

#endif
