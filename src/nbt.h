/*
# Copyright (c) 2021 Reed A. Cartwright <reed@cartwright.ht>
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
#ifndef FRAGMITES_NBT_H
#define FRAGMITES_NBT_H

#include <R.h>
#include <Rinternals.h>
#include <stdbool.h>

enum NBT_FORMAT { FMT_LE = 0, FMT_BE = 1, FMT_LV = 2, FMT_BV = 3 };
typedef enum NBT_FORMAT nbt_format_t;

SEXP R_read_nbt(SEXP r_rawval, SEXP r_format);
SEXP R_write_nbt(SEXP r_value, SEXP r_format);

SEXP read_nbt_values(const unsigned char** ptr, const unsigned char* end,
                     nbt_format_t fmt);
SEXP read_nbt_value(const unsigned char** ptr, const unsigned char* end,
                    nbt_format_t fmt);

R_xlen_t write_nbt_value(SEXP r_value, unsigned char* ptr, R_xlen_t n,
                         nbt_format_t fmt);
R_xlen_t write_nbt_values(SEXP r_value, unsigned char* ptr, R_xlen_t n,
                          nbt_format_t fmt);

#define return_nbt_error()                                                  \
    {                                                                       \
        Rf_error("Malformed NBT data at %s, line %d.", __FILE__, __LINE__); \
        return R_NilValue;                                                  \
    }
#define return_nbt_error0()                                                 \
    {                                                                       \
        Rf_error("Malformed NBT data at %s, line %d.", __FILE__, __LINE__); \
        return 0;                                                           \
    }
#define return_nbt_error_tag(x)                                           \
    {                                                                     \
        Rf_error("Malformed NBT data with tag `%d` at %s, line %d.", (x), \
                 __FILE__, __LINE__);                                     \
        return R_NilValue;                                                \
    }

#define return_nbt_error_msg(err, msg, ...)                           \
    do {                                                              \
        Rf_error("Malformed NBT data at %s, line %d: " msg, __FILE__, \
                 __LINE__, __VA_ARGS__);                              \
        return (err);                                                 \
    } while(false)

#define return_nbt_error_msgnil(msg, ...) \
    return_nbt_error_msg(R_NilValue, msg, __VA_ARGS__)
#define return_nbt_error_msg0(msg, ...) \
    return_nbt_error_msg(0, msg, __VA_ARGS__)

#endif
