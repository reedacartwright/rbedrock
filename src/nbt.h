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

#include <R.h>
#include <Rinternals.h>
#include <stdbool.h>

SEXP read_nbt(SEXP r_rawval);
SEXP read_nbt_values(const unsigned char** ptr, const unsigned char* end);
SEXP read_nbt_value(const unsigned char** ptr, const unsigned char* end);

SEXP write_nbt(SEXP r_value);
R_xlen_t write_nbt_value(SEXP r_value, unsigned char** ptr, const unsigned char* end);
R_xlen_t write_nbt_values(SEXP r_value, unsigned char** ptr, const unsigned char* end);

#define return_nbt_error() { Rf_error("Malformed NBT data: at %s, line %d.",  __FILE__, __LINE__ ); return R_NilValue; }
#define return_nbt_error0() { Rf_error("Malformed NBT data: at %s, line %d.",  __FILE__, __LINE__ ); return 0; }
#define return_nbt_error_tag(x) { Rf_error("Malformed NBT data with tag `%d`: at %s, line %d.", (x), __FILE__, __LINE__ ); return R_NilValue; }
