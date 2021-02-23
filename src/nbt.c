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

#define R_NO_REMAP

#include "nbt.h"

enum NBT_TYPE {
    TAG_END = 0,
    TAG_BYTE,
    TAG_SHORT,
    TAG_INT,
    TAG_LONG,
    TAG_FLOAT,
    TAG_DOUBLE,
    TAG_BYTE_ARRAY,
    TAG_STRING,
    TAG_LIST,
    TAG_COMPOUND,
    TAG_INT_ARRAY,
    TAG_LONG_ARRAY
};
typedef enum NBT_TYPE nbt_type_t;

SEXP read_nbt_payload(const unsigned char* p, const unsigned char* end, nbt_type_t tag);

SEXP read_nbt_payload_character(const unsigned char* p, const unsigned char* end) {
    unsigned short len;
    if(end-p < sizeof(short)) {
        return R_NilValue;
    }
    memcpy(&len, p, sizeof(len));
    p += sizeof(len);
    if(end-p < len) {
        return R_NilValue;        
    }
    return Rf_mkCharLenCE((const char*)p, len, CE_UTF8);
}

SEXP read_nbt_payload_integer(const unsigned char* p, const unsigned char* end, int size, int n) {
    if(end-p < size*n) {
        return R_NilValue;
    }
    SEXP res = PROTECT(Rf_allocVector(INTSXP, n));
    int *x = INTEGER(res);
    for(int i=0; i < n; ++i) {
        memcpy(x+i, p, size);
        p += size;
    }
    UNPROTECT(1);
    return res;
}

SEXP read_nbt_payload_real(const unsigned char* p, const unsigned char* end, int size, int n) {
    if(end-p < size*n) {
        return R_NilValue;
    }
    SEXP res = PROTECT(Rf_allocVector(REALSXP, n));
    double *x = REAL(res);
    for(int i=0; i < n; ++i) {
        if(size == 4) {
            float f;
            memcpy(&f, p, size);
            x[i] = f;
        } else {
            memcpy(x+i, p, size);
        }
        p += size;
    }
    UNPROTECT(1);
    return res;
}

SEXP read_nbt_payload(const unsigned char* p, const unsigned char* end, nbt_type_t tag) {
    int array_len;
    SEXP r_ret;
    SEXP r_class;

    // load length for array values
    switch(tag) {
     case TAG_BYTE_ARRAY:
     case TAG_INT_ARRAY:
     case TAG_LONG_ARRAY:
        if(end-p < 4) {
            return R_NilValue;
        }
        memcpy(&array_len, p, 4);
        p += 4;
        break;
     default:
        array_len = 1;
        break;
    }

    // read payloads
    switch(tag) {
     case TAG_END:
        break;
     case TAG_BYTE:
     case TAG_BYTE_ARRAY:
        return read_nbt_payload_integer(p, end, 1, array_len);
     case TAG_SHORT:
        return read_nbt_payload_integer(p, end, 2, array_len);
     case TAG_INT:
     case TAG_INT_ARRAY:
        return read_nbt_payload_integer(p, end, 4, array_len);
     case TAG_FLOAT:
        return read_nbt_payload_real(p, end, 4, array_len);
     case TAG_DOUBLE:
        return read_nbt_payload_real(p, end, 8, array_len);
     case TAG_LONG:
     case TAG_LONG_ARRAY:
        PROTECT(r_ret = read_nbt_payload_real(p, end, 8, array_len));
        PROTECT(r_class = Rf_mkString("integer64"));
        Rf_setAttrib(r_ret, R_ClassSymbol, r_class);
        UNPROTECT(2);
        return r_ret;
     case TAG_STRING:
        return Rf_ScalarString(read_nbt_payload_character(p, end));
     case TAG_LIST:
        return read_nbt_list_payload(p, end);
     case TAG_COMPOUND:
        return read_nbt_compound_payload(p, end);
    }
    return R_NilValue;
}

SEXP read_nbt(SEXP r_value) {
    if(Rf_isNull(r_value)) {
        return R_NilValue;
    }
    if(TYPEOF(r_value) != RAWSXP) {
        error_return("Argument is not a raw type or NULL.");
    }

    size_t len = XLENGTH(r_value);
    unsigned char *buffer = RAW(r_value);




}