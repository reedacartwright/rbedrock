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

#include <limits.h>

#include "nbt.h"

#define return_nbt_error() error_return("Malformed NBT data.")

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
    TAG_LONG_ARRAY,
    TAG_CHECK
};
typedef enum NBT_TYPE nbt_type_t;

static SEXP read_nbt_payload(const unsigned char** p, const unsigned char* end, nbt_type_t tag);
static SEXP make_nbtnode(SEXP r_payload, SEXP r_name, nbt_type_t tag);
SEXP read_nbt_node(const unsigned char** ptr, const unsigned char* end);

static SEXP read_nbt_payload_character(const unsigned char** ptr, const unsigned char* end) {
    const unsigned char *p = *ptr;
    unsigned short len;
    if(end-p < sizeof(short)) {
        return R_NilValue;
    }
    memcpy(&len, p, sizeof(len));
    p += sizeof(len);
    if(end-p < len) {
        return R_NilValue;        
    }
    *ptr = p+len;
    return Rf_mkCharLenCE((const char*)p, len, CE_UTF8);
}

static SEXP read_nbt_payload_integer(const unsigned char** ptr, const unsigned char* end, int size, int n) {
    const unsigned char *p = *ptr;
    if(end-p < size*n) {
        return R_NilValue;
    }
    SEXP res = PROTECT(Rf_allocVector(INTSXP, n));
    int *x = INTEGER(res);
    for(int i=0; i < n; ++i) {
        if(size == 1) {
            char y;
            memcpy(&y, p, 1);
            x[i] = y;
        } else if(size == 2) {
            short y;
            memcpy(&y, p, 2);
            x[i] = y;            
        } else {
            memcpy(x+i, p, 4);
        }
        p += size;
    }
    *ptr = p;
    UNPROTECT(1);
    return res;
}

static SEXP read_nbt_payload_real(const unsigned char** ptr, const unsigned char* end, int size, int n) {
    const unsigned char *p = *ptr;
    if(end-p < size*n) {
        return R_NilValue;
    }
    SEXP res = PROTECT(Rf_allocVector(REALSXP, n));
    double *x = REAL(res);
    for(int i=0; i < n; ++i) {
        if(size == 4) {
            float f;
            memcpy(&f, p, 4);
            x[i] = f;
        } else {
            memcpy(x+i, p, 8);
        }
        p += size;
    }
    *ptr = p;
    UNPROTECT(1);
    return res;
}

static SEXP read_nbt_list_payload(const unsigned char** ptr, const unsigned char* end) {
    const unsigned char *p = *ptr;
    if(end - p < 5) {
        return R_NilValue;
    }
    nbt_type_t list_tag = *p;
    int list_len;
    memcpy(&list_len, p+1,4);
    p += 5;

    SEXP r_ret = PROTECT(Rf_allocVector(VECSXP, list_len));
    for(int i = 0; i < list_len; ++i) {
        SET_VECTOR_ELT(r_ret, i, read_nbt_payload(&p, end, list_tag));
    }
    *ptr = p;
    UNPROTECT(1);
    switch(list_tag) {
        case TAG_BYTE:
        case TAG_SHORT:
        case TAG_INT:
            r_ret = Rf_coerceVector(r_ret, INTSXP);
            break;
        case TAG_LONG:
        case TAG_FLOAT:
        case TAG_DOUBLE:
            r_ret = Rf_coerceVector(r_ret, REALSXP);
            break;
        case TAG_STRING:
            r_ret = Rf_coerceVector(r_ret, STRSXP);
            break;
        default:
            break;
    }
    PROTECT(r_ret);
    Rf_setAttrib(r_ret, Rf_install("list_tag"), Rf_ScalarInteger(list_tag));
    UNPROTECT(1);    
    return r_ret;
}

static SEXP make_nbtnode(SEXP r_payload, SEXP r_name, nbt_type_t tag) {
    SEXP r_ret;
    PROTECT(r_payload);
    PROTECT(r_name);
    if(tag == TAG_LONG || tag == TAG_LONG_ARRAY) {
        SEXP r_class;
        PROTECT(r_class = Rf_allocVector(STRSXP, 2));
        SET_STRING_ELT(r_class, 0, Rf_mkChar("nbtnode"));
        SET_STRING_ELT(r_class, 1, Rf_mkChar("integer64"));
        UNPROTECT(1);
        Rf_setAttrib(r_payload, R_ClassSymbol, r_class);
    } else {
        Rf_setAttrib(r_payload, R_ClassSymbol, Rf_mkString("nbtnode"));
    }
    Rf_setAttrib(r_payload, Rf_install("tag"), Rf_ScalarInteger(tag));
    PROTECT(r_ret = Rf_list1(r_payload));
    if(XLENGTH(r_name) > 0) {
        SET_TAG(r_ret, Rf_installTrChar(r_name));
    }
    UNPROTECT(3);
    return r_ret;
}

SEXP read_nbt_compound_payload(const unsigned char** ptr, const unsigned char* end,
        int max_elements) {
    SEXP r_ret = R_NilValue;
    SEXP last = R_NilValue;
    for(int i = 0; *ptr < end && i < max_elements; ++i) {
        SEXP r_node = read_nbt_node(ptr, end);
        if(Rf_isNull(r_node)) {
            // we have encountered an END node
            break;
        }
        if(Rf_isNull(r_ret)) {
            PROTECT(r_ret = r_node);
            last = r_ret;
        } else {
            last = SETCDR(last, r_node);
        }
    }
    if(!Rf_isNull(r_ret)) {
        UNPROTECT(1);
    }
    return Rf_PairToVectorList(r_ret);
}

static SEXP read_nbt_payload(const unsigned char** ptr, const unsigned char* end, nbt_type_t tag) {
    int array_len;
    SEXP r_ret;
    SEXP r_class;

    // load length for array values
    switch(tag) {
     case TAG_BYTE_ARRAY:
     case TAG_INT_ARRAY:
     case TAG_LONG_ARRAY:
        {
            if(end-*ptr < 4) {
                return R_NilValue;
            }
            memcpy(&array_len, *ptr, 4);
            *ptr += 4;
            break;
        }
     default:
        array_len = 1;
        break;
    }

    // read payloads
    switch(tag) {
     case TAG_END:
        return R_NilValue;
     case TAG_BYTE:
     case TAG_BYTE_ARRAY:
        return read_nbt_payload_integer(ptr, end, 1, array_len);
     case TAG_SHORT:
        return read_nbt_payload_integer(ptr, end, 2, array_len);
     case TAG_INT:
     case TAG_INT_ARRAY:
        return read_nbt_payload_integer(ptr, end, 4, array_len);
     case TAG_FLOAT:
        return read_nbt_payload_real(ptr, end, 4, array_len);
     case TAG_DOUBLE:
     case TAG_LONG:
     case TAG_LONG_ARRAY:
        return read_nbt_payload_real(ptr, end, 8, array_len);
     case TAG_STRING:
        return Rf_ScalarString(read_nbt_payload_character(ptr, end));
     case TAG_LIST:
        return read_nbt_list_payload(ptr, end);
     case TAG_COMPOUND:
        return read_nbt_compound_payload(ptr, end, INT_MAX);
     default:
        break;
    }
    Rf_error("Malformed NBT tag: '%d'", tag);
    return R_NilValue;
}

SEXP read_nbt_node(const unsigned char** ptr, const unsigned char* end) {
    if(end-*ptr < 1) {
        return_nbt_error();
    }
    nbt_type_t tag = **ptr;
    *ptr += 1;
    if(tag == TAG_END) {
        return R_NilValue;
    } else if( (unsigned int)tag >= TAG_CHECK) {
        Rf_error("Malformed NBT tag: '%d'", tag);
        return R_NilValue;
    }
    SEXP name = PROTECT(read_nbt_payload_character(ptr, end));
    if(Rf_isNull(name)) {
        return_nbt_error();
    }
    SEXP payload = PROTECT(read_nbt_payload(ptr, end, tag));
    if(Rf_isNull(payload)) {
        return_nbt_error();
    }
    UNPROTECT(2);
    return make_nbtnode(payload,name,tag);
}

SEXP read_nbt(SEXP r_value, SEXP r_max_elements) {
    if(Rf_isNull(r_value)) {
        return R_NilValue;
    }
    int max_elements = INT_MAX;
    if(!Rf_isNull(r_max_elements)) {
        max_elements = Rf_asInteger(r_max_elements);
    }
    if(TYPEOF(r_value) != RAWSXP) {
        error_return("Argument is not a raw type or NULL.");
    }

    size_t len = XLENGTH(r_value);
    const unsigned char *buffer = RAW(r_value);
    const unsigned char *p = buffer;
    SEXP r_ret = read_nbt_compound_payload(&p, buffer+len, max_elements);
    R_xlen_t bytes_read = p-buffer;
    if(!Rf_isNull(r_max_elements) && bytes_read <= len) {
        Rf_setAttrib(r_ret, Rf_install("bytes_read"), Rf_ScalarInteger(bytes_read));
    } else if(bytes_read != len) {
        Rf_error("Malformed NBT data: %d bytes were read out of %d bytes total", (int)(bytes_read), (int)len);
        return R_NilValue;
    }
    return r_ret;
}
