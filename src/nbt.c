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
#include <stdbool.h>

#include "nbt.h"
#include "support.h"

static SEXP g_tag_symbol = NULL;
static SEXP g_ptype_symbol = NULL;
static SEXP g_bytes_read_symbol = NULL;

void rbedrock_init_nbt(void) {
    g_tag_symbol = Rf_install("tag");
    g_ptype_symbol = Rf_install("ptype");
    g_bytes_read_symbol = Rf_install("bytes_read");
}

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
    // Check for embedded nulls
    bool has_null = false;
    for(int i=0; i < len; ++i) {
        if(p[i] == '\0') {
            has_null = true;
            break;
        }
    }
    // If a string has embedded nulls, we will a raw vector here.
    if(has_null) {
        SEXP r_ret = Rf_allocVector(RAWSXP, len);
        memcpy(RAW(r_ret), p, len);
        return r_ret;
    }
    return Rf_ScalarString(Rf_mkCharLenCE((const char*)p, len, CE_UTF8));
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
            signed char y;
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

static SEXP read_nbt_payload_integer64(const unsigned char** ptr, const unsigned char* end, int size, int n) {
    SEXP r_payload = PROTECT(read_nbt_payload_real(ptr, end, size, n));
    if(Rf_isNull(r_payload)) {
        UNPROTECT(1);
        return r_payload;
    }
    SEXP r_class = PROTECT(Rf_ScalarString(Rf_mkChar("integer64")));
    Rf_setAttrib(r_payload, R_ClassSymbol, r_class);
    UNPROTECT(2);
    return r_payload;
}

static SEXP read_nbt_list_payload(const unsigned char** ptr, const unsigned char* end) {
    if(end - *ptr < 5) {
        return R_NilValue;
    }
    nbt_type_t list_tag = **ptr;
    int list_len;
    memcpy(&list_len, *ptr+1,4);
    *ptr += 5;

    if(list_len == 0 && list_tag != TAG_END) {
        Rf_warning("Malformed NBT data. An empty LIST has list_tag of '%d' instead of 0.", list_tag);
    }
    SEXP r_ret = PROTECT(Rf_allocVector(VECSXP, list_len));
    SEXP r_val;
    SEXP r_payload;
    for(int i = 0; i < list_len; ++i) {
        r_payload = PROTECT(read_nbt_payload(ptr, end, list_tag));
        if(Rf_isNull(r_payload)) {
            return_nbt_error();
        }
        const char *names[] = {"tag", "payload", ""};
        r_val = PROTECT(Rf_mkNamed(VECSXP, names));
        SET_VECTOR_ELT(r_val, 0, Rf_ScalarInteger((int)list_tag));
        SET_VECTOR_ELT(r_val, 1, r_payload);

        SET_VECTOR_ELT(r_ret, i, r_val);
        UNPROTECT(2);
    }
    UNPROTECT(1);
    return r_ret;
}

static SEXP read_nbt_compound_payload(const unsigned char** ptr, const unsigned char* end) {
    SEXP r_ret = PROTECT(create_stretchy_list());
    SEXP r_val;
    for(;;) {
        if(*ptr >= end) {
            // We've run out of space.
            return_nbt_error_tag(10);
        }
        r_val = PROTECT(read_nbt_value(ptr, end));
        if(Rf_isNull(r_val)) {
            UNPROTECT(1);
            break; // NULL value signals end of compound
        }
        grow_stretchy_list(r_ret, r_val);
        UNPROTECT(1);
    }
    UNPROTECT(1);
    return Rf_PairToVectorList(CDR(r_ret));
}

static SEXP read_nbt_payload(const unsigned char** ptr, const unsigned char* end, nbt_type_t tag) {
    int array_len = 1;

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
        break;
    }

    // read payloads
    switch(tag) {
     case TAG_END:
        return Rf_allocVector(VECSXP, 0);
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
        return read_nbt_payload_real(ptr, end, 8, array_len);
     case TAG_LONG:
     case TAG_LONG_ARRAY:
        return read_nbt_payload_integer64(ptr, end, 8, array_len);
     case TAG_STRING:
        return read_nbt_payload_character(ptr, end);
     case TAG_LIST:
        return read_nbt_list_payload(ptr, end);
     case TAG_COMPOUND:
        return read_nbt_compound_payload(ptr, end);
     default:
        break;
    }
    return_nbt_error_tag(tag);
}

SEXP read_nbt_value(const unsigned char** ptr, const unsigned char* end) {
    SEXP r_name;
    SEXP r_payload;
    if(*ptr >= end) {
        return_nbt_error();
    }
    nbt_type_t tag = **ptr;
    *ptr += 1;
    if(tag == TAG_END) {
        return R_NilValue;
    }
    if( (unsigned int)tag >= TAG_CHECK ) {
        return_nbt_error_tag(tag);
    }
    r_name = PROTECT(read_nbt_payload_character(ptr, end));
    if(Rf_isNull(r_name)) {
        return_nbt_error();
    }
    r_payload = PROTECT(read_nbt_payload(ptr, end, tag));
    if(Rf_isNull(r_payload)) {
        return_nbt_error();
    }
    const char *names[] = {"name", "tag", "payload", ""};
    SEXP r_ret = PROTECT(Rf_mkNamed(VECSXP, names));
    SET_VECTOR_ELT(r_ret, 0, r_name);
    SET_VECTOR_ELT(r_ret, 1, Rf_ScalarInteger((int)tag));
    SET_VECTOR_ELT(r_ret, 2, r_payload);
    UNPROTECT(3);
    return r_ret;
}


SEXP read_nbt_values(const unsigned char** ptr, const unsigned char* end) {
    SEXP r_ret = PROTECT(create_stretchy_list());
    SEXP r_val;
    while(*ptr < end) {
        r_val = PROTECT(read_nbt_value(ptr, end));
        if(Rf_isNull(r_val)) {
            // We should not encounter a 0 tag in this context
            return_nbt_error_tag(0);
        }
        grow_stretchy_list(r_ret, r_val);
        UNPROTECT(1);
    }
    UNPROTECT(1);
    return Rf_PairToVectorList(CDR(r_ret));
}

SEXP read_nbt(SEXP r_value) {
    if(Rf_isNull(r_value)) {
        return R_NilValue;
    }
    if(TYPEOF(r_value) != RAWSXP) {
        error_return("Argument is not a raw type or NULL.");
    }

    size_t len = XLENGTH(r_value);
    const unsigned char *buffer = RAW(r_value);
    const unsigned char *p = buffer;
    return read_nbt_values(&p, buffer+len);
}

static R_xlen_t write_nbt_integer_payload(SEXP r_value, unsigned char** ptr,
    unsigned char* end, int size, bool is_array) {
    // validate data
    if(is_array) {
        if(!Rf_isInteger(r_value)) {
            return_nbt_error0();
        }
    } else {
        if(!IS_SCALAR(r_value, INTSXP)) {
            return_nbt_error0();
        }
    }
    unsigned char *p = *ptr;
    R_xlen_t len = XLENGTH(r_value);
    int *data = INTEGER(r_value);
    R_xlen_t retsz = len*size + is_array * 4;
    if(end-p < retsz) {
        // do nothing except return size if there is no buffer space
        return retsz;
    }
    if(is_array) {
        int ilen = (int)len;
        memcpy(p, &ilen, sizeof(ilen));
        p += sizeof(ilen);
    }
    switch(size) {
     case 1:
        for(R_xlen_t i=0; i < len; ++i) {
            signed char y = (signed char)data[i];
            memcpy(p, &y, sizeof(y));
            p += sizeof(y);
        }
        break;
     case 2:
        for(R_xlen_t i=0; i < len; ++i) {
            short y = (short)data[i];
            memcpy(p, &y, sizeof(y));
            p += sizeof(y);
        }
        break;
     case 4:
        memcpy(p, data, 4*len);
        p += 4*len;
        break;
     default:
        return_nbt_error0();
    }
    // update start ptr and return size
    *ptr = p;
    return retsz;
}

static R_xlen_t write_nbt_real_payload(SEXP r_value, unsigned char** ptr,
    unsigned char* end, int size, bool is_array) {
    // validate data
    if(is_array) {
        if(!Rf_isReal(r_value)) {
            return_nbt_error0();
        }
    } else {
        if(!IS_SCALAR(r_value, REALSXP)) {
            return_nbt_error0();
        }
    }
    unsigned char *p = *ptr;
    R_xlen_t len = XLENGTH(r_value);
    double *data = REAL(r_value);
    R_xlen_t retsz = len*size + is_array * 4;
    if(end-p < retsz) {
        // do nothing except return size if there is no buffer space
        return retsz;
    }
    if(is_array) {
        int ilen = (int)len;
        memcpy(p, &ilen, sizeof(ilen));
        p += sizeof(ilen);
    }
    switch(size) {
     case 4:
        for(R_xlen_t i=0; i < len; ++i) {
            float y = (float)data[i];
            memcpy(p, &y, sizeof(y));
            p += sizeof(y);
        }
        break;
     case 8:
        memcpy(p, data, 8*len);
        p += 8*len;
        break;
     default:
        return_nbt_error0();
    }
    // update start ptr and return size
    *ptr = p;
    return retsz;
}

static R_xlen_t write_nbt_character_payload(SEXP r_value, unsigned char** ptr, unsigned char* end) {
    // validate data
    const char *str = NULL;
    unsigned short len = 0;
    if(TYPEOF(r_value) == CHARSXP) {
        str = Rf_translateCharUTF8(r_value);
        len = (unsigned short)strlen(str);
    } else if(IS_SCALAR(r_value, STRSXP)) {
        str = Rf_translateCharUTF8(STRING_ELT(r_value, 0));
        len = (unsigned short)strlen(str);
    } else if(TYPEOF(r_value) == RAWSXP) {
        str = (const char*)RAW(r_value);
        len = (unsigned short)XLENGTH(r_value);
    } else if(!Rf_isNull(r_value)) {
        return_nbt_error0();
    }
    unsigned char *p = *ptr;
    R_xlen_t retsz = len + sizeof(len);
    if(end-p < retsz) {
        // do nothing except return size if there is no buffer space
        return retsz;
    }
    // write data
    memcpy(p, &len, sizeof(len));
    p += sizeof(len);
    if(len > 0) {
        memcpy(p, str, len);
        p += len;
    }
    // update start ptr and return size
    *ptr = p;
    return retsz;
}

static R_xlen_t write_nbt_tag(int tag, unsigned char** ptr, unsigned char* end) {
    if(end-*ptr >= 1) {
        **ptr = (unsigned char)tag;
        *ptr += 1;
    }
    return 1;
}

static R_xlen_t write_nbt_payload(SEXP r_value, unsigned char** ptr, unsigned char* end, int tag);

static R_xlen_t write_nbt_list_payload(SEXP r_value, unsigned char** ptr, unsigned char* end) {
    // validate data
    if(TYPEOF(r_value) != VECSXP) {
        return_nbt_error0();
    }
    // Identify tag. Tag is 0 for empty lists, and the tag of the first element otherwise
    int tag = 0;
    if(XLENGTH(r_value) > 0) {
        SEXP r_obj = VECTOR_ELT(r_value, 0);
        tag = Rf_asInteger(get_list_element(r_obj, "tag"));
    }

    R_xlen_t len = 0;

    len += write_nbt_tag(tag, ptr, end);
    int sz = (int)XLENGTH(r_value);
    if(end-*ptr >= sizeof(sz)) {
        memcpy(*ptr, &sz, sizeof(sz));
        *ptr += sizeof(sz);
    }
    len += sizeof(sz);
    for(int i=0; i < sz; ++i) {
        SEXP r_obj = VECTOR_ELT(r_value, i);
        int tag2 = Rf_asInteger(get_list_element(r_obj, "tag"));
        if(tag2 != tag) {
            return_nbt_error0();
        }
        SEXP r_payload = get_list_element(r_obj, "payload");
        len += write_nbt_payload(r_payload, ptr, end, tag);
    }
    return len;
}

static R_xlen_t write_nbt_compound_payload(SEXP r_value, unsigned char** ptr, unsigned char* end) {
    R_xlen_t len = write_nbt_values(r_value, ptr, end);
    len += write_nbt_tag(0, ptr, end);
    return len;
}

static R_xlen_t write_nbt_payload(SEXP r_value, unsigned char** ptr, unsigned char* end, int tag) {
    switch(tag) {
     case TAG_END:
        return 0;
     case TAG_BYTE:
        return write_nbt_integer_payload(r_value, ptr, end, 1, false);
     case TAG_SHORT:
        return write_nbt_integer_payload(r_value, ptr, end, 2, false);
     case TAG_INT:
        return write_nbt_integer_payload(r_value, ptr, end, 4, false);
     case TAG_FLOAT:
        return write_nbt_real_payload(r_value, ptr, end, 4, false);
     case TAG_LONG:
     case TAG_DOUBLE:
        return write_nbt_real_payload(r_value, ptr, end, 8, false);
     case TAG_BYTE_ARRAY:
        return write_nbt_integer_payload(r_value, ptr, end, 1, true);
     case TAG_STRING:
        return write_nbt_character_payload(r_value, ptr, end);
     case TAG_LIST:
        return write_nbt_list_payload(r_value, ptr, end);
     case TAG_COMPOUND:
        return write_nbt_compound_payload(r_value, ptr, end);
     case TAG_INT_ARRAY:
        return write_nbt_integer_payload(r_value, ptr, end, 4, true);
     case TAG_LONG_ARRAY:
        return write_nbt_real_payload(r_value, ptr, end, 8, true);
     default:
        break;
    }
    return_nbt_error0();
}

R_xlen_t write_nbt_value(SEXP r_value, unsigned char** ptr, unsigned char* end) {
    PROTECT(r_value);
    int tag = Rf_asInteger(get_list_element(r_value, "tag"));
    SEXP r_name = get_list_element(r_value, "name");
    SEXP r_payload = get_list_element(r_value, "payload");

    size_t len = 0;
    len += write_nbt_tag(tag, ptr, end);
    len += write_nbt_character_payload(r_name, ptr, end);
    len += write_nbt_payload(r_payload, ptr, end, tag);
    UNPROTECT(1);
    return len;
}

R_xlen_t write_nbt_values(SEXP r_value, unsigned char** ptr, unsigned char* end) {
    // validate data
    if(TYPEOF(r_value) != VECSXP) {
        return_nbt_error0();
    }
    PROTECT(r_value);
    R_xlen_t len = 0;

    for(R_xlen_t i = 0; i < XLENGTH(r_value); ++i) {
        len += write_nbt_value(VECTOR_ELT(r_value, i), ptr, end);
    }
    UNPROTECT(1);
    return len;
}

SEXP write_nbt(SEXP r_value) {
    // stack-based buffer
    unsigned char buffer[8192];

    if(Rf_isNull(r_value)) {
        return R_NilValue;
    }

    unsigned char *p = buffer;
    
    // try to write the nbt data with the stack
    // fall back to a heap allocation if needed
    R_xlen_t len = write_nbt_values(r_value, &p, p+8192);
    SEXP ret = PROTECT(Rf_allocVector(RAWSXP, len));
    if(len <= 8192 && p-buffer == len) {
        memcpy(RAW(ret), buffer, len);
    } else {
        p = RAW(ret);
        R_xlen_t len2 = write_nbt_values(r_value, &p, RAW(ret)+len);
        if(len2 != len || p-RAW(ret) != len2) {
            return_nbt_error();
        }
    }
    UNPROTECT(1);
    return ret;
}
