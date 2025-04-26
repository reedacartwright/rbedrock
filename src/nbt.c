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

static ptrdiff_t payload_size(int tag, int n) {
    int width = 0;
    switch(tag) {
     case TAG_BYTE:
     case TAG_BYTE_ARRAY:
     case TAG_STRING:
        width = 1;
        break;
     case TAG_SHORT:
        width = 2;
        break;
     case TAG_INT:
     case TAG_INT_ARRAY:
     case TAG_FLOAT:
        width = 4;
        break;
     case TAG_LONG:
     case TAG_LONG_ARRAY:
     case TAG_DOUBLE:
        width = 8;
        break;
     default:
        break;
    };
    return n*width;
}

static SEXP read_nbt_payload_numeric(const unsigned char** ptr,
                                     const unsigned char* end,
                                     int tag, int n) {
    const unsigned char *p = *ptr;
    if(end-p < payload_size(tag, n)) {
        return_nbt_error();
    }
    // Store numeric values as doubles and avoid loss of information from NAs
    SEXP res = PROTECT(Rf_allocVector(REALSXP, n));
    double *x = REAL(res);
    signed char ybyte;
    signed short yshort;
    signed int yint;
    float yfloat;
    double ydouble;
    for(int i=0; i < n; ++i) {
        switch(tag) {
         case TAG_BYTE:
         case TAG_BYTE_ARRAY:
            memcpy(&ybyte, p, 1);
            x[i] = (double)ybyte;
            p += 1;
            break;
         case TAG_SHORT:
            memcpy(&yshort, p, 2);
            x[i] = (double)yshort;
            p += 2;
            break;
         case TAG_INT:
         case TAG_INT_ARRAY:
            memcpy(&yint, p, 4);
            x[i] = (double)yint;
            p += 4;
            break;
         case TAG_FLOAT:
            memcpy(&yfloat, p, 4);
            x[i] = (float)yfloat;
            p += 4;
            break;
         case TAG_LONG:
         case TAG_LONG_ARRAY:
            /* While longs are now recorded as strings, keep this pass through.*/;
         case TAG_DOUBLE:
            memcpy(&ydouble, p, 8);
            x[i] = ydouble;
            p += 8;
            break;
         default:
            return_nbt_error_tag(tag);
            break;
        };
    }
    *ptr = p;
    UNPROTECT(1);
    return res;
}

static SEXP read_nbt_payload_integer64(const unsigned char** ptr, const unsigned char* end, int tag, int n) {
    const unsigned char *p = *ptr;
    if(end-p < payload_size(tag, n)) {
        return_nbt_error();
    }
    SEXP res = PROTECT(Rf_allocVector(STRSXP, n));
    signed long long ylong;
    char buffer[22];
    for(int i=0; i < n; ++i) {
        memcpy(&ylong, p, 8);
        p += 8;
        snprintf(buffer, sizeof(buffer), "%lli", ylong);
        SET_STRING_ELT(res, i, Rf_mkCharCE(buffer, CE_UTF8));
    }
    *ptr = p;
    UNPROTECT(1);
    return res;
}

static SEXP create_nbt_list_payload(SEXP r_value, nbt_type_t list_type) {
    PROTECT(r_value);
    const char *names[] = {"value", "type", ""};
    SEXP r_ret = PROTECT(Rf_mkNamed(VECSXP, names));
    SET_VECTOR_ELT(r_ret, 0, r_value);
    SET_VECTOR_ELT(r_ret, 1, Rf_ScalarInteger((int)list_type));
    UNPROTECT(2);
    return r_ret;
}

static SEXP read_nbt_list_payload_numeric(const unsigned char** ptr,
                                          const unsigned char* end,
                                          nbt_type_t type, int n) {
    SEXP r_value = read_nbt_payload_numeric(ptr, end, type, n);
    return create_nbt_list_payload(r_value, type);
}

static SEXP read_nbt_list_payload_integer64(const unsigned char** ptr,
                                          const unsigned char* end,
                                          nbt_type_t type, int n) {
    SEXP r_value = read_nbt_payload_integer64(ptr, end, type, n);
    return create_nbt_list_payload(r_value, type);
}

static SEXP read_nbt_list_payload(const unsigned char** ptr,
                                  const unsigned char* end) {
    nbt_type_t list_type;
    int list_len;
    if(end - *ptr < 5) {
        return R_NilValue;
    }
    list_type = **ptr;
    memcpy(&list_len, *ptr + 1, 4);
    *ptr += 5;
    if(list_len == 0 && list_type != TAG_END) {
        Rf_warning("Malformed NBT data. An empty LIST has list_tag of '%d' instead of 0.", list_type);
    }
    switch(list_type) {
     case TAG_BYTE:
     case TAG_SHORT:
     case TAG_INT:
     case TAG_FLOAT:
     case TAG_DOUBLE:
        return read_nbt_list_payload_numeric(ptr, end, list_type, list_len);
     case TAG_LONG:
        return read_nbt_list_payload_integer64(ptr, end, list_type, list_len);
     default:
        break;
    };

    SEXP r_payload = PROTECT(Rf_allocVector(VECSXP, list_len));
    for(int i = 0; i < list_len; ++i) {
        SEXP r_val = PROTECT(read_nbt_payload(ptr, end, list_type));
        if(Rf_isNull(r_payload)) {
            return_nbt_error();
        }
        SET_VECTOR_ELT(r_payload, i, r_val);
        UNPROTECT(1);
    }
    if(list_type != TAG_STRING) {
        UNPROTECT(1);
        return create_nbt_list_payload(r_payload, list_type);
    }
    // Try to optimize lists of strings
    for(int i = 0; i < list_len; ++i) {
        if(!IS_SCALAR(VECTOR_ELT(r_payload, i), STRSXP)) {
            UNPROTECT(1);
            return create_nbt_list_payload(r_payload, list_type);
        }
    }
    SEXP r_string = PROTECT(Rf_allocVector(STRSXP, list_len));
    for(int i = 0; i < list_len; ++i) {
        SEXP v = STRING_ELT(VECTOR_ELT(r_payload, i), 0);
        SET_STRING_ELT(r_string, i, v);
    }
    UNPROTECT(2);
    return create_nbt_list_payload(r_string, list_type);
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
    // load length for array
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
     case TAG_SHORT:
     case TAG_INT:
     case TAG_INT_ARRAY:
     case TAG_FLOAT:
     case TAG_DOUBLE:
        return read_nbt_payload_numeric(ptr, end, tag, array_len);
     case TAG_LONG:
     case TAG_LONG_ARRAY:
        return read_nbt_payload_integer64(ptr, end, tag, array_len);
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
    SEXP r_name, r_payload;
    nbt_type_t tag;
    if(*ptr >= end) {
        return_nbt_error();
    }
    tag = **ptr;
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
    SEXP r_ret;
    if(tag == TAG_LIST) {
        const char *names[] = {"name", "tag", "value", "type", ""};
        r_ret = PROTECT(Rf_mkNamed(VECSXP, names));
        SET_VECTOR_ELT(r_ret, 2, VECTOR_ELT(r_payload, 0));
        SET_VECTOR_ELT(r_ret, 3, VECTOR_ELT(r_payload, 1));
    } else {
        const char *names[] = {"name", "tag", "value", ""};
        r_ret = PROTECT(Rf_mkNamed(VECSXP, names));        
        SET_VECTOR_ELT(r_ret, 2, r_payload);
    }
    SET_VECTOR_ELT(r_ret, 0, r_name);
    SET_VECTOR_ELT(r_ret, 1, Rf_ScalarInteger((int)tag));
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

static R_xlen_t write_nbt_numeric_payload(SEXP r_value, unsigned char** ptr,
    const unsigned char* end, int tag, bool is_array) {
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
    R_xlen_t retsz = payload_size(tag, len) + is_array * 4;

    if(end-p < retsz) {
        // do nothing except return size if there is no buffer space
        return retsz;
    }
    if(is_array) {
        int ilen = (int)len;
        memcpy(p, &ilen, sizeof(ilen));
        p += sizeof(ilen);
    }
    signed char ybyte;
    signed short yshort;
    signed int yint;
    float yfloat;
    double ydouble;
    for(R_xlen_t i = 0; i < len; ++i) {
        switch(tag) {
         case TAG_BYTE:
         case TAG_BYTE_ARRAY:
            ybyte = (signed char)data[i];
            memcpy(p, &ybyte, 1);
            p += 1;
            break;
         case TAG_SHORT:
            yshort = (signed short)data[i];
            memcpy(p, &yshort, 2);
            p += 2;
            break;
         case TAG_INT:
         case TAG_INT_ARRAY:
            yint = (signed int)data[i];
            memcpy(p, &yint, 4);
            p += 4;
            break;
         case TAG_FLOAT:
            yfloat = (float)data[i];
            memcpy(p, &yfloat, 4);
            p += 4;
            break;
         case TAG_LONG:
         case TAG_LONG_ARRAY:
         case TAG_DOUBLE:
            ydouble = (double)data[i];
            memcpy(p, &ydouble, 8);
            p += 8;
            break;
         default:
            return_nbt_error0();
            break;
        };
    }
    // update start ptr and return size
    *ptr = p;
    return retsz;
}

static R_xlen_t write_nbt_integer64_payload(SEXP r_value, unsigned char** ptr,
    const unsigned char* end, int tag, bool is_array) {
    // validate data
    if(is_array) {
        if(!Rf_isString(r_value)) {
            return_nbt_error0();
        }
    } else {
        if(!IS_SCALAR(r_value, STRSXP)) {
            return_nbt_error0();
        }
    }
    unsigned char *p = *ptr;
    R_xlen_t len = XLENGTH(r_value);
    R_xlen_t retsz = payload_size(tag, len) + is_array * 4;

    if(end-p < retsz) {
        // do nothing except return size if there is no buffer space
        return retsz;
    }
    if(is_array) {
        int ilen = (int)len;
        memcpy(p, &ilen, sizeof(ilen));
        p += sizeof(ilen);
    }
    signed long long ylong;
    const char *str = NULL;
    char * strend = NULL;
    for(R_xlen_t i = 0; i < len; ++i) {
        str = Rf_translateCharUTF8(STRING_ELT(r_value, i));
        ylong = strtoll(str, &strend, 10);
        if(*strend != '\0') {
            return_nbt_error0();
        }
        memcpy(p, &ylong, 8);
        p += 8;
    }
    // update start ptr and return size
    *ptr = p;
    return retsz;
}

static R_xlen_t write_nbt_character_payload(SEXP r_value, unsigned char** ptr,
    const unsigned char* end, int tag, bool is_array) {
    // Validate storage
    if(is_array) {
        if(!(Rf_isString(r_value) || TYPEOF(r_value) == VECSXP)) {
            return_nbt_error0();
        }
    } else {
        if(!(IS_SCALAR(r_value, STRSXP) ||
             TYPEOF(r_value) == RAWSXP ||
             TYPEOF(r_value) == CHARSXP)) {
            return_nbt_error0();
        }
    }
    R_xlen_t retsz = 0;
    if(is_array) {
        R_xlen_t len = XLENGTH(r_value);
        int ilen = (int)len;
        if(end - *ptr >= 4) {
            memcpy(*ptr, &ilen, 4);
            *ptr += 4;
        }
        retsz += 4;
        for(R_xlen_t i = 0; i < len; ++i) {
            SEXP r;
            if(Rf_isString(r_value)) {
                r = STRING_ELT(r_value, i);
            } else {
                r = VECTOR_ELT(r_value, i);
            }
            retsz += write_nbt_character_payload(r, ptr, end, tag, false);
        }
        return retsz;
    }
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
    retsz = len + sizeof(len);
    if(end - p < retsz) {
        // do nothing except return size if there is no buffer space
        return retsz;
    }
    // write data
    memcpy(p, &len, 2);
    p += 2;
    if(len > 0) {
        memcpy(p, str, len);
        p += len;
    }
    // update start ptr and return size
    *ptr = p;
    return retsz;
}

static R_xlen_t write_nbt_tag(int tag, unsigned char** ptr,
    const unsigned char* end) {
    if(end-*ptr >= 1) {
        **ptr = (unsigned char)tag;
        *ptr += 1;
    }
    return 1;
}

static R_xlen_t write_nbt_payload(SEXP r_value, unsigned char** ptr,
    const unsigned char* end,
    const int tag);


static SEXPTYPE get_nbt_list_storage(nbt_type_t type) {
    switch(type) {
     case TAG_BYTE:
     case TAG_SHORT:
     case TAG_INT:
     case TAG_FLOAT:
     case TAG_DOUBLE:
        return REALSXP;
    case TAG_STRING:
    case TAG_LONG:
        return STRSXP;
    default:
        break;
    };
    return VECSXP;
}

static R_xlen_t write_nbt_list_payload(SEXP r_value, unsigned char** ptr,
                                       const unsigned char* end) {
    
    SEXP r_payload = get_list_element(r_value, "value");
    nbt_type_t type = Rf_asInteger(get_list_element(r_value, "type"));

    R_xlen_t len = 0;
    len += write_nbt_tag(type, ptr, end);
    SEXPTYPE mode = get_nbt_list_storage(type);
    if(mode == REALSXP) {
        len += write_nbt_numeric_payload(r_payload, ptr, end, type, true);
        return len;
    }
    if(type == TAG_LONG) {
        len += write_nbt_integer64_payload(r_payload, ptr, end, type, true);
        return len;
    }
    if(type == TAG_STRING) {
        len += write_nbt_character_payload(r_payload, ptr, end, type, true);
        return len;
    }
    if(TYPEOF(r_payload) != VECSXP) {
        return_nbt_error0();
    }

    int sz = (int)XLENGTH(r_payload);
    if(end - *ptr >= sizeof(sz)) {
        memcpy(*ptr, &sz, sizeof(sz));
        *ptr += sizeof(sz);
    }
    len += sizeof(sz);

    for(int i=0; i < sz; ++i) {
        SEXP r_obj = VECTOR_ELT(r_payload, i);
        len += write_nbt_payload(r_obj, ptr, end, type);
    }
    return len;
}

static R_xlen_t write_nbt_compound_payload(SEXP r_value, unsigned char** ptr,
    const unsigned char* end) {
    R_xlen_t len = write_nbt_values(r_value, ptr, end);
    len += write_nbt_tag(0, ptr, end);
    return len;
}

static R_xlen_t write_nbt_payload(SEXP r_value, unsigned char** ptr,
    const unsigned char* end, int tag) {
    switch(tag) {
     case TAG_END:
        return 0;
     case TAG_BYTE:
     case TAG_SHORT:
     case TAG_INT:
     case TAG_FLOAT:
     case TAG_DOUBLE:
        return write_nbt_numeric_payload(r_value, ptr, end, tag, false);
     case TAG_LONG:
        return write_nbt_integer64_payload(r_value, ptr, end, tag, false);
     case TAG_INT_ARRAY:
     case TAG_BYTE_ARRAY:
        return write_nbt_numeric_payload(r_value, ptr, end, tag, true);
     case TAG_LONG_ARRAY:
        return write_nbt_integer64_payload(r_value, ptr, end, tag, true);
     case TAG_STRING:
        return write_nbt_character_payload(r_value, ptr, end, tag, false);
     case TAG_LIST:
        return write_nbt_list_payload(r_value, ptr, end);
     case TAG_COMPOUND:
        return write_nbt_compound_payload(r_value, ptr, end);
     default:
        break;
    }
    return_nbt_error0();
}

R_xlen_t write_nbt_value(SEXP r_value, unsigned char** ptr, const unsigned char* end) {
    PROTECT(r_value);
    int tag = Rf_asInteger(get_list_element(r_value, "tag"));
    SEXP r_name = get_list_element(r_value, "name");
    SEXP r_payload;

    size_t len = 0;
    len += write_nbt_tag(tag, ptr, end);
    len += write_nbt_character_payload(r_name, ptr, end, 1, false);
    if(tag == TAG_LIST) {
        r_payload = r_value;
    } else {
        r_payload = get_list_element(r_value, "value");
    }
    len += write_nbt_payload(r_payload, ptr, end, tag);

    UNPROTECT(1);
    return len;
}

R_xlen_t write_nbt_values(SEXP r_value, unsigned char** ptr, const unsigned char* end) {
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
