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

#include <R_ext/Visibility.h>

#include <limits.h>
#include <stdbool.h>
#include <assert.h>

#include "nbt.h"
#include "support.h"
#include "binary.h"

static SEXP g_tag_symbol = NULL;
static SEXP g_ptype_symbol = NULL;
static SEXP g_bytes_read_symbol = NULL;

void rbedrock_init_nbt(void) {
    g_tag_symbol = Rf_install("tag");
    g_ptype_symbol = Rf_install("ptype");
    g_bytes_read_symbol = Rf_install("bytes_read");
}

enum NBT_TAG {
    TAG_END = 0,
    TAG_BYTE = 1,
    TAG_SHORT = 2,
    TAG_INT = 3,
    TAG_LONG = 4,
    TAG_FLOAT = 5,
    TAG_DOUBLE = 6,
    TAG_BYTE_ARRAY = 7,
    TAG_STRING = 8,
    TAG_LIST = 9,
    TAG_COMPOUND = 10,
    TAG_INT_ARRAY = 11,
    TAG_LONG_ARRAY = 12,
    TAG_CHECK
};
typedef enum NBT_TAG nbt_tag_t;

enum NBT_TYPE {
    TYPE_END = 0,
    TYPE_BYTE = 1,
    TYPE_SHORT = 2,
    TYPE_INT = 3,
    TYPE_LONG = 4,
    TYPE_FLOAT = 5,
    TYPE_DOUBLE = 6,
    TYPE_BYTE_ARRAY = 7,
    TYPE_STRING = 8,
    TYPE_LIST = 9,
    TYPE_COMPOUND = 10,
    TYPE_INT_ARRAY = 11,
    TYPE_LONG_ARRAY = 12,

    TYPE_RAW_STRING = 58,

    TYPE_LIST_OF_END = 100,
    TYPE_LIST_OF_BYTE = 101,
    TYPE_LIST_OF_SHORT = 102,
    TYPE_LIST_OF_INT = 103,
    TYPE_LIST_OF_LONG = 104,
    TYPE_LIST_OF_FLOAT = 105,
    TYPE_LIST_OF_DOUBLE = 106,
    TYPE_LIST_OF_BYTE_ARRAY = 107,
    TYPE_LIST_OF_STRING = 108,
    TYPE_LIST_OF_LIST = 109,
    TYPE_LIST_OF_COMPOUND = 110,
    TYPE_LIST_OF_INT_ARRAY = 111,
    TYPE_LIST_OF_LONG_ARRAY = 112,

    TYPE_LIST_OF_RAW_STRING = 158
};
typedef enum NBT_TYPE nbt_type_t;

static char get_binary_format(nbt_type_t type, nbt_format_t fmt) {
    // FMT_LE = 0, FMT_BE = 1, FMT_LV = 2, FMT_BV = 3
    const char integer_format[] = "lbvv";
    const char float_format[] = "lblb";

    switch(type) {
     case TYPE_BYTE:
     case TYPE_BYTE_ARRAY:
     case TYPE_LIST_OF_BYTE:
     case TYPE_STRING:
     case TYPE_RAW_STRING:
     case TYPE_SHORT:
     case TYPE_LIST_OF_SHORT:
     case TYPE_INT:
     case TYPE_INT_ARRAY:
     case TYPE_LIST_OF_INT:
     case TYPE_LONG:
     case TYPE_LONG_ARRAY:
     case TYPE_LIST_OF_LONG:
        return integer_format[fmt];
     case TYPE_FLOAT:
     case TYPE_LIST_OF_FLOAT:
     case TYPE_DOUBLE:
     case TYPE_LIST_OF_DOUBLE:
        return float_format[fmt];
     default:
        break;
    };
    return '0';
}

static SEXP read_nbt_payload(const unsigned char** p, const unsigned char* end,
                             nbt_type_t type, nbt_format_t fmt);

static ptrdiff_t payload_size(nbt_type_t type, int n) {
    int width = 0;
    switch(type) {
     case TYPE_BYTE:
     case TYPE_BYTE_ARRAY:
     case TYPE_LIST_OF_BYTE:
     case TYPE_STRING:
     case TYPE_RAW_STRING:
        width = 1;
        break;
     case TYPE_SHORT:
     case TYPE_LIST_OF_SHORT:
        width = 2;
        break;
     case TYPE_INT:
     case TYPE_INT_ARRAY:
     case TYPE_LIST_OF_INT:
     case TYPE_FLOAT:
     case TYPE_LIST_OF_FLOAT:
        width = 4;
        break;
     case TYPE_LONG:
     case TYPE_LONG_ARRAY:
     case TYPE_LIST_OF_LONG:
     case TYPE_DOUBLE:
     case TYPE_LIST_OF_DOUBLE:
        width = 8;
        break;
     default:
        break;
    };
    return n*width;
}

static bool type_has_length(nbt_type_t type) {
    switch(type) {
     case TYPE_BYTE_ARRAY:
     case TYPE_INT_ARRAY:
     case TYPE_LONG_ARRAY:
     case TYPE_LIST_OF_END:
     case TYPE_LIST_OF_BYTE:
     case TYPE_LIST_OF_SHORT:
     case TYPE_LIST_OF_INT:
     case TYPE_LIST_OF_LONG:
     case TYPE_LIST_OF_FLOAT:
     case TYPE_LIST_OF_DOUBLE:
     case TYPE_LIST_OF_BYTE_ARRAY:
     case TYPE_LIST_OF_STRING:
     case TYPE_LIST_OF_RAW_STRING:
     case TYPE_LIST_OF_LIST:
     case TYPE_LIST_OF_COMPOUND:
     case TYPE_LIST_OF_INT_ARRAY:
     case TYPE_LIST_OF_LONG_ARRAY:
        return true;
     default:
        break;
    };
    return false;
}

static int read_payload_length(const unsigned char** ptr,
                               const unsigned char* end,
                               nbt_type_t type, nbt_format_t fmt) {
    if(!type_has_length(type)) {
        return 1;
    }
    int n;
    char dfmt = get_binary_format(TYPE_INT, fmt);
    const unsigned char * p = decode_sint(&n, *ptr, end - *ptr, dfmt);
    if(p == NULL) {
        return -1;
    }
    *ptr = p;
    return n;
}

static SEXP read_nbt_payload_character(const unsigned char** ptr,
                                       const unsigned char* end,
                                       nbt_type_t type, nbt_format_t fmt) {
    int n = read_payload_length(ptr, end, type, fmt);
    if(n == -1) {
        return R_NilValue;
    }
    // Check for embedded nulls and valid format
    bool has_null = false;
    char dfmt = get_binary_format(TYPE_STRING, fmt);
    const unsigned char *p = *ptr;
    for(int i = 0; i < n; ++i) {
        unsigned short len;
        p = decode_ushort(&len, p, end - p, dfmt);
        if(p == NULL || end - p < len) {
            return R_NilValue;
        }
        for(int j=0; !has_null && j < len; ++j) {
            if(p[j] == '\0') {
                has_null = true;
            }
        }
        p += len;
    }
    // reset p back to *ptr
    p = *ptr;
    SEXP r_ret;
    if(!has_null) {
        // create character vector
        r_ret = PROTECT(Rf_allocVector(STRSXP, n));
        for(R_xlen_t i = 0; i < XLENGTH(r_ret); ++i) {
            unsigned short len;
            p = decode_ushort(&len, p, end - p, dfmt);
            SET_STRING_ELT(r_ret, i,
                Rf_mkCharLenCE((const char*)p, len, CE_UTF8));
            p += len;
        }
    } else if(n == 1 && (type == TYPE_STRING || type == TYPE_RAW_STRING)) {
        // create raw vector
        unsigned short len;
        p = decode_ushort(&len, p, end - p, dfmt);
        r_ret = PROTECT(Rf_allocVector(RAWSXP, len));
        memcpy(RAW(r_ret), p, len);
        p += len;
    } else {
        // create a list of raws
        r_ret = PROTECT(Rf_allocVector(VECSXP, n));
        for(R_xlen_t i = 0; i < XLENGTH(r_ret); ++i) {
            unsigned short len;
            p = decode_ushort(&len, p, end - p, dfmt);
            SEXP r = PROTECT(Rf_allocVector(RAWSXP, len));
            memcpy(RAW(r), p, len);
            p += len;
            SET_VECTOR_ELT(r_ret, i, r);
            UNPROTECT(1);
        }
    }
    *ptr = p;
    UNPROTECT(1);
    return r_ret;
}

static SEXP read_nbt_payload_numeric(const unsigned char** ptr,
                                     const unsigned char* end,
                                     nbt_type_t type, nbt_format_t fmt) {
    int n = read_payload_length(ptr, end, type, fmt);
    if(n == -1) {
        return R_NilValue;
    }
    // Store numeric values as doubles to avoid loss of information from NAs
    SEXP r_ret = PROTECT(Rf_allocVector(REALSXP, n));
    double *x = REAL(r_ret);
    signed char ybyte;
    signed short yshort;
    signed int yint;
    float yfloat;
    
    char dfmt = get_binary_format(type, fmt);
    const unsigned char *p = *ptr;
    for(int i=0; i < n; ++i) {
        switch(type) {
         case TYPE_BYTE:
         case TYPE_BYTE_ARRAY:
         case TYPE_LIST_OF_BYTE:
            p = decode_sbyte(&ybyte, p, end - p, dfmt);
            x[i] = (double)ybyte;
            break;
         case TYPE_SHORT:
         case TYPE_LIST_OF_SHORT:
            p = decode_sshort(&yshort, p, end - p, dfmt);
            x[i] = (double)yshort;
            break;
         case TYPE_INT:
         case TYPE_INT_ARRAY:
         case TYPE_LIST_OF_INT:
            p = decode_sint(&yint, p, end - p, dfmt);
            x[i] = (double)yint;
            break;
         case TYPE_FLOAT:
         case TYPE_LIST_OF_FLOAT:
            p = decode_float(&yfloat, p, end - p, dfmt);
            x[i] = (double)yfloat;
            break;
        /* While longs are now recorded as strings, keep this pass through.*/
         case TYPE_LONG:
         case TYPE_LONG_ARRAY:
         case TYPE_LIST_OF_LONG:
         case TYPE_DOUBLE:
         case TYPE_LIST_OF_DOUBLE:
            p = decode_double(x + i, p, end - p, dfmt);
            break;
         default:
            return_nbt_error_tag(type);
            break;
        };
        if(p == NULL) {
            UNPROTECT(1);
            return R_NilValue;
        }
    }
    *ptr = p;
    UNPROTECT(1);
    return r_ret;
}

static SEXP read_nbt_payload_integer64(const unsigned char** ptr,
                                       const unsigned char* end,
                                       nbt_type_t type, nbt_format_t fmt) {
    int n = read_payload_length(ptr, end, type, fmt);
    if(n == -1) {
        return R_NilValue;
    }
    SEXP r_ret = PROTECT(Rf_allocVector(STRSXP, n));
    signed long long ylong;
    char buffer[22];
    char dfmt = get_binary_format(TYPE_LONG, fmt);
    const unsigned char *p = *ptr;
    for(int i=0; i < n; ++i) {
        p = decode_slong((int64_t*)&ylong, p, end - p, dfmt);
        snprintf(buffer, sizeof(buffer), "%lli", ylong);
        SET_STRING_ELT(r_ret, i, Rf_mkCharCE(buffer, CE_UTF8));
        if(p == NULL) {
            UNPROTECT(1);
            return R_NilValue;
        }
    }
    *ptr = p;
    UNPROTECT(1);
    return r_ret;
}

static SEXP read_nbt_payload_empty_list(const unsigned char** ptr,
                                        const unsigned char* end,
                                        nbt_type_t type,
                                        nbt_format_t fmt) {
    int n = read_payload_length(ptr, end, type, fmt);
    if(n == -1) {
        return R_NilValue;
    }
    if(n > 0) {
        return_nbt_error_tag(TAG_END);
    }
    return Rf_allocVector(VECSXP, 0);
}

static SEXP read_nbt_payload_basic_list(const unsigned char** ptr,
                                        const unsigned char* end,
                                        nbt_type_t type, nbt_format_t fmt) {
    int n = read_payload_length(ptr, end, type, fmt);
    if(n == -1) {
        return R_NilValue;
    }
    nbt_type_t inner_type = type % TYPE_LIST_OF_END;

    SEXP r_ret = PROTECT(Rf_allocVector(VECSXP, n));
    for(R_xlen_t i = 0; i < XLENGTH(r_ret); ++i) {
        SET_VECTOR_ELT(r_ret, i, read_nbt_payload(ptr, end, inner_type, fmt));
    }
    UNPROTECT(1);
    return r_ret;
}

static SEXP read_nbt_payload_nested_list(const unsigned char** ptr,
                                         const unsigned char* end,
                                         nbt_type_t type, nbt_format_t fmt) {
    int n = read_payload_length(ptr, end, type, fmt);
    if(n == -1) {
        return R_NilValue;
    }
    SEXP r_ret = PROTECT(Rf_allocVector(VECSXP, n));
    for(R_xlen_t i = 0; i < XLENGTH(r_ret); ++i) {
        const char *names[] = {"type", "value", ""};
        SEXP r_inner = PROTECT(Rf_mkNamed(VECSXP, names));
        if(*ptr >= end) {
            return_nbt_error();
        }
        nbt_type_t inner_type = TYPE_LIST_OF_END;
        inner_type += **ptr;
        *ptr += 1;
        SET_VECTOR_ELT(r_inner, 0, Rf_ScalarInteger(inner_type));
        SET_VECTOR_ELT(r_inner, 1, read_nbt_payload(ptr, end, inner_type, fmt));
        UNPROTECT(1);
        SET_VECTOR_ELT(r_ret, i, r_inner);
    }
    UNPROTECT(1);
    return r_ret;
}

static SEXP read_nbt_payload_compound(const unsigned char** ptr,
                                      const unsigned char* end,
                                      nbt_type_t type,
                                      nbt_format_t fmt) {
    SEXP r_ret = PROTECT(create_stretchy_list());
    SEXP r_val;
    for(;;) {
        if(*ptr >= end) {
            // We've run out of space.
            return_nbt_error_tag(10);
        }
        r_val = PROTECT(read_nbt_value(ptr, end, fmt));
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

static SEXP read_nbt_payload(const unsigned char** ptr,
                             const unsigned char* end,
                             nbt_type_t type, nbt_format_t fmt) {
    // read payloads
    switch(type) {
     case TYPE_END:
        return Rf_allocVector(VECSXP, 0);
     case TYPE_BYTE:
     case TYPE_SHORT:
     case TYPE_INT:
     case TYPE_FLOAT:
     case TYPE_DOUBLE:
     case TYPE_BYTE_ARRAY:
     case TYPE_INT_ARRAY:
     case TYPE_LIST_OF_BYTE:
     case TYPE_LIST_OF_SHORT:
     case TYPE_LIST_OF_INT:
     case TYPE_LIST_OF_FLOAT:
     case TYPE_LIST_OF_DOUBLE:
        return read_nbt_payload_numeric(ptr, end, type, fmt);
     case TYPE_LONG:
     case TYPE_LONG_ARRAY:
     case TYPE_LIST_OF_LONG:
        return read_nbt_payload_integer64(ptr, end, type, fmt);
     case TYPE_STRING:
     case TYPE_LIST_OF_STRING:
     case TYPE_RAW_STRING:
     case TYPE_LIST_OF_RAW_STRING:
        return read_nbt_payload_character(ptr, end, type, fmt);
     case TYPE_COMPOUND:
        return read_nbt_payload_compound(ptr, end, type, fmt);
     case TYPE_LIST:
     case TYPE_LIST_OF_END:
        return read_nbt_payload_empty_list(ptr, end, type, fmt);
     case TYPE_LIST_OF_COMPOUND:
     case TYPE_LIST_OF_BYTE_ARRAY:
     case TYPE_LIST_OF_INT_ARRAY:
     case TYPE_LIST_OF_LONG_ARRAY:
        return read_nbt_payload_basic_list(ptr, end, type, fmt);
     case TYPE_LIST_OF_LIST:
        return read_nbt_payload_nested_list(ptr, end, type, fmt);
     default:
        break;
    }
    return_nbt_error_tag(type);
}

SEXP read_nbt_value(const unsigned char** ptr, const unsigned char* end,
    nbt_format_t fmt) {
    SEXP r_name, r_payload;
    nbt_tag_t tag;
    if(*ptr >= end) {
        return_nbt_error();
    }
    tag = **ptr;
    *ptr += 1;
    if(tag == TAG_END) {
        return R_NilValue;
    }
    if((unsigned int)tag >= TAG_CHECK) {
        return_nbt_error_tag(tag);
    }
    r_name = PROTECT(read_nbt_payload_character(ptr, end, TYPE_STRING, fmt));
    if(Rf_isNull(r_name)) {
        return_nbt_error();
    }
    // read list tag if needed
    int type_offset = TYPE_END;
    if(tag == TAG_LIST) {
        if(*ptr >= end) {
            return_nbt_error();
        }
        type_offset = TYPE_LIST_OF_END;
        tag = **ptr;
        *ptr += 1;
    }
    // calculate type
    nbt_type_t type = tag + type_offset;

    r_payload = PROTECT(read_nbt_payload(ptr, end, type, fmt));
    if(Rf_isNull(r_payload)) {
        return_nbt_error();
    }
    // Adjust character types for raw strings
    if(type == TYPE_STRING && TYPEOF(r_payload) == RAWSXP) {
        type = TYPE_RAW_STRING;
    } else if(type == TYPE_LIST_OF_STRING && TYPEOF(r_payload) == VECSXP) {
        type = TYPE_LIST_OF_RAW_STRING;
    }
    const char *names[] = {"name", "type", "value", ""};
    SEXP r_ret = PROTECT(Rf_mkNamed(VECSXP, names));        
    SET_VECTOR_ELT(r_ret, 0, r_name);
    SET_VECTOR_ELT(r_ret, 1, Rf_ScalarInteger(type));
    SET_VECTOR_ELT(r_ret, 2, r_payload);
    UNPROTECT(3);
    return r_ret;
}

SEXP read_nbt_values(const unsigned char** ptr, const unsigned char* end,
                     nbt_format_t fmt) {
    SEXP r_ret = PROTECT(create_stretchy_list());
    SEXP r_val;
    while(*ptr < end) {
        r_val = PROTECT(read_nbt_value(ptr, end, fmt));
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

static
unsigned char* write_nbt_payload(SEXP r_value, unsigned char* ptr,
    unsigned char* const end, nbt_type_t type, nbt_format_t fmt,
    R_xlen_t* len);

static
unsigned char* write_nbt_tag(nbt_tag_t tag, unsigned char* ptr,
    unsigned char* const end, R_xlen_t *len) {
    assert(ptr != NULL);
    assert(end != NULL);
    assert(ptr <= end);
    assert(len != NULL);

    *len += 1;
    if(end - ptr < 1) {
        return (unsigned char*)end;
    }
    *ptr = (unsigned char)tag;
    return ptr + 1;
}

static
unsigned char* write_nbt_payload_length(SEXP r_value, unsigned char* ptr,
    unsigned char* const end, nbt_format_t fmt, R_xlen_t *len) {

    char dfmt = get_binary_format(TYPE_INT, fmt);
    return encode_sint(XLENGTH(r_value), ptr, end - ptr, dfmt, len);
}

static
unsigned char* write_nbt_payload_numeric(SEXP r_value, unsigned char* ptr,
    unsigned char* const end, nbt_type_t type, nbt_format_t fmt,
    R_xlen_t *len) {

    r_value = PROTECT(Rf_coerceVector(r_value, REALSXP));

    // validate data
    if(!type_has_length(type)) {
        if(XLENGTH(r_value) != 1) {
            return_nbt_error_msg0("Payload of rnbt type %d does not a scalar.",
                type);
        }
    }
    if(type_has_length(type)) {
        ptr = write_nbt_payload_length(r_value, ptr, end, fmt, len);
    }
    char dfmt = get_binary_format(type, fmt);
    double *x = REAL(r_value);
    int n = XLENGTH(r_value);

    for(int i=0; i < n; ++i) {
        switch(type) {
         case TYPE_BYTE:
         case TYPE_BYTE_ARRAY:
         case TYPE_LIST_OF_BYTE:
            ptr = encode_sbyte((signed char)x[i], ptr, end - ptr, dfmt, len);
            break;
         case TYPE_SHORT:
         case TYPE_LIST_OF_SHORT:
            ptr = encode_sshort((signed short)x[i], ptr, end - ptr, dfmt, len);
            break;
         case TYPE_INT:
         case TYPE_INT_ARRAY:
         case TYPE_LIST_OF_INT:
            ptr = encode_sint((signed int)x[i], ptr, end - ptr, dfmt, len);
            break;
         case TYPE_FLOAT:
         case TYPE_LIST_OF_FLOAT:
            ptr = encode_float((float)x[i], ptr, end - ptr, dfmt, len);
            break;
        /* While longs are now recorded as strings, keep this pass through.*/
         case TYPE_LONG:
         case TYPE_LONG_ARRAY:
         case TYPE_LIST_OF_LONG:
         case TYPE_DOUBLE:
         case TYPE_LIST_OF_DOUBLE:
            ptr = encode_double(x[i], ptr, end - ptr, dfmt, len);
            break;
         default:
            return_nbt_error0();
            break;
        };
    }

    UNPROTECT(1);
    return ptr;
}

static
unsigned char* write_nbt_payload_integer64(SEXP r_value, unsigned char* ptr,
    unsigned char* const end, nbt_type_t type, nbt_format_t fmt,
    R_xlen_t *len) {

    // validate data
    if(type_has_length(type)) {
        if(!Rf_isString(r_value)) {
            return_nbt_error0();
        }
    } else {
        if(!IS_SCALAR(r_value, STRSXP)) {
            return_nbt_error0();
        }
    }
    if(type_has_length(type)) {
        ptr = write_nbt_payload_length(r_value, ptr, end, fmt, len);
    }
    char dfmt = get_binary_format(type, fmt);
    int n = XLENGTH(r_value);

    for(int i = 0; i < n; ++i) {
        const char* str = Rf_translateCharUTF8(STRING_ELT(r_value, i));
        char * strend;
        signed long long ylong = strtoll(str, &strend, 10);
        if(*strend != '\0') {
            return_nbt_error0();
        }
        ptr = encode_slong(ylong, ptr, end - ptr, dfmt, len);
    }
    return ptr;
}

static
unsigned char* write_nbt_payload_character_impl(const char *val,
    unsigned short n, unsigned char* ptr, unsigned char* const end,
    nbt_format_t fmt, R_xlen_t *len) {
    char dfmt = get_binary_format(TYPE_STRING, fmt);

    ptr = encode_ushort(n, ptr, end - ptr, dfmt, len);
    *len += n;
    if(end - ptr < n) {
        return end;
    }
    memcpy(ptr, val, n);
    return ptr + n;
}

static
unsigned char* write_nbt_payload_character(SEXP r_value, unsigned char* ptr,
    unsigned char* const end, nbt_type_t type, nbt_format_t fmt,
    R_xlen_t *len) {
    assert(ptr != NULL);
    assert(end != NULL);
    assert(ptr <= end);
    assert(len != NULL);

    // Validate storage
    if(type == TYPE_LIST_OF_STRING) {
        if(!(Rf_isString(r_value))) {
            return_nbt_error0();
        }
    } else if(type == TYPE_LIST_OF_RAW_STRING) {
        if(TYPEOF(r_value) != VECSXP) {
            return_nbt_error0();
        }
    } else if(type == TYPE_RAW_STRING) {
        if(TYPEOF(r_value) != RAWSXP) {
            return_nbt_error0();
        }
    } else {
        if(!IS_SCALAR(r_value, STRSXP) && TYPEOF(r_value) != CHARSXP) {
            return_nbt_error0();
        }
    }

    if(type_has_length(type)) {
        ptr = write_nbt_payload_length(r_value, ptr, end, fmt, len);
    }

    if(type == TYPE_LIST_OF_STRING) {
        int n = (int)XLENGTH(r_value);
        for(int i = 0; i < n; ++i) {
            const char *str = Rf_translateCharUTF8(STRING_ELT(r_value, i));
            ptr = write_nbt_payload_character_impl(str, strlen(str), ptr, end,
                fmt, len);
        }
    } else if(type == TYPE_LIST_OF_RAW_STRING) {
        int n = (int)XLENGTH(r_value);
        for(int i = 0; i < n; ++i) {
            SEXP r = VECTOR_ELT(r_value, i);
            ptr = write_nbt_payload_character_impl((const char*)RAW(r),
                XLENGTH(r), ptr, end, fmt, len);
        }        
    } else if(type == TYPE_RAW_STRING) {
        ptr = write_nbt_payload_character_impl((const char*)RAW(r_value),
                XLENGTH(r_value), ptr, end, fmt, len);
    } else {
        const char *str = Rf_translateCharUTF8(STRING_ELT(r_value, 0));
        ptr = write_nbt_payload_character_impl(str, strlen(str), ptr, end, fmt,
                len);
    }
    return ptr;
}

static
unsigned char* write_nbt_payload_compound(SEXP r_value, unsigned char* ptr,
    unsigned char* const end, nbt_type_t type, nbt_format_t fmt,
    R_xlen_t *len) {

    if(TYPEOF(r_value) != VECSXP) {
        return_nbt_error0();
    }

    R_xlen_t r_len = XLENGTH(r_value);
    for(R_xlen_t i = 0; i < r_len; ++i){
        SEXP ri = VECTOR_ELT(r_value, i);
        R_xlen_t n = end - ptr;
        R_xlen_t k = write_nbt_value(ri, ptr, n, fmt);
        *len += k;
        if(k <= n) {
            ptr += k;
        } else {
            ptr = end;
        }
    }
    ptr = write_nbt_tag(TAG_END, ptr, end, len);

    return ptr;
}

static
unsigned char* write_nbt_payload_empty_list(SEXP r_value, unsigned char* ptr,
    unsigned char* const end, nbt_type_t type, nbt_format_t fmt,
    R_xlen_t *len) {
    if(XLENGTH(r_value) != 0) {
        return_nbt_error0();
    }
    return write_nbt_payload_length(r_value, ptr, end, fmt, len);
}

static
unsigned char* write_nbt_payload_basic_list(SEXP r_value, unsigned char* ptr,
    unsigned char* const end, nbt_type_t type, nbt_format_t fmt,
    R_xlen_t *len) {

    if(TYPEOF(r_value) != VECSXP) {
        return_nbt_error0();
    }

    ptr = write_nbt_payload_length(r_value, ptr, end, fmt, len);

    nbt_type_t inner_type = type % TYPE_LIST_OF_END;

    int r_len = (int)XLENGTH(r_value);
    for(int i = 0; i < r_len; ++i){
        SEXP ri = VECTOR_ELT(r_value, i);
        ptr = write_nbt_payload(ri, ptr, end, inner_type, fmt, len);
    }
    return ptr;
}

static
unsigned char* write_nbt_payload_nested_list(SEXP r_value, unsigned char* ptr,
    unsigned char* const end, nbt_type_t type, nbt_format_t fmt,
    R_xlen_t *len) {
    
    if(TYPEOF(r_value) != VECSXP) {
        return_nbt_error0();
    }

    ptr = write_nbt_payload_length(r_value, ptr, end, fmt, len);

    int r_len = (int)XLENGTH(r_value);
    for(int i = 0; i < r_len; ++i){
        SEXP ri = VECTOR_ELT(r_value, i);
        nbt_type_t inner_type = Rf_asInteger(get_list_element(ri, "type"));
        SEXP r_payload = get_list_element(ri, "value");
        nbt_tag_t tag;
        if(type == TYPE_LIST_OF_RAW_STRING) {
            tag = TAG_STRING;
        } else {
            tag = inner_type % TYPE_LIST_OF_END;
        }
        ptr = write_nbt_tag(tag, ptr, end, len);
        ptr = write_nbt_payload(r_payload, ptr, end, inner_type, fmt, len);
    }

    return ptr;
}

static unsigned char* write_nbt_payload(SEXP r_value, unsigned char* ptr,
    unsigned char* const end, nbt_type_t type, nbt_format_t fmt,
    R_xlen_t* len) {
    assert(ptr != NULL);
    assert(end != NULL);
    assert(ptr <= end);
    assert(len != NULL);

    // write payloads
    switch(type) {
     case TYPE_END:
        return ptr;
     case TYPE_BYTE:
     case TYPE_SHORT:
     case TYPE_INT:
     case TYPE_FLOAT:
     case TYPE_DOUBLE:
     case TYPE_BYTE_ARRAY:
     case TYPE_INT_ARRAY:
     case TYPE_LIST_OF_BYTE:
     case TYPE_LIST_OF_SHORT:
     case TYPE_LIST_OF_INT:
     case TYPE_LIST_OF_FLOAT:
     case TYPE_LIST_OF_DOUBLE:
        return write_nbt_payload_numeric(r_value, ptr, end, type, fmt, len);
     case TYPE_LONG:
     case TYPE_LONG_ARRAY:
     case TYPE_LIST_OF_LONG:
        return write_nbt_payload_integer64(r_value, ptr, end, type, fmt, len);
     case TYPE_STRING:
     case TYPE_LIST_OF_STRING:
     case TYPE_RAW_STRING:
     case TYPE_LIST_OF_RAW_STRING:
        return write_nbt_payload_character(r_value, ptr, end, type, fmt, len);
     case TYPE_COMPOUND:
        return write_nbt_payload_compound(r_value, ptr, end, type, fmt, len);
     case TYPE_LIST:
     case TYPE_LIST_OF_END:
        return write_nbt_payload_empty_list(r_value, ptr, end, type, fmt, len);
     case TYPE_LIST_OF_COMPOUND:
     case TYPE_LIST_OF_BYTE_ARRAY:
     case TYPE_LIST_OF_INT_ARRAY:
     case TYPE_LIST_OF_LONG_ARRAY:
        return write_nbt_payload_basic_list(r_value, ptr, end, type, fmt, len);
     case TYPE_LIST_OF_LIST:
        return write_nbt_payload_nested_list(r_value, ptr, end, type, fmt, len);
     default:
        break;
    }
    return_nbt_error0();
}

R_xlen_t write_nbt_value(SEXP r_value, unsigned char* ptr, R_xlen_t n,
                         nbt_format_t fmt) {
    PROTECT(r_value);
    int type = Rf_asInteger(get_list_element(r_value, "type"));
    SEXP r_name = get_list_element(r_value, "name");
    SEXP r_payload = get_list_element(r_value, "value");

    nbt_tag_t tag;
    if(type == TYPE_RAW_STRING) {
        tag = TAG_STRING;
    } else if(type >= TYPE_LIST_OF_END) {
        tag = TAG_LIST;
    } else {
        tag = type % TYPE_LIST_OF_END;
    }

    R_xlen_t len = 0;
    unsigned char* end = ptr + n;

    ptr = write_nbt_tag(tag, ptr, end, &len);
    ptr = write_nbt_payload_character(r_name, ptr, end, TYPE_STRING, fmt, &len);

    if(tag == TAG_LIST) {
        if(type == TYPE_LIST_OF_RAW_STRING) {
            tag = TAG_STRING;
        } else {
            tag = type % TYPE_LIST_OF_END;
        }
        ptr = write_nbt_tag(tag, ptr, end, &len);
    }

    ptr = write_nbt_payload(r_payload, ptr, end, type, fmt, &len);

    UNPROTECT(1);
    return len;
}

R_xlen_t write_nbt_values(SEXP r_value, unsigned char* ptr, R_xlen_t n,
                          nbt_format_t fmt) {
    // validate data
    if(TYPEOF(r_value) != VECSXP) {
        return_nbt_error0();
    }
    PROTECT(r_value);
    R_xlen_t totlen = 0;
    for(R_xlen_t i = 0; i < XLENGTH(r_value); ++i) {
        R_xlen_t len = write_nbt_value(VECTOR_ELT(r_value, i), ptr, n, fmt);
        totlen += len;
        len = (len > n) ? n : len;
        ptr += len;
        n -= len;
    }
    UNPROTECT(1);
    return totlen;
}

SEXP attribute_visible R_read_nbt(SEXP r_value, SEXP r_format) {
    if(Rf_isNull(r_value)) {
        return R_NilValue;
    }
    if(TYPEOF(r_value) != RAWSXP) {
        error_return("Argument is not a raw type.");
    }
    nbt_format_t fmt = Rf_asInteger(r_format);

    size_t len = XLENGTH(r_value);
    const unsigned char *buffer = RAW(r_value);
    const unsigned char *p = buffer;
    return read_nbt_values(&p, buffer+len, fmt);
}

SEXP attribute_visible R_write_nbt(SEXP r_value, SEXP r_format) {
    if(Rf_isNull(r_value)) {
        return R_NilValue;
    }
    nbt_format_t fmt = Rf_asInteger(r_format);

    // Try to write the nbt data with the stack and fall back to the heap
    // if needed.
    unsigned char buffer[8192];
    R_xlen_t len = write_nbt_values(r_value, buffer, sizeof(buffer), fmt);
    SEXP ret = PROTECT(Rf_allocVector(RAWSXP, len));
    if(len <= 8192) {
        memcpy(RAW(ret), buffer, len);
    } else {
        R_xlen_t len2 = write_nbt_values(r_value, RAW(ret), XLENGTH(ret), fmt);
        if(len2 != len) {
            return_nbt_error();
        }
    }
    UNPROTECT(1);
    return ret;
}
