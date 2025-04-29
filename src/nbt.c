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
            SET_STRING_ELT(r_ret, i, Rf_mkCharLenCE((const char*)p, len, CE_UTF8));
            p += len;
        }
    } else if(n == 1) {
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

SEXP attribute_visible R_write_nbt(SEXP r_value) {
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
