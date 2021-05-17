/*
# Copyright (c) 2020,2021 Reed A. Cartwright <reed@cartwright.ht>
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
#include "key_conv.h"

#include <stdio.h>
#include <string.h>
#include <stdbool.h>

#define CHUNK_KEY_TAG_MIN 33
#define CHUNK_KEY_TAG_MAX 64
#define CHUNK_KEY_LEGACY_VERSION_TAG 118
#define CHUNK_KEY_DIM_MAX 2
#define CHUNK_KEY_SUBCHUNK_MAX 31

static char encode_hex(unsigned int x) {
    x = x & 15;
    if(x < 10) {
        return '0'+x;
    } else {
        return 'A'+x-10;
    }
}

static int decode_hex_digit(unsigned char ch) {
    if('0' <= ch && ch <= '9') {
        return ch - '0';
    } else if('A' <= ch && ch <= 'F') {
        return ch - 'A' + 10;
    } else if('a' <= ch && ch <= 'f') {
        return ch - 'a' + 10;
    }
    return -1;
}

static size_t str_to_uint(const char *str, size_t len, unsigned int *out) {
    unsigned int val = 0;
    size_t i = 0;
    for(;i<len;++i) {
        char ch = str[i];
        if('0' > ch || ch > '9') {
            break;
        }
        ch = ch - '0';
        val = val*10 + ch;
    }
    if(out != NULL) {
        *out = val;
    }
    return i;
}

static size_t str_to_int(const char *str, size_t len, int *out) {
    if(len == 0) {
        return 0;
    }
    size_t ret;
    int val;
    unsigned int u;
    if(str[0] == '-') {
        ret = str_to_uint(str+1, len-1, &u);
        ret += 1;
        val = -u;
    } else {
        ret = str_to_uint(str, len, &u);
        val = u;
    }
    if(out != NULL) {
        *out = val;
    }
    return ret;
}

// Percent encode keys.
// Writes up to buffer_len-1 characters into buffer.
// Appends '\0' to written string.
// Returns the total length of the encoding (minus terminating null), even if all
// characters were not written.
static size_t percent_encode(const unsigned char *key, size_t key_len, char *buffer, size_t buffer_len) {
    // Identify the beginning of the string up until the first
    // character that needs encoding. Most strings can be copied
    // as is.
    int i = 0;
    for(; i != key_len; ++i) {
        unsigned char ch = key[i];
        if(ch <= 0x20 || ch >= 0x7F || ch == '%' || ch == '@') {
            break;
        }
    }
    // copy beginning of string
    size_t ret_len = i;
    if(ret_len > 0) {
        if(buffer_len > ret_len) {
            memcpy(buffer, key, ret_len);
        } else if(buffer_len > 0) {
            memcpy(buffer, key, buffer_len-1);
        }
    }
    // encode the rest of the string
    for(; i != key_len; ++i) {
        unsigned char ch = key[i];

        if(ch <= 0x20 || ch >= 0x7F || ch == '%' || ch == '@') {
            if(buffer_len > ret_len+3 ) {
                buffer[ret_len] = '%';
                buffer[ret_len+1] = encode_hex(ch >> 4);
                buffer[ret_len+2] = encode_hex(ch & 0xF);
            }
            ret_len += 3;
        } else {
            if(buffer_len > ret_len+1 ) {
                buffer[ret_len] = ch;
            }
            ret_len += 1;
        }
    }
    if(buffer_len > ret_len) {
        buffer[ret_len] = '\0';
    } else if(buffer_len > 0) {
        buffer[buffer_len-1] = '\0';
    }
    return ret_len;
}

// Percent decode keys.
// Writes up to buffer_len characters into buffer.
// Returns the total length of the encoding, even if all
// characters were not written.
static size_t percent_decode(const char *key, size_t key_len, unsigned char *buffer, size_t buffer_len) {
    size_t i = 0;
    for(; i < key_len; ++i) {
        if(key[i] == '%') {
            break;
        }
    }
    // copy beginning of string
    size_t ret_len = i;
    if(ret_len > 0) {
        if(buffer_len > ret_len) {
            memcpy(buffer, key, ret_len);
        } else if(buffer_len > 0) {
            memcpy(buffer, key, buffer_len);
        }
    }
    // decode the rest of the string
    for(; i != key_len; ++i) {
        unsigned char ch = key[i];
        if(ch == '%' && i+2 < key_len) {
            int a = decode_hex_digit(key[i+1]);
            int b = decode_hex_digit(key[i+2]);
            if(a != -1 && b != -1) {
                ch = (unsigned char)(a*16+b);
                i += 2;
            }
        }
        if(buffer_len > ret_len) {
            buffer[ret_len] = ch;
        }
        ret_len += 1;
    }
    return ret_len;
}

size_t chunkkey_decode(const char *key, size_t key_len, unsigned char *buffer, size_t buffer_len) {
    unsigned int u = 0;
    int x = 0, z = 0;
    unsigned int dimension = 0;
    signed char tag = 0;
    signed char subtag = -1;

    size_t sz = 0;
    size_t i = 0;

    if(key[i] != '@') {
        return 0;
    }
    i += 1;
    // decode x
    sz = str_to_int(key+i, key_len-i, &x);
    i += sz;
    if(sz == 0 || i == key_len || key[i] != ':') {
        return 0;
    }
    i += 1;
    // decode z
    sz = str_to_int(key+i, key_len-i, &z);
    i += sz;
    if(sz == 0 || i == key_len || key[i] != ':') {
        return 0;
    }
    i += 1;
    // decode dimension
    sz = str_to_uint(key+i, key_len-i, &dimension);
    i += sz;
    if(sz == 0 || i == key_len || key[i] != ':') {
        return 0;
    }
    i += 1;
    // decode tag
    sz = str_to_uint(key+i, key_len-i, &u);
    if(sz == 0) {
        return 0;
    }
    tag = u;
    i += sz;
    if(i < key_len) {
        // decode subtag
        if(key[i] != '-') {
            return 0;
        }
        i += 1;
        sz = str_to_uint(key+i, key_len-i, &u);
        i += sz;
        if(sz == 0 || i != key_len) {
            return 0;
        }
        subtag = u;
    }
    // Validate values
    if(tag < CHUNK_KEY_TAG_MIN) {
        return 0;
    } else if(tag > CHUNK_KEY_TAG_MAX && tag != CHUNK_KEY_LEGACY_VERSION_TAG) {
        return 0;
    } else if(dimension > CHUNK_KEY_DIM_MAX) {
        return 0;
    } else if(subtag != -1 && subtag > CHUNK_KEY_SUBCHUNK_MAX) {
        return 0;
    }

    // Check buffer space
    size_t decode_len = 8+4*(dimension != 0)+1+(subtag != -1);
    if(buffer_len < decode_len) {
        return decode_len; // # nocov
    }
    // Write values
    i = 0;
    memcpy(buffer+i,&x,4);
    i += 4;
    memcpy(buffer+i,&z,4);
    i += 4;
    if(dimension > 0) {
        memcpy(buffer+i,&dimension,4);
        i += 4;
    }
    buffer[i] = tag;
    if(subtag != -1) {
        buffer[i+1] = subtag;
    }
    return decode_len;

}

// convert an internal rawkey to a human-readable format
// keys for chunk data are converted to @x:z:d:t-s
// everything else is percent encoded.
// Writes up to buffer_len-1 characters into buffer.
// Appends '\0' to written string.
// Returns the total length of the encoding (minus terminating null), even if all
// characters were not written.
size_t rawkey_to_chrkey(const unsigned char *key, size_t key_len, char *buffer, size_t buffer_len) {
    unsigned int dimension = 0;
    int x = 0;
    int z = 0;
    signed char tag = 0;
    signed char subtag = -1;
    bool is_chunk_key = true;

    switch(key_len) {
    case 10:
        subtag = key[9];
    case 9:
        tag = key[8];
        memcpy(&x, key + 0, 4);
        memcpy(&z, key + 4, 4);
        break;
    case 14:
        subtag = key[13];
    case 13:
        tag = key[12];
        memcpy(&x, key + 0, 4);
        memcpy(&z, key + 4, 4);
        memcpy(&dimension, key + 8, 4);
        break;
    default:
        is_chunk_key = false;
    }
    if(is_chunk_key) {
        if(tag < CHUNK_KEY_TAG_MIN) {
            is_chunk_key = false;
        } else if(tag > CHUNK_KEY_TAG_MAX && tag != CHUNK_KEY_LEGACY_VERSION_TAG) {
            is_chunk_key = false;
        } else if(dimension > CHUNK_KEY_DIM_MAX) {
            is_chunk_key = false;
        } else if(subtag != -1 && subtag > CHUNK_KEY_SUBCHUNK_MAX) {
            is_chunk_key = false;
        }
    }
    if(!is_chunk_key) {
        return percent_encode(key, key_len, buffer, buffer_len);
    } else if(subtag != -1) {
        return snprintf(buffer, buffer_len, "@%d:%d:%u:%u-%u", x, z, dimension,
                                (unsigned int)tag, (unsigned int)subtag);
    }
    return snprintf(buffer, buffer_len, "@%d:%d:%u:%u", x, z, dimension,
                                (unsigned int)tag);
}

// Take a VECSXP of raw, internal keys and covert them to human-readable keys.
SEXP rawkeys_to_chrkeys(SEXP r_keys) {
    // Maximum chunk data key size is 44 = 1+11+1+11+1+10+1+3+1+3+1
    // Maximum other key size is 3*len.
    char buffer[2048];
    const int buffer_len = 2048;

    if(Rf_isNull(r_keys)) {
        return R_NilValue;
    }
    if(TYPEOF(r_keys) != VECSXP) {
        error_return("Argument 'keys' is not a list.");
    }
    R_xlen_t sz = XLENGTH(r_keys);

    SEXP r_ret = PROTECT(Rf_allocVector(STRSXP, sz));
    for(R_xlen_t i = 0; i < sz; ++i) {
        SEXP elm = VECTOR_ELT(r_keys, i);
        if(Rf_isNull(elm)) {
            SET_STRING_ELT(r_ret, i, NA_STRING);
            continue;
        }
        if(TYPEOF(elm) != RAWSXP) {
            Rf_error("Element %td of argument 'key' is not a raw type or NULL.", i);
            return R_NilValue;
        }
        R_xlen_t raw_len = XLENGTH(elm);
        size_t key_len = rawkey_to_chrkey(RAW(elm), raw_len, buffer, buffer_len);
        if(key_len >= buffer_len) {
            Rf_error("Conversion of element %td of argument 'key' exceeded buffer space.", i+1);
            return R_NilValue;
        }
        SET_STRING_ELT(r_ret, i, Rf_mkCharLenCE(buffer, key_len, CE_UTF8));
    }
    UNPROTECT(1);
    return r_ret;
}

// convert a human-readable key to a raw key
// keys for chunk data are converted from @x:z:d:t-s
// everything else is percent decoded.
// Writes up to buffer_len characters into buffer.
// Returns the total length of the encoding, even if all
// characters were not written.
size_t chrkey_to_rawkey(const char *key, size_t key_len, unsigned char *buffer, size_t buffer_len) {
    if(key_len == 0) {
        return 0;
    }
    // check to see if it can be decoded as a key for chunk data
    size_t ret = chunkkey_decode(key, key_len, buffer, buffer_len);
    if(ret > 0) {
        return ret;
    }
    return percent_decode(key, key_len, buffer, buffer_len);    
}

SEXP chrkeys_to_rawkeys(SEXP r_keys) {
    unsigned char buffer[2048];
    const int buffer_len = 2048;    

    if(Rf_isNull(r_keys)) {
        return R_NilValue;
    }
    if(!Rf_isString(r_keys)) {
        error_return("Argument 'keys' is not a vector of strings.");
    }
    R_xlen_t sz = XLENGTH(r_keys);
    SEXP r_ret = PROTECT(Rf_allocVector(VECSXP, sz));

    for(R_xlen_t i = 0; i < sz; ++i) {
        SEXP elm = STRING_ELT(r_keys, i);
        if(elm == NA_STRING) {
            SET_VECTOR_ELT(r_ret, i, R_NilValue);
            continue;
        }
        size_t raw_len = chrkey_to_rawkey(CHAR(elm), XLENGTH(elm), buffer, buffer_len);
        if(raw_len > buffer_len) {
            Rf_error("Conversion of element %td of argument 'key' exceeded buffer space.", i+1);
            return R_NilValue;
        }
        SET_VECTOR_ELT(r_ret, i, Rf_allocVector(RAWSXP, raw_len));
        SEXP out = VECTOR_ELT(r_ret, i);
        memcpy(RAW(out), buffer, raw_len);
    }

    UNPROTECT(1);
    return r_ret;
}
