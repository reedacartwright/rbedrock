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

// convert an internal rawkey to a human-readable format
// keys for chunk data are converted to @x:z:d:t-s
// everything else is percent encoded.
size_t rawkey_to_chrkey(const unsigned char *key, size_t key_len, char *buffer, size_t buffer_len) {
    unsigned int dimension = 0;
    int x = 0;
    int z = 0;
    char tag = 0;
    char subtag = -1;
    bool is_chunk_key = true;
    int ret_len = 0;

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
        } else if(subtag > CHUNK_KEY_SUBCHUNK_MAX) {
            is_chunk_key = false;
        }
    }
    if(is_chunk_key) {
        if(subtag != -1) {
            ret_len = snprintf(buffer, buffer_len, "@%i:%i:%u:%u-%u", x, z, dimension,
                                (unsigned int)tag, (unsigned int)subtag);
        } else {
            ret_len = snprintf(buffer, buffer_len, "@%i:%i:%u:%u", x, z, dimension,
                                (unsigned int)tag);
        }
    } else {
        // Identify the beginning of the string up until the first
        // character that needs encoding. Most strings can be copied
        // as is.
        int i = 0;
        for(; i != key_len; ++i) {
            char ch = key[i];
            if(ch <= 0x20 || ch >= 0x7F || ch == '%' || ch == '@') {
                break;
            }
        }
        // copy beginning of string
        ret_len = i;
        if(ret_len > 0) {
            if(buffer_len > ret_len) {
                memcpy(buffer, key, ret_len);
            } else if(buffer_len > 0) {
                memcpy(buffer, key, buffer_len-1);
            }
        }
        // encode the rest of the string
        for(; i != key_len; ++i) {
            char ch = key[i];

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
    }
    return ret_len;
}

// Take a VECSXP of raw, internal keys and covert them to human-readable keys.
SEXP rawkeys_to_chrkeys(SEXP r_keys) {
    // Maximum chunk data key size is 44 = 1+11+1+11+1+10+1+3+1+3+1
    // Maximum other key size is 3*len.

    if(r_keys == R_NilValue) {
        return R_NilValue;
    }
    if(TYPEOF(r_keys) != VECSXP) {
        error_return("Argument 'keys' is not a list.");
    }
    // Allocate a buffer to hold keys
    int buffer_len = 8192;
    char * buffer = R_alloc(buffer_len, sizeof(char));
    if(buffer == NULL) {
        error_return("insufficient memory to allocate character buffer");
    }

    R_xlen_t sz = XLENGTH(r_keys);

    SEXP r_ret = PROTECT(Rf_allocVector(STRSXP, sz));
    for(R_xlen_t i = 0; i < sz; ++i) {
        SEXP elm = VECTOR_ELT(r_keys, i);
        if(elm == R_NilValue) {
            SET_STRING_ELT(r_ret, i, NA_STRING);
            continue;
        }
        if(TYPEOF(elm) != RAWSXP) {
            Rf_error("Element %td of argument 'key' is not a raw type or NULL.", i);
            return R_NilValue;
        }
        R_xlen_t raw_len = XLENGTH(elm);
        if(buffer_len < 3*raw_len+1) {
            buffer = S_realloc(buffer, 3*raw_len+1, buffer_len, sizeof(char));
            buffer_len = 3*raw_len+1;
            if(buffer == NULL) {
                error_return("insufficient memory to allocate character buffer");
            }
        }
        size_t key_len = rawkey_to_chrkey(RAW(elm), raw_len, buffer, buffer_len);
        SET_STRING_ELT(r_ret, i, Rf_mkCharLenCE(buffer, key_len, CE_UTF8));
    }
    UNPROTECT(1);
    return r_ret;
}
