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

#define CHRKEY_PREFIX_CHUNK "chunk:"
#define CHRKEY_PREFIX_PLAIN "plain:"
#define CHRKEY_PREFIX_ACTOR "actor:"
#define CHRKEY_PREFIX_ACTOR_DIGEST_KEYS "acdig:" //Not sure about this one

#define RAWKEY_PREFIX_ACTOR "actorprefix"
#define RAWKEY_PREFIX_ACTOR_DIGEST_KEYS "digp"

#define CHUNK_KEY_TAG_MIN 33
#define CHUNK_KEY_TAG_MAX 96
#define CHUNK_KEY_LEGACY_VERSION_TAG 118
#define CHUNK_KEY_DIM_MAX 2
#define CHUNK_KEY_SUBCHUNK_MIN -32
#define CHUNK_KEY_SUBCHUNK_MAX 31

enum KEY_TYPE {
    PLAIN = 0,        // plain:~local_player
    CHUNK,            // chunk:x:z:d:t:s
    ACTOR,            // actor:0000000000000100
    ACTOR_DIGEST_KEYS // acdat:x:z:d
};

static char encode_hex(unsigned char x) {
    x = x & 15;
    if(x < 10) {
        return (char)('0'+x);
    } else {
        return (char)('A'+x-10);
    }
}

static unsigned char decode_hex_digit(char ch) {
    if('0' <= ch && ch <= '9') {
        return (unsigned char)(ch - '0');
    } else if('A' <= ch && ch <= 'F') {
        return (unsigned char)(ch - 'A' + 10);
    } else if('a' <= ch && ch <= 'f') {
        return (unsigned char)(ch - 'a' + 10);
    }
    return 0xFF;
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

static bool has_prefix(const char *key, size_t key_len, const char *prefix) {
    size_t prefix_len = strlen(prefix);
    if(key_len < prefix_len) {
        return false;
    }
    return(strncmp(key, prefix, prefix_len) == 0);
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
        if(ch <= 0x20 || ch >= 0x7F || ch == '%') {
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

        if(ch <= 0x20 || ch >= 0x7F || ch == '%') {
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
            unsigned char a = decode_hex_digit(key[i+1]);
            unsigned char b = decode_hex_digit(key[i+2]);
            if(a < 16  && b < 16) {
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

// decode a prefix in the format of x:z:d
//  returns how many bytes from key were read.
size_t decode_chunk_prefix(const char *key, size_t key_len, int *x, int *z, unsigned int *dimension) {
    size_t sz = 0;
    size_t i = 0;

    // decode x
    sz = str_to_int(key+i, key_len-i, x);
    i += sz;
    if(sz == 0 || i == key_len || key[i] != ':') {
        return 0;
    }
    i += 1;
    // decode z
    sz = str_to_int(key+i, key_len-i, z);
    i += sz;
    if(sz == 0 || i == key_len || key[i] != ':') {
        return 0;
    }
    i += 1;
    // decode dimension
    sz = str_to_uint(key+i, key_len-i, dimension);
    i += sz;
    if(sz == 0) {
        return 0;
    }
    // validate dimension
    if(*dimension > CHUNK_KEY_DIM_MAX) {
        return 0;
    }
    return i;
}

// decode x:z:dimension:tag and x:z:dimension:tag:subtag
// writes decoding into buffer and returns the number of byte written
// if buffer is too small, it returns the number of bytes that would be written
size_t chunkkey_decode(const char *key, size_t key_len, unsigned char *buffer, size_t buffer_len) {
    unsigned int u = 0;
    int d = 0;
    int x = 0, z = 0;
    unsigned int dimension = 0;
    signed char tag = 0;
    signed char subtag = 0;
    bool has_subtag = false;

    size_t sz = 0;
    size_t i = 0;

    // decode prefix
    sz = decode_chunk_prefix(key, key_len, &x, &z, &dimension);
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
    tag = (signed char)u;
    i += sz;
    if(i < key_len) {
        // decode subtag
        if(key[i] != ':') {
            return 0;
        }
        i += 1;
        sz = str_to_int(key+i, key_len-i, &d);
        i += sz;
        if(sz == 0 || i != key_len) {
            return 0;
        }
        subtag = (signed char)d;
        has_subtag = true;
    }
    // Validate values
    if(tag < CHUNK_KEY_TAG_MIN) {
        return 0;
    } else if(tag > CHUNK_KEY_TAG_MAX && tag != CHUNK_KEY_LEGACY_VERSION_TAG) {
        return 0;
    } else if(subtag < CHUNK_KEY_SUBCHUNK_MIN || subtag > CHUNK_KEY_SUBCHUNK_MAX) {
        return 0;
    }

    // Check buffer space
    size_t decode_len = 8+4*(dimension != 0)+1+(has_subtag);
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
    if(has_subtag) {
        buffer[i+1] = subtag;
    }
    return decode_len;
}

// decode x:z:dimension
// writes decoding into buffer and returns the number of byte written
// if buffer is too small, it returns the number of bytes that would be written
size_t digkey_decode(const char *key, size_t key_len, unsigned char *buffer, size_t buffer_len) {
    int x = 0, z = 0;
    unsigned int dimension = 0;

    // decode prefix
    size_t sz = decode_chunk_prefix(key, key_len, &x, &z, &dimension);
    if(sz == 0 || key_len > sz) {
        return 0;
    }

    const size_t prefix_len = strlen(RAWKEY_PREFIX_ACTOR_DIGEST_KEYS);
    size_t decode_len = prefix_len+8+4*(dimension != 0);
    if(buffer_len < decode_len) {
        return decode_len; // # nocov
    }
    size_t i=0;
    // prefix
    memcpy(buffer, RAWKEY_PREFIX_ACTOR_DIGEST_KEYS, prefix_len);
    i += prefix_len;
    // x
    memcpy(buffer+i,&x,4);
    i += 4;
    // z
    memcpy(buffer+i,&z,4);
    i += 4;
    // dimension
    if(dimension > 0) {
        memcpy(buffer+i,&dimension,4);
        /* i += 4; */
    }

    return decode_len;
}


// decode aabbccddeeffgghh
// writes decoding into buffer and returns the number of byte written
// if buffer is too small, it returns the number of bytes that would be written
size_t actorkey_decode(const char *key, size_t key_len, unsigned char *buffer, size_t buffer_len) {

    // validate key
    if(key_len != 16) {
        return 0;
    }
    for(int i=0;i<16;++i) {
        unsigned char a = decode_hex_digit(key[i]);
        if(a >= 16) {
            return 0;
        }
    }
    const size_t prefix_len = strlen(RAWKEY_PREFIX_ACTOR);
    size_t decode_len = prefix_len+8;
    if(buffer_len < decode_len) {
        return decode_len; // # nocov
    }
    memcpy(buffer, RAWKEY_PREFIX_ACTOR, prefix_len);
    for(int i=0;i<16;i+=2) {
        unsigned char a = decode_hex_digit(key[i]);
        unsigned char b = decode_hex_digit(key[i+1]);
        unsigned char ch = (unsigned char)(a*16+b);
        buffer[prefix_len+i/2] = ch;
    }

    return decode_len;
}

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
    signed char subtag = 0;
    bool has_subtag = false;

    enum KEY_TYPE key_type;

    // Detect key type
    switch(key_len) {
     case 10:
        subtag = key[9];
        has_subtag = true;
     case  9:
        tag = key[8];
        key_type = CHUNK;
        break;
     case 14:
        subtag = key[13];
        has_subtag = true;
     case 13:
        tag = key[12];
        key_type = CHUNK;
        break;
     default:
        {
            if(has_prefix((char *)key, key_len, RAWKEY_PREFIX_ACTOR)) {
                key_type = ACTOR;
            } else if(has_prefix((char *)key, key_len, RAWKEY_PREFIX_ACTOR_DIGEST_KEYS)) {
                key_type = ACTOR_DIGEST_KEYS;
            } else {
                key_type = PLAIN;
            }
            break;
        }
    }
    // validate data
    if(key_type == CHUNK) {
        if(tag < CHUNK_KEY_TAG_MIN) {
            key_type = PLAIN;
        } else if(tag > CHUNK_KEY_TAG_MAX && tag != CHUNK_KEY_LEGACY_VERSION_TAG) {
            key_type = PLAIN;
        } else if(subtag < CHUNK_KEY_SUBCHUNK_MIN || subtag > CHUNK_KEY_SUBCHUNK_MAX) {
            key_type = PLAIN;
        }
    } else if(key_type == ACTOR_DIGEST_KEYS) {
        size_t len = strlen(RAWKEY_PREFIX_ACTOR_DIGEST_KEYS);
        if(key_len != len+8 && key_len != len+12) {
            key_type = PLAIN;
        }
    } else if(key_type == ACTOR) {
        size_t len = strlen(RAWKEY_PREFIX_ACTOR);
        if(key_len != len+8) {
            key_type = PLAIN;
        }
    }

    // extract chunk_prefix coordinates
    if(key_type == CHUNK || key_type == ACTOR_DIGEST_KEYS) {
        const unsigned char * p = key;
        size_t len = key_len;
        if(key_type == ACTOR_DIGEST_KEYS) {
            p += strlen(RAWKEY_PREFIX_ACTOR_DIGEST_KEYS);
            len -= strlen(RAWKEY_PREFIX_ACTOR_DIGEST_KEYS);
        }
        memcpy(&x, p + 0, 4);
        memcpy(&z, p + 4, 4);
        if(len >= 12) {
            memcpy(&dimension, p + 8, 4);
            // validate dimension
            if(dimension > CHUNK_KEY_DIM_MAX) {
                key_type = PLAIN;
            }
        }
    }

    if(key_type == CHUNK && has_subtag) {
        return snprintf(buffer, buffer_len, CHRKEY_PREFIX_CHUNK "%d:%d:%u:%u:%d", x, z, dimension,
            (unsigned int)tag, (int)subtag);
    } else if(key_type == CHUNK) {
        return snprintf(buffer, buffer_len, CHRKEY_PREFIX_CHUNK "%d:%d:%u:%u", x, z, dimension,
            (unsigned int)tag);
    } else if(key_type == ACTOR_DIGEST_KEYS) {
        return snprintf(buffer, buffer_len,
                CHRKEY_PREFIX_ACTOR_DIGEST_KEYS "%d:%d:%u", x, z, dimension);
    } else if(key_type == ACTOR) {
        key = key + strlen(RAWKEY_PREFIX_ACTOR);
        size_t prefix_len = strlen(CHRKEY_PREFIX_ACTOR);
        if(prefix_len+2*8+1 < buffer_len) {
            memcpy(buffer, (char*)CHRKEY_PREFIX_ACTOR, prefix_len);
            buffer += prefix_len;
            for(int i=0; i < 8; ++i) {
                unsigned char ch = key[i];
                buffer[0] = encode_hex(ch >> 4);
                buffer[1] = encode_hex(ch & 0xF);
                buffer += 2;
            }
            buffer[0] = '\0';
        }
        return prefix_len+2*8;
    }
    // Plain keys
    size_t prefix_len = strlen(CHRKEY_PREFIX_PLAIN);
    if(prefix_len < buffer_len) {
        memcpy(buffer, CHRKEY_PREFIX_PLAIN, prefix_len);
        buffer += prefix_len;
        buffer_len -= prefix_len;
    } else {
        buffer_len = 0; // # nocov
    }
    return prefix_len + percent_encode(key, key_len, buffer, buffer_len);
}

// Take a VECSXP of raw, internal keys and covert them to human-readable keys.
SEXP rawkeys_to_chrkeys(SEXP r_keys) {
    // Maximum chunk data key size is 45 = 1+11+1+11+1+10+1+3+1+4+1
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
        SET_STRING_ELT(r_ret, i, Rf_mkCharLenCE(buffer, (int)key_len, CE_UTF8));
    }
    UNPROTECT(1);
    return r_ret;
}

// convert a human-readable key to a raw key
// Writes up to buffer_len characters into buffer.
// Returns the total length of the encoding, even if all
// characters were not written.
size_t chrkey_to_rawkey(const char *key, size_t key_len, unsigned char *buffer, size_t buffer_len) {
    if(key_len == 0) {
        return 0;
    }
    size_t ret = 0;
    enum KEY_TYPE key_type = PLAIN;
    if(has_prefix(key, key_len, CHRKEY_PREFIX_PLAIN)) {
        key += strlen(CHRKEY_PREFIX_PLAIN);
        key_len -= strlen(CHRKEY_PREFIX_PLAIN);
    } else if(has_prefix(key, key_len, CHRKEY_PREFIX_CHUNK)) {
        size_t len = strlen(CHRKEY_PREFIX_CHUNK);
        key_type = CHUNK;
        ret = chunkkey_decode(key+len, key_len-len, buffer, buffer_len);
    } else if(has_prefix(key, key_len, CHRKEY_PREFIX_ACTOR)) {
        size_t len = strlen(CHRKEY_PREFIX_ACTOR);
        key_type = ACTOR;
        ret = actorkey_decode(key+len, key_len-len, buffer, buffer_len);
    } else if(has_prefix(key, key_len, CHRKEY_PREFIX_ACTOR_DIGEST_KEYS)) {
        size_t len = strlen(CHRKEY_PREFIX_ACTOR_DIGEST_KEYS);
        key_type = ACTOR_DIGEST_KEYS;
        ret = digkey_decode(key+len, key_len-len, buffer, buffer_len);
    } else {
        Rf_warning("Unknown or missing prefix in key; assuming key is a plain key.");
    }

    // check to see if key has been decoded, otherwise assume plain key
    if(ret > 0) {
        return ret;
    }
    if(key_type == PLAIN) {
        /*noop*/;
    } else if(key_type == CHUNK) {
        Rf_warning("Invalid chunk key format; assuming key is a plain key.");
    } else if(key_type == ACTOR) {
        Rf_warning("Invalid actor key format; assuming key is a plain key.");
    } else if(key_type == ACTOR_DIGEST_KEYS) {
        Rf_warning("Invalid actor digest keys key format; assuming key is a plain key.");
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
