// Copyright (c) 2016 Richard G. FitzJohn
// Copyright (c) 2021 Reed A. Cartwright

// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions are
// met:

//     Redistributions of source code must retain the above copyright
//     notice, this list of conditions and the following disclaimer.

//     Redistributions in binary form must reproduce the above copyright
//     notice, this list of conditions and the following disclaimer in
//     the documentation and/or other materials provided with the
//     distribution.

// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
// "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
// LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
// A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
// HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
// SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
// LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
// DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
// THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
// (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
// OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

#include "support.h"

size_t get_data(SEXP data, const char **data_contents, const char *name);
size_t get_keys_len(SEXP keys);
void get_keys_data(size_t len, SEXP keys, const char **data, size_t *data_len);

size_t get_key(SEXP key, const char **key_data) {
    return get_data(key, key_data, "key");
}

size_t get_key_maybe_nil(SEXP key, const char **key_data) {
    if(TYPEOF(key) == NILSXP) {
        *key_data = NULL;
        return 0;
    }
    return get_key(key, key_data);
}

size_t get_value(SEXP value, const char **value_data) {
    return get_data(value, value_data, "data");
}

size_t get_keys(SEXP keys, const char ***key_data, size_t **key_len) {
    size_t len = get_keys_len(keys);
    *key_data = (const char **)R_alloc(len, sizeof(const char *));
    *key_len = (size_t *)R_alloc(len, sizeof(size_t));
    get_keys_data(len, keys, *key_data, *key_len);
    return len;
}

size_t get_starts_with(SEXP starts_with, const char **starts_with_data) {
    if(Rf_isNull(starts_with)) {
        *starts_with_data = NULL;
        return 0;
    } else {
        return get_data(starts_with, starts_with_data, "starts_with");
    }
}

size_t get_data(SEXP data, const char **data_contents, const char *name) {
    if(TYPEOF(data) != RAWSXP) {
        Rf_error("Invalid data type for %s; expected raw", name);        
    }
    *data_contents = (const char *)RAW(data);
    return length(data);
}

size_t get_keys_len(SEXP keys) {
    return TYPEOF(keys) == RAWSXP ? 1 : (size_t)length(keys);
}

void get_keys_data(size_t len, SEXP keys, const char **data, size_t *data_len) {
    if(TYPEOF(keys) == RAWSXP) {
        data[0] = (char *)RAW(keys);
        data_len[0] = length(keys);
    } else if(TYPEOF(keys) == VECSXP) {
        for(size_t i = 0; i < len; ++i) {
            data_len[i] = get_key(VECTOR_ELT(keys, i), data + i);
        }
    } else {
        Rf_error("Invalid type; expected a raw vector");
    }
}

// This is the same strategy as redux.
SEXP raw_string_to_sexp(const char *str, size_t len) {
    SEXP ret;
    ret = PROTECT(allocVector(RAWSXP, len));
    memcpy(RAW(ret), str, len);
    UNPROTECT(1);
    return ret;
}

// Same as from ring
bool scalar_logical(SEXP x) {
    if(TYPEOF(x) == LGLSXP && LENGTH(x) == 1) {
        int ret = INTEGER(x)[0];
        if(ret == NA_LOGICAL) {
            Rf_error("Expected a non-missing logical scalar");
        }
        return (bool)(ret);
    } else {
        Rf_error("Expected a logical scalar");
        return 0;
    }
}

size_t scalar_size(SEXP x) {
    int len = LENGTH(x);
    int value = 0;
    if(len == 1) {
        if(TYPEOF(x) == INTSXP) {
            value = INTEGER(x)[0];
            if(value == NA_INTEGER) {
                Rf_error("Expected a non-missing (& finite) size");
            }
        } else if(TYPEOF(x) == REALSXP) {
            double rvalue = REAL(x)[0];
            if(!R_FINITE(rvalue)) {
                Rf_error("Expected a non-missing (& finite) size");
            }
            value = (int)rvalue;
        } else {
            Rf_error("Expected a scalar size");
        }
        if(value < 0) {
            Rf_error("Expected a positive size");
        }
    } else {
        Rf_error("Expected a scalar size");
    }
    return (size_t)value;
}

const char *scalar_character(SEXP x) {
    if(LENGTH(x) == 1 && TYPEOF(x) == STRSXP) {
        return CHAR(STRING_ELT(x, 0));
    } else {
        Rf_error("Expected a scalar string");
        return NULL;
    }
}

int scalar_int(SEXP x) {
    int len = LENGTH(x);
    int value = 0;
    if(len == 1) {
        if(TYPEOF(x) == INTSXP) {
            value = INTEGER(x)[0];
            if(value == NA_INTEGER) {
                Rf_error("Expected a non-missing integer.");
            }
        } else if(TYPEOF(x) == REALSXP) {
            double rvalue = REAL(x)[0];
            if(!R_FINITE(rvalue)) {
                Rf_error("Expected a non-missing integer.");
            }
            value = (int)rvalue;
        } else {
            Rf_error("Expected a scalar integer.");
        }
    } else {
        Rf_error("Expected a scalar integer.");
    }
    return value;
}
