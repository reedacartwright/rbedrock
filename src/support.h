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

#include <R.h>
#include <Rinternals.h>
#include <stdbool.h>

size_t get_key(SEXP key, const char **key_data);
size_t get_key_maybe_nil(SEXP key, const char **key_data);
size_t get_value(SEXP value, const char **value_data);
size_t get_keys(SEXP keys, const char ***key_data, size_t **key_len);
size_t get_starts_with(SEXP starts_with, const char **starts_with_data);

SEXP raw_string_to_sexp(const char *str, size_t len);
bool scalar_logical(SEXP x);
size_t scalar_size(SEXP x);
int scalar_int(SEXP x);
const char *scalar_character(SEXP x);

SEXP create_stretchy_list();
void grow_stretchy_list(SEXP l, SEXP s);
SEXP create_stretchy_list_with_name(SEXP s, SEXP tag);
void grow_stretchy_list_with_name(SEXP l, SEXP s, SEXP tag);
