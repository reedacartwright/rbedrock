// Copyright (c) 2016, Richard G. FitzJohn

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

#include <R_ext/Rdynload.h>
#include <R_ext/Visibility.h>
#include <Rversion.h>
#include <Rinternals.h>

#include "bedrock_leveldb.h"
#include "key_conv.h"
#include "nbt.h"
#include "subchunk.h"
#include "random.h"
#include "actors.h"

// for testing:
SEXP bedrock_leveldb_test_cleanup(void) {
    bedrock_leveldb_cleanup();
    bedrock_leveldb_init();
    return R_NilValue;
}

void rbedrock_init_nbt(void);
void rbedrock_init_blocks(void);
void rbedrock_init_random(void);

void attribute_visible R_init_rbedrock(DllInfo *info) {
    R_registerRoutines(info, NULL, NULL, NULL, NULL);
    R_useDynamicSymbols(info, TRUE);
    R_forceSymbols(info, TRUE);

    bedrock_leveldb_init();

    rbedrock_init_nbt();
    rbedrock_init_blocks();
    rbedrock_init_random();
}

// This can't be easily tested
// # nocov start
void R_unload_rbedrock(DllInfo *info) {
    bedrock_leveldb_cleanup();
}
// # nocov end
