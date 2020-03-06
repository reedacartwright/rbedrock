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


#include "bedrock_leveldb.h"
#include <R_ext/Rdynload.h>
#include <Rversion.h>

// for testing:
SEXP bedrock_leveldb_test_cleanup() {
  bedrock_leveldb_cleanup();
  bedrock_leveldb_init();
  return R_NilValue;
}

static const R_CallMethodDef call_methods[] = {
  {"Cbedrock_leveldb_open",               (DL_FUNC) &bedrock_leveldb_open,              10},
  {"Cbedrock_leveldb_close",              (DL_FUNC) &bedrock_leveldb_close,              2},
  {"Cbedrock_leveldb_destroy",            (DL_FUNC) &bedrock_leveldb_destroy,            1},
  {"Cbedrock_leveldb_repair",             (DL_FUNC) &bedrock_leveldb_repair,             1},
  {"Cbedrock_leveldb_property",           (DL_FUNC) &bedrock_leveldb_property,           3},

  {"Cbedrock_leveldb_get",                (DL_FUNC) &bedrock_leveldb_get,                5},
  {"Cbedrock_leveldb_mget",               (DL_FUNC) &bedrock_leveldb_mget,               6},
  {"Cbedrock_leveldb_put",                (DL_FUNC) &bedrock_leveldb_put,                4},
  {"Cbedrock_leveldb_mput",               (DL_FUNC) &bedrock_leveldb_mput,               4},
  {"Cbedrock_leveldb_delete",             (DL_FUNC) &bedrock_leveldb_delete,             5},

  {"Cbedrock_leveldb_iter_create",        (DL_FUNC) &bedrock_leveldb_iter_create,        2},
  {"Cbedrock_leveldb_iter_destroy",       (DL_FUNC) &bedrock_leveldb_iter_destroy,       2},
  {"Cbedrock_leveldb_iter_valid",         (DL_FUNC) &bedrock_leveldb_iter_valid,         1},
  {"Cbedrock_leveldb_iter_seek_to_first", (DL_FUNC) &bedrock_leveldb_iter_seek_to_first, 1},
  {"Cbedrock_leveldb_iter_seek_to_last",  (DL_FUNC) &bedrock_leveldb_iter_seek_to_last,  1},
  {"Cbedrock_leveldb_iter_seek",          (DL_FUNC) &bedrock_leveldb_iter_seek,          2},
  {"Cbedrock_leveldb_iter_next",          (DL_FUNC) &bedrock_leveldb_iter_next,          2},
  {"Cbedrock_leveldb_iter_prev",          (DL_FUNC) &bedrock_leveldb_iter_prev,          2},
  {"Cbedrock_leveldb_iter_key",           (DL_FUNC) &bedrock_leveldb_iter_key,           3},
  {"Cbedrock_leveldb_iter_value",         (DL_FUNC) &bedrock_leveldb_iter_value,         3},

  {"Cbedrock_leveldb_snapshot_create",    (DL_FUNC) &bedrock_leveldb_snapshot_create,    1},

  {"Cbedrock_leveldb_writebatch_create",  (DL_FUNC) &bedrock_leveldb_writebatch_create,  0},
  {"Cbedrock_leveldb_writebatch_destroy", (DL_FUNC) &bedrock_leveldb_writebatch_destroy, 2},
  {"Cbedrock_leveldb_writebatch_clear",   (DL_FUNC) &bedrock_leveldb_writebatch_clear,   1},
  {"Cbedrock_leveldb_writebatch_put",     (DL_FUNC) &bedrock_leveldb_writebatch_put,     3},
  {"Cbedrock_leveldb_writebatch_mput",    (DL_FUNC) &bedrock_leveldb_writebatch_mput,    3},
  {"Cbedrock_leveldb_writebatch_delete",  (DL_FUNC) &bedrock_leveldb_writebatch_delete,  2},
  {"Cbedrock_leveldb_write",              (DL_FUNC) &bedrock_leveldb_write,              3},

  {"Cbedrock_leveldb_approximate_sizes",  (DL_FUNC) &bedrock_leveldb_approximate_sizes,  3},
  {"Cbedrock_leveldb_compact_range",      (DL_FUNC) &bedrock_leveldb_compact_range,      3},

  {"Cbedrock_leveldb_readoptions",        (DL_FUNC) &bedrock_leveldb_readoptions,        3},
  {"Cbedrock_leveldb_writeoptions",       (DL_FUNC) &bedrock_leveldb_writeoptions,       1},

  {"Cbedrock_leveldb_keys_len",           (DL_FUNC) &bedrock_leveldb_keys_len,           3},
  {"Cbedrock_leveldb_keys",               (DL_FUNC) &bedrock_leveldb_keys,               4},
  {"Cbedrock_leveldb_strkeys",            (DL_FUNC) &bedrock_leveldb_strkeys,            2},
  {"Cbedrock_leveldb_exists",             (DL_FUNC) &bedrock_leveldb_exists,             3},
  {"Cbedrock_leveldb_version",            (DL_FUNC) &bedrock_leveldb_version,            0},

  // For debugging:
  {"Cbedrock_leveldb_tag",                (DL_FUNC) &bedrock_leveldb_tag,                1},

  // For testing:
  {"Cbedrock_leveldb_test_cleanup",       (DL_FUNC) &bedrock_leveldb_test_cleanup,       0},

  {NULL,                           NULL,                                   0}
};

void R_init_rbedrock(DllInfo *info) {
  bedrock_leveldb_init();
  R_registerRoutines(info, NULL, call_methods, NULL, NULL);
#if defined(R_VERSION) && R_VERSION >= R_Version(3, 3, 0)
  R_useDynamicSymbols(info, FALSE);
  R_forceSymbols(info, TRUE);
#endif
}

// This can't be easily tested
// # nocov start
void R_unload_rbedrock(DllInfo *info) {
  bedrock_leveldb_cleanup();
}
// # nocov end
