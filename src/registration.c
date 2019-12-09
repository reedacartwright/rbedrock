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


#include "rleveldb.h"
#include <R_ext/Rdynload.h>
#include <Rversion.h>

// for testing:
SEXP rleveldb_test_cleanup() {
  rleveldb_cleanup();
  rleveldb_init();
  return R_NilValue;
}

static const R_CallMethodDef call_methods[] = {
  {"Crleveldb_open",               (DL_FUNC) &rleveldb_open,              10},
  {"Crleveldb_close",              (DL_FUNC) &rleveldb_close,              2},
  {"Crleveldb_destroy",            (DL_FUNC) &rleveldb_destroy,            1},
  {"Crleveldb_repair",             (DL_FUNC) &rleveldb_repair,             1},
  {"Crleveldb_property",           (DL_FUNC) &rleveldb_property,           3},

  {"Crleveldb_get",                (DL_FUNC) &rleveldb_get,                5},
  {"Crleveldb_mget",               (DL_FUNC) &rleveldb_mget,               6},
  {"Crleveldb_put",                (DL_FUNC) &rleveldb_put,                4},
  {"Crleveldb_mput",               (DL_FUNC) &rleveldb_mput,               4},
  {"Crleveldb_delete",             (DL_FUNC) &rleveldb_delete,             5},

  {"Crleveldb_iter_create",        (DL_FUNC) &rleveldb_iter_create,        2},
  {"Crleveldb_iter_destroy",       (DL_FUNC) &rleveldb_iter_destroy,       2},
  {"Crleveldb_iter_valid",         (DL_FUNC) &rleveldb_iter_valid,         1},
  {"Crleveldb_iter_seek_to_first", (DL_FUNC) &rleveldb_iter_seek_to_first, 1},
  {"Crleveldb_iter_seek_to_last",  (DL_FUNC) &rleveldb_iter_seek_to_last,  1},
  {"Crleveldb_iter_seek",          (DL_FUNC) &rleveldb_iter_seek,          2},
  {"Crleveldb_iter_next",          (DL_FUNC) &rleveldb_iter_next,          2},
  {"Crleveldb_iter_prev",          (DL_FUNC) &rleveldb_iter_prev,          2},
  {"Crleveldb_iter_key",           (DL_FUNC) &rleveldb_iter_key,           3},
  {"Crleveldb_iter_value",         (DL_FUNC) &rleveldb_iter_value,         3},

  {"Crleveldb_snapshot_create",    (DL_FUNC) &rleveldb_snapshot_create,    1},

  {"Crleveldb_writebatch_create",  (DL_FUNC) &rleveldb_writebatch_create,  0},
  {"Crleveldb_writebatch_destroy", (DL_FUNC) &rleveldb_writebatch_destroy, 2},
  {"Crleveldb_writebatch_clear",   (DL_FUNC) &rleveldb_writebatch_clear,   1},
  {"Crleveldb_writebatch_put",     (DL_FUNC) &rleveldb_writebatch_put,     3},
  {"Crleveldb_writebatch_mput",    (DL_FUNC) &rleveldb_writebatch_mput,    3},
  {"Crleveldb_writebatch_delete",  (DL_FUNC) &rleveldb_writebatch_delete,  2},
  {"Crleveldb_write",              (DL_FUNC) &rleveldb_write,              3},

  {"Crleveldb_approximate_sizes",  (DL_FUNC) &rleveldb_approximate_sizes,  3},
  {"Crleveldb_compact_range",      (DL_FUNC) &rleveldb_compact_range,      3},

  {"Crleveldb_readoptions",        (DL_FUNC) &rleveldb_readoptions,        3},
  {"Crleveldb_writeoptions",       (DL_FUNC) &rleveldb_writeoptions,       1},

  {"Crleveldb_keys_len",           (DL_FUNC) &rleveldb_keys_len,           3},
  {"Crleveldb_keys",               (DL_FUNC) &rleveldb_keys,               4},
  {"Crleveldb_exists",             (DL_FUNC) &rleveldb_exists,             3},
  {"Crleveldb_version",            (DL_FUNC) &rleveldb_version,            0},

  // For debugging:
  {"Crleveldb_tag",                (DL_FUNC) &rleveldb_tag,                1},

  // For testing:
  {"Crleveldb_test_cleanup",       (DL_FUNC) &rleveldb_test_cleanup,       0},

  {NULL,                           NULL,                                   0}
};

void R_init_rbedrock(DllInfo *info) {
  rleveldb_init();
  R_registerRoutines(info, NULL, call_methods, NULL, NULL);
#if defined(R_VERSION) && R_VERSION >= R_Version(3, 3, 0)
  R_useDynamicSymbols(info, FALSE);
  R_forceSymbols(info, TRUE);
#endif
}

// This can't be easily tested
// # nocov start
void R_unload_rbedrock(DllInfo *info) {
  rleveldb_cleanup();
}
// # nocov end
