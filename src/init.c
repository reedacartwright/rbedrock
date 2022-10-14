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

#include "db.h"
#include "keys.h"
#include "nbt.h"
#include "chunk.h"
#include "random.h"

// for testing:
SEXP rbedrock_db_test_cleanup() {
    rbedrock_cleanup_db();
    rbedrock_init_db();
    return R_NilValue;
}

static const R_CallMethodDef call_methods[] = {
    {"rbedrock_db_open", (DL_FUNC)&rbedrock_db_open, 10},
    {"rbedrock_db_close", (DL_FUNC)&rbedrock_db_close, 2},
    {"rbedrock_db_destroy", (DL_FUNC)&rbedrock_db_destroy, 1},
    {"rbedrock_db_is_open", (DL_FUNC)&rbedrock_db_is_open, 1},
    {"rbedrock_db_repair", (DL_FUNC)&rbedrock_db_repair, 1},
    {"rbedrock_db_property", (DL_FUNC)&rbedrock_db_property, 3},

    {"rbedrock_db_get", (DL_FUNC)&rbedrock_db_get, 3},
    {"rbedrock_db_mget", (DL_FUNC)&rbedrock_db_mget, 3},
    {"rbedrock_db_mget_prefix", (DL_FUNC)&rbedrock_db_mget_prefix, 3},
    {"rbedrock_db_put", (DL_FUNC)&rbedrock_db_put, 4},
    {"rbedrock_db_mput", (DL_FUNC)&rbedrock_db_mput, 4},
    {"rbedrock_db_delete", (DL_FUNC)&rbedrock_db_delete, 5},

    {"rbedrock_db_iter_create", (DL_FUNC)&rbedrock_db_iter_create, 2},
    {"rbedrock_db_iter_destroy", (DL_FUNC)&rbedrock_db_iter_destroy,
     2},
    {"rbedrock_db_iter_valid", (DL_FUNC)&rbedrock_db_iter_valid, 1},
    {"rbedrock_db_iter_seek_to_first",
     (DL_FUNC)&rbedrock_db_iter_seek_to_first, 1},
    {"rbedrock_db_iter_seek_to_last",
     (DL_FUNC)&rbedrock_db_iter_seek_to_last, 1},
    {"rbedrock_db_iter_seek", (DL_FUNC)&rbedrock_db_iter_seek, 2},
    {"rbedrock_db_iter_next", (DL_FUNC)&rbedrock_db_iter_next, 2},
    {"rbedrock_db_iter_prev", (DL_FUNC)&rbedrock_db_iter_prev, 2},
    {"rbedrock_db_iter_key", (DL_FUNC)&rbedrock_db_iter_key, 2},
    {"rbedrock_db_iter_value", (DL_FUNC)&rbedrock_db_iter_value, 2},

    {"rbedrock_db_snapshot_create",
     (DL_FUNC)&rbedrock_db_snapshot_create, 1},

    {"rbedrock_db_writebatch_create",
     (DL_FUNC)&rbedrock_db_writebatch_create, 0},
    {"rbedrock_db_writebatch_destroy",
     (DL_FUNC)&rbedrock_db_writebatch_destroy, 2},
    {"rbedrock_db_writebatch_clear",
     (DL_FUNC)&rbedrock_db_writebatch_clear, 1},
    {"rbedrock_db_writebatch_put",
     (DL_FUNC)&rbedrock_db_writebatch_put, 3},
    {"rbedrock_db_writebatch_mput",
     (DL_FUNC)&rbedrock_db_writebatch_mput, 3},
    {"rbedrock_db_writebatch_delete",
     (DL_FUNC)&rbedrock_db_writebatch_delete, 2},
    {"rbedrock_db_writebatch_mdelete",
     (DL_FUNC)&rbedrock_db_writebatch_mdelete, 2},
    {"rbedrock_db_write", (DL_FUNC)&rbedrock_db_write, 3},

    {"rbedrock_db_approximate_sizes",
     (DL_FUNC)&rbedrock_db_approximate_sizes, 3},
    {"rbedrock_db_compact_range", (DL_FUNC)&rbedrock_db_compact_range,
     3},

    {"rbedrock_db_readoptions", (DL_FUNC)&rbedrock_db_readoptions, 3},
    {"rbedrock_db_writeoptions", (DL_FUNC)&rbedrock_db_writeoptions,
     1},

    {"rbedrock_db_keys_len", (DL_FUNC)&rbedrock_db_keys_len, 3},
    {"rbedrock_db_keys", (DL_FUNC)&rbedrock_db_keys, 3},
    {"rbedrock_db_exists", (DL_FUNC)&rbedrock_db_exists, 3},
    {"rbedrock_db_version", (DL_FUNC)&rbedrock_db_version, 0},

    // For debugging:
    {"rbedrock_db_tag", (DL_FUNC)&rbedrock_db_tag, 1},

    // For testing:
    {"rbedrock_db_test_cleanup", (DL_FUNC)&rbedrock_db_test_cleanup,
     0},

    {"rbedrock_keys_raw_to_hum", (DL_FUNC)&rbedrock_keys_raw_to_hum, 1},
    {"rbedrock_keys_hum_to_raw", (DL_FUNC)&rbedrock_keys_hum_to_raw, 1},

    {"rbedrock_nbt_read", (DL_FUNC)&rbedrock_nbt_read, 1},
    {"rbedrock_nbt_write", (DL_FUNC)&rbedrock_nbt_write, 1},

    {"rbedrock_chunk_read_subchunk", (DL_FUNC)&rbedrock_chunk_read_subchunk, 1},
    {"rbedrock_chunk_write_subchunk", (DL_FUNC)&rbedrock_chunk_write_subchunk, 4},
    {"rbedrock_chunk_read_biomes", (DL_FUNC)&rbedrock_chunk_read_biomes, 1},
    {"rbedrock_chunk_write_biomes", (DL_FUNC)&rbedrock_chunk_write_biomes, 2},

    {"rbedrock_random_seed", (DL_FUNC)&rbedrock_random_seed, 1},
    {"rbedrock_random_state", (DL_FUNC)&rbedrock_random_state, 1},
    {"rbedrock_random_get_uint", (DL_FUNC)&rbedrock_random_get_uint, 2},
    {"rbedrock_random_get_int", (DL_FUNC)&rbedrock_random_get_int, 3},
    {"rbedrock_random_get_double", (DL_FUNC)&rbedrock_random_get_double, 1},
    {"rbedrock_random_get_float", (DL_FUNC)&rbedrock_random_get_float, 3},

    {"rbedrock_random_create_seed", (DL_FUNC)&rbedrock_random_create_seed, 6},

    {NULL, NULL, 0}};

void rbedrock_init_nbt();
void rbedrock_init_chunk();
void rbedrock_init_random();

void attribute_visible R_init_rbedrock(DllInfo *info) {
    R_registerRoutines(info, NULL, call_methods, NULL, NULL);
    R_useDynamicSymbols(info, FALSE);
    R_forceSymbols(info, TRUE);

    rbedrock_init_db();

    rbedrock_init_nbt();
    rbedrock_init_chunk();
    rbedrock_init_random();
}

// This can't be easily tested
// # nocov start
void R_unload_rbedrock(DllInfo *info) {
    rbedrock_cleanup_db();
}
// # nocov end
