// Copyright (c) 2016 Richard G. FitzJohn
// Copyright (c) 2020,2021 Reed A. Cartwright

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

SEXP bedrock_leveldb_open(SEXP r_path, SEXP r_create_if_missing,
                          SEXP r_error_if_exists, SEXP r_paranoid_checks,
                          SEXP r_write_buffer_size, SEXP r_max_open_files,
                          SEXP r_block_size, SEXP r_cache_capacity,
                          SEXP r_bloom_filter_bits_per_key,
                          SEXP r_compression_level);
SEXP bedrock_leveldb_close(SEXP r_db, SEXP r_error_if_closed);
SEXP bedrock_leveldb_destroy(SEXP r_path);
SEXP bedrock_leveldb_is_open(SEXP r_db);
SEXP bedrock_leveldb_repair(SEXP r_path);
SEXP bedrock_leveldb_property(SEXP r_db, SEXP r_name, SEXP r_error_if_missing);

SEXP bedrock_leveldb_get(SEXP r_db, SEXP r_key, SEXP r_readoptions);
SEXP bedrock_leveldb_mget(SEXP r_db, SEXP r_keys, SEXP r_readoptions);
SEXP bedrock_leveldb_mget_prefix(SEXP r_db, SEXP r_starts_with,
                          SEXP r_readoptions);

SEXP bedrock_leveldb_put(SEXP r_db, SEXP r_key, SEXP r_value,
                         SEXP r_writeoptions);
SEXP bedrock_leveldb_mput(SEXP r_db, SEXP r_key, SEXP r_value,
                          SEXP r_writeoptions);

SEXP bedrock_leveldb_delete(SEXP r_db, SEXP r_key, SEXP r_report,
                            SEXP r_readoptions, SEXP r_writeoptions);
SEXP bedrock_leveldb_delete_silent(SEXP r_db, SEXP r_key, SEXP r_writeoptions);
SEXP bedrock_leveldb_delete_report(SEXP r_db, SEXP r_key, SEXP r_readoptions,
                                   SEXP r_writeoptions);

SEXP bedrock_leveldb_iter_create(SEXP r_db, SEXP r_readoptions);
SEXP bedrock_leveldb_iter_destroy(SEXP r_it, SEXP r_error_if_destroyed);
SEXP bedrock_leveldb_iter_valid(SEXP r_it);
SEXP bedrock_leveldb_iter_seek_to_first(SEXP r_it);
SEXP bedrock_leveldb_iter_seek_to_last(SEXP r_it);
SEXP bedrock_leveldb_iter_seek(SEXP r_it, SEXP r_key);
SEXP bedrock_leveldb_iter_next(SEXP r_it, SEXP r_error_if_invalid);
SEXP bedrock_leveldb_iter_prev(SEXP r_it, SEXP r_error_if_invalid);
SEXP bedrock_leveldb_iter_key(SEXP r_it,
                              SEXP r_error_if_invalid);
SEXP bedrock_leveldb_iter_value(SEXP r_it,
                                SEXP r_error_if_invalid);

SEXP bedrock_leveldb_snapshot_create(SEXP r_db);

SEXP bedrock_leveldb_writebatch_create(void);
SEXP bedrock_leveldb_writebatch_destroy(SEXP r_writebatch,
                                        SEXP error_if_destroyed);
SEXP bedrock_leveldb_writebatch_clear(SEXP r_writebatch);
SEXP bedrock_leveldb_writebatch_put(SEXP r_writebatch, SEXP r_key,
                                    SEXP r_value);
SEXP bedrock_leveldb_writebatch_mput(SEXP r_writebatch, SEXP r_key,
                                     SEXP r_value);
SEXP bedrock_leveldb_writebatch_delete(SEXP r_writebatch, SEXP r_key);
SEXP bedrock_leveldb_writebatch_mdelete(SEXP r_writebatch, SEXP r_keys);
SEXP bedrock_leveldb_write(SEXP r_db, SEXP r_writebatch, SEXP r_writeoptions);

SEXP bedrock_leveldb_approximate_sizes(SEXP r_db, SEXP r_start_key,
                                       SEXP r_limit_key);
SEXP bedrock_leveldb_compact_range(SEXP r_db, SEXP r_start_key,
                                   SEXP r_limit_key);

SEXP bedrock_leveldb_readoptions(SEXP r_verify_checksums, SEXP r_fill_cache,
                                 SEXP r_snapshot);
SEXP bedrock_leveldb_writeoptions(SEXP r_sync);

SEXP bedrock_leveldb_keys(SEXP r_db, SEXP r_starts_with,
                          SEXP r_readoptions);
SEXP bedrock_leveldb_keys_len(SEXP r_db, SEXP r_starts_with,
                              SEXP r_readoptions);
SEXP bedrock_leveldb_exists(SEXP r_db, SEXP r_key, SEXP r_readoptions);
SEXP bedrock_leveldb_version(void);
SEXP bedrock_leveldb_tag(SEXP r_db);
void bedrock_leveldb_init(void);
void bedrock_leveldb_cleanup(void);
