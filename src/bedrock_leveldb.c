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

#define R_NO_REMAP
#include "bedrock_leveldb.h"

#include <R_ext/Visibility.h>
#include <leveldb/c.h>
#include <stdbool.h>

#include "key_conv.h"
#include "support.h"

leveldb_readoptions_t *default_readoptions;
leveldb_writeoptions_t *default_writeoptions;
// Internals:
leveldb_t *bedrock_leveldb_get_db(SEXP r_db, bool closed_error);
leveldb_iterator_t *bedrock_leveldb_get_iterator(SEXP r_it, bool closed_error);
leveldb_snapshot_t *bedrock_leveldb_get_snapshot(SEXP r_snapshot,
                                                 bool closed_error);
leveldb_writebatch_t *bedrock_leveldb_get_writebatch(SEXP r_writebatch,
                                                     bool closed_error);
leveldb_readoptions_t *bedrock_leveldb_get_readoptions(SEXP r_readoptions,
                                                       bool closed_error);
leveldb_writeoptions_t *bedrock_leveldb_get_writeoptions(SEXP r_writeoptions,
                                                         bool closed_error);
bool check_iterator(leveldb_iterator_t *it, SEXP r_error_if_invalid);

// Finalizers
static void bedrock_leveldb_finalize(SEXP r_db);
static void bedrock_leveldb_iter_finalize(SEXP r_it);
static void bedrock_leveldb_snapshot_finalize(SEXP r_snapshot);
static void bedrock_leveldb_writebatch_finalize(SEXP r_writebatch);
static void bedrock_leveldb_readoptions_finalize(SEXP r_readoptions);
static void bedrock_leveldb_writeoptions_finalize(SEXP r_writeoptions);
static void bedrock_leveldb_cache_finalize(SEXP r_cache);
static void bedrock_leveldb_filterpolicy_finalize(SEXP r_filterpolicy);
static void bedrock_leveldb_compressor_finalize(SEXP r_compressor);

// Other internals
void bedrock_leveldb_handle_error(char *err);
leveldb_options_t *bedrock_leveldb_collect_options(
    SEXP r_create_if_missing, SEXP r_error_if_exists, SEXP r_paranoid_checks,
    SEXP r_write_buffer_size, SEXP r_max_open_files, SEXP r_block_size);

bool iter_key_starts_with(leveldb_iterator_t *it, const char *starts_with,
                          R_len_t starts_with_len);

// Slightly different
R_len_t bedrock_leveldb_get_keys_len(leveldb_t *db, const char *starts_with,
                                     R_len_t starts_with_len,
                                     leveldb_readoptions_t *readoptions);
void bedrock_leveldb_get_exists(leveldb_t *db, R_len_t num_key,
                                const char **key_data, R_len_t *key_len,
                                leveldb_readoptions_t *readoptions, int *found);

enum bedrock_leveldb_tag_index {
    TAG_PATH,
    TAG_CACHE,
    TAG_FILTERPOLICY,
    TAG_ITERATORS,
    TAG_COMPRESSOR0,
    TAG_COMPRESSOR1,
    TAG_LENGTH  // don't store anything here!
};

// Implementations:
SEXP attribute_visible R_bedrock_leveldb_open(
    SEXP r_path, SEXP r_create_if_missing, SEXP r_error_if_exists,
    SEXP r_paranoid_checks, SEXP r_write_buffer_size, SEXP r_max_open_files,
    SEXP r_block_size, SEXP r_cache_capacity, SEXP r_bloom_filter_bits_per_key,
    SEXP r_compression_level) {
    // Unimplemented options:
    // * a general set_filter_policy
    // * set_env
    // * set_info_log
    // * set_comparator
    // * restart_interval
    //
    // There is some gymnastics here to avoid leaking in the case of an
    // R error (perhaps thrown by the coersion functions).
    SEXP r_cache_ptr = R_NilValue;
    SEXP r_filterpolicy_ptr = R_NilValue;
    SEXP r_compressor_0 = R_NilValue;
    SEXP r_compressor_1 = R_NilValue;
    leveldb_cache_t *cache = NULL;
    leveldb_filterpolicy_t *filterpolicy = NULL;
    leveldb_compressor_t *compressor_0 = NULL;
    leveldb_compressor_t *compressor_1 = NULL;

    bool has_cache = !Rf_isNull(r_cache_capacity);
    bool has_filterpolicy = !Rf_isNull(r_bloom_filter_bits_per_key);
    if(has_cache) {
        cache = leveldb_cache_create_lru(scalar_size(r_cache_capacity));
        r_cache_ptr = PROTECT(R_MakeExternalPtr(cache, R_NilValue, R_NilValue));
        R_RegisterCFinalizer(r_cache_ptr, bedrock_leveldb_cache_finalize);
    }
    if(has_filterpolicy) {
        size_t bits_per_key = scalar_size(r_bloom_filter_bits_per_key);
        filterpolicy = leveldb_filterpolicy_create_bloom((int)bits_per_key);
        r_filterpolicy_ptr =
            PROTECT(R_MakeExternalPtr(filterpolicy, R_NilValue, R_NilValue));
        R_RegisterCFinalizer(r_filterpolicy_ptr,
                             bedrock_leveldb_filterpolicy_finalize);
    }
    const char *path = scalar_character(r_path);
    leveldb_options_t *options = bedrock_leveldb_collect_options(
        r_create_if_missing, r_error_if_exists, r_paranoid_checks,
        r_write_buffer_size, r_max_open_files, r_block_size);
    if(has_cache) {
        leveldb_options_set_cache(options, cache);
    }
    if(has_filterpolicy) {
        leveldb_options_set_filter_policy(options, filterpolicy);
    }

    int compression_level = -1;
    if(!Rf_isNull(r_compression_level)) {
        compression_level = scalar_int(r_compression_level);
    }
    compressor_0 = leveldb_compressor_create(leveldb_zlib_raw_compression,
                                             compression_level);
    r_compressor_0 =
        PROTECT(R_MakeExternalPtr(compressor_0, R_NilValue, R_NilValue));
    R_RegisterCFinalizer(r_compressor_0, bedrock_leveldb_compressor_finalize);
    compressor_1 =
        leveldb_compressor_create(leveldb_zlib_compression, compression_level);
    r_compressor_1 =
        PROTECT(R_MakeExternalPtr(compressor_1, R_NilValue, R_NilValue));
    R_RegisterCFinalizer(r_compressor_1, bedrock_leveldb_compressor_finalize);

    leveldb_options_set_compressor(options, 0, compressor_0);
    leveldb_options_set_compressor(options, 1, compressor_1);

    char *err = NULL;
    leveldb_t *db = leveldb_open(options, path, &err);
    leveldb_options_destroy(options);
    bedrock_leveldb_handle_error(err);

    SEXP tag = PROTECT(Rf_allocVector(VECSXP, TAG_LENGTH));
    SET_VECTOR_ELT(tag, TAG_PATH, r_path);
    SET_VECTOR_ELT(tag, TAG_CACHE, r_cache_ptr);
    SET_VECTOR_ELT(tag, TAG_FILTERPOLICY, r_filterpolicy_ptr);
    SET_VECTOR_ELT(tag, TAG_ITERATORS, R_NilValue);  // will be a pairlist
    SET_VECTOR_ELT(tag, TAG_COMPRESSOR0, r_compressor_0);
    SET_VECTOR_ELT(tag, TAG_COMPRESSOR1, r_compressor_1);

    SEXP r_db = PROTECT(R_MakeExternalPtr(db, tag, R_NilValue));
    R_RegisterCFinalizer(r_db, bedrock_leveldb_finalize);
    UNPROTECT(4 + has_cache + has_filterpolicy);
    return r_db;
}

// TODO: this needs to happen during finalize too!
SEXP attribute_visible R_bedrock_leveldb_close(SEXP r_db,
                                               SEXP r_error_if_closed) {
    leveldb_t *db =
        bedrock_leveldb_get_db(r_db, scalar_logical(r_error_if_closed));
    if(db != NULL) {
        SEXP tag = bedrock_leveldb_tag(r_db);
        SEXP r_iterators = VECTOR_ELT(tag, TAG_ITERATORS);
        while(!Rf_isNull(r_iterators)) {
            R_bedrock_leveldb_iter_destroy(CAR(r_iterators),
                                           Rf_ScalarLogical(false));
            r_iterators = CDR(r_iterators);
        }
        leveldb_close(db);
        R_ClearExternalPtr(r_db);
    }
    return Rf_ScalarLogical(db != NULL);
}

SEXP attribute_visible R_bedrock_leveldb_destroy(SEXP r_path) {
    const char *path = scalar_character(r_path);
    leveldb_options_t *options = leveldb_options_create();
    char *err = NULL;
    leveldb_destroy_db(options, path, &err);
    leveldb_options_destroy(options);
    bedrock_leveldb_handle_error(err);
    return Rf_ScalarLogical(true);
}

SEXP attribute_visible R_bedrock_leveldb_is_open(SEXP r_db) {
    return Rf_ScalarLogical(bedrock_leveldb_get_db(r_db, false) != NULL);
}

SEXP attribute_visible R_bedrock_leveldb_repair(SEXP r_path) {
    const char *path = scalar_character(r_path);
    leveldb_options_t *options = leveldb_options_create();
    char *err = NULL;
    leveldb_repair_db(options, path, &err);
    leveldb_options_destroy(options);
    bedrock_leveldb_handle_error(err);
    return Rf_ScalarLogical(true);
}

SEXP attribute_visible R_bedrock_leveldb_property(SEXP r_db, SEXP r_name,
                                                  SEXP r_error_if_missing) {
    leveldb_t *db = bedrock_leveldb_get_db(r_db, true);
    const char *name = scalar_character(r_name);
    bool error_if_missing = scalar_logical(r_error_if_missing);
    char *value = leveldb_property_value(db, name);
    SEXP ret;
    if(value != NULL) {
        ret = Rf_mkString(value);
        leveldb_free(value);
    } else if(error_if_missing) {
        Rf_error("No such property '%s'", name);
    } else {
        ret = R_NilValue;
    }
    return ret;
}

SEXP attribute_visible R_bedrock_leveldb_get(SEXP r_db, SEXP r_key,
                                             SEXP r_readoptions) {
    leveldb_t *db = bedrock_leveldb_get_db(r_db, true);
    const char *key_data = NULL;
    R_len_t key_len = get_key(r_key, &key_data);

    leveldb_readoptions_t *readoptions =
        bedrock_leveldb_get_readoptions(r_readoptions, true);

    char *err = NULL;
    size_t read_len;
    char *read =
        leveldb_get(db, readoptions, key_data, key_len, &read_len, &err);
    bedrock_leveldb_handle_error(err);

    SEXP ret;
    if(read != NULL) {
        ret = raw_string_to_sexp(read, read_len);
        leveldb_free(read);
    } else {
        ret = R_NilValue;
    }

    return ret;
}

SEXP attribute_visible R_bedrock_leveldb_mget(SEXP r_db, SEXP r_keys,
                                              SEXP r_readoptions) {
    leveldb_t *db = bedrock_leveldb_get_db(r_db, true);
    leveldb_readoptions_t *readoptions =
        bedrock_leveldb_get_readoptions(r_readoptions, true);

    const char **key_data = NULL;
    R_len_t *key_len = NULL;
    R_len_t num_key = get_keys(r_keys, &key_data, &key_len);

    SEXP ret = PROTECT(Rf_allocVector(VECSXP, num_key));

    for(R_len_t i = 0; i < num_key; ++i) {
        char *err = NULL;
        size_t read_len;
        char *read = leveldb_get(db, readoptions, key_data[i], key_len[i],
                                 &read_len, &err);
        bedrock_leveldb_handle_error(err);
        if(read != NULL) {
            SEXP el = PROTECT(raw_string_to_sexp(read, read_len));
            SET_VECTOR_ELT(ret, i, el);
            leveldb_free(read);
            UNPROTECT(1);
        } else {
            SET_VECTOR_ELT(ret, i, R_NilValue);
        }
    }

    UNPROTECT(1);
    return ret;
}

SEXP attribute_visible R_bedrock_leveldb_mget_prefix(SEXP r_db,
                                                     SEXP r_starts_with,
                                                     SEXP r_readoptions) {
    leveldb_t *db = bedrock_leveldb_get_db(r_db, true);
    leveldb_readoptions_t *readoptions =
        bedrock_leveldb_get_readoptions(r_readoptions, true);
    const char *starts_with = NULL;
    const R_len_t starts_with_len =
        get_starts_with(r_starts_with, &starts_with);

    SEXP r_ret_key = PROTECT(create_stretchy_list());
    SEXP r_ret_value = PROTECT(create_stretchy_list());
    SEXP r_value, r_key;

    leveldb_iterator_t *it = leveldb_create_iterator(db, readoptions);
    leveldb_iter_seek(it, starts_with, starts_with_len);

    for(; leveldb_iter_valid(it); leveldb_iter_next(it)) {
        if(!iter_key_starts_with(it, starts_with, starts_with_len)) {
            break;
        }
        size_t data_len;
        const char *data;
        data = leveldb_iter_key(it, &data_len);
        r_key = PROTECT(raw_string_to_sexp(data, data_len));
        data = leveldb_iter_value(it, &data_len);
        r_value = PROTECT(raw_string_to_sexp(data, data_len));
        grow_stretchy_list(r_ret_value, r_value);
        grow_stretchy_list(r_ret_key, r_key);
        UNPROTECT(2);
    }
    leveldb_iter_destroy(it);

    const char *names[] = {"keys", "values", ""};
    SEXP r_ret = PROTECT(Rf_mkNamed(VECSXP, names));
    SET_VECTOR_ELT(r_ret, 0, Rf_PairToVectorList(CDR(r_ret_key)));
    SET_VECTOR_ELT(r_ret, 1, Rf_PairToVectorList(CDR(r_ret_value)));
    UNPROTECT(3);
    return r_ret;
}

SEXP attribute_visible R_bedrock_leveldb_put(SEXP r_db, SEXP r_key,
                                             SEXP r_value,
                                             SEXP r_writeoptions) {
    leveldb_t *db = bedrock_leveldb_get_db(r_db, true);
    leveldb_writeoptions_t *writeoptions =
        bedrock_leveldb_get_writeoptions(r_writeoptions, true);
    const char *key_data = NULL, *value_data = NULL;
    R_len_t key_len = get_key(r_key, &key_data),
            value_len = get_value(r_value, &value_data);

    char *err = NULL;
    leveldb_put(db, writeoptions, key_data, key_len, value_data, value_len,
                &err);
    bedrock_leveldb_handle_error(err);

    return R_NilValue;
}

// This is a slightly odd construction and could be done entirely in R
// space (indeed, perhaps it should be?).  But using the higher level
// R API here means that we can avoid leaks of the writebatch object
// if any of the keys can't be extracted.  The total cost of doing
// this is at most a couple of allocations and it avoids a lot of
// duplicated code.
SEXP attribute_visible R_bedrock_leveldb_mput(SEXP r_db, SEXP r_key,
                                              SEXP r_value,
                                              SEXP r_writeoptions) {
    SEXP r_writebatch = PROTECT(R_bedrock_leveldb_writebatch_create());
    R_bedrock_leveldb_writebatch_mput(r_writebatch, r_key, r_value);
    R_bedrock_leveldb_write(r_db, r_writebatch, r_writeoptions);
    UNPROTECT(1);
    return R_NilValue;
}

SEXP attribute_visible R_bedrock_leveldb_delete(SEXP r_db, SEXP r_key,
                                                SEXP r_report,
                                                SEXP r_readoptions,
                                                SEXP r_writeoptions) {
    if(scalar_logical(r_report)) {
        return R_bedrock_leveldb_delete_report(r_db, r_key, r_readoptions,
                                               r_writeoptions);
    } else {
        return R_bedrock_leveldb_delete_silent(r_db, r_key, r_writeoptions);
    }
}

// This is the simple delete: it just deletes things and does not
// report back anything about what was done (these keys may or may not
// exist).
SEXP attribute_visible R_bedrock_leveldb_delete_silent(SEXP r_db, SEXP r_key,
                                                       SEXP r_writeoptions) {
    leveldb_t *db = bedrock_leveldb_get_db(r_db, true);
    const char **key_data = NULL;
    R_len_t *key_len = NULL;
    R_len_t num_key = get_keys(r_key, &key_data, &key_len);
    leveldb_writeoptions_t *writeoptions =
        bedrock_leveldb_get_writeoptions(r_writeoptions, true);

    for(R_len_t i = 0; i < num_key; ++i) {
        char *err = NULL;
        leveldb_delete(db, writeoptions, key_data[i], key_len[i], &err);
        bedrock_leveldb_handle_error(err);
    }

    return R_NilValue;
}

// This is quite a bit more complicated; we first iterate through and
// find out what exists, arranging to return that back to R in the
// first place.  Then we go through and do the deletion.
SEXP attribute_visible R_bedrock_leveldb_delete_report(SEXP r_db, SEXP r_key,
                                                       SEXP r_readoptions,
                                                       SEXP r_writeoptions) {
    leveldb_t *db = bedrock_leveldb_get_db(r_db, true);
    const char **key_data = NULL;
    R_len_t *key_len = NULL;
    R_len_t num_key = get_keys(r_key, &key_data, &key_len);

    // This might fail so I'm doing it up here
    leveldb_writeoptions_t *writeoptions =
        bedrock_leveldb_get_writeoptions(r_writeoptions, true);

    SEXP r_found = PROTECT(Rf_allocVector(LGLSXP, num_key));
    int *found = INTEGER(r_found);

    leveldb_readoptions_t *readoptions = default_readoptions;
    // NOTE: leak danger on throw, so nothing between here and the
    // writebatch_destroys may throw (and therefore can't use the R
    // API).
    leveldb_writebatch_t *writebatch = leveldb_writebatch_create();

    // First, work out what exists:
    bedrock_leveldb_get_exists(db, num_key, key_data, key_len, readoptions,
                               found);

    bool do_delete = false;
    for(R_len_t i = 0; i < num_key; ++i) {
        if(found[i]) {
            leveldb_writebatch_delete(writebatch, key_data[i], key_len[i]);
            do_delete = true;
        }
    }

    if(do_delete) {
        char *err = NULL;
        leveldb_write(db, writeoptions, writebatch, &err);
        // NOTE: This must come here *and* in the else (but not outside
        // the if/else) because that way we don't leak the writebatch
        // object on error.
        leveldb_writebatch_destroy(writebatch);
        bedrock_leveldb_handle_error(err);
    } else {
        leveldb_writebatch_destroy(writebatch);
    }

    UNPROTECT(1);
    return r_found;
}

// Iterators
SEXP attribute_visible R_bedrock_leveldb_iter_create(SEXP r_db,
                                                     SEXP r_readoptions) {
    leveldb_t *db = bedrock_leveldb_get_db(r_db, true);
    leveldb_readoptions_t *readoptions =
        bedrock_leveldb_get_readoptions(r_readoptions, true);
    leveldb_iterator_t *it = leveldb_create_iterator(db, readoptions);

    SEXP r_it = PROTECT(R_MakeExternalPtr(it, r_db, R_NilValue));
    R_RegisterCFinalizer(r_it, bedrock_leveldb_iter_finalize);

    SEXP db_tag = bedrock_leveldb_tag(r_db);
    SEXP r_iterators = VECTOR_ELT(db_tag, TAG_ITERATORS);
    SET_VECTOR_ELT(db_tag, TAG_ITERATORS, CONS(r_it, r_iterators));

    UNPROTECT(1);
    return r_it;
}

SEXP attribute_visible
R_bedrock_leveldb_iter_destroy(SEXP r_it, SEXP r_error_if_destroyed) {
    bool error_if_destroyed = scalar_logical(r_error_if_destroyed);
    leveldb_iterator_t *it =
        bedrock_leveldb_get_iterator(r_it, error_if_destroyed);
    if(it != NULL) {
        leveldb_iter_destroy(it);
        R_ClearExternalPtr(r_it);
    }
    return Rf_ScalarLogical(it != NULL);
}

SEXP attribute_visible R_bedrock_leveldb_iter_valid(SEXP r_it) {
    leveldb_iterator_t *it = bedrock_leveldb_get_iterator(r_it, true);
    return Rf_ScalarLogical(leveldb_iter_valid(it));
}

SEXP attribute_visible R_bedrock_leveldb_iter_seek_to_first(SEXP r_it) {
    leveldb_iterator_t *it = bedrock_leveldb_get_iterator(r_it, true);
    leveldb_iter_seek_to_first(it);
    return R_NilValue;
}

SEXP attribute_visible R_bedrock_leveldb_iter_seek_to_last(SEXP r_it) {
    leveldb_iterator_t *it = bedrock_leveldb_get_iterator(r_it, true);
    leveldb_iter_seek_to_last(it);
    return R_NilValue;
}

SEXP attribute_visible R_bedrock_leveldb_iter_seek(SEXP r_it, SEXP r_key) {
    leveldb_iterator_t *it = bedrock_leveldb_get_iterator(r_it, true);
    const char *key_data = NULL;
    R_len_t key_len = get_key(r_key, &key_data);
    leveldb_iter_seek(it, key_data, key_len);
    return R_NilValue;
}

SEXP attribute_visible R_bedrock_leveldb_iter_next(SEXP r_it,
                                                   SEXP r_error_if_invalid) {
    leveldb_iterator_t *it = bedrock_leveldb_get_iterator(r_it, true);
    if(check_iterator(it, r_error_if_invalid)) {
        leveldb_iter_next(it);
    }
    return R_NilValue;
}

SEXP attribute_visible R_bedrock_leveldb_iter_prev(SEXP r_it,
                                                   SEXP r_error_if_invalid) {
    leveldb_iterator_t *it = bedrock_leveldb_get_iterator(r_it, true);
    if(check_iterator(it, r_error_if_invalid)) {
        leveldb_iter_prev(it);
    }
    return R_NilValue;
}

SEXP attribute_visible R_bedrock_leveldb_iter_key(SEXP r_it,
                                                  SEXP r_error_if_invalid) {
    leveldb_iterator_t *it = bedrock_leveldb_get_iterator(r_it, true);
    size_t len;
    if(!check_iterator(it, r_error_if_invalid)) {
        return R_NilValue;
    }
    const char *data = leveldb_iter_key(it, &len);
    return raw_string_to_sexp(data, len);
}

SEXP attribute_visible R_bedrock_leveldb_iter_value(SEXP r_it,
                                                    SEXP r_error_if_invalid) {
    leveldb_iterator_t *it = bedrock_leveldb_get_iterator(r_it, true);
    if(!check_iterator(it, r_error_if_invalid)) {
        return R_NilValue;
    }
    size_t len;
    const char *data = leveldb_iter_value(it, &len);
    return raw_string_to_sexp(data, len);
}

// Snapshots
SEXP attribute_visible R_bedrock_leveldb_snapshot_create(SEXP r_db) {
    leveldb_t *db = bedrock_leveldb_get_db(r_db, true);
    const leveldb_snapshot_t *snapshot = leveldb_create_snapshot(db);
    SEXP r_snapshot =
        PROTECT(R_MakeExternalPtr((void *)snapshot, r_db, R_NilValue));
    R_RegisterCFinalizer(r_snapshot, bedrock_leveldb_snapshot_finalize);
    UNPROTECT(1);
    return r_snapshot;
}

// Batch
SEXP attribute_visible R_bedrock_leveldb_writebatch_create(void) {
    leveldb_writebatch_t *writebatch = leveldb_writebatch_create();
    SEXP r_writebatch =
        PROTECT(R_MakeExternalPtr((void *)writebatch, R_NilValue, R_NilValue));
    R_RegisterCFinalizer(r_writebatch, bedrock_leveldb_writebatch_finalize);
    UNPROTECT(1);
    return r_writebatch;
}

SEXP attribute_visible R_bedrock_leveldb_writebatch_destroy(
    SEXP r_writebatch, SEXP r_error_if_destroyed) {
    bool error_if_destroyed = scalar_logical(r_error_if_destroyed);
    leveldb_writebatch_t *writebatch =
        bedrock_leveldb_get_writebatch(r_writebatch, error_if_destroyed);
    if(writebatch != NULL) {
        leveldb_writebatch_destroy(writebatch);
        R_ClearExternalPtr(r_writebatch);
    }
    return Rf_ScalarLogical(writebatch != NULL);
}

SEXP attribute_visible R_bedrock_leveldb_writebatch_clear(SEXP r_writebatch) {
    leveldb_writebatch_t *writebatch =
        bedrock_leveldb_get_writebatch(r_writebatch, true);
    leveldb_writebatch_clear(writebatch);
    return R_NilValue;
}

SEXP attribute_visible R_bedrock_leveldb_writebatch_put(SEXP r_writebatch,
                                                        SEXP r_key,
                                                        SEXP r_value) {
    leveldb_writebatch_t *writebatch =
        bedrock_leveldb_get_writebatch(r_writebatch, true);
    const char *key_data = NULL, *value_data = NULL;
    R_len_t key_len = get_key(r_key, &key_data),
            value_len = get_value(r_value, &value_data);
    leveldb_writebatch_put(writebatch, key_data, key_len, value_data,
                           value_len);
    return R_NilValue;
}

SEXP attribute_visible R_bedrock_leveldb_writebatch_mput(SEXP r_writebatch,
                                                         SEXP r_key,
                                                         SEXP r_value) {
    leveldb_writebatch_t *writebatch =
        bedrock_leveldb_get_writebatch(r_writebatch, true);
    const char **key_data = NULL;
    R_len_t *key_len = NULL;
    R_len_t num_key = get_keys(r_key, &key_data, &key_len);

    const bool value_is_string = TYPEOF(r_value) == STRSXP;
    if(!value_is_string && TYPEOF(r_value) != VECSXP) {
        Rf_error("Expected a character vector or list for 'value'");
    }
    if(XLENGTH(r_value) != num_key) {
        Rf_error("Expected %d values but received %" R_PRIdXLEN_T, num_key,
                 XLENGTH(r_value));
    }
    for(R_len_t i = 0; i < num_key; ++i) {
        const char *value_data;
        SEXP el =
            value_is_string ? STRING_ELT(r_value, i) : VECTOR_ELT(r_value, i);
        R_len_t value_len = get_value(el, &value_data);
        leveldb_writebatch_put(writebatch, key_data[i], key_len[i], value_data,
                               value_len);
    }

    return R_NilValue;
}

SEXP attribute_visible R_bedrock_leveldb_writebatch_delete(SEXP r_writebatch,
                                                           SEXP r_key) {
    leveldb_writebatch_t *writebatch =
        bedrock_leveldb_get_writebatch(r_writebatch, true);
    const char *key_data = NULL;
    R_len_t key_len = get_key(r_key, &key_data);
    leveldb_writebatch_delete(writebatch, key_data, key_len);
    return R_NilValue;
}

SEXP attribute_visible R_bedrock_leveldb_writebatch_mdelete(SEXP r_writebatch,
                                                            SEXP r_keys) {
    leveldb_writebatch_t *writebatch =
        bedrock_leveldb_get_writebatch(r_writebatch, true);
    const char **key_data = NULL;
    R_len_t *key_len = NULL;
    R_len_t num_key = get_keys(r_keys, &key_data, &key_len);

    for(R_len_t i = 0; i < num_key; ++i) {
        leveldb_writebatch_delete(writebatch, key_data[i], key_len[i]);
    }

    return R_NilValue;
}

// NOTE: arguments 2 & 3 transposed with respect to leveldb API
SEXP attribute_visible R_bedrock_leveldb_write(SEXP r_db, SEXP r_writebatch,
                                               SEXP r_writeoptions) {
    leveldb_t *db = bedrock_leveldb_get_db(r_db, true);
    leveldb_writeoptions_t *writeoptions =
        bedrock_leveldb_get_writeoptions(r_writeoptions, true);
    leveldb_writebatch_t *writebatch =
        bedrock_leveldb_get_writebatch(r_writebatch, true);
    char *err = NULL;
    leveldb_write(db, writeoptions, writebatch, &err);
    bedrock_leveldb_handle_error(err);
    return R_NilValue;
}

SEXP attribute_visible R_bedrock_leveldb_approximate_sizes(SEXP r_db,
                                                           SEXP r_start_key,
                                                           SEXP r_limit_key) {
    leveldb_t *db = bedrock_leveldb_get_db(r_db, true);

    const char **start_key = NULL, **limit_key = NULL;
    R_len_t *start_key_len = NULL, *limit_key_len = NULL;
    R_len_t num_start = get_keys(r_start_key, &start_key, &start_key_len),
            num_limit = get_keys(r_limit_key, &limit_key, &limit_key_len);
    if(num_start != num_limit) {
        Rf_error("Expected 'limit_key' to be a length %d vector", num_start);
    }

    uint64_t *sizes = (uint64_t *)R_alloc(num_start, sizeof(uint64_t));
    size_t *sz_start_key_len = (size_t *)R_alloc(num_start, sizeof(size_t));
    size_t *sz_limit_key_len = (size_t *)R_alloc(num_start, sizeof(size_t));
    for(R_len_t i = 0; i < num_start; ++i) {
        sz_start_key_len[i] = start_key_len[i];
        sz_limit_key_len[i] = limit_key_len[i];
    }

    leveldb_approximate_sizes(db, num_start, start_key, sz_start_key_len,
                              limit_key, sz_limit_key_len, sizes);
    SEXP ret = PROTECT(Rf_allocVector(INTSXP, num_start));
    int *isizes = INTEGER(ret);
    for(R_len_t i = 0; i < num_start; ++i) {
        isizes[i] = (int)sizes[i];
    }
    UNPROTECT(1);
    return ret;
}

SEXP attribute_visible R_bedrock_leveldb_compact_range(SEXP r_db,
                                                       SEXP r_start_key,
                                                       SEXP r_limit_key) {
    leveldb_t *db = bedrock_leveldb_get_db(r_db, true);
    const char *start_key = NULL, *limit_key = NULL;
    R_len_t start_key_len = get_key_maybe_nil(r_start_key, &start_key),
            limit_key_len = get_key_maybe_nil(r_limit_key, &limit_key);
    leveldb_compact_range(db, start_key, start_key_len, limit_key,
                          limit_key_len);
    return R_NilValue;
}

// Options
SEXP attribute_visible R_bedrock_leveldb_readoptions(SEXP r_verify_checksums,
                                                     SEXP r_fill_cache,
                                                     SEXP r_snapshot) {
    leveldb_readoptions_t *options = leveldb_readoptions_create();
    SEXP tag = PROTECT(Rf_allocVector(VECSXP, 3));
    SET_VECTOR_ELT(tag, 0, r_verify_checksums);
    SET_VECTOR_ELT(tag, 1, r_fill_cache);
    SET_VECTOR_ELT(tag, 2, r_snapshot);
    SEXP ret = PROTECT(R_MakeExternalPtr(options, tag, R_NilValue));
    R_RegisterCFinalizer(ret, bedrock_leveldb_readoptions_finalize);
    if(!Rf_isNull(r_verify_checksums)) {
        bool verify_checksums = scalar_logical(r_verify_checksums);
        leveldb_readoptions_set_verify_checksums(options, verify_checksums);
    }
    if(!Rf_isNull(r_fill_cache)) {
        leveldb_readoptions_set_fill_cache(options,
                                           scalar_logical(r_fill_cache));
    }
    if(!Rf_isNull(r_snapshot)) {
        leveldb_readoptions_set_snapshot(
            options, bedrock_leveldb_get_snapshot(r_snapshot, true));
    }

    UNPROTECT(2);
    return ret;
}

SEXP attribute_visible R_bedrock_leveldb_writeoptions(SEXP r_sync) {
    leveldb_writeoptions_t *options = leveldb_writeoptions_create();
    SEXP tag = PROTECT(Rf_allocVector(VECSXP, 1));
    SET_VECTOR_ELT(tag, 0, r_sync);
    SEXP ret = PROTECT(R_MakeExternalPtr(options, tag, R_NilValue));
    R_RegisterCFinalizer(ret, bedrock_leveldb_writeoptions_finalize);
    if(!Rf_isNull(r_sync)) {
        leveldb_writeoptions_set_sync(options, scalar_logical(r_sync));
    }
    UNPROTECT(2);
    return ret;
}

// Built on top of the leveldb api.
SEXP attribute_visible R_bedrock_leveldb_keys(SEXP r_db, SEXP r_starts_with,
                                              SEXP r_readoptions) {
    leveldb_t *db = bedrock_leveldb_get_db(r_db, true);
    leveldb_readoptions_t *readoptions =
        bedrock_leveldb_get_readoptions(r_readoptions, true);
    const char *starts_with = NULL;
    const R_len_t starts_with_len =
        get_starts_with(r_starts_with, &starts_with);

    SEXP r_ret = PROTECT(create_stretchy_list());
    SEXP r_value;

    leveldb_iterator_t *it = leveldb_create_iterator(db, readoptions);
    if(starts_with_len == 0) {
        leveldb_iter_seek_to_first(it);
    } else {
        leveldb_iter_seek(it, starts_with, starts_with_len);
    }

    for(; leveldb_iter_valid(it); leveldb_iter_next(it)) {
        if(!iter_key_starts_with(it, starts_with, starts_with_len)) {
            break;
        }
        size_t key_len;
        const char *key_data = leveldb_iter_key(it, &key_len);
        r_value = PROTECT(raw_string_to_sexp(key_data, key_len));
        grow_stretchy_list(r_ret, r_value);
        UNPROTECT(1);
    }
    leveldb_iter_destroy(it);

    UNPROTECT(1);
    return Rf_PairToVectorList(CDR(r_ret));
}

SEXP attribute_visible R_bedrock_leveldb_keys_len(SEXP r_db, SEXP r_starts_with,
                                                  SEXP r_readoptions) {
    leveldb_t *db = bedrock_leveldb_get_db(r_db, true);
    leveldb_readoptions_t *readoptions =
        bedrock_leveldb_get_readoptions(r_readoptions, true);
    const char *starts_with = NULL;
    const R_len_t starts_with_len =
        get_starts_with(r_starts_with, &starts_with);
    return Rf_ScalarInteger((int)bedrock_leveldb_get_keys_len(
        db, starts_with, starts_with_len, readoptions));
}

SEXP attribute_visible R_bedrock_leveldb_exists(SEXP r_db, SEXP r_key,
                                                SEXP r_readoptions) {
    leveldb_t *db = bedrock_leveldb_get_db(r_db, true);
    leveldb_readoptions_t *readoptions =
        bedrock_leveldb_get_readoptions(r_readoptions, true);
    const char **key_data = NULL;
    R_len_t *key_len = NULL;
    R_len_t num_key = get_keys(r_key, &key_data, &key_len);
    SEXP r_found = PROTECT(Rf_allocVector(LGLSXP, num_key));
    int *found = INTEGER(r_found);
    bedrock_leveldb_get_exists(db, num_key, key_data, key_len, readoptions,
                               found);
    UNPROTECT(1);
    return r_found;
}

SEXP attribute_visible R_bedrock_leveldb_version(void) {
    SEXP ret = PROTECT(Rf_allocVector(INTSXP, 2));
    INTEGER(ret)[0] = leveldb_major_version();
    INTEGER(ret)[1] = leveldb_minor_version();
    UNPROTECT(1);
    return ret;
}

// For internal use:
SEXP bedrock_leveldb_tag(SEXP r_db) { return R_ExternalPtrTag(r_db); }

// For package management:
void bedrock_leveldb_init(void) {
    default_readoptions = leveldb_readoptions_create();
    default_writeoptions = leveldb_writeoptions_create();
}

void bedrock_leveldb_cleanup(void) {
    leveldb_readoptions_destroy(default_readoptions);    // #nocov
    leveldb_writeoptions_destroy(default_writeoptions);  // #nocov
}

// Internal function definitions:
void bedrock_leveldb_finalize(SEXP r_db) {
    leveldb_t *db = bedrock_leveldb_get_db(r_db, false);
    if(db != NULL) {
        leveldb_close(db);
        R_ClearExternalPtr(r_db);
    }
}

void bedrock_leveldb_iter_finalize(SEXP r_it) {
    leveldb_iterator_t *it = bedrock_leveldb_get_iterator(r_it, false);
    if(it != NULL) {
        leveldb_iter_destroy(it);
        R_ClearExternalPtr(r_it);
    }
}

void bedrock_leveldb_snapshot_finalize(SEXP r_snapshot) {
    leveldb_snapshot_t *snapshot =
        bedrock_leveldb_get_snapshot(r_snapshot, false);
    if(snapshot != NULL) {
        leveldb_t *db =
            bedrock_leveldb_get_db(bedrock_leveldb_tag(r_snapshot), false);
        if(db) {
            leveldb_release_snapshot(db, snapshot);
        }
        R_ClearExternalPtr(r_snapshot);
    }
}

void bedrock_leveldb_writebatch_finalize(SEXP r_writebatch) {
    leveldb_writebatch_t *writebatch =
        bedrock_leveldb_get_writebatch(r_writebatch, false);
    if(writebatch) {
        leveldb_writebatch_destroy(writebatch);
        R_ClearExternalPtr(r_writebatch);
    }
}

void bedrock_leveldb_readoptions_finalize(SEXP r_readoptions) {
    leveldb_readoptions_t *readoptions =
        bedrock_leveldb_get_readoptions(r_readoptions, false);
    if(readoptions) {
        leveldb_readoptions_destroy(readoptions);
        R_ClearExternalPtr(r_readoptions);
    }
}

void bedrock_leveldb_writeoptions_finalize(SEXP r_writeoptions) {
    leveldb_writeoptions_t *writeoptions =
        bedrock_leveldb_get_writeoptions(r_writeoptions, false);
    if(writeoptions) {
        leveldb_writeoptions_destroy(writeoptions);
        R_ClearExternalPtr(r_writeoptions);
    }
}

void bedrock_leveldb_cache_finalize(SEXP r_cache) {
    if(TYPEOF(r_cache) == EXTPTRSXP) {
        leveldb_cache_t *cache = (leveldb_cache_t *)R_ExternalPtrAddr(r_cache);
        if(cache) {
            leveldb_cache_destroy(cache);
            R_ClearExternalPtr(r_cache);
        }
    }
}

void bedrock_leveldb_filterpolicy_finalize(SEXP r_filterpolicy) {
    if(TYPEOF(r_filterpolicy) == EXTPTRSXP) {
        leveldb_filterpolicy_t *filterpolicy =
            (leveldb_filterpolicy_t *)R_ExternalPtrAddr(r_filterpolicy);
        if(filterpolicy) {
            leveldb_filterpolicy_destroy(filterpolicy);
            R_ClearExternalPtr(r_filterpolicy);
        }
    }
}

void bedrock_leveldb_compressor_finalize(SEXP r_compressor) {
    if(TYPEOF(r_compressor) == EXTPTRSXP) {
        leveldb_compressor_t *compressor =
            (leveldb_compressor_t *)R_ExternalPtrAddr(r_compressor);
        if(compressor) {
            leveldb_compressor_destroy(compressor);
            R_ClearExternalPtr(r_compressor);
        }
    }
}

leveldb_t *bedrock_leveldb_get_db(SEXP r_db, bool closed_error) {
    void *db = NULL;
    if(TYPEOF(r_db) != EXTPTRSXP) {
        Rf_error("Expected an external pointer");
    }
    db = (leveldb_t *)R_ExternalPtrAddr(r_db);
    if(!db && closed_error) {
        Rf_error("leveldb handle is not open; can't connect");
    }
    return (leveldb_t *)db;
}

// TODO: distinguish here between an iterator and db handle by
// checking the SEXP on the tag?
leveldb_iterator_t *bedrock_leveldb_get_iterator(SEXP r_it, bool closed_error) {
    void *it = NULL;
    if(TYPEOF(r_it) != EXTPTRSXP) {
        Rf_error("Expected an external pointer");
    }
    it = (leveldb_iterator_t *)R_ExternalPtrAddr(r_it);
    if(!it && closed_error) {
        Rf_error("leveldb iterator is not open; can't connect");
    }
    return (leveldb_iterator_t *)it;
}

leveldb_snapshot_t *bedrock_leveldb_get_snapshot(SEXP r_snapshot,
                                                 bool closed_error) {
    void *snapshot = NULL;
    if(TYPEOF(r_snapshot) != EXTPTRSXP) {
        Rf_error("Expected an external pointer");
    }
    snapshot = (leveldb_snapshot_t *)R_ExternalPtrAddr(r_snapshot);
    if(!snapshot && closed_error) {
        Rf_error("leveldb snapshot is not open; can't connect");
    }
    return (leveldb_snapshot_t *)snapshot;
}

leveldb_writebatch_t *bedrock_leveldb_get_writebatch(SEXP r_writebatch,
                                                     bool closed_error) {
    void *writebatch = NULL;
    if(TYPEOF(r_writebatch) != EXTPTRSXP) {
        Rf_error("Expected an external pointer");
    }
    writebatch = (leveldb_writebatch_t *)R_ExternalPtrAddr(r_writebatch);
    if(!writebatch && closed_error) {
        Rf_error("leveldb writebatch is not open; can't connect");
    }
    return (leveldb_writebatch_t *)writebatch;
}

leveldb_readoptions_t *bedrock_leveldb_get_readoptions(SEXP r_readoptions,
                                                       bool closed_error) {
    if(Rf_isNull(r_readoptions)) {
        return default_readoptions;
    }
    void *readoptions = NULL;
    if(TYPEOF(r_readoptions) != EXTPTRSXP) {
        Rf_error("Expected an external pointer");
    }
    readoptions = (leveldb_readoptions_t *)R_ExternalPtrAddr(r_readoptions);
    if(!readoptions && closed_error) {
        Rf_error("leveldb readoptions is not open; can't connect");
    }
    return (leveldb_readoptions_t *)readoptions;
}

leveldb_writeoptions_t *bedrock_leveldb_get_writeoptions(SEXP r_writeoptions,
                                                         bool closed_error) {
    if(Rf_isNull(r_writeoptions)) {
        return default_writeoptions;
    }
    void *writeoptions = NULL;
    if(TYPEOF(r_writeoptions) != EXTPTRSXP) {
        Rf_error("Expected an external pointer");
    }
    writeoptions = (leveldb_writeoptions_t *)R_ExternalPtrAddr(r_writeoptions);
    if(!writeoptions && closed_error) {
        Rf_error("leveldb writeoptions is not open; can't connect");
    }
    return (leveldb_writeoptions_t *)writeoptions;
}

void bedrock_leveldb_handle_error(char *err) {
    if(err != NULL) {
        // Make a copy of err, so we can free it before calling Rf_error.
        R_len_t len = strlen(err);
        char *msg = (char *)R_alloc(len + 1, sizeof(char));
        memcpy(msg, err, len + 1);
        leveldb_free(err);
        Rf_error("%s", msg);
    }
}

R_len_t bedrock_leveldb_get_keys_len(leveldb_t *db, const char *starts_with,
                                     R_len_t starts_with_len,
                                     leveldb_readoptions_t *readoptions) {
    leveldb_iterator_t *it = leveldb_create_iterator(db, readoptions);

    size_t n = 0;

    if(starts_with_len == 0) {
        leveldb_iter_seek_to_first(it);
    } else {
        leveldb_iter_seek(it, starts_with, starts_with_len);
    }

    for(; leveldb_iter_valid(it); leveldb_iter_next(it)) {
        if(!iter_key_starts_with(it, starts_with, starts_with_len)) {
            break;
        }
        ++n;
    }
    leveldb_iter_destroy(it);
    return n;
}

// NOTE: this uses `int*` for found, not `bool*` because it is
// designed to work with passing things back using an R LGLSXP (where
// things are stored as integers because of NA values)
void bedrock_leveldb_get_exists(leveldb_t *db, R_len_t num_key,
                                const char **key_data, R_len_t *key_len,
                                leveldb_readoptions_t *readoptions,
                                int *found) {
    leveldb_iterator_t *it = leveldb_create_iterator(db, readoptions);
    for(R_len_t i = 0; i < num_key; ++i) {
        leveldb_iter_seek(it, key_data[i], key_len[i]);
        if(leveldb_iter_valid(it)) {
            size_t it_key_len;
            const char *it_key_data = leveldb_iter_key(it, &it_key_len);
            found[i] = (it_key_len == key_len[i] &&
                        memcmp(it_key_data, key_data[i], key_len[i]) == 0);
        } else {
            found[i] = 0;
        }
    }
    leveldb_iter_destroy(it);
}

leveldb_options_t *bedrock_leveldb_collect_options(
    SEXP r_create_if_missing, SEXP r_error_if_exists, SEXP r_paranoid_checks,
    SEXP r_write_buffer_size, SEXP r_max_open_files, SEXP r_block_size) {
    leveldb_options_t *options = leveldb_options_create();
    // TODO: put a finaliser on options so that we can error safely in
    // the scalar_logical commands on early exit.  Otherwise there is
    // not really a wonderful way of doing this.  The simplest route
    // would be to check on the R side really and then we don't have to
    // do any of the hard work.
    if(!Rf_isNull(r_create_if_missing)) {
        leveldb_options_set_create_if_missing(
            options, scalar_logical(r_create_if_missing));
    }
    if(!Rf_isNull(r_error_if_exists)) {
        leveldb_options_set_error_if_exists(options,
                                            scalar_logical(r_error_if_exists));
    }
    if(!Rf_isNull(r_paranoid_checks)) {
        leveldb_options_set_paranoid_checks(options,
                                            scalar_logical(r_paranoid_checks));
    }
    if(!Rf_isNull(r_write_buffer_size)) {
        leveldb_options_set_write_buffer_size(options,
                                              scalar_size(r_write_buffer_size));
    }
    if(!Rf_isNull(r_max_open_files)) {
        leveldb_options_set_max_open_files(options,
                                           (int)scalar_size(r_max_open_files));
    }
    if(!Rf_isNull(r_block_size)) {
        leveldb_options_set_block_size(options, scalar_size(r_block_size));
    }
    return options;
}

bool check_iterator(leveldb_iterator_t *it, SEXP r_error_if_invalid) {
    bool valid = leveldb_iter_valid(it);
    if(!valid) {
        if(scalar_logical(r_error_if_invalid)) {
            Rf_error("Iterator is not valid");
        }
    }
    return valid;
}

leveldb_readoptions_t *default_readoptions = NULL;
leveldb_writeoptions_t *default_writeoptions = NULL;

bool iter_key_starts_with(leveldb_iterator_t *it, const char *starts_with,
                          R_len_t starts_with_len) {
    if(starts_with_len <= 0) {
        return true;
    }
    size_t key_len;
    const char *key_data = leveldb_iter_key(it, &key_len);
    return key_len >= starts_with_len &&
           memcmp(key_data, starts_with, starts_with_len) == 0;
}
