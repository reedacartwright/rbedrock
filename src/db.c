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

#include "db.h"

#include <leveldb/c.h>
#include <stdbool.h>

#include "keys.h"
#include "support.h"

leveldb_readoptions_t *default_readoptions;
leveldb_writeoptions_t *default_writeoptions;

// Internals:
leveldb_writebatch_t *rbedrock_db_get_writebatch(SEXP r_writebatch,
                                                     bool closed_error);
bool check_iterator(leveldb_iterator_t *it, SEXP r_error_if_invalid);

// Finalizers
static void finalize_handle(SEXP r_ptr);
static void finalize_snapshot(SEXP r_ptr);
static void finalize_iterator(SEXP r_ptr);
static void finalize_readoptions(SEXP r_ptr);
static void finalize_writeoptions(SEXP r_ptr);

static void finalize_writebatch(SEXP r_writebatch);

// Other internals
static void handle_leveldb_error(char *err);
static leveldb_readoptions_t * create_readoptions(SEXP r_list);
static leveldb_writeoptions_t * create_writeoptions(SEXP r_list);
static SEXP wrap_readoptions(leveldb_readoptions_t *ptr);
static SEXP wrap_writeoptions(leveldb_writeoptions_t *ptr);
static SEXP create_iterator(SEXP r_db, SEXP r_readoptions);

leveldb_options_t *rbedrock_db_collect_options(
    SEXP r_create_if_missing, SEXP r_error_if_exists, SEXP r_paranoid_checks,
    SEXP r_write_buffer_size, SEXP r_max_open_files, SEXP r_block_size);

bool iter_key_starts_with(leveldb_iterator_t *it, const char *starts_with,
                          size_t starts_with_len);

// Slightly different
static size_t keys_len_impl(leveldb_t *db, const char *starts_with,
                            size_t starts_with_len,
                            leveldb_readoptions_t *readoptions);
static void exists_impl(leveldb_t *db, size_t num_key,
                        const char **key_data, size_t *key_len,
                        leveldb_readoptions_t *readoptions, int *found);

// For package management:
void rbedrock_init_db(void) {
    default_readoptions = leveldb_readoptions_create();
    default_writeoptions = leveldb_writeoptions_create();
}

void rbedrock_cleanup_db(void) {
    leveldb_readoptions_destroy(default_readoptions);    // #nocov
    leveldb_writeoptions_destroy(default_writeoptions);  // #nocov
}

/**** HANDLE FUNCTIONS ****/

typedef struct bedrockdb_handle {
    leveldb_t *db;                          /* leveldb handle */
    leveldb_cache_t *cache;                 /* leveldb cache */
    leveldb_filterpolicy_t *filter_policy;  /* leveldb filters */
    leveldb_compressor_t *compressors[2];   /* leveldb compressors */
    int slot;       /* location of this handle in the session history */
    SEXP r_ext_ptr; /* address of external pointer for this handle */
} bedrockdb_handle_t;

#define MAX_HANDLES 1024
static unsigned int num_bedrockdb = 0; /* number of dbs opened in session */
static bedrockdb_handle_t * opened_handles[MAX_HANDLES]; /* currently opened channels */

enum rbedrock_db_prot_index {
    PROT_PATH,
    PROT_ITERATORS,
    PROT_SNAPSHOTS,
    PROT_LENGTH  // don't store anything here!
};

enum rbedrock_db_snap_prot_index {
    SNAP_PROT_CELL,
    SNAP_PROT_DB,
    SNAP_PROT_LENGTH  // don't store anything here!
};

static void * get_external_address(SEXP r_ptr, bool error_on_clear, const char * custom_msg) {
    if(TYPEOF(r_ptr) != EXTPTRSXP) {
        Rf_error("Invalid object: object is not an external pointer.");
    }
    void *ptr = R_ExternalPtrAddr(r_ptr);
    if(!ptr && error_on_clear) {
        if(custom_msg == NULL) {
            Rf_error("Cannot access object: external pointer is cleared.");
        } else {
            Rf_error(custom_msg);
        }
    }
    return ptr;
}

static SEXP get_protected(SEXP r_db) {
    if(TYPEOF(r_db) != EXTPTRSXP) {
        Rf_error("Invalid object: object is not an external pointer.");
    }
    return R_ExternalPtrProtected(r_db);
}

static bedrockdb_handle_t * get_handle_ptr(SEXP r_db, bool error_on_closed) {
    return get_external_address(r_db, error_on_closed,
        "Can't connect to db: leveldb handle is not open.");
}

static leveldb_t *get_db_ptr(SEXP r_db) {
    bedrockdb_handle_t *handle = get_handle_ptr(r_db, true);
    return handle->db;
}

static int close_handle_impl(bedrockdb_handle_t *handle) {
    if(!handle) {
        return 0;
    }
    /* Remove this handle from the list of open handles */
    if(handle->slot < MAX_HANDLES) {
        opened_handles[handle->slot] = NULL;
    }

    /* Destroy all remaining iterators and snapshots. */
    SEXP prot = get_protected(handle->r_ext_ptr);
    SEXP r_list = VECTOR_ELT(prot, PROT_ITERATORS);
    SEXP r_first = CDR(r_list);
    if(r_first != R_NilValue) {
        // Detach the list
        PROTECT(r_first);
        SETCAR(r_first, R_NilValue);
        SETCDR(r_list, R_NilValue);
        // Destroy all iterators
        while(!Rf_isNull(r_first)) {
            SEXP r_ptr = TAG(r_first);
            leveldb_iterator_t *it = get_external_address(r_ptr, false, NULL);
            if(it != NULL) {
                leveldb_iter_destroy(it);
            }
            R_ClearExternalPtr(r_ptr);
            r_first = CDR(r_first);
        }
        UNPROTECT(1);
    }

    r_list = VECTOR_ELT(prot, PROT_SNAPSHOTS);
    r_first = CDR(r_list);
    if(r_first != R_NilValue) {
        // Detach the list
        PROTECT(r_first);
        SETCAR(r_first, R_NilValue);
        SETCDR(r_list, R_NilValue);
        // Destroy all iterators
        while(!Rf_isNull(r_first)) {
            SEXP r_ptr = TAG(r_first);
             const leveldb_snapshot_t *snapshot = get_external_address(r_ptr, false, NULL);
            if(snapshot != NULL && handle->db != NULL) {
                leveldb_release_snapshot(handle->db, snapshot);
            }
            R_ClearExternalPtr(r_ptr);
            r_first = CDR(r_first);
        }
        UNPROTECT(1);
    }

    /* Close Db and Clean Up */
    if(handle->db) {
        leveldb_close(handle->db);
        handle->db = NULL;
    }
    if(handle->cache) {
        leveldb_cache_destroy(handle->cache);
        handle->cache = NULL;
    }
    if(handle->filter_policy) {
        leveldb_filterpolicy_destroy(handle->filter_policy);
        handle->filter_policy = NULL;
    }
    if(handle->compressors[0]) {
        leveldb_compressor_destroy(handle->compressors[0]);
        handle->compressors[0] = NULL;
    }
    if(handle->compressors[1]) {
        leveldb_compressor_destroy(handle->compressors[1]);
        handle->compressors[1] = NULL;
    }
    R_ClearExternalPtr(handle->r_ext_ptr);
    R_Free(handle);

    return 1;
}

static void finalize_handle(SEXP r_ptr) {
    bedrockdb_handle_t *handle = R_ExternalPtrAddr(r_ptr);
    if(!handle) {
        return;
    }
    Rf_warning("closing unused bedrockdb handle in slot %d\n", handle->slot);

    close_handle_impl(handle);
    R_ClearExternalPtr(r_ptr); /* not really needed */
}

/**** IMPLEMENTATIONS ****/

SEXP rbedrock_db_open(SEXP r_path, SEXP r_create_if_missing,
                          SEXP r_error_if_exists, SEXP r_paranoid_checks,
                          SEXP r_write_buffer_size, SEXP r_max_open_files,
                          SEXP r_block_size,
                          SEXP r_cache_capacity,
                          SEXP r_bloom_filter_bits_per_key,
                          SEXP r_compression_level) {

    int compression_level = -1;
    bedrockdb_handle_t * handle;

    // Allocate metadata 
    SEXP r_prot = PROTECT(allocVector(VECSXP, PROT_LENGTH));
    SET_VECTOR_ELT(r_prot, PROT_PATH, r_path);
    SET_VECTOR_ELT(r_prot, PROT_ITERATORS, double_list_create());
    SET_VECTOR_ELT(r_prot, PROT_SNAPSHOTS, double_list_create());

    // Allocate and register handle
    handle = Calloc(1, bedrockdb_handle_t);
    handle->slot = num_bedrockdb;
    if(handle->slot < MAX_HANDLES) {
        opened_handles[handle->slot] = handle;
    }
    SEXP r_db = PROTECT(R_MakeExternalPtr(handle, Rf_install("rbedrock_bedrockdb_handle"), r_prot));
    handle->r_ext_ptr = r_db;
    R_RegisterCFinalizerEx(r_db, finalize_handle, true);

    // Setup Options
    if(!Rf_isNull(r_cache_capacity)) {
        handle->cache = leveldb_cache_create_lru(scalar_size(r_cache_capacity));
    }
    if(!Rf_isNull(r_bloom_filter_bits_per_key)) {
        size_t bits_per_key = scalar_size(r_bloom_filter_bits_per_key);
        handle->filter_policy = leveldb_filterpolicy_create_bloom((int)bits_per_key);
    }
    if(!Rf_isNull(r_compression_level)) {
        compression_level = scalar_int(r_compression_level);
    }    
    handle->compressors[0] = leveldb_compressor_create(leveldb_zlib_raw_compression, compression_level);
    handle->compressors[1] = leveldb_compressor_create(leveldb_zlib_compression, compression_level);

    const char *path = scalar_character(r_path);

    leveldb_options_t *options = rbedrock_db_collect_options(
        r_create_if_missing, r_error_if_exists, r_paranoid_checks,
        r_write_buffer_size, r_max_open_files, r_block_size);
    if(handle->cache) {
        leveldb_options_set_cache(options, handle->cache);
    }
    if(handle->filter_policy) {
        leveldb_options_set_filter_policy(options, handle->filter_policy);
    }

    leveldb_options_set_compressor(options, 0, handle->compressors[0]);
    leveldb_options_set_compressor(options, 1, handle->compressors[1]);

    // Open the database
    char *err = NULL;
    handle->db = leveldb_open(options, path, &err);
    leveldb_options_destroy(options);

    // Propagate message and clean up.
    if(err) {
        close_handle_impl(handle);
        R_ClearExternalPtr(r_db);
        handle_leveldb_error(err);
    }
    // Bump the opened DB number beca
    ++num_bedrockdb;
    UNPROTECT(2);
    return r_db;
}

SEXP rbedrock_db_close(SEXP r_db, SEXP r_error_if_closed) {
    bedrockdb_handle_t *handle = get_handle_ptr(r_db,
        scalar_logical(r_error_if_closed));
    int status = close_handle_impl(handle);
    R_ClearExternalPtr(r_db); /* not really needed */
    return ScalarLogical(status != 0);
}

SEXP rbedrock_closeall(void) {
    int m = (num_bedrockdb < MAX_HANDLES) ? num_bedrockdb : MAX_HANDLES;
    for(int i = 0; i < m; ++i) {
        close_handle_impl(opened_handles[i]);
    }
    return R_NilValue;
}

SEXP rbedrock_db_is_open(SEXP r_db) {
    return ScalarLogical(get_handle_ptr(r_db, false) != NULL);
}

SEXP rbedrock_db_destroy(SEXP r_path) {
    const char *path = scalar_character(r_path);
    leveldb_options_t *options = leveldb_options_create();
    char *err = NULL;
    leveldb_destroy_db(options, path, &err);
    leveldb_options_destroy(options);
    handle_leveldb_error(err);
    return ScalarLogical(true);
}

SEXP rbedrock_db_repair(SEXP r_path) {
    const char *path = scalar_character(r_path);
    leveldb_options_t *options = leveldb_options_create();
    char *err = NULL;
    leveldb_repair_db(options, path, &err);
    leveldb_options_destroy(options);
    handle_leveldb_error(err);
    return ScalarLogical(true);
}

SEXP rbedrock_db_property(SEXP r_db, SEXP r_name, SEXP r_error_if_missing) {
    leveldb_t *db = get_db_ptr(r_db);
    const char *name = scalar_character(r_name);
    bool error_if_missing = scalar_logical(r_error_if_missing);
    char *value = leveldb_property_value(db, name);
    SEXP ret;
    if(value != NULL) {
        ret = mkString(value);
        leveldb_free(value);
    } else if(error_if_missing) {
        Rf_error("No such property '%s'", name);
    } else {
        ret = R_NilValue;
    }
    return ret;
}

SEXP rbedrock_db_get(SEXP r_db, SEXP r_key, SEXP r_readoptions) {
    leveldb_t *db = get_db_ptr(r_db);
    const char *key_data = NULL;
    size_t key_len = get_key(r_key, &key_data);

    char *err = NULL;
    size_t read_len;

    leveldb_readoptions_t *readoptions = create_readoptions(r_readoptions);

    char *read = leveldb_get(db, readoptions, key_data, key_len, &read_len, &err);

    leveldb_readoptions_destroy(readoptions);

    handle_leveldb_error(err);

    SEXP ret;
    if(read != NULL) {
        ret = raw_string_to_sexp(read, read_len);
        leveldb_free(read);
    } else  {
        ret = R_NilValue;
    }

    return ret;
}

SEXP rbedrock_db_mget(SEXP r_db, SEXP r_keys, SEXP r_readoptions) {
    leveldb_t *db = get_db_ptr(r_db);

    const char **key_data = NULL;
    size_t *key_len = NULL;

    size_t num_key = get_keys(r_keys, &key_data, &key_len);

    SEXP ret = PROTECT(allocVector(VECSXP, num_key));

    // Create readoptions and Wrap it in case R throws an unexpected error.
    leveldb_readoptions_t *readoptions = create_readoptions(r_readoptions);
    SEXP r_ptr = PROTECT(wrap_readoptions(readoptions));

    for(size_t i = 0; i < num_key; ++i) {
        char *err = NULL;
        size_t read_len;
        char *read = leveldb_get(db, readoptions, key_data[i], key_len[i],
                                 &read_len, &err);
        if(err) {
            leveldb_free(read);
            leveldb_readoptions_destroy(readoptions);
            R_ClearExternalPtr(r_ptr);
            handle_leveldb_error(err);
        }
        if(read != NULL) {
            SEXP el = PROTECT(raw_string_to_sexp(read, read_len));
            SET_VECTOR_ELT(ret, i, el);
            leveldb_free(read);
            UNPROTECT(1);
        } else {
            SET_VECTOR_ELT(ret, i, R_NilValue);
        }
    }
    // Cleanup
    leveldb_readoptions_destroy(readoptions);
    R_ClearExternalPtr(r_ptr);
    UNPROTECT(2);
    return ret;
}

SEXP rbedrock_db_mget_prefix(SEXP r_db, SEXP r_starts_with,
                          SEXP r_readoptions) {
    leveldb_t *db = get_db_ptr(r_db);

    const char *starts_with = NULL;
    const size_t starts_with_len = get_starts_with(r_starts_with, &starts_with);

    SEXP r_ret_key = PROTECT(create_stretchy_list());
    SEXP r_ret_value = PROTECT(create_stretchy_list());
    SEXP r_value, r_key;

    SEXP r_it = PROTECT(create_iterator(r_db, r_readoptions));
    leveldb_iterator_t *it = R_ExternalPtrAddr(r_it);

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
    R_ClearExternalPtr(r_it);

    const char *names[] = {"keys", "values", ""};
    SEXP r_ret = PROTECT(Rf_mkNamed(VECSXP, names));
    SET_VECTOR_ELT(r_ret, 0, Rf_PairToVectorList(CDR(r_ret_key)));
    SET_VECTOR_ELT(r_ret, 1, Rf_PairToVectorList(CDR(r_ret_value)));
    UNPROTECT(4);
    return r_ret;
}

static SEXP db_write_impl(SEXP r_db, SEXP r_keys, SEXP r_values, SEXP r_writeoptions) {
    leveldb_t *db = get_db_ptr(r_db);

    R_xlen_t num_ops = XLENGTH(r_keys);

    // Avoid any R functions that can throw until these two
    // objects are destroyed.
    // This might fail. Do it first;
    leveldb_writeoptions_t *writeoptions = create_writeoptions(r_writeoptions);
    leveldb_writebatch_t *batch = leveldb_writebatch_create();

    for(R_xlen_t i=0; i < num_ops; ++i) {
        SEXP r_key = VECTOR_ELT(r_keys, i);
        SEXP r_value = R_NilValue;
        if(!Rf_isNull(r_values)) {
            r_value = VECTOR_ELT(r_values, i);
        }
        if(Rf_isNull(r_value)) {
            leveldb_writebatch_delete(batch, (char*)RAW(r_key), XLENGTH(r_key));
        } else {
            leveldb_writebatch_put(batch, (char*)RAW(r_key), XLENGTH(r_key),
                (char*)RAW(r_value), XLENGTH(r_value));
        }
    }
    char *err = NULL;
    leveldb_write(db, writeoptions, batch, &err);
    leveldb_writebatch_destroy(batch);
    leveldb_writeoptions_destroy(writeoptions);

    // this may throw
    handle_leveldb_error(err);
    return R_NilValue;
}

SEXP rbedrock_db_write(SEXP r_db, SEXP r_keys, SEXP r_values, SEXP r_writeoptions, SEXP r_allow_delete) {
    bool allow_delete = scalar_logical(r_allow_delete);

    // Validate inputs
    if(TYPEOF(r_keys) != VECSXP) {
        Rf_error("Invalid writebatch: keys must be a list of raws.");
    }
    if(TYPEOF(r_values) != VECSXP) {
        if(!r_allow_delete) {
            Rf_error("Invalid writebatch: values must be a list of raws.");
        }
        if(!Rf_isNull(r_values)) {
            Rf_error("Invalid writebatch: values must be a list or NULL.");
        }
    }

    R_xlen_t num_ops = XLENGTH(r_keys);
    if(!Rf_isNull(r_values) && XLENGTH(r_values) != num_ops) {
        Rf_error("Invalid writebatch: keys and values have different lengths; %d and %d.",
            XLENGTH(r_keys), XLENGTH(r_values));
    }
    for(R_xlen_t i=0; i < num_ops; ++i) {
        if(TYPEOF(VECTOR_ELT(r_keys, i)) != RAWSXP) {
            Rf_error("Invalid writebatch: keys must raw.");
        }
        if(Rf_isNull(r_values)) {
            continue;
        }
        if(TYPEOF(VECTOR_ELT(r_values, i)) != RAWSXP) {
            if(!r_allow_delete) {
                Rf_error("Invalid writebatch: values must be raw.");
            }
            if(!Rf_isNull(VECTOR_ELT(r_values, i))) {
                Rf_error("Invalid writebatch: values must be raw or NULL.");
            }
        }
    }

    return db_write_impl(r_db, r_keys, r_values, r_writeoptions);
}

SEXP rbedrock_db_put(SEXP r_db, SEXP r_key, SEXP r_value,
                         SEXP r_writeoptions) {
    leveldb_t *db = get_db_ptr(r_db);
    const char *key_data = NULL, *value_data = NULL;
    size_t key_len = get_key(r_key, &key_data),
           value_len = get_value(r_value, &value_data);

    char *err = NULL;
    leveldb_writeoptions_t *writeoptions = create_writeoptions(r_writeoptions);
    leveldb_put(db, writeoptions, key_data, key_len, value_data, value_len,
                &err);
    leveldb_writeoptions_destroy(writeoptions);
    handle_leveldb_error(err);

    return R_NilValue;
}

SEXP rbedrock_db_delete(SEXP r_db, SEXP r_key, SEXP r_writeoptions) {
    leveldb_t *db = get_db_ptr(r_db);
    const char *key_data = NULL;
    size_t key_len = get_key(r_key, &key_data);
    char *err = NULL;

    leveldb_writeoptions_t *writeoptions = create_writeoptions(r_writeoptions);
    leveldb_delete(db, writeoptions, key_data, key_len, &err);
    leveldb_writeoptions_destroy(writeoptions);
    handle_leveldb_error(err);

    return R_NilValue;
}

/**** ITERATORS ****/

static void finalize_iterator(SEXP r_ptr) {
    leveldb_iterator_t *it = get_external_address(r_ptr, false, NULL);
    if(it != NULL) {
        leveldb_iter_destroy(it);
        
        SEXP cell = R_ExternalPtrProtected(r_ptr);
        if(cell != R_NilValue) {
            double_list_remove(cell);
        }
    }
    R_ClearExternalPtr(r_ptr);
}

static leveldb_iterator_t * get_iterator(SEXP r_ptr) {
    return get_external_address(r_ptr, true, NULL);
}

static SEXP create_iterator(SEXP r_db, SEXP r_readoptions) {
    leveldb_t *db = get_db_ptr(r_db);
    leveldb_readoptions_t *readoptions = create_readoptions(r_readoptions);
    leveldb_iterator_t *it = leveldb_create_iterator(db, readoptions);
    leveldb_readoptions_destroy(readoptions);

    SEXP r_it = PROTECT(R_MakeExternalPtr(it, Rf_install("rbedrock_bedrockdb_iterator"), R_NilValue));
    R_RegisterCFinalizerEx(r_it, finalize_iterator, false);

    UNPROTECT(1);
    return r_it;
}

SEXP rbedrock_db_iter_create(SEXP r_db, SEXP r_readoptions) {
    SEXP r_it = PROTECT(create_iterator(r_db, r_readoptions));

    // Register iterator with db so it gets reclaimed when db closes
    SEXP r_prot = get_protected(r_db);
    SEXP r_iterators = VECTOR_ELT(r_prot, PROT_ITERATORS);
    SEXP cell = double_list_insert(r_iterators, r_it);
    R_SetExternalPtrProtected(r_it, cell);

    UNPROTECT(1);
    return r_it;
}

SEXP rbedrock_iter_destroy(SEXP r_ptr, SEXP r_error_on_clear) {
    bool raise_error = scalar_logical(r_error_on_clear);

    leveldb_iterator_t *it = get_external_address(r_ptr, raise_error, NULL);

    if(it != NULL) {
        leveldb_iter_destroy(it);

        SEXP cell = R_ExternalPtrProtected(r_ptr);
        if(cell != R_NilValue) {
            double_list_remove(cell);
        }
    }
    R_ClearExternalPtr(r_ptr);
    return Rf_ScalarLogical(it != NULL);
}

SEXP rbedrock_iter_isnil(SEXP r_it) {
    leveldb_iterator_t *it = get_external_address(r_it, false, NULL);
    return ScalarLogical(it == NULL);
}

SEXP rbedrock_iter_valid(SEXP r_it) {
    leveldb_iterator_t *it = get_iterator(r_it);
    return ScalarLogical(leveldb_iter_valid(it));
}

SEXP rbedrock_iter_seek_to_first(SEXP r_it) {
    leveldb_iterator_t *it = get_iterator(r_it);
    leveldb_iter_seek_to_first(it);
    return R_NilValue;
}

SEXP rbedrock_iter_seek_to_last(SEXP r_it) {
    leveldb_iterator_t *it = get_iterator(r_it);
    leveldb_iter_seek_to_last(it);
    return R_NilValue;
}

SEXP rbedrock_iter_seek(SEXP r_it, SEXP r_key) {
    leveldb_iterator_t *it = get_iterator(r_it);
    const char *key_data = NULL;
    size_t key_len = get_key(r_key, &key_data);
    leveldb_iter_seek(it, key_data, key_len);
    return R_NilValue;
}

SEXP rbedrock_iter_next(SEXP r_it, SEXP r_error_if_invalid) {
    leveldb_iterator_t *it = get_iterator(r_it);
    if(check_iterator(it, r_error_if_invalid)) {
        leveldb_iter_next(it);
    }
    return R_NilValue;
}

SEXP rbedrock_iter_prev(SEXP r_it, SEXP r_error_if_invalid) {
    leveldb_iterator_t *it = get_iterator(r_it);
    if(check_iterator(it, r_error_if_invalid)) {
        leveldb_iter_prev(it);
    }
    return R_NilValue;
}

SEXP rbedrock_iter_key(SEXP r_it,
                              SEXP r_error_if_invalid) {
    leveldb_iterator_t *it = get_iterator(r_it);
    size_t len;
    if(!check_iterator(it, r_error_if_invalid)) {
        return R_NilValue;
    }
    const char *data = leveldb_iter_key(it, &len);
    return raw_string_to_sexp(data, len);
}

SEXP rbedrock_iter_value(SEXP r_it,
                                SEXP r_error_if_invalid) {
    leveldb_iterator_t *it = get_iterator(r_it);
    if(!check_iterator(it, r_error_if_invalid)) {
        return R_NilValue;
    }
    size_t len;
    const char *data = leveldb_iter_value(it, &len);
    return raw_string_to_sexp(data, len);
}

/**** SNAPSHOT SUPPORT ****/

static const leveldb_snapshot_t *get_snapshot(SEXP r_ptr) {
    return get_external_address(r_ptr, true, NULL);
}

static void finalize_snapshot(SEXP r_ptr) {
    const leveldb_snapshot_t *snapshot = get_external_address(r_ptr, false, NULL);
    if(snapshot == NULL) {
        return;
    }
    SEXP r_prot = R_ExternalPtrProtected(r_ptr);
    leveldb_t *db = get_external_address(VECTOR_ELT(r_prot, SNAP_PROT_DB), false, NULL);
    if(db) {
        leveldb_release_snapshot(db, snapshot);
        SEXP cell = VECTOR_ELT(r_prot, SNAP_PROT_CELL);
        if(cell != R_NilValue) {
            double_list_remove(cell);
        }
    }
    R_ClearExternalPtr(r_ptr);
}

SEXP rbedrock_db_snapshot_create(SEXP r_db) {
    leveldb_t *db = get_db_ptr(r_db);
    const leveldb_snapshot_t *snapshot = leveldb_create_snapshot(db);

    SEXP r_snapshot = PROTECT(R_MakeExternalPtr((void*)snapshot, Rf_install("rbedrock_bedrockdb_snapshot"), R_NilValue));
    R_RegisterCFinalizerEx(r_snapshot, finalize_snapshot, false);

    SEXP r_db_prot = get_protected(r_db);
    SEXP r_snaps = VECTOR_ELT(r_db_prot, PROT_SNAPSHOTS);
    SEXP cell = double_list_insert(r_snaps, r_snapshot);

    // Allocate metadata 
    SEXP r_prot = PROTECT(allocVector(VECSXP, SNAP_PROT_LENGTH));
    SET_VECTOR_ELT(r_prot, SNAP_PROT_CELL, cell);
    SET_VECTOR_ELT(r_prot, SNAP_PROT_DB,   r_db);

    R_SetExternalPtrProtected(r_snapshot, r_prot);

    UNPROTECT(2);
    return r_snapshot;
}

SEXP rbedrock_db_snapshot_release(SEXP r_db, SEXP r_snapshot, SEXP r_error_if_released) {
    bool raise_error = scalar_logical(r_error_if_released);

    leveldb_t *db = get_db_ptr(r_db);
    const leveldb_snapshot_t *snapshot = get_external_address(r_snapshot, raise_error, NULL);
    if(db == NULL || snapshot == NULL) {
        return R_NilValue;
    }
    leveldb_release_snapshot(db, snapshot);
    SEXP r_prot = R_ExternalPtrProtected(r_snapshot);
    SEXP cell = VECTOR_ELT(r_prot, SNAP_PROT_CELL);
    if(cell != R_NilValue) {
        double_list_remove(cell);
    }
    R_ClearExternalPtr(r_snapshot);
    return R_NilValue;
}

SEXP rbedrock_snapshot_isnil(SEXP r_snapshot) {
    const leveldb_snapshot_t *snapshot = get_external_address(r_snapshot, false, NULL);
    return ScalarLogical(snapshot == NULL);
}

/**** READOPTIONS ****/

static leveldb_readoptions_t * create_readoptions(SEXP r_list) {
    if(Rf_isNull(r_list)) {
        return leveldb_readoptions_create();
    }
    if(TYPEOF(r_list) != VECSXP) {
        Rf_error("Invalid readoptions: object is not a list.");
    }
    // Default values
    bool verify_checksums = false;
    bool fill_cache = true;
    const leveldb_snapshot_t * snapshot = NULL;

    // Read provided values
    SEXP names = Rf_getAttrib(r_list, R_NamesSymbol);
    for (R_xlen_t i = 0; i < XLENGTH(r_list); i++) {
        const char *str = CHAR(STRING_ELT(names, i));
        if(strcmp(str, "verify_checksums") == 0) {
            verify_checksums = scalar_logical(VECTOR_ELT(r_list, i));
        } else if(strcmp(str, "fill_cache") == 0) {
            fill_cache = scalar_logical(VECTOR_ELT(r_list, i));
        } else if(strcmp(str, "snapshot") == 0) {
            snapshot = get_snapshot(VECTOR_ELT(r_list, i));
        }
    }

    // Create and setup readoptions.
    leveldb_readoptions_t *options = leveldb_readoptions_create();
    leveldb_readoptions_set_verify_checksums(options, verify_checksums);
    leveldb_readoptions_set_fill_cache(options, fill_cache);
    leveldb_readoptions_set_snapshot(options, snapshot);
    return options;
}

SEXP wrap_readoptions(leveldb_readoptions_t *ptr) {
    SEXP r_ptr = PROTECT(R_MakeExternalPtr(ptr,
        Rf_install("rbedrock_bedrockdb_readoptions"), R_NilValue));
    R_RegisterCFinalizerEx(r_ptr, finalize_readoptions, false);
    UNPROTECT(1);
    return r_ptr;
}

void finalize_readoptions(SEXP r_ptr) {
    leveldb_readoptions_t *opt = get_external_address(r_ptr, false, NULL);
    if(opt != NULL) {
        leveldb_readoptions_destroy(opt);
    }
    R_ClearExternalPtr(r_ptr);
}

/**** WRITEOPTIONS ****/

static leveldb_writeoptions_t * create_writeoptions(SEXP r_list) {
    if(Rf_isNull(r_list)) {
        return leveldb_writeoptions_create();
    }
    if(TYPEOF(r_list) != VECSXP) {
        Rf_error("Invalid writeoptions: object is not a list.");
    }
    // Default values
    bool sync = false;

    // Read provided values
    SEXP names = Rf_getAttrib(r_list, R_NamesSymbol);
    for (R_xlen_t i = 0; i < XLENGTH(r_list); i++) {
        const char *str = CHAR(STRING_ELT(names, i));
        if(strcmp(str, "sync") == 0) {
            sync = scalar_logical(VECTOR_ELT(r_list, i));
        }
    }

    // Create and setup readoptions.
    leveldb_writeoptions_t *options = leveldb_writeoptions_create();
    leveldb_writeoptions_set_sync(options, sync);
    return options;
}

SEXP wrap_writeoptions(leveldb_writeoptions_t *ptr) {
    SEXP r_ptr = PROTECT(R_MakeExternalPtr(ptr,
        Rf_install("rbedrock_bedrockdb_writeoptions"), R_NilValue));
    R_RegisterCFinalizerEx(r_ptr, finalize_writeoptions, false);
    UNPROTECT(1);
    return r_ptr;
}

void finalize_writeoptions(SEXP r_ptr) {
    leveldb_writeoptions_t *opt = get_external_address(r_ptr, false, NULL);
    if(opt != NULL) {
        leveldb_writeoptions_destroy(opt);
    }
    R_ClearExternalPtr(r_ptr);
}

// Built on top of the leveldb api.
SEXP rbedrock_db_keys(SEXP r_db, SEXP r_starts_with,
                          SEXP r_readoptions) {
    leveldb_t *db = get_db_ptr(r_db);
    const char *starts_with = NULL;
    const size_t starts_with_len = get_starts_with(r_starts_with, &starts_with);

    SEXP r_ret = PROTECT(create_stretchy_list());
    SEXP r_value;

    SEXP r_it = PROTECT(create_iterator(r_db, r_readoptions));
    leveldb_iterator_t *it = R_ExternalPtrAddr(r_it);

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

    // cleanup
    leveldb_iter_destroy(it);
    R_ClearExternalPtr(r_it);
    UNPROTECT(2);
    return Rf_PairToVectorList(CDR(r_ret));
}

SEXP rbedrock_db_keys_len(SEXP r_db, SEXP r_starts_with,
                              SEXP r_readoptions) {
    leveldb_t *db = get_db_ptr(r_db);
    leveldb_readoptions_t *readoptions = create_readoptions(r_readoptions);
    SEXP r_ptr = PROTECT(wrap_readoptions(readoptions));

    const char *starts_with = NULL;
    const size_t starts_with_len = get_starts_with(r_starts_with, &starts_with);
    int ret = (int)keys_len_impl(db, starts_with, starts_with_len, readoptions);

    leveldb_readoptions_destroy(readoptions);
    R_ClearExternalPtr(r_ptr);
    UNPROTECT(1);
    return ScalarInteger(ret);
}

SEXP rbedrock_db_exists(SEXP r_db, SEXP r_key, SEXP r_readoptions) {
    leveldb_t *db = get_db_ptr(r_db);
    leveldb_readoptions_t *readoptions = create_readoptions(r_readoptions);
    SEXP r_ptr = PROTECT(wrap_readoptions(readoptions));

    const char **key_data = NULL;
    size_t *key_len = NULL;
    size_t num_key = get_keys(r_key, &key_data, &key_len);
    SEXP r_found = PROTECT(allocVector(LGLSXP, num_key));
    int *found = INTEGER(r_found);
    exists_impl(db, num_key, key_data, key_len, readoptions, found);

    leveldb_readoptions_destroy(readoptions);
    R_ClearExternalPtr(r_ptr);
    UNPROTECT(2);
    return r_found;
}

SEXP rbedrock_leveldb_version() {
    SEXP ret = PROTECT(allocVector(INTSXP, 2));
    INTEGER(ret)[0] = leveldb_major_version();
    INTEGER(ret)[1] = leveldb_minor_version();
    UNPROTECT(1);
    return ret;
}

size_t keys_len_impl(leveldb_t *db, const char *starts_with,
                                    size_t starts_with_len,
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
void exists_impl(leveldb_t *db, size_t num_key,
                 const char **key_data, size_t *key_len,
                 leveldb_readoptions_t *readoptions,
                 int *found) {
    leveldb_iterator_t *it = leveldb_create_iterator(db, readoptions);
    for(size_t i = 0; i < num_key; ++i) {
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

leveldb_options_t *rbedrock_db_collect_options(
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
                          size_t starts_with_len) {
    if(starts_with_len == 0) {
        return true;
    }
    size_t key_len;
    const char *key_data = leveldb_iter_key(it, &key_len);
    return key_len >= starts_with_len &&
           memcmp(key_data, starts_with, starts_with_len) == 0;
}

// Debugging function
SEXP rbedrock_db_prot(SEXP r_db) {
    return get_protected(r_db);
}

// Propagate leveldb error
static void handle_leveldb_error(char *err) {
    if(err == NULL) {
        return;
    }
    // err has been allocated by leveldb. We need to copy it into
    // an R-managed buffer before throwing the error.
    size_t len = strlen(err);
    char *msg = (char *)R_alloc(len + 1, sizeof(char));
    memcpy(msg, err, len + 1);
    leveldb_free(err);
    Rf_error(msg);
}

/**** ADDITIONAL FUNCTIONS ****/

SEXP rbedrock_db_approximate_sizes(SEXP r_db, SEXP r_start_key,
                                       SEXP r_limit_key) {
    leveldb_t *db = get_db_ptr(r_db);

    const char **start_key = NULL, **limit_key = NULL;
    size_t *start_key_len = NULL, *limit_key_len = NULL;
    size_t num_start = get_keys(r_start_key, &start_key, &start_key_len),
           num_limit = get_keys(r_limit_key, &limit_key, &limit_key_len);
    if(num_start != num_limit) {
        Rf_error("Expected 'limit_key' to be a length %d vector", num_start);
    }

    uint64_t *sizes = (uint64_t *)R_alloc(num_start, sizeof(uint64_t));
    leveldb_approximate_sizes(db, (int)num_start, start_key, start_key_len,
                              limit_key, limit_key_len, sizes);
    SEXP ret = PROTECT(allocVector(INTSXP, num_start));
    int *isizes = INTEGER(ret);
    for(size_t i = 0; i < num_start; ++i) {
        isizes[i] = (int)sizes[i];
    }
    UNPROTECT(1);
    return ret;
}

SEXP rbedrock_db_compact_range(SEXP r_db, SEXP r_start_key,
                                   SEXP r_limit_key) {
    leveldb_t *db = get_db_ptr(r_db);
    const char *start_key = NULL, *limit_key = NULL;
    size_t start_key_len = get_key_maybe_nil(r_start_key, &start_key),
           limit_key_len = get_key_maybe_nil(r_limit_key, &limit_key);
    leveldb_compact_range(db, start_key, start_key_len, limit_key,
                          limit_key_len);
    return R_NilValue;
}
