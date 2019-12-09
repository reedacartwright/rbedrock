#include "rleveldb.h"

#include <stdbool.h>
#include "leveldb-mcpe/include/leveldb/c.h"
#include "support.h"

leveldb_readoptions_t * default_readoptions;
leveldb_writeoptions_t * default_writeoptions;
// Internals:
leveldb_t* rleveldb_get_db(SEXP r_db, bool closed_error);
leveldb_iterator_t* rleveldb_get_iterator(SEXP r_it, bool closed_error);
leveldb_snapshot_t* rleveldb_get_snapshot(SEXP r_snapshot, bool closed_error);
leveldb_writebatch_t* rleveldb_get_writebatch(SEXP r_writebatch,
                                              bool closed_error);
leveldb_readoptions_t* rleveldb_get_readoptions(SEXP r_readoptions,
                                                bool closed_error);
leveldb_writeoptions_t* rleveldb_get_writeoptions(SEXP r_writeoptions,
                                                  bool closed_error);
bool check_iterator(leveldb_iterator_t *it, SEXP r_error_if_invalid);


// Finalisers
static void rleveldb_finalize(SEXP r_db);
static void rleveldb_iter_finalize(SEXP r_it);
static void rleveldb_snapshot_finalize(SEXP r_snapshot);
static void rleveldb_writebatch_finalize(SEXP r_writebatch);
static void rleveldb_readoptions_finalize(SEXP r_readoptions);
static void rleveldb_writeoptions_finalize(SEXP r_writeoptions);
static void rleveldb_cache_finalize(SEXP r_cache);
static void rleveldb_filterpolicy_finalize(SEXP r_filterpolicy);


// Other internals
void rleveldb_handle_error(char* err);
leveldb_options_t* rleveldb_collect_options(SEXP r_create_if_missing,
                                            SEXP r_error_if_exists,
                                            SEXP r_paranoid_checks,
                                            SEXP r_write_buffer_size,
                                            SEXP r_max_open_files,
                                            SEXP r_block_size,
                                            SEXP r_use_compression);
bool iter_key_starts_with(leveldb_iterator_t *it, const char *starts_with,
                          size_t starts_with_len);

// Slightly different
size_t rleveldb_get_keys_len(leveldb_t *db,
                             const char *starts_with, size_t starts_with_len,
                             leveldb_readoptions_t *readoptions);
void rleveldb_get_exists(leveldb_t *db, size_t num_key,
                         const char **key_data, size_t *key_len,
                         leveldb_readoptions_t *readoptions, int *found);

enum rleveldb_tag_index {
  TAG_PATH,
  TAG_CACHE,
  TAG_FILTERPOLICY,
  TAG_ITERATORS,
  TAG_LENGTH // don't store anything here!
};

// Implementations:
SEXP rleveldb_open(SEXP r_path,
                      SEXP r_create_if_missing,
                      SEXP r_error_if_exists,
                      SEXP r_paranoid_checks,
                      SEXP r_write_buffer_size,
                      SEXP r_max_open_files,
                      SEXP r_block_size,
                      SEXP r_use_compression,
                      SEXP r_cache_capacity,
                      SEXP r_bloom_filter_bits_per_key) {
  // Unimplemented options:
  // * a general set_filter_policy
  // * set_env
  // * set_info_log
  // * set_comparator
  // * restart_interval
  //
  // There is some gymnastics here to avoid leaking in the case of an
  // R error (perhaps thrown by the coersion functions).
  SEXP r_cache_ptr = R_NilValue, r_filterpolicy_ptr = R_NilValue;
  leveldb_cache_t* cache = NULL;
  leveldb_filterpolicy_t* filterpolicy = NULL;
  bool
    has_cache = r_cache_capacity != R_NilValue,
    has_filterpolicy = r_bloom_filter_bits_per_key != R_NilValue;
  if (has_cache) {
    cache = leveldb_cache_create_lru(scalar_size(r_cache_capacity));
    r_cache_ptr = PROTECT(R_MakeExternalPtr(cache, R_NilValue, R_NilValue));
    R_RegisterCFinalizer(r_cache_ptr, rleveldb_cache_finalize);
  }
  if (has_filterpolicy) {
    size_t bits_per_key = scalar_size(r_bloom_filter_bits_per_key);
    filterpolicy = leveldb_filterpolicy_create_bloom(bits_per_key);
    r_filterpolicy_ptr =
      PROTECT(R_MakeExternalPtr(filterpolicy, R_NilValue, R_NilValue));
    R_RegisterCFinalizer(r_filterpolicy_ptr, rleveldb_filterpolicy_finalize);
  }
  const char *path = scalar_character(r_path);
  leveldb_options_t *options =
    rleveldb_collect_options(r_create_if_missing, r_error_if_exists,
                             r_paranoid_checks, r_write_buffer_size,
                             r_max_open_files, r_block_size,
                             r_use_compression);
  if (has_cache) {
    leveldb_options_set_cache(options, cache);
  }
  if (has_filterpolicy) {
    leveldb_options_set_filter_policy(options, filterpolicy);
  }

  char *err = NULL;
  leveldb_t *db = leveldb_open(options, path, &err);
  leveldb_options_destroy(options);
  rleveldb_handle_error(err);

  SEXP tag = PROTECT(allocVector(VECSXP, TAG_LENGTH));
  SET_VECTOR_ELT(tag, TAG_PATH, r_path);
  SET_VECTOR_ELT(tag, TAG_CACHE, r_cache_ptr);
  SET_VECTOR_ELT(tag, TAG_FILTERPOLICY, r_filterpolicy_ptr);
  SET_VECTOR_ELT(tag, TAG_ITERATORS, R_NilValue); // will be a pairlist

  SEXP r_db = PROTECT(R_MakeExternalPtr(db, tag, R_NilValue));
  R_RegisterCFinalizer(r_db, rleveldb_finalize);
  UNPROTECT(2 + has_cache + has_filterpolicy);
  return r_db;
}

// TODO: this needs to happen during finalize too!
SEXP rleveldb_close(SEXP r_db, SEXP r_error_if_closed) {
  leveldb_t *db = rleveldb_get_db(r_db, scalar_logical(r_error_if_closed));
  if (db != NULL) {
    SEXP tag = rleveldb_tag(r_db);
    SEXP r_iterators = VECTOR_ELT(tag, TAG_ITERATORS);
    while (r_iterators != R_NilValue) {
      rleveldb_iter_destroy(CAR(r_iterators), ScalarLogical(false));
      r_iterators = CDR(r_iterators);
    }
    leveldb_close(db);
    R_ClearExternalPtr(r_db);
  }
  return ScalarLogical(db != NULL);
}

SEXP rleveldb_destroy(SEXP r_path) {
  const char *path = scalar_character(r_path);
  leveldb_options_t *options = leveldb_options_create();
  char *err = NULL;
  leveldb_destroy_db(options, path, &err);
  leveldb_options_destroy(options);
  rleveldb_handle_error(err);
  return ScalarLogical(true);
}

SEXP rleveldb_repair(SEXP r_path) {
  const char *path = scalar_character(r_path);
  leveldb_options_t *options = leveldb_options_create();
  char *err = NULL;
  leveldb_repair_db(options, path, &err);
  leveldb_options_destroy(options);
  rleveldb_handle_error(err);
  return ScalarLogical(true);
}

SEXP rleveldb_property(SEXP r_db, SEXP r_name, SEXP r_error_if_missing) {
  leveldb_t *db = rleveldb_get_db(r_db, true);
  const char *name = scalar_character(r_name);
  bool error_if_missing = scalar_logical(r_error_if_missing);
  char *value = leveldb_property_value(db, name);
  SEXP ret;
  if (value != NULL) {
    ret = mkString(value);
    leveldb_free(value);
  } else if (error_if_missing) {
    Rf_error("No such property '%s'", name);
  } else {
    ret = R_NilValue;
  }
  return ret;
}

SEXP rleveldb_get(SEXP r_db, SEXP r_key, SEXP r_as_raw,
                  SEXP r_error_if_missing, SEXP r_readoptions) {
  leveldb_t *db = rleveldb_get_db(r_db, true);
  const char *key_data = NULL;
  size_t key_len = get_key(r_key, &key_data);

  bool error_if_missing = scalar_logical(r_error_if_missing);
  return_as as_raw = to_return_as(r_as_raw);
  leveldb_readoptions_t *readoptions =
    rleveldb_get_readoptions(r_readoptions, true);

  char *err = NULL;
  size_t read_len;
  char* read = leveldb_get(db, readoptions, key_data, key_len, &read_len, &err);
  rleveldb_handle_error(err);

  SEXP ret;
  if (read != NULL) {
    ret = raw_string_to_sexp(read, read_len, as_raw);
    leveldb_free(read);
  } else if (!error_if_missing) {
    ret = R_NilValue;
  } else if (TYPEOF(r_key) == STRSXP) {
    Rf_error("Key '%s' not found in database", key_data);
  } else {
    Rf_error("Key not found in database");
  }

  return ret;
}

SEXP rleveldb_mget(SEXP r_db, SEXP r_key, SEXP r_as_raw,
                   SEXP r_missing_value, SEXP r_missing_report,
                   SEXP r_readoptions) {
  leveldb_t *db = rleveldb_get_db(r_db, true);
  leveldb_readoptions_t *readoptions =
    rleveldb_get_readoptions(r_readoptions, true);
  return_as as_raw = to_return_as(r_as_raw);
  bool missing_report = scalar_logical(r_missing_report);
  if (as_raw == AS_STRING) {
    if (r_missing_value == R_NilValue) {
      r_missing_value = NA_STRING;
    } else if (TYPEOF(r_missing_value) != STRSXP ||
               length(r_missing_value) != 1) {
      Rf_error("If as_raw = TRUE, missing_value must be a scalar character");
    } else {
      r_missing_value = STRING_ELT(r_missing_value, 0);
    }
  }

  const char **key_data = NULL;
  size_t *key_len = NULL;
  size_t num_key = get_keys(r_key, &key_data, &key_len);
  bool *missing = NULL;

  SEXPTYPE ret_type = as_raw == AS_STRING ? STRSXP : VECSXP;
  SEXP ret = PROTECT(allocVector(ret_type, num_key));

  size_t n_missing = 0;
  for (size_t i = 0; i < num_key; ++i) {
    char *err = NULL;
    size_t read_len;
    char* read = leveldb_get(db, readoptions, key_data[i], key_len[i],
                             &read_len, &err);
    rleveldb_handle_error(err);
    if (read != NULL) {
      SEXP el = PROTECT(raw_string_to_sexp(read, read_len, as_raw));
      if (as_raw == AS_STRING) {
        SET_STRING_ELT(ret, i, STRING_ELT(el, 0));
      } else {
        SET_VECTOR_ELT(ret, i, el);
      }
      leveldb_free(read);
      UNPROTECT(1);
    } else {
      if (as_raw == AS_STRING) {
        SET_STRING_ELT(ret, i, r_missing_value);
      } else {
        SET_VECTOR_ELT(ret, i, r_missing_value);
      }
      if (missing_report) {
        n_missing++;
        if (missing == NULL) {
          missing = (bool*) R_alloc(num_key, sizeof(bool));
          memset(missing, 0, num_key * sizeof(bool));
        }
        missing[i] = true;
      }
    }
  }

  if (n_missing > 0) {
    SEXP ret_missing = PROTECT(allocVector(INTSXP, n_missing));
    for (size_t i = 0, j = 0; i < num_key; ++i) {
      if (missing[i]) {
        INTEGER(ret_missing)[j++] = i + 1;
      }
    }
    setAttrib(ret, install("missing"), ret_missing);
    UNPROTECT(1);
  }

  UNPROTECT(1);
  return ret;
}

SEXP rleveldb_put(SEXP r_db, SEXP r_key, SEXP r_value, SEXP r_writeoptions) {
  leveldb_t *db = rleveldb_get_db(r_db, true);
  leveldb_writeoptions_t *writeoptions =
    rleveldb_get_writeoptions(r_writeoptions, true);
  const char *key_data = NULL, *value_data = NULL;
  size_t
    key_len = get_key(r_key, &key_data),
    value_len = get_value(r_value, &value_data);

  char *err = NULL;
  leveldb_put(db, writeoptions, key_data, key_len, value_data, value_len, &err);
  rleveldb_handle_error(err);

  return R_NilValue;
}

// This is a slightly odd construction and could be done entirely in R
// space (indeed, perhaps it should be?).  But using the higher level
// R API here means that we can avoid leaks of the writebatch object
// if any of the keys can't be extracted.  The total cost of doing
// this is at most a couple of allocations and it avoids a lot of
// duplicated code.
SEXP rleveldb_mput(SEXP r_db, SEXP r_key, SEXP r_value, SEXP r_writeoptions) {
  SEXP r_writebatch = PROTECT(rleveldb_writebatch_create());
  rleveldb_writebatch_mput(r_writebatch, r_key, r_value);
  rleveldb_write(r_db, r_writebatch, r_writeoptions);
  UNPROTECT(1);
  return R_NilValue;
}

SEXP rleveldb_delete(SEXP r_db, SEXP r_key, SEXP r_report,
                     SEXP r_readoptions, SEXP r_writeoptions) {
  if (scalar_logical(r_report)) {
    return rleveldb_delete_report(r_db, r_key, r_readoptions, r_writeoptions);
  } else {
    return rleveldb_delete_silent(r_db, r_key, r_writeoptions);
  }
}

// This is the simple delete: it just deletes things and does not
// report back anything about what was done (these keys may or may not
// exist).
SEXP rleveldb_delete_silent(SEXP r_db, SEXP r_key, SEXP r_writeoptions) {
  leveldb_t *db = rleveldb_get_db(r_db, true);
  const char **key_data = NULL;
  size_t *key_len = NULL;
  size_t num_key = get_keys(r_key, &key_data, &key_len);
  leveldb_writeoptions_t *writeoptions =
    rleveldb_get_writeoptions(r_writeoptions, true);

  for (size_t i = 0; i < num_key; ++i) {
    char *err = NULL;
    leveldb_delete(db, writeoptions, key_data[i], key_len[i], &err);
    rleveldb_handle_error(err);
  }

  return R_NilValue;
}

// This is quite a bit more complicated; we first iterate through and
// find out what exists, arranging to return that back to R in the
// first place.  Then we go through and do the deletion.
SEXP rleveldb_delete_report(SEXP r_db, SEXP r_key, SEXP r_readoptions,
                            SEXP r_writeoptions) {
  leveldb_t *db = rleveldb_get_db(r_db, true);
  const char **key_data = NULL;
  size_t *key_len = NULL;
  size_t num_key = get_keys(r_key, &key_data, &key_len);

  // This might fail so I'm doing it up here
  leveldb_writeoptions_t *writeoptions =
    rleveldb_get_writeoptions(r_writeoptions, true);

  SEXP r_found = PROTECT(allocVector(LGLSXP, num_key));
  int *found = INTEGER(r_found);

  leveldb_readoptions_t *readoptions = default_readoptions;
  // NOTE: leak danger on throw, so nothing between here and the
  // writebatch_destroys may throw (and therefore can't use the R
  // API).
  leveldb_writebatch_t *writebatch = leveldb_writebatch_create();

  // First, work out what exists:
  rleveldb_get_exists(db, num_key, key_data, key_len, readoptions, found);

  bool do_delete = false;
  for (size_t i = 0; i < num_key; ++i) {
    if (found[i]) {
      leveldb_writebatch_delete(writebatch, key_data[i], key_len[i]);
      do_delete = true;
    }
  }

  if (do_delete) {
    char *err = NULL;
    leveldb_write(db, writeoptions, writebatch, &err);
    // NOTE: This must come here *and* in the else (but not outside
    // the if/else) because that way we don't leak the writebatch
    // object on error.
    leveldb_writebatch_destroy(writebatch);
    rleveldb_handle_error(err);
  } else {
    leveldb_writebatch_destroy(writebatch);
  }

  UNPROTECT(1);
  return r_found;
}

// Iterators
SEXP rleveldb_iter_create(SEXP r_db, SEXP r_readoptions) {
  leveldb_t *db = rleveldb_get_db(r_db, true);
  leveldb_readoptions_t *readoptions =
    rleveldb_get_readoptions(r_readoptions, true);
  leveldb_iterator_t *it = leveldb_create_iterator(db, readoptions);

  SEXP r_it = PROTECT(R_MakeExternalPtr(it, r_db, R_NilValue));
  R_RegisterCFinalizer(r_it, rleveldb_iter_finalize);

  SEXP db_tag = rleveldb_tag(r_db);
  SEXP r_iterators = VECTOR_ELT(db_tag, TAG_ITERATORS);
  SET_VECTOR_ELT(db_tag, TAG_ITERATORS, CONS(r_it, r_iterators));

  UNPROTECT(1);
  return r_it;
}

SEXP rleveldb_iter_destroy(SEXP r_it, SEXP r_error_if_destroyed) {
  bool error_if_destroyed = scalar_logical(r_error_if_destroyed);
  leveldb_iterator_t *it = rleveldb_get_iterator(r_it, error_if_destroyed);
  if (it != NULL) {
    leveldb_iter_destroy(it);
    R_ClearExternalPtr(r_it);
  }
  return ScalarLogical(it != NULL);
}

SEXP rleveldb_iter_valid(SEXP r_it) {
  leveldb_iterator_t *it = rleveldb_get_iterator(r_it, true);
  return ScalarLogical(leveldb_iter_valid(it));
}

SEXP rleveldb_iter_seek_to_first(SEXP r_it) {
  leveldb_iterator_t *it = rleveldb_get_iterator(r_it, true);
  leveldb_iter_seek_to_first(it);
  return R_NilValue;
}

SEXP rleveldb_iter_seek_to_last(SEXP r_it) {
  leveldb_iterator_t *it = rleveldb_get_iterator(r_it, true);
  leveldb_iter_seek_to_last(it);
  return R_NilValue;
}

SEXP rleveldb_iter_seek(SEXP r_it, SEXP r_key) {
  leveldb_iterator_t *it = rleveldb_get_iterator(r_it, true);
  const char *key_data = NULL;
  size_t key_len = get_key(r_key, &key_data);
  leveldb_iter_seek(it, key_data, key_len);
  return R_NilValue;
}

SEXP rleveldb_iter_next(SEXP r_it, SEXP r_error_if_invalid) {
  leveldb_iterator_t *it = rleveldb_get_iterator(r_it, true);
  if (check_iterator(it, r_error_if_invalid)) {
    leveldb_iter_next(it);
  }
  return R_NilValue;
}

SEXP rleveldb_iter_prev(SEXP r_it, SEXP r_error_if_invalid) {
  leveldb_iterator_t *it = rleveldb_get_iterator(r_it, true);
  if (check_iterator(it, r_error_if_invalid)) {
    leveldb_iter_prev(it);
  }
  return R_NilValue;
}

SEXP rleveldb_iter_key(SEXP r_it, SEXP r_as_raw, SEXP r_error_if_invalid) {
  leveldb_iterator_t *it = rleveldb_get_iterator(r_it, true);
  return_as as_raw = to_return_as(r_as_raw);
  size_t len;
  if (!check_iterator(it, r_error_if_invalid)) {
    return R_NilValue;
  }
  const char *data = leveldb_iter_key(it, &len);
  return raw_string_to_sexp(data, len, as_raw);
}

SEXP rleveldb_iter_value(SEXP r_it, SEXP r_as_raw, SEXP r_error_if_invalid) {
  leveldb_iterator_t *it = rleveldb_get_iterator(r_it, true);
  return_as as_raw = to_return_as(r_as_raw);
  if (!check_iterator(it, r_error_if_invalid)) {
    return R_NilValue;
  }
  size_t len;
  const char *data = leveldb_iter_value(it, &len);
  return raw_string_to_sexp(data, len, as_raw);
}

// Snapshots
SEXP rleveldb_snapshot_create(SEXP r_db) {
  leveldb_t *db = rleveldb_get_db(r_db, true);
  const leveldb_snapshot_t *snapshot = leveldb_create_snapshot(db);
  SEXP r_snapshot =
    PROTECT(R_MakeExternalPtr((void*) snapshot, r_db, R_NilValue));
  R_RegisterCFinalizer(r_snapshot, rleveldb_snapshot_finalize);
  UNPROTECT(1);
  return r_snapshot;
}

// Batch
SEXP rleveldb_writebatch_create() {
  leveldb_writebatch_t *writebatch = leveldb_writebatch_create();
  SEXP r_writebatch =
    PROTECT(R_MakeExternalPtr((void*) writebatch, R_NilValue, R_NilValue));
  R_RegisterCFinalizer(r_writebatch, rleveldb_writebatch_finalize);
  UNPROTECT(1);
  return r_writebatch;
}

SEXP rleveldb_writebatch_destroy(SEXP r_writebatch, SEXP r_error_if_destroyed) {
  bool error_if_destroyed = scalar_logical(r_error_if_destroyed);
  leveldb_writebatch_t *writebatch =
    rleveldb_get_writebatch(r_writebatch, error_if_destroyed);
  if (writebatch != NULL) {
    leveldb_writebatch_destroy(writebatch);
    R_ClearExternalPtr(r_writebatch);
  }
  return ScalarLogical(writebatch != NULL);
}

SEXP rleveldb_writebatch_clear(SEXP r_writebatch) {
  leveldb_writebatch_t *writebatch =
    rleveldb_get_writebatch(r_writebatch, true);
  leveldb_writebatch_clear(writebatch);
  return R_NilValue;
}

SEXP rleveldb_writebatch_put(SEXP r_writebatch, SEXP r_key, SEXP r_value) {
  leveldb_writebatch_t *writebatch =
    rleveldb_get_writebatch(r_writebatch, true);
  const char *key_data = NULL, *value_data = NULL;
  size_t
    key_len = get_key(r_key, &key_data),
    value_len = get_value(r_value, &value_data);
  leveldb_writebatch_put(writebatch, key_data, key_len, value_data, value_len);
  return R_NilValue;
}

SEXP rleveldb_writebatch_mput(SEXP r_writebatch, SEXP r_key, SEXP r_value) {
  leveldb_writebatch_t *writebatch =
    rleveldb_get_writebatch(r_writebatch, true);
  const char **key_data = NULL;
  size_t *key_len = NULL;
  size_t num_key = get_keys(r_key, &key_data, &key_len);

  const bool value_is_string = TYPEOF(r_value) == STRSXP;
  if (!value_is_string && TYPEOF(r_value) != VECSXP) {
    Rf_error("Expected a character vector or list for 'value'");
  }
  if ((size_t)length(r_value) != num_key) {
    Rf_error("Expected %d values but recieved %d", num_key, length(r_value));
  }
  for (size_t i = 0; i < num_key; ++i) {
    const char *value_data;
    SEXP el = value_is_string ? STRING_ELT(r_value, i) : VECTOR_ELT(r_value, i);
    size_t value_len = get_value(el, &value_data);
    leveldb_writebatch_put(writebatch, key_data[i], key_len[i],
                           value_data, value_len);
  }

  return R_NilValue;
}

SEXP rleveldb_writebatch_delete(SEXP r_writebatch, SEXP r_key) {
  leveldb_writebatch_t *writebatch =
    rleveldb_get_writebatch(r_writebatch, true);
  const char *key_data = NULL;
  size_t key_len = get_key(r_key, &key_data);
  leveldb_writebatch_delete(writebatch, key_data, key_len);
  return R_NilValue;
}

// NOTE: arguments 2 & 3 transposed with respect to leveldb API
SEXP rleveldb_write(SEXP r_db, SEXP r_writebatch, SEXP r_writeoptions) {
  leveldb_t *db = rleveldb_get_db(r_db, true);
  leveldb_writeoptions_t *writeoptions =
    rleveldb_get_writeoptions(r_writeoptions, true);
  leveldb_writebatch_t *writebatch =
    rleveldb_get_writebatch(r_writebatch, true);
  char *err = NULL;
  leveldb_write(db, writeoptions, writebatch, &err);
  rleveldb_handle_error(err);
  return R_NilValue;
}

SEXP rleveldb_approximate_sizes(SEXP r_db, SEXP r_start_key, SEXP r_limit_key) {
  leveldb_t *db = rleveldb_get_db(r_db, true);

  const char **start_key = NULL, **limit_key = NULL;
  size_t *start_key_len = NULL, *limit_key_len = NULL;
  size_t
    num_start = get_keys(r_start_key, &start_key, &start_key_len),
    num_limit = get_keys(r_limit_key, &limit_key, &limit_key_len);
  if (num_start != num_limit) {
    Rf_error("Expected 'limit_key' to be a length %d vector", num_start);
  }

  uint64_t *sizes = (uint64_t*)R_alloc(num_start, sizeof(uint64_t));
  leveldb_approximate_sizes(db, num_start,
                            start_key, start_key_len,
                            limit_key, limit_key_len,
                            sizes);
  SEXP ret = PROTECT(allocVector(INTSXP, num_start));
  int *isizes = INTEGER(ret);
  for (size_t i = 0; i < num_start; ++i) {
    isizes[i] = sizes[i];
  }
  UNPROTECT(1);
  return ret;
}

SEXP rleveldb_compact_range(SEXP r_db, SEXP r_start_key, SEXP r_limit_key) {
  leveldb_t *db = rleveldb_get_db(r_db, true);
  const char *start_key = NULL, *limit_key = NULL;
  size_t
    start_key_len = get_key(r_start_key, &start_key),
    limit_key_len = get_key(r_limit_key, &limit_key);
  leveldb_compact_range(db, start_key, start_key_len, limit_key, limit_key_len);
  return R_NilValue;
}

// Options
SEXP rleveldb_readoptions(SEXP r_verify_checksums, SEXP r_fill_cache,
                          SEXP r_snapshot) {
  leveldb_readoptions_t * options = leveldb_readoptions_create();
  SEXP tag = PROTECT(allocVector(VECSXP, 3));
  SET_VECTOR_ELT(tag, 0, r_verify_checksums);
  SET_VECTOR_ELT(tag, 1, r_fill_cache);
  SET_VECTOR_ELT(tag, 2, r_snapshot);
  SEXP ret = PROTECT(R_MakeExternalPtr(options, tag, R_NilValue));
  R_RegisterCFinalizer(ret, rleveldb_readoptions_finalize);
  if (r_verify_checksums != R_NilValue) {
    bool verify_checksums = scalar_logical(r_verify_checksums);
    leveldb_readoptions_set_verify_checksums(options, verify_checksums);
  }
  if (r_fill_cache != R_NilValue) {
    leveldb_readoptions_set_fill_cache(options, scalar_logical(r_fill_cache));
  }
  if (r_snapshot != R_NilValue) {
    leveldb_readoptions_set_snapshot(options,
                                     rleveldb_get_snapshot(r_snapshot, true));
  }

  UNPROTECT(2);
  return ret;
}

SEXP rleveldb_writeoptions(SEXP r_sync) {
  leveldb_writeoptions_t * options = leveldb_writeoptions_create();
  SEXP tag = PROTECT(allocVector(VECSXP, 1));
  SET_VECTOR_ELT(tag, 0, r_sync);
  SEXP ret = PROTECT(R_MakeExternalPtr(options, tag, R_NilValue));
  R_RegisterCFinalizer(ret, rleveldb_writeoptions_finalize);
  if (r_sync != R_NilValue) {
    leveldb_writeoptions_set_sync(options, scalar_logical(r_sync));
  }
  UNPROTECT(2);
  return ret;
}

// Built on top of the leveldb api:

// TODO: at the moment this works with two passes; one computes the
// number of keys and the other collects the keys.  That's not a bad
// call, really, but it would be nice to be able to *collect* things.
// To do that efficiently we need a growing data structure.  It's
// possible that pairlists may accomplish that fairly effectively.
// This is going to be needed for a "match pattern" find function,
// though the bigger hurdle is going to be doing any sort of actual
// pattern matching aside from "starts with"
//
// In any case this would be nice to rework to use pairlists as I
// think that would be a nice solution and not too hard to implement.
SEXP rleveldb_keys(SEXP r_db, SEXP r_starts_with, SEXP r_as_raw,
                   SEXP r_readoptions) {
  leveldb_t *db = rleveldb_get_db(r_db, true);
  return_as as_raw = to_return_as(r_as_raw);
  leveldb_readoptions_t *readoptions =
    rleveldb_get_readoptions(r_readoptions, true);
  bool as_string = as_raw == AS_STRING;
  const char *starts_with = NULL;
  const size_t starts_with_len = get_starts_with(r_starts_with, &starts_with);

  size_t n = rleveldb_get_keys_len(db, starts_with, starts_with_len,
                                   readoptions);
  SEXP ret = PROTECT(allocVector(as_string ? STRSXP : VECSXP, n));

  leveldb_iterator_t *it = leveldb_create_iterator(db, readoptions);
  leveldb_iter_seek_to_first(it);
  size_t key_len;
  for (size_t i = 0; i < n; leveldb_iter_next(it)) {
    if (iter_key_starts_with(it, starts_with, starts_with_len)) {
      const char *key_data = leveldb_iter_key(it, &key_len);
      if (as_string) {
        SET_STRING_ELT(ret, i, mkCharLen(key_data, key_len));
      } else {
        SET_VECTOR_ELT(ret, i, raw_string_to_sexp(key_data, key_len, as_raw));
      }
      ++i;
    }
  }
  leveldb_iter_destroy(it);

  UNPROTECT(1);
  return ret;
}

SEXP rleveldb_keys_len(SEXP r_db, SEXP r_starts_with, SEXP r_readoptions) {
  leveldb_t *db = rleveldb_get_db(r_db, true);
  leveldb_readoptions_t *readoptions =
    rleveldb_get_readoptions(r_readoptions, true);
  const char *starts_with = NULL;
  const size_t starts_with_len = get_starts_with(r_starts_with, &starts_with);
  return ScalarInteger(rleveldb_get_keys_len(db, starts_with,
                                             starts_with_len, readoptions));
}

SEXP rleveldb_exists(SEXP r_db, SEXP r_key, SEXP r_readoptions) {
  leveldb_t *db = rleveldb_get_db(r_db, true);
  leveldb_readoptions_t *readoptions =
    rleveldb_get_readoptions(r_readoptions, true);
  const char **key_data = NULL;
  size_t *key_len = NULL;
  size_t num_key = get_keys(r_key, &key_data, &key_len);
  SEXP r_found = PROTECT(allocVector(LGLSXP, num_key));
  int *found = INTEGER(r_found);
  rleveldb_get_exists(db, num_key, key_data, key_len, readoptions, found);
  UNPROTECT(1);
  return r_found;
}

SEXP rleveldb_version() {
  SEXP ret = PROTECT(allocVector(INTSXP, 2));
  INTEGER(ret)[0] = leveldb_major_version();
  INTEGER(ret)[1] = leveldb_minor_version();
  UNPROTECT(1);
  return ret;
}

// For internal use:
SEXP rleveldb_tag(SEXP r_db) {
  return R_ExternalPtrTag(r_db);
}

// For package management:
void rleveldb_init() {
  default_readoptions = leveldb_readoptions_create();
  default_writeoptions = leveldb_writeoptions_create();
}

void rleveldb_cleanup() {
  leveldb_readoptions_destroy(default_readoptions); // #nocov
  leveldb_writeoptions_destroy(default_writeoptions); // #nocov
}

// Internal function definitions:
void rleveldb_finalize(SEXP r_db) {
  leveldb_t* db = rleveldb_get_db(r_db, false);
  if (db != NULL) {
    leveldb_close(db);
    R_ClearExternalPtr(r_db);
  }
}

void rleveldb_iter_finalize(SEXP r_it) {
  leveldb_iterator_t* it = rleveldb_get_iterator(r_it, false);
  if (it != NULL) {
    leveldb_iter_destroy(it);
    R_ClearExternalPtr(r_it);
  }
}

void rleveldb_snapshot_finalize(SEXP r_snapshot) {
  leveldb_snapshot_t* snapshot = rleveldb_get_snapshot(r_snapshot, false);
  if (snapshot != NULL) {
    leveldb_t *db = rleveldb_get_db(rleveldb_tag(r_snapshot), false);
    if (db) {
      leveldb_release_snapshot(db, snapshot);
    }
    R_ClearExternalPtr(r_snapshot);
  }
}

void rleveldb_writebatch_finalize(SEXP r_writebatch) {
  leveldb_writebatch_t* writebatch =
    rleveldb_get_writebatch(r_writebatch, false);
  if (writebatch) {
    leveldb_writebatch_destroy(writebatch);
    R_ClearExternalPtr(r_writebatch);
  }
}

void rleveldb_readoptions_finalize(SEXP r_readoptions) {
  leveldb_readoptions_t* readoptions =
    rleveldb_get_readoptions(r_readoptions, false);
  if (readoptions) {
    leveldb_readoptions_destroy(readoptions);
    R_ClearExternalPtr(r_readoptions);
  }
}

void rleveldb_writeoptions_finalize(SEXP r_writeoptions) {
  leveldb_writeoptions_t* writeoptions =
    rleveldb_get_writeoptions(r_writeoptions, false);
  if (writeoptions) {
    leveldb_writeoptions_destroy(writeoptions);
    R_ClearExternalPtr(r_writeoptions);
  }
}

void rleveldb_cache_finalize(SEXP r_cache) {
  if (TYPEOF(r_cache) == EXTPTRSXP) {
    leveldb_cache_t* cache = (leveldb_cache_t*) R_ExternalPtrAddr(r_cache);
    if (cache) {
      leveldb_cache_destroy(cache);
      R_ClearExternalPtr(r_cache);
    }
  }
}

void rleveldb_filterpolicy_finalize(SEXP r_filterpolicy) {
  if (TYPEOF(r_filterpolicy) == EXTPTRSXP) {
    leveldb_filterpolicy_t* filterpolicy =
      (leveldb_filterpolicy_t*) R_ExternalPtrAddr(r_filterpolicy);
    if (filterpolicy) {
      leveldb_filterpolicy_destroy(filterpolicy);
      R_ClearExternalPtr(r_filterpolicy);
    }
  }
}

leveldb_t* rleveldb_get_db(SEXP r_db, bool closed_error) {
  void *db = NULL;
  if (TYPEOF(r_db) != EXTPTRSXP) {
    Rf_error("Expected an external pointer");
  }
  db = (leveldb_t*) R_ExternalPtrAddr(r_db);
  if (!db && closed_error) {
    Rf_error("leveldb handle is not open; can't connect");
  }
  return (leveldb_t*) db;
}

// TODO: distinguish here between an iterator and db handle by
// checking the SEXP on the tag?
leveldb_iterator_t* rleveldb_get_iterator(SEXP r_it, bool closed_error) {
  void *it = NULL;
  if (TYPEOF(r_it) != EXTPTRSXP) {
    Rf_error("Expected an external pointer");
  }
  it = (leveldb_iterator_t*) R_ExternalPtrAddr(r_it);
  if (!it && closed_error) {
    Rf_error("leveldb iterator is not open; can't connect");
  }
  return (leveldb_iterator_t*) it;
}

leveldb_snapshot_t* rleveldb_get_snapshot(SEXP r_snapshot, bool closed_error) {
  void *snapshot = NULL;
  if (TYPEOF(r_snapshot) != EXTPTRSXP) {
    Rf_error("Expected an external pointer");
  }
  snapshot = (leveldb_snapshot_t*) R_ExternalPtrAddr(r_snapshot);
  if (!snapshot && closed_error) {
    Rf_error("leveldb snapshot is not open; can't connect");
  }
  return (leveldb_snapshot_t*) snapshot;
}

leveldb_writebatch_t* rleveldb_get_writebatch(SEXP r_writebatch, bool closed_error) {
  void *writebatch = NULL;
  if (TYPEOF(r_writebatch) != EXTPTRSXP) {
    Rf_error("Expected an external pointer");
  }
  writebatch = (leveldb_writebatch_t*) R_ExternalPtrAddr(r_writebatch);
  if (!writebatch && closed_error) {
    Rf_error("leveldb writebatch is not open; can't connect");
  }
  return (leveldb_writebatch_t*) writebatch;
}

leveldb_readoptions_t* rleveldb_get_readoptions(SEXP r_readoptions,
                                                bool closed_error) {
  if (r_readoptions == R_NilValue) {
    return default_readoptions;
  }
  void *readoptions = NULL;
  if (TYPEOF(r_readoptions) != EXTPTRSXP) {
    Rf_error("Expected an external pointer");
  }
  readoptions = (leveldb_readoptions_t*) R_ExternalPtrAddr(r_readoptions);
  if (!readoptions && closed_error) {
    Rf_error("leveldb readoptions is not open; can't connect");
  }
  return (leveldb_readoptions_t*) readoptions;
}

leveldb_writeoptions_t* rleveldb_get_writeoptions(SEXP r_writeoptions,
                                                  bool closed_error) {
  if (r_writeoptions == R_NilValue) {
    return default_writeoptions;
  }
  void *writeoptions = NULL;
  if (TYPEOF(r_writeoptions) != EXTPTRSXP) {
    Rf_error("Expected an external pointer");
  }
  writeoptions = (leveldb_writeoptions_t*) R_ExternalPtrAddr(r_writeoptions);
  if (!writeoptions && closed_error) {
    Rf_error("leveldb writeoptions is not open; can't connect");
  }
  return (leveldb_writeoptions_t*) writeoptions;
}

void rleveldb_handle_error(char* err) {
  if (err != NULL) {
    size_t len = strlen(err);
    char * msg = (char*) R_alloc(len + 1, sizeof(char));
    memcpy(msg, err, len + 1);
    leveldb_free(err);
    error(msg);
  }
}

size_t rleveldb_get_keys_len(leveldb_t *db,
                             const char *starts_with, size_t starts_with_len,
                             leveldb_readoptions_t *readoptions) {
  leveldb_iterator_t *it = leveldb_create_iterator(db, readoptions);
  size_t n = 0;
  for (leveldb_iter_seek_to_first(it);
       leveldb_iter_valid(it);
       leveldb_iter_next(it)) {
    if (iter_key_starts_with(it, starts_with, starts_with_len)) {
      ++n;
    }
  }
  leveldb_iter_destroy(it);
  return n;
}

// NOTE: this uses `int*` for found, not `bool*` because it is
// designed to work with passing things back using an R LGLSXP (where
// things are stored as integers because of NA values)
void rleveldb_get_exists(leveldb_t *db, size_t num_key,
                         const char **key_data, size_t *key_len,
                         leveldb_readoptions_t *readoptions, int *found) {
  leveldb_iterator_t *it = leveldb_create_iterator(db, readoptions);
  for (size_t i = 0; i < num_key; ++i) {
    leveldb_iter_seek(it, key_data[i], key_len[i]);
    if (leveldb_iter_valid(it)) {
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

leveldb_options_t* rleveldb_collect_options(SEXP r_create_if_missing,
                                            SEXP r_error_if_exists,
                                            SEXP r_paranoid_checks,
                                            SEXP r_write_buffer_size,
                                            SEXP r_max_open_files,
                                            SEXP r_block_size,
                                            SEXP r_use_compression) {
  leveldb_options_t *options = leveldb_options_create();
  // TODO: put a finaliser on options so that we can error safely in
  // the scalar_logical commands on early exit.  Otherwise there is
  // not really a wonderful way of doing this.  The simplest route
  // would be to check on the R side really and then we don't have to
  // do any of the hard work.
  if (r_create_if_missing != R_NilValue) {
    leveldb_options_set_create_if_missing(options,
                                          scalar_logical(r_create_if_missing));
  }
  if (r_error_if_exists != R_NilValue) {
    leveldb_options_set_error_if_exists(options,
                                        scalar_logical(r_error_if_exists));
  }
  if (r_paranoid_checks != R_NilValue) {
    leveldb_options_set_paranoid_checks(options,
                                        scalar_logical(r_paranoid_checks));
  }
  if (r_write_buffer_size != R_NilValue) {
    leveldb_options_set_write_buffer_size(options,
                                          scalar_size(r_write_buffer_size));
  }
  if (r_max_open_files != R_NilValue) {
    leveldb_options_set_max_open_files(options,
                                       scalar_size(r_max_open_files));
  }
  if (r_block_size != R_NilValue) {
    leveldb_options_set_block_size(options,
                                   scalar_size(r_block_size));
  }
  if (r_use_compression != R_NilValue) {
    leveldb_options_set_compression(options,
                                    scalar_size(r_use_compression));
  }

  return options;
}

bool check_iterator(leveldb_iterator_t *it, SEXP r_error_if_invalid) {
  bool valid = leveldb_iter_valid(it);
  if (!valid) {
    if (scalar_logical(r_error_if_invalid)) {
      Rf_error("Iterator is not valid");
    }
  }
  return valid;
}

leveldb_readoptions_t * default_readoptions = NULL;
leveldb_writeoptions_t * default_writeoptions = NULL;

bool iter_key_starts_with(leveldb_iterator_t *it, const char *starts_with,
                          size_t starts_with_len) {
  if (starts_with_len == 0) {
    return true;
  }
  size_t key_len;
  const char *key_data = leveldb_iter_key(it, &key_len);
  return key_len >= starts_with_len &&
    memcmp(key_data, starts_with, starts_with_len) == 0;
}
