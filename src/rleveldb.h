#include <R.h>
#include <Rinternals.h>

SEXP rleveldb_open(SEXP r_path,
                   SEXP r_create_if_missing,
                   SEXP r_error_if_exists,
                   SEXP r_paranoid_checks,
                   SEXP r_write_buffer_size,
                   SEXP r_max_open_files,
                   SEXP r_block_size,
                   SEXP r_use_compression,
                   SEXP r_cache_capacity,
                   SEXP r_bloom_filter_bits_per_key);
SEXP rleveldb_close(SEXP r_db, SEXP r_error_if_closed);
SEXP rleveldb_destroy(SEXP r_path);
SEXP rleveldb_repair(SEXP r_path);
SEXP rleveldb_property(SEXP r_db, SEXP r_name, SEXP r_error_if_missing);

SEXP rleveldb_get(SEXP r_db, SEXP r_key, SEXP r_as_raw,
                  SEXP r_error_if_missing, SEXP r_readoptions);
SEXP rleveldb_mget(SEXP r_db, SEXP r_key, SEXP r_as_raw,
                   SEXP r_missing_value, SEXP r_missing_report,
                   SEXP r_readoptions);

SEXP rleveldb_put(SEXP r_db, SEXP r_key, SEXP r_value, SEXP r_writeoptions);
SEXP rleveldb_mput(SEXP r_db, SEXP r_key, SEXP r_value, SEXP r_writeoptions);

SEXP rleveldb_delete(SEXP r_db, SEXP r_key, SEXP r_report,
                     SEXP r_readoptions, SEXP r_writeoptions);
SEXP rleveldb_delete_silent(SEXP r_db, SEXP r_key, SEXP r_writeoptions);
SEXP rleveldb_delete_report(SEXP r_db, SEXP r_key, SEXP r_readoptions,
                            SEXP r_writeoptions);

SEXP rleveldb_iter_create(SEXP r_db, SEXP r_readoptions);
SEXP rleveldb_iter_destroy(SEXP r_it, SEXP r_error_if_destroyed);
SEXP rleveldb_iter_valid(SEXP r_it);
SEXP rleveldb_iter_seek_to_first(SEXP r_it);
SEXP rleveldb_iter_seek_to_last(SEXP r_it);
SEXP rleveldb_iter_seek(SEXP r_it, SEXP r_key);
SEXP rleveldb_iter_next(SEXP r_it, SEXP r_error_if_invalid);
SEXP rleveldb_iter_prev(SEXP r_it, SEXP r_error_if_invalid);
SEXP rleveldb_iter_key(SEXP r_it, SEXP r_as_raw, SEXP r_error_if_invalid);
SEXP rleveldb_iter_value(SEXP r_it, SEXP r_as_raw, SEXP r_error_if_invalid);

SEXP rleveldb_snapshot_create(SEXP r_db);

SEXP rleveldb_writebatch_create();
SEXP rleveldb_writebatch_destroy(SEXP r_writebatch, SEXP error_if_destroyed);
SEXP rleveldb_writebatch_clear(SEXP r_writebatch);
SEXP rleveldb_writebatch_put(SEXP r_writebatch, SEXP r_key, SEXP r_value);
SEXP rleveldb_writebatch_mput(SEXP r_writebatch, SEXP r_key, SEXP r_value);
SEXP rleveldb_writebatch_delete(SEXP r_writebatch, SEXP r_key);
SEXP rleveldb_write(SEXP r_db, SEXP r_writebatch, SEXP r_writeoptions);

SEXP rleveldb_approximate_sizes(SEXP r_db, SEXP r_start_key, SEXP r_limit_key);
SEXP rleveldb_compact_range(SEXP r_db, SEXP r_start_key, SEXP r_limit_key);

SEXP rleveldb_readoptions(SEXP r_verify_checksums, SEXP r_fill_cache,
                          SEXP r_snapshot);
SEXP rleveldb_writeoptions(SEXP r_sync);

SEXP rleveldb_keys(SEXP r_db, SEXP r_starts_with, SEXP r_as_raw,
                   SEXP r_readoptions);
SEXP rleveldb_keys_len(SEXP r_db, SEXP r_starts_with, SEXP r_readoptions);
SEXP rleveldb_exists(SEXP r_db, SEXP r_key, SEXP r_readoptions);
SEXP rleveldb_version();
SEXP rleveldb_tag(SEXP r_db);
void rleveldb_init();
void rleveldb_cleanup();
