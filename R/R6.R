leveldb <- function(path,
                    create_if_missing = TRUE,
                    error_if_exists = NULL,
                    paranoid_checks = NULL,
                    write_buffer_size = NULL,
                    max_open_files = NULL,
                    cache_capacity = NULL,
                    block_size = NULL,
                    use_compression = NULL,
                    bloom_filter_bits_per_key = NULL) {
  R6_leveldb$new(path, create_if_missing, error_if_exists,
                 paranoid_checks, write_buffer_size, max_open_files,
                 cache_capacity, block_size, use_compression,
                 bloom_filter_bits_per_key)
}

##' @importFrom R6 R6Class
R6_leveldb <- R6::R6Class(
  "leveldb",
  public = list(
    db = NULL,
    path = NULL,
    initialize = function(path, ...) {
      self$path <- path
      self$db <- leveldb_open(path, ...)
    },
    close = function(error_if_closed = FALSE) {
      leveldb_close(self$db, error_if_closed)
    },
    destroy = function() {
      self$close()
      leveldb_destroy(self$path)
    },
    property = function(name, error_if_missing = FALSE) {
      leveldb_property(self$db, name, error_if_missing)
    },

    get = function(key, as_raw = NULL, error_if_missing = FALSE,
                   readoptions = NULL) {
      leveldb_get(self$db, key, as_raw, error_if_missing, readoptions)
    },
    mget = function(key, as_raw = NULL, missing_value = NULL,
                    missing_report = TRUE, readoptions = NULL) {
      leveldb_mget(self$db, key, as_raw, missing_value, missing_report,
                   readoptions)
    },

    put = function(key, value, writeoptions = NULL) {
      leveldb_put(self$db, key, value, writeoptions)
    },
    mput = function(key, value, writeoptions = NULL) {
      leveldb_mput(self$db, key, value, writeoptions)
    },

    delete = function(key, report = FALSE,
                      readoptions = NULL, writeoptions = NULL) {
      leveldb_delete(self$db, key, report, readoptions, writeoptions)
    },
    exists = function(key, readoptions = NULL) {
      leveldb_exists(self$db, key, readoptions)
    },
    keys = function(starts_with = NULL, as_raw = FALSE, readoptions = NULL) {
      leveldb_keys(self$db, starts_with, as_raw, readoptions)
    },
    keys_len = function(starts_with = NULL, readoptions = NULL) {
      leveldb_keys_len(self$db, starts_with, readoptions)
    },
    iterator = function(readoptions = NULL) {
      R6_leveldb_iterator$new(self$db, readoptions)
    },
    writebatch = function() {
      R6_leveldb_writebatch$new(self$db)
    },
    snapshot = function() {
      leveldb_snapshot(self$db)
    },
    approximate_sizes = function(start, limit) {
      leveldb_approximate_sizes(self$db, start, limit)
    },
    compact_range = function(start, limit) {
      leveldb_compact_range(self$db, start, limit)
    }
  ))

R6_leveldb_iterator <- R6::R6Class(
  "leveldb_iterator",
  public = list(
    it = NULL,
    initialize = function(db, readoptions) {
      self$it <- leveldb_iter_create(db, readoptions)
    },
    destroy = function(error_if_destroyed = FALSE) {
      leveldb_iter_destroy(self$it, error_if_destroyed)
    },
    valid = function() {
      leveldb_iter_valid(self$it)
    },
    seek_to_first = function() {
      leveldb_iter_seek_to_first(self$it)
      invisible(self)
    },
    seek_to_last = function() {
      leveldb_iter_seek_to_last(self$it)
      invisible(self)
    },
    seek = function(key) {
      leveldb_iter_seek(self$it, key)
      invisible(self)
    },
    move_next = function(error_if_invalid = FALSE) {
      leveldb_iter_next(self$it, error_if_invalid)
      invisible(self)
    },
    move_prev = function(error_if_invalid = FALSE) {
      leveldb_iter_prev(self$it, error_if_invalid)
      invisible(self)
    },
    key = function(as_raw = NULL, error_if_invalid = FALSE) {
      leveldb_iter_key(self$it, as_raw, error_if_invalid)
    },
    value = function(as_raw = NULL, error_if_invalid = FALSE) {
      leveldb_iter_value(self$it, as_raw, error_if_invalid)
    }
  ))

R6_leveldb_writebatch <- R6::R6Class(
  "leveldb_writebatch",
  public = list(
    ptr = NULL,
    db = NULL,

    initialize = function(db) {
      self$db <- db
      self$ptr <- leveldb_writebatch_create()
    },
    destroy = function(error_if_destroyed = FALSE) {
      leveldb_writebatch_destroy(self$ptr, error_if_destroyed)
    },
    clear = function() {
      leveldb_writebatch_clear(self$ptr)
      invisible(self)
    },
    put = function(key, value) {
      leveldb_writebatch_put(self$ptr, key, value)
      invisible(self)
    },
    mput = function(key, value) {
      leveldb_writebatch_mput(self$ptr, key, value)
      invisible(self)
    },
    delete = function(key) {
      leveldb_writebatch_delete(self$ptr, key)
      invisible(self)
    },
    write = function(writeoptions = NULL) {
      leveldb_write(self$db, self$ptr, writeoptions)
      invisible(self)
    }
  ))
