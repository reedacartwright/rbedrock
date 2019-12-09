# Copyright (c) 2016, Richard G. FitzJohn

# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are
# met:

#     Redistributions of source code must retain the above copyright
#     notice, this list of conditions and the following disclaimer.

#     Redistributions in binary form must reproduce the above copyright
#     notice, this list of conditions and the following disclaimer in
#     the documentation and/or other materials provided with the
#     distribution.

# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
# "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
# LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
# A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
# HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
# SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
# LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
# DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
# THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
# (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
# OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

##' Create a \code{leveldb} object, to interact with a LevelDB
##' database.
##'
##' For all optional arguments (i.e., all but \code{path}) a value of
##' \code{NULL} means that we use the \code{LevelDB} default; the
##' LevelDB default of each argument is indicated in the argument
##' documentation.
##'
##' This function returns an "R6" class with a number of methods.
##' @title Open a LevelDB Database
##'
##' @param path The path to the database, as stored on the filesystem.
##'   This will create a \emph{directory} at this path if one does not
##'   exist (and if \code{create_if_missing} is \code{TRUE}.
##'
##' @param create_if_missing Create the database if one does not
##'   already exist at \code{path} (this defaults to \code{TRUE},
##'   which differs from the LevelDB default of \code{FALSE}).
##'
##' @param error_if_exists Throw an error if the database already
##'   exists at \code{path}  (LevelDB default: \code{FALSE}).
##'
##' @param paranoid_checks If \code{TRUE}, LevelDB will do aggressive
##'   checking of the data it is processing and will stop early if it
##'   detects any errors.  This may have unforeseen ramifications: for
##'   example, a corruption of one DB entry may cause a large number
##'   of entries to become unreadable or for the entire DB to become
##'   unopenable.  (LevelDB default: \code{FALSE}).
##'
##' @param write_buffer_size Amount of data (in bytes) to build up in
##'   memory (backed by an unsorted log on disk) before converting to
##'   a sorted on-disk file.  Larger values increase performance,
##'   especially during bulk loads.  Up to two write buffers may be
##'   held in memory at the same time, so you may wish to adjust this
##'   parameter to control memory usage.  Also, a larger write buffer
##'   will result in a longer recovery time the next time the database
##'   is opened.
##'
##' @param max_open_files Number of files that can be used by the
##'   database.  You may need to increase this if your database has a
##'   large working set (budget one open file per 2MB of working set).
##'   (LevelDB default: 1000).
##'
##' @param block_size The approximate size of user data packed per
##'   block (user data is stored in a set of blocks, and a block is
##'   the unit of reading from disk).  The block size here corresponds
##'   to uncompressed data; the actual size of the unit read frmo disk
##'   may be smaller if compression is enabled (LevelDB default: 4K)
##'
##' @param use_compression Compress blocks using the "Snappy"
##'   compression algorithm (LevelDB default: \code{TRUE})
##'
##' @param cache_capacity The size of the cache to use.  If
##'   non-\code{NULL} this must be a non-negative integer, indicating
##'   the size of the cache in bytes.  If \code{NULL} (the default)
##'   then LevelDB will create an 8MB internal cache.
##'
##' @param bloom_filter_bits_per_key If non-NULL, this sets up a
##'   "filter policy" to reduce disk reads.  A good value for
##'   bits_per_key // is 10, which yields a filter with ~ 1% false
##'   positive rate.  Further information from the LevelDB headers
##'   (filter_policy.h) "This object is responsible for
##'   creating a small filter from a set of keys.  These filters are
##'   stored in leveldb and are consulted automatically by leveldb to
##'   decide whether or not to read some information from disk. In
##'   many cases, a filter can cut down the number of disk seeks form
##'   a handful to a single disk seek per DB::Get() call"
##' @export
##' @author Rich FitzJohn
##' @useDynLib rbedrock, .registration = TRUE
leveldb_open <- function(path,
                         create_if_missing = NULL,
                         error_if_exists = NULL,
                         paranoid_checks = NULL,
                         write_buffer_size = NULL,
                         max_open_files = NULL,
                         block_size = NULL,
                         use_compression = NULL,
                         cache_capacity = NULL,
                         bloom_filter_bits_per_key = NULL) {
  ptr <- .Call(Crleveldb_open, path, create_if_missing, error_if_exists,
               paranoid_checks, write_buffer_size, max_open_files,
               block_size, use_compression,
               cache_capacity, bloom_filter_bits_per_key)
  attr(ptr, "options") <- list(path = path,
                               create_if_missing = create_if_missing,
                               error_if_exists = error_if_exists,
                               paranoid_checks = paranoid_checks,
                               write_buffer_size = write_buffer_size,
                               max_open_files = max_open_files,
                               block_size = block_size,
                               use_compression = use_compression,
                               cache_capacity = cache_capacity,
                               bloom_filter_bits_per_key =
                                 bloom_filter_bits_per_key)
  class(ptr) <- c("leveldb_connection", "leveldb_options")
  ptr
}

leveldb_close <- function(db, error_if_closed = FALSE) {
  .Call(Crleveldb_close, db, error_if_closed)
}

leveldb_destroy <- function(path) {
  .Call(Crleveldb_destroy, path)
}

leveldb_repair <- function(path) {
  .Call(Crleveldb_repair, path)
}

leveldb_property <- function(db, path, error_if_missing = FALSE) {
  .Call(Crleveldb_property, db, path, error_if_missing)
}

leveldb_get <- function(db, key, as_raw = NULL, error_if_missing = FALSE,
                        readoptions = NULL) {
  .Call(Crleveldb_get, db, key, as_raw, error_if_missing, readoptions)
}

leveldb_mget <- function(db, key, as_raw = NULL, missing_value = NULL,
                         missing_report = TRUE, readoptions = NULL) {
  .Call(Crleveldb_mget, db, key, as_raw, missing_value, missing_report,
        readoptions)
}

leveldb_put <- function(db, key, value, writeoptions = NULL) {
  .Call(Crleveldb_put, db, key, value, writeoptions)
}

leveldb_mput <- function(db, key, value, writeoptions = NULL) {
  .Call(Crleveldb_mput, db, key, value, writeoptions)
}

leveldb_delete <- function(db, key, report = FALSE,
                           readoptions = NULL, writeoptions = NULL) {
  .Call(Crleveldb_delete, db, key, report, readoptions, writeoptions)
}

leveldb_iter_create <- function(db, readoptions = NULL) {
  .Call(Crleveldb_iter_create, db, readoptions)
}

leveldb_iter_destroy <- function(it, error_if_destroyed = FALSE) {
  .Call(Crleveldb_iter_destroy, it, error_if_destroyed)
}

leveldb_iter_valid <- function(it) {
  .Call(Crleveldb_iter_valid, it)
}

leveldb_iter_seek_to_first <- function(it) {
  .Call(Crleveldb_iter_seek_to_first, it)
}

leveldb_iter_seek_to_last <- function(it) {
  .Call(Crleveldb_iter_seek_to_last, it)
}

leveldb_iter_seek <- function(it, key) {
  .Call(Crleveldb_iter_seek, it, key)
}

leveldb_iter_next <- function(it, error_if_invalid = FALSE) {
  .Call(Crleveldb_iter_next, it, error_if_invalid)
}

leveldb_iter_prev <- function(it, error_if_invalid = FALSE) {
  .Call(Crleveldb_iter_prev, it, error_if_invalid)
}

leveldb_iter_key <- function(it, as_raw = NULL, error_if_invalid = FALSE) {
  .Call(Crleveldb_iter_key, it, as_raw, error_if_invalid)
}

leveldb_iter_value <- function(it, as_raw = NULL,
                               error_if_invalid = FALSE) {
  .Call(Crleveldb_iter_value, it, as_raw, error_if_invalid)
}

leveldb_snapshot <- function(db) {
  ptr <- .Call(Crleveldb_snapshot_create, db)
  attr(ptr, "timestamp") <- Sys.time()
  class(ptr) <- "leveldb_snapshot"
  ptr
}

leveldb_writebatch_create <- function() {
  .Call(Crleveldb_writebatch_create)
}

leveldb_writebatch_destroy <- function(writebatch, error_if_destroyed = FALSE) {
  .Call(Crleveldb_writebatch_destroy, writebatch, error_if_destroyed)
}

leveldb_writebatch_clear <- function(writebatch) {
  .Call(Crleveldb_writebatch_clear, writebatch)
}

leveldb_writebatch_put <- function(writebatch, key, value) {
  .Call(Crleveldb_writebatch_put, writebatch, key, value)
}

leveldb_writebatch_mput <- function(writebatch, key, value) {
  .Call(Crleveldb_writebatch_mput, writebatch, key, value)
}

leveldb_writebatch_delete <- function(writebatch, key) {
  .Call(Crleveldb_writebatch_delete, writebatch, key)
}

leveldb_write <- function(db, writebatch, writeoptions = NULL) {
  .Call(Crleveldb_write, db, writebatch, writeoptions)
}

leveldb_approximate_sizes <- function(db, start, limit) {
  .Call(Crleveldb_approximate_sizes, db, start, limit)
}

leveldb_compact_range <- function(db, start, limit) {
  .Call(Crleveldb_compact_range, db, start, limit)
}

leveldb_readoptions <- function(verify_checksums = NULL, fill_cache = NULL,
                                snapshot = NULL) {
  ptr <- .Call(Crleveldb_readoptions, verify_checksums, fill_cache, snapshot)
  attr(ptr, "options") <- list(verify_checksums = verify_checksums,
                               fill_cache = fill_cache,
                               snapshot = snapshot)
  class(ptr) <- c("leveldb_readoptions", "leveldb_options")
  ptr
}

leveldb_writeoptions <- function(sync = NULL) {
  ptr <- .Call(Crleveldb_writeoptions, sync)
  class(ptr) <- c("leveldb_writeoptions", "leveldb_options")
  attr(ptr, "options") <- list(sync = sync)
  ptr
}

leveldb_keys_len <- function(db, starts_with = NULL, readoptions = NULL) {
  .Call(Crleveldb_keys_len, db, starts_with, readoptions)
}

leveldb_keys <- function(db, starts_with = NULL, as_raw = FALSE,
                         readoptions = NULL) {
  .Call(Crleveldb_keys, db, starts_with, as_raw, readoptions)
}

leveldb_exists <- function(db, key, readoptions = NULL) {
  .Call(Crleveldb_exists, db, key, readoptions)
}

leveldb_version <- function() {
  ret <- list(.Call(Crleveldb_version))
  class(ret) <- "numeric_version"
  ret
}

##' @export
as.character.leveldb_snapshot <- function(x, ...) {
  sprintf("<leveldb_snapshot> @ %s", attr(x, "timestamp"))
}

##" @export
print.leveldb_snapshot <- function(x, ...) {
  cat(as.character(x), "\n")
  invisible(x)
}

##' @export
names.leveldb_options <- function(x, ...) {
  names(attr(x, "options", exact = TRUE))
}

##' @export
`$.leveldb_options` <- function(x, i) {
  attr(x, "options")[[i]]
}

##' @export
`[[.leveldb_options` <- function(x, i, ...) {
  attr(x, "options")[[i]]
}

##' @export
`$<-.leveldb_options` <- function(x, i, value) {
  stop(sprintf("%s objects are immutable", class(x)[[1L]]))
}

##' @export
`[[<-.leveldb_options` <- function(x, ..., value) {
  stop(sprintf("%s objects are immutable", class(x)[[1L]]))
}

##' @export
as.character.leveldb_options <- function(x, ...) {
  f <- function(x) {
    if (is.null(x)) {
      "<not set>"
    } else {
      as.character(x)
    }
  }
  value <- vapply(names(x), function(i) f(x[[i]]), character(1))
  txt <- c(sprintf("<%s>", class(x)[[1]]),
           sprintf("  - %s: %s", names(x), value))
  paste(txt, collapse = "\n")
}

##' @export
print.leveldb_options <- function(x, ...) {
  cat(as.character(x), "\n")
  invisible(x)
}
