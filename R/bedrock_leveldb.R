# Copyright (c) 2016 Richard G. FitzJohn
# Copyright (c) 2021 Reed A. Cartwright <reed@cartwrig.ht>

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

#' Create a \code{leveldb} object, to interact with a LevelDB
#' database.
#'
#' For all optional arguments (i.e., all but \code{path}) a value of
#' \code{NULL} means that we use the \code{LevelDB} default; the
#' LevelDB default of each argument is indicated in the argument
#' documentation.
#'
#' This function returns an 'R6' class with a number of methods.
#' @title Open a LevelDB Database
#'
#' @param path The path to the database, as stored on the filesystem.
#'   This will create a \emph{directory} at this path if one does not
#'   exist (and if \code{create_if_missing} is \code{TRUE}.
#'
#' @param create_if_missing Create the database if one does not
#'   already exist at \code{path} (this defaults to \code{TRUE},
#'   which differs from the LevelDB default of \code{FALSE}).
#'
#' @param error_if_exists Throw an error if the database already
#'   exists at \code{path}  (LevelDB default: \code{FALSE}).
#'
#' @param paranoid_checks If \code{TRUE}, LevelDB will do aggressive
#'   checking of the data it is processing and will stop early if it
#'   detects any errors.  This may have unforeseen ramifications: for
#'   example, a corruption of one DB entry may cause a large number
#'   of entries to become unreadable or for the entire DB to become
#'   unopenable.  (LevelDB default: \code{FALSE}).
#'
#' @param write_buffer_size Amount of data (in bytes) to build up in
#'   memory (backed by an unsorted log on disk) before converting to
#'   a sorted on-disk file.  Larger values increase performance,
#'   especially during bulk loads.  Up to two write buffers may be
#'   held in memory at the same time, so you may wish to adjust this
#'   parameter to control memory usage.  Also, a larger write buffer
#'   will result in a longer recovery time the next time the database
#'   is opened.
#'
#' @param max_open_files Number of files that can be used by the
#'   database.  You may need to increase this if your database has a
#'   large working set (budget one open file per 2MB of working set).
#'   (LevelDB default: 1000).
#'
#' @param block_size The approximate size of user data packed per
#'   block (user data is stored in a set of blocks, and a block is
#'   the unit of reading from disk).  The block size here corresponds
#'   to uncompressed data; the actual size of the unit read from disk
#'   may be smaller if compression is enabled (LevelDB default: 4K)
#'
#' @param cache_capacity The size of the cache to use.  If
#'   non-\code{NULL} this must be a non-negative integer, indicating
#'   the size of the cache in bytes.  If \code{NULL} (the default)
#'   then LevelDB will create an 8MB internal cache.
#'
#' @param bloom_filter_bits_per_key If non-NULL, this sets up a
#'   'filter policy' to reduce disk reads.  A good value for
#'   bits_per_key is 10, which yields a filter with ~ 1% false
#'   positive rate.  Further information from the LevelDB headers
#'   (filter_policy.h) "This object is responsible for
#'   creating a small filter from a set of keys.  These filters are
#'   stored in leveldb and are consulted automatically by leveldb to
#'   decide whether or not to read some information from disk. In
#'   many cases, a filter can cut down the number of disk seeks form
#'   a handful to a single disk seek per `DB::Get()` call"
#' @export
#' @keywords internal
#' @author Rich FitzJohn
#' @useDynLib rbedrock, .registration = TRUE
bedrock_leveldb_open <- function(path, create_if_missing = NULL,
                                 error_if_exists = NULL,
                                 paranoid_checks = NULL,
                                 write_buffer_size = NULL,
                                 max_open_files = NULL,
                                 block_size = NULL,
                                 cache_capacity = NULL,
                                 bloom_filter_bits_per_key = NULL,
                                 compression_level = NULL) {
    ptr <- .Call(Cbedrock_leveldb_open, path, create_if_missing,
                 error_if_exists, paranoid_checks, write_buffer_size,
                 max_open_files, block_size, cache_capacity,
                 bloom_filter_bits_per_key, compression_level)
    attr(ptr, "options") <- list(path = path,
        create_if_missing = create_if_missing,
        error_if_exists = error_if_exists,
        paranoid_checks = paranoid_checks,
        write_buffer_size = write_buffer_size,
        max_open_files = max_open_files,
        block_size = block_size,
        cache_capacity = cache_capacity,
        bloom_filter_bits_per_key = bloom_filter_bits_per_key,
        compression_level = compression_level)
    class(ptr) <- c("bedrock_leveldb_connection", "bedrock_leveldb_options")
    ptr
}

#nolint start : object_length_linter
bedrock_leveldb_close <- function(db, error_if_closed = FALSE) {
    .Call(Cbedrock_leveldb_close, db, error_if_closed)
}

bedrock_leveldb_destroy <- function(path) {
    .Call(Cbedrock_leveldb_destroy, path)
}

bedrock_leveldb_is_open <- function(db) {
    .Call(Cbedrock_leveldb_is_open, db)
}

bedrock_leveldb_repair <- function(path) {
    .Call(Cbedrock_leveldb_repair, path)
}

bedrock_leveldb_property <- function(db, path, error_if_missing = FALSE) {
    .Call(Cbedrock_leveldb_property, db, path, error_if_missing)
}

bedrock_leveldb_get <- function(db, key, readoptions = NULL) {
    .Call(Cbedrock_leveldb_get, db, key, readoptions)
}

bedrock_leveldb_mget <- function(db, key, readoptions = NULL) {
    .Call(Cbedrock_leveldb_mget, db, key, readoptions)
}

bedrock_leveldb_mget_prefix <- function(db, starts_with, readoptions = NULL) {
    .Call(Cbedrock_leveldb_mget_prefix, db, starts_with, readoptions)
}

bedrock_leveldb_put <- function(db, key, value, writeoptions = NULL) {
    .Call(Cbedrock_leveldb_put, db, key, value, writeoptions)
}

bedrock_leveldb_mput <- function(db, key, value, writeoptions = NULL) {
    .Call(Cbedrock_leveldb_mput, db, key, value, writeoptions)
}

bedrock_leveldb_delete <- function(db, key, report = FALSE, readoptions = NULL,
                                   writeoptions = NULL) {
    .Call(Cbedrock_leveldb_delete, db, key, report, readoptions, writeoptions)
}

bedrock_leveldb_iter_create <- function(db, readoptions = NULL) {
    .Call(Cbedrock_leveldb_iter_create, db, readoptions)
}

bedrock_leveldb_iter_destroy <- function(it, error_if_destroyed = FALSE) {
    .Call(Cbedrock_leveldb_iter_destroy, it, error_if_destroyed)
}

bedrock_leveldb_iter_valid <- function(it) {
    .Call(Cbedrock_leveldb_iter_valid, it)
}

bedrock_leveldb_iter_seek_to_first <- function(it) {
    .Call(Cbedrock_leveldb_iter_seek_to_first, it)
}

bedrock_leveldb_iter_seek_to_last <- function(it) {
    .Call(Cbedrock_leveldb_iter_seek_to_last, it)
}

bedrock_leveldb_iter_seek <- function(it, key) {
    .Call(Cbedrock_leveldb_iter_seek, it, key)
}

bedrock_leveldb_iter_next <- function(it, error_if_invalid = FALSE) {
    .Call(Cbedrock_leveldb_iter_next, it, error_if_invalid)
}

bedrock_leveldb_iter_prev <- function(it, error_if_invalid = FALSE) {
    .Call(Cbedrock_leveldb_iter_prev, it, error_if_invalid)
}

bedrock_leveldb_iter_key <- function(it, error_if_invalid = FALSE) {
    .Call(Cbedrock_leveldb_iter_key, it, error_if_invalid)
}

bedrock_leveldb_iter_value <- function(it, error_if_invalid = FALSE) {
    .Call(Cbedrock_leveldb_iter_value, it, error_if_invalid)
}

bedrock_leveldb_snapshot <- function(db) {
    ptr <- .Call(Cbedrock_leveldb_snapshot_create, db)
    attr(ptr, "timestamp") <- Sys.time()
    class(ptr) <- "bedrock_leveldb_snapshot"
    ptr
}

bedrock_leveldb_writebatch_create <- function() {
    .Call(Cbedrock_leveldb_writebatch_create)
}

bedrock_leveldb_writebatch_destroy <- function(writebatch,
                                               error_if_destroyed = FALSE) {
    .Call(Cbedrock_leveldb_writebatch_destroy, writebatch, error_if_destroyed)
}

bedrock_leveldb_writebatch_clear <- function(writebatch) {
    .Call(Cbedrock_leveldb_writebatch_clear, writebatch)
}

bedrock_leveldb_writebatch_put <- function(writebatch, key, value) {
    .Call(Cbedrock_leveldb_writebatch_put, writebatch, key, value)
}

bedrock_leveldb_writebatch_mput <- function(writebatch, key, value) {
    .Call(Cbedrock_leveldb_writebatch_mput, writebatch, key, value)
}

bedrock_leveldb_writebatch_delete <- function(writebatch, key) {
    .Call(Cbedrock_leveldb_writebatch_delete, writebatch, key)
}

bedrock_leveldb_writebatch_mdelete <- function(writebatch, keys) {
    .Call(Cbedrock_leveldb_writebatch_mdelete, writebatch, keys)
}

bedrock_leveldb_write <- function(db, writebatch, writeoptions = NULL) {
    .Call(Cbedrock_leveldb_write, db, writebatch, writeoptions)
}

bedrock_leveldb_approximate_sizes <- function(db, start, limit) {
    .Call(Cbedrock_leveldb_approximate_sizes, db, start, limit)
}

bedrock_leveldb_compact_range <- function(db, start, limit) {
    .Call(Cbedrock_leveldb_compact_range, db, start, limit)
}

bedrock_leveldb_readoptions <- function(verify_checksums = NULL,
                                        fill_cache = NULL,
                                        snapshot = NULL) {
    ptr <- .Call(Cbedrock_leveldb_readoptions, verify_checksums, fill_cache,
                 snapshot)
    attr(ptr, "options") <- list(verify_checksums = verify_checksums,
                                 fill_cache = fill_cache,
                                 snapshot = snapshot)
    class(ptr) <- c("bedrock_leveldb_readoptions", "bedrock_leveldb_options")
    ptr
}

bedrock_leveldb_writeoptions <- function(sync = NULL) {
    ptr <- .Call(Cbedrock_leveldb_writeoptions, sync)
    class(ptr) <- c("bedrock_leveldb_writeoptions", "bedrock_leveldb_options")
    attr(ptr, "options") <- list(sync = sync)
    ptr
}

bedrock_leveldb_keys_len <- function(db, starts_with = NULL,
                                     readoptions = NULL) {
    .Call(Cbedrock_leveldb_keys_len, db, starts_with, readoptions)
}

bedrock_leveldb_keys <- function(db, starts_with = NULL, readoptions = NULL) {
    .Call(Cbedrock_leveldb_keys, db, starts_with, readoptions)
}

bedrock_leveldb_exists <- function(db, key, readoptions = NULL) {
    .Call(Cbedrock_leveldb_exists, db, key, readoptions)
}

bedrock_leveldb_version <- function() {
    ret <- list(.Call(Cbedrock_leveldb_version))
    class(ret) <- "numeric_version"
    ret
}
#nolint end

#' @export
as.character.bedrock_leveldb_snapshot <- function(x, ...) {
    sprintf("<bedrock_leveldb_snapshot> @ %s", attr(x, "timestamp"))
}

#' @export
print.bedrock_leveldb_snapshot <- function(x, ...) {
    cat(as.character(x), "\n")
    invisible(x)
}

#' @export
names.bedrock_leveldb_options <- function(x, ...) {
    names(attr(x, "options", exact = TRUE))
}

#' @export
`$.bedrock_leveldb_options` <- function(x, i) {
    attr(x, "options")[[i]]
}

#' @export
`[[.bedrock_leveldb_options` <- function(x, i, ...) {
    attr(x, "options")[[i]]
}

#' @export
`$<-.bedrock_leveldb_options` <- function(x, i, value) {
    stop(sprintf("%s objects are immutable", class(x)[[1L]]))
}

#' @export
`[[<-.bedrock_leveldb_options` <- function(x, ..., value) {
    stop(sprintf("%s objects are immutable", class(x)[[1L]]))
}

#' @export
as.character.bedrock_leveldb_options <- function(x, ...) {
    f <- function(x) {
        if (is.null(x)) {
            "<not set>"
        } else {
            as.character(x)
        }
    }
    value <- vapply(names(x), function(i) f(x[[i]]), character(1))
    txt <- c(sprintf("<%s>", class(x)[[1]]), sprintf("  - %s: %s", names(x),
                                                     value))
    paste(txt, collapse = "\n")
}

#' @export
print.bedrock_leveldb_options <- function(x, ...) {
    cat(as.character(x), "\n")
    invisible(x)
}
