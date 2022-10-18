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
#'   (filter_policy.h) 'This object is responsible for
#'   creating a small filter from a set of keys.  These filters are
#'   stored in leveldb and are consulted automatically by leveldb to
#'   decide whether or not to read some information from disk. In
#'   many cases, a filter can cut down the number of disk seeks form
#'   a handful to a single disk seek per DB::Get() call'
#' @export
#' @keywords internal
#' @author Rich FitzJohn
#' @useDynLib rbedrock, .registration = TRUE
db_open <- function(path, create_if_missing = NULL, error_if_exists = NULL, 
    paranoid_checks = NULL, write_buffer_size = NULL, max_open_files = NULL, block_size = NULL, 
    cache_capacity = NULL, bloom_filter_bits_per_key = NULL, compression_level = NULL) {
    ptr <- .Call(rbedrock_db_open, path, create_if_missing, error_if_exists, 
        paranoid_checks, write_buffer_size, max_open_files, block_size, 
        cache_capacity, bloom_filter_bits_per_key, compression_level)
    attr(ptr, "options") <- list(path = path, create_if_missing = create_if_missing, 
        error_if_exists = error_if_exists, paranoid_checks = paranoid_checks, write_buffer_size = write_buffer_size, 
        max_open_files = max_open_files, block_size = block_size, 
        cache_capacity = cache_capacity, bloom_filter_bits_per_key = bloom_filter_bits_per_key,
        compression_level = compression_level)
    class(ptr) <- c("rbedrock_db_connection", "rbedrock_db_options")
    ptr
}
db_close <- function(db, error_if_closed = FALSE) {
    .Call(rbedrock_db_close, db, error_if_closed)
}
db_destroy <- function(path) {
    .Call(rbedrock_db_destroy, path)
}
db_is_open <- function(db) {
    .Call(rbedrock_db_is_open, db)
}
db_repair <- function(path) {
    .Call(rbedrock_db_repair, path)
}
db_property <- function(db, path, error_if_missing = FALSE) {
    .Call(rbedrock_db_property, db, path, error_if_missing)
}
db_get <- function(db, key, readoptions = NULL) {
    .Call(rbedrock_db_get, db, key, readoptions)
}
db_mget <- function(db, key, readoptions = NULL) {
    .Call(rbedrock_db_mget, db, key, readoptions)
}
db_mget_prefix <- function(db, starts_with, readoptions = NULL) {
    .Call(rbedrock_db_mget_prefix, db, starts_with, readoptions)
}
db_put <- function(db, key, value, writeoptions = NULL) {
    .Call(rbedrock_db_put, db, key, value, writeoptions)
}
db_delete <- function(db, key, writeoptions = NULL) {
    .Call(rbedrock_db_delete, db, key, writeoptions)
}
db_write <- function(db, keys, values, writeoptions = NULL, allow_delete = TRUE) {
    .Call(rbedrock_db_write, db, keys, values, writeoptions, allow_delete)
}
db_mput <- function(db, keys, values, writeoptions = NULL) {
    db_write(db, keys, values, writeoptions, allow_delete = FALSE)
}
db_mdelete <- function(db, keys, writeoptions = NULL) {
    db_write(db, keys, NULL, writeoptions, allow_delete = TRUE)
}
db_iter_create <- function(db, readoptions = NULL) {
    .Call(rbedrock_db_iter_create, db, readoptions)
}
iter_destroy <- function(it, error_if_destroyed = FALSE) {
    .Call(rbedrock_iter_destroy, it, error_if_destroyed)
}
iter_valid <- function(it) {
    .Call(rbedrock_iter_valid, it)
}
iter_isnil <- function(it) {
    .Call(rbedrock_iter_isnil, it)
}
iter_seek_to_first <- function(it) {
    .Call(rbedrock_iter_seek_to_first, it)
}
iter_seek_to_last <- function(it) {
    .Call(rbedrock_iter_seek_to_last, it)
}
iter_seek <- function(it, key) {
    .Call(rbedrock_iter_seek, it, key)
}
iter_next <- function(it, error_if_invalid = FALSE) {
    .Call(rbedrock_iter_next, it, error_if_invalid)
}
iter_prev <- function(it, error_if_invalid = FALSE) {
    .Call(rbedrock_iter_prev, it, error_if_invalid)
}
iter_key <- function(it, error_if_invalid = FALSE) {
    .Call(rbedrock_iter_key, it, error_if_invalid)
}
iter_value <- function(it, error_if_invalid = FALSE) {
    .Call(rbedrock_iter_value, it, error_if_invalid)
}
db_snapshot <- function(db) {
    ptr <- .Call(rbedrock_db_snapshot_create, db)
    attr(ptr, "timestamp") <- Sys.time()
    class(ptr) <- "rbedrock_db_snapshot"
    ptr
}
db_snapshot_release <- function(db, snapshot, error_if_released = FALSE) {
    .Call(rbedrock_db_snapshot_release, db, snapshot, error_if_released)
}
snapshot_isnil <- function(snapshot) {
    .Call(rbedrock_snapshot_isnil, snapshot)
}
db_approximate_sizes <- function(db, start, limit) {
    .Call(rbedrock_db_approximate_sizes, db, start, limit)
}
db_compact_range <- function(db, start, limit) {
    .Call(rbedrock_db_compact_range, db, start, limit)
}
db_keys_len <- function(db, starts_with = NULL, readoptions = NULL) {
    .Call(rbedrock_db_keys_len, db, starts_with, readoptions)
}
db_keys <- function(db, starts_with = NULL, readoptions = NULL) {
    .Call(rbedrock_db_keys, db, starts_with, readoptions)
}
db_exists <- function(db, key, readoptions = NULL) {
    .Call(rbedrock_db_exists, db, key, readoptions)
}
leveldb_version <- function() {
    ret <- list(.Call(rbedrock_leveldb_version))
    class(ret) <- "numeric_version"
    ret
}
#' @export
as.character.rbedrock_db_snapshot <- function(x, ...) {
    sprintf("<rbedrock_db_snapshot> @ %s", attr(x, "timestamp"))
}
#' @export
print.rbedrock_db_snapshot <- function(x, ...) {
    cat(as.character(x), "\n")
    invisible(x)
}
#' @export
names.rbedrock_db_options <- function(x, ...) {
    names(attr(x, "options", exact = TRUE))
}
#' @export
`$.rbedrock_db_options` <- function(x, i) {
    attr(x, "options")[[i]]
}
#' @export
`[[.rbedrock_db_options` <- function(x, i, ...) {
    attr(x, "options")[[i]]
}
#' @export
`$<-.rbedrock_db_options` <- function(x, i, value) {
    stop(sprintf("%s objects are immutable", class(x)[[1L]]))
}
#' @export
`[[<-.rbedrock_db_options` <- function(x, ..., value) {
    stop(sprintf("%s objects are immutable", class(x)[[1L]]))
}
#' @export
as.character.rbedrock_db_options <- function(x, ...) {
    f <- function(x) {
        if (is.null(x)) {
            "<not set>"
        } else {
            as.character(x)
        }
    }
    value <- vapply(names(x), function(i) f(x[[i]]), character(1))
    txt <- c(sprintf("<%s>", class(x)[[1]]), sprintf("  - %s: %s", names(x), value))
    paste(txt, collapse = "\n")
}
#' @export
print.rbedrock_db_options <- function(x, ...) {
    cat(as.character(x), "\n")
    invisible(x)
}
