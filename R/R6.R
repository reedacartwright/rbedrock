# Copyright (c) 2016, Richard G. FitzJohn
# Copyright (c) 2020, Reed A. Cartwright

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

#' Open a Bedrock Edition world for reading and writing.
#'
#' `bedrockdb` opens a handle to a leveldb database that contains
#' save-game data for a Bedrock Edition world. On success, it returns
#' an R6 class of type 'bedrockdb' that can be used directly for
#' low-level reading and writing access to the db or can be passed to
#' higher-level functions. The handle to the database can be closed
#' by passing it to `close`.
#'
#' @param path The path to a world folder. If the path does not exist, it is
#'   assumed to be the base name of a world folder in the local minecraftWorlds
#'   directory.
#' @param create_if_missing Create world database if it doesn't exist.
#' @param error_if_exists Raise an error if the world database already exists.
#' @param paranoid_checks Internal leveldb option
#' @param write_buffer_size Internal leveldb option
#' @param max_open_files Internal leveldb option
#' @param block_size Internal leveldb option
#' @param cache_capacity Internal leveldb option
#' @param bloom_filter_bits_per_key Internal leveldb option
#' @param compression_level Internal leveldb option
#' @param compact Compact database before closing.
#' @param con An database object created by bedrockdb.
#' @param ... arguments passed to or from other methods.
#' @param x An object.
#'
#' @return On success, `bedrockdb` returns an R6 class of type 'bedrockdb'.
#'
#' @export
#' @examples
#' # open an example works and get all keys
#' dbpath <- rbedrock_example_world("example1.mcworld")
#' db <- bedrockdb(dbpath)
#' keys <- get_keys(db)
#' close(db)
#'
#' \dontrun{
#'
#' # open a world in the minecraftWorlds folder using a world id.
#' db <- bedrockdb("lrkkYFpUABA=")
#' # do something with db ...
#' close(db)
#'
#' # open a world using absolute path
#' db <- bedrockdb("C:\\\\minecraftWorlds\\\\my_world")
#' # do something with db ...
#' close(db)
#' }

bedrockdb <- function(path,
                      create_if_missing = FALSE,
                      error_if_exists = NULL,
                      paranoid_checks = NULL,
                      write_buffer_size = 4194304L,
                      max_open_files = NULL,
                      block_size = 163840L,
                      cache_capacity = 83886080L,
                      bloom_filter_bits_per_key = 10L,
                      compression_level = -1L) {
    R6_bedrockdb$new(path,
                     create_if_missing,
                     error_if_exists,
                     paranoid_checks,
                     write_buffer_size,
                     max_open_files,
                     block_size,
                     cache_capacity,
                     bloom_filter_bits_per_key,
                     compression_level)
}

#' @export
#' @rdname bedrockdb
close.bedrockdb <- function(con, compact = FALSE, ...) {
    if (isTRUE(compact)) {
        inform("Compacting database...")
        con$compact_range()
    }
    con$close(...)
}

#' @export
#' @rdname bedrockdb
is_bedrockdb <- function(x) {
    inherits(x, "bedrockdb")
}

#' @importFrom R6 R6Class
R6_bedrockdb <- R6::R6Class("bedrockdb", public = list(
    db = NULL,
    path = NULL,
    levelname = NULL,
    leveldat = NULL,
    leveldat_is_dirty = FALSE,
    unique_id = NULL,
    initialize = function(path, ...) {
        path <- fs::path_real(.fixup_path(path))
        dat <- read_leveldat(path)
        self$levelname <- payload(dat$LevelName)
        self$path <- fs::path(path, "db")
        self$db <- bedrock_leveldb_open(self$path, ...)
        self$leveldat <- dat
    },
    close = function(error_if_closed = FALSE) {
        if (self$leveldat_is_dirty) {
            write_leveldat(self$leveldat, fs::path_dir(self$path))
        }
        ret <- bedrock_leveldb_close(self$db, error_if_closed)
        invisible(ret)
    },
    destroy = function() {
        self$close()
        ret <- bedrock_leveldb_destroy(self$path)
        invisible(ret)
    },
    is_open = function() {
        bedrock_leveldb_is_open(self$db)
    },
    property = function(name, error_if_missing = FALSE) {
        bedrock_leveldb_property(self$db, name, error_if_missing)
    },
    get = function(key, readoptions = NULL) {
        bedrock_leveldb_get(self$db, key, readoptions)
    },
    mget = function(keys, readoptions = NULL) {
        bedrock_leveldb_mget(self$db, keys, readoptions)
    },
    mget_prefix = function(starts_with, readoptions = NULL) {
        bedrock_leveldb_mget_prefix(self$db, starts_with, readoptions)
    },
    put = function(key, value, writeoptions = NULL) {
        bedrock_leveldb_put(self$db, key, value, writeoptions)
        invisible(self)
    },
    mput = function(keys, values, writeoptions = NULL) {
        bedrock_leveldb_mput(self$db, keys, values, writeoptions)
        invisible(self)
    },
    delete = function(keys, report = FALSE, readoptions = NULL,
                      writeoptions = NULL) {
        bedrock_leveldb_delete(self$db, keys, report, readoptions,
            writeoptions)
    },
    exists = function(key, readoptions = NULL) {
        bedrock_leveldb_exists(self$db, key, readoptions)
    },
    keys = function(starts_with = NULL, readoptions = NULL) {
        bedrock_leveldb_keys(self$db, starts_with, readoptions)
    },
    keys_len = function(starts_with = NULL, readoptions = NULL) {
        bedrock_leveldb_keys_len(self$db, starts_with, readoptions)
    },
    iterator = function(readoptions = NULL) {
        R6_bedrockdb_iterator$new(self$db, readoptions)
    },
    writebatch = function() {
        R6_bedrockdb_writebatch$new(self$db)
    },
    snapshot = function() {
        bedrock_leveldb_snapshot(self$db)
    },
    approximate_sizes = function(start, limit) {
        bedrock_leveldb_approximate_sizes(self$db, start, limit)
    },
    compact_range = function(start = NULL, limit = NULL) {
        bedrock_leveldb_compact_range(self$db, start, limit)
        invisible(self)
    },
    create_unique_ids = function(n) {
        if (is_null(self$unique_id)) {
            cnt <- payload(self$leveldat$worldStartCount %||% nbt_long(0))
            self$leveldat$worldStartCount <- nbt_long(cnt - 1)
            self$leveldat_is_dirty <- TRUE
            self$unique_id <- (cnt - 2^32) * 2^32
        }
        ret <- self$unique_id + seq_len(n)
        self$unique_id <- self$unique_id + n
        ret
    }
))

R6_bedrockdb_iterator <- R6::R6Class("bedrockdb_iterator", public = list(
    it = NULL,
    initialize = function(db, readoptions) {
        self$it <- bedrock_leveldb_iter_create(db, readoptions)
    },
    destroy = function(error_if_destroyed = FALSE) {
        ret <- bedrock_leveldb_iter_destroy(self$it, error_if_destroyed)
        invisible(ret)
    },
    valid = function() {
        bedrock_leveldb_iter_valid(self$it)
    },
    seek_to_first = function() {
        bedrock_leveldb_iter_seek_to_first(self$it)
        invisible(self)
    },
    seek_to_last = function() {
        bedrock_leveldb_iter_seek_to_last(self$it)
        invisible(self)
    },
    seek = function(key) {
        bedrock_leveldb_iter_seek(self$it, key)
        invisible(self)
    },
    move_next = function(error_if_invalid = FALSE) {
        bedrock_leveldb_iter_next(self$it, error_if_invalid)
        invisible(self)
    },
    move_prev = function(error_if_invalid = FALSE) {
        bedrock_leveldb_iter_prev(self$it, error_if_invalid)
        invisible(self)
    },
    key = function(error_if_invalid = FALSE) {
        bedrock_leveldb_iter_key(self$it, error_if_invalid)
    },
    value = function(error_if_invalid = FALSE) {
        bedrock_leveldb_iter_value(self$it, error_if_invalid)
    }
))

R6_bedrockdb_writebatch <- R6::R6Class("bedrockdb_writebatch", public = list(
    ptr = NULL,
    db = NULL,
    initialize = function(db) {
        self$db <- db
        self$ptr <- bedrock_leveldb_writebatch_create()
    },
    destroy = function(error_if_destroyed = FALSE) {
        ret <- bedrock_leveldb_writebatch_destroy(self$ptr, error_if_destroyed)
        invisible(ret)
    },
    clear = function() {
        bedrock_leveldb_writebatch_clear(self$ptr)
        invisible(self)
    },
    put = function(key, value) {
        bedrock_leveldb_writebatch_put(self$ptr, key, value)
        invisible(self)
    },
    mput = function(keys, values) {
        bedrock_leveldb_writebatch_mput(self$ptr, keys, values)
        invisible(self)
    },
    delete = function(key) {
        bedrock_leveldb_writebatch_delete(self$ptr, key)
        invisible(self)
    },
    mdelete = function(keys) {
        bedrock_leveldb_writebatch_mdelete(self$ptr, keys)
        invisible(self)
    },
    write = function(writeoptions = NULL) {
        bedrock_leveldb_write(self$db, self$ptr, writeoptions)
        invisible(self)
    }
))
