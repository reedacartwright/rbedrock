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

bedrockdb <- function(path, create_if_missing = FALSE, error_if_exists = NULL, paranoid_checks = NULL, 
    write_buffer_size = 4194304L, max_open_files = NULL, block_size = NULL, 
    cache_capacity = 41943040L, bloom_filter_bits_per_key = 10L, compression_level = -1L) {
    R6_bedrockdb$new(path, create_if_missing, error_if_exists, paranoid_checks, write_buffer_size, 
        max_open_files, block_size, cache_capacity, bloom_filter_bits_per_key, compression_level)
}

#' @export
#' @rdname bedrockdb
close.rbedrock_db <- function(con, compact = FALSE, ...) {
    if(isTRUE(compact)) {
        inform("Compacting database...")
        con$compact_range()
    }
    con$close(...)
}

#' @rdname bedrockdb
#' @export
is_bedrockdb <- function(x) {
    inherits(x, "rbedrock_db")
}

#' @rdname bedrockdb
#' @export
close_all_bedrockdb <- function(x) {
    .Call(rbedrock_closeall)
}

#' @importFrom R6 R6Class
R6_bedrockdb <- R6::R6Class("rbedrock_db", public = list(db = NULL, path = NULL, levelname = NULL, 
    initialize = function(path, ...) {
        path <- fs::path_real(.fixup_path(path))
        namefile <- fs::path(path, "levelname.txt")
        self$levelname <- readr::read_file(namefile)
        self$path <- fs::path(path, "db")
        self$db <- db_open(self$path, ...)
    },
    close = function(error_if_closed = FALSE) {
        ret <- db_close(self$db, error_if_closed)
        invisible(ret)
    },
    destroy = function() {
        self$close()
        ret <- db_destroy(self$path)
        invisible(ret)
    },
    is_open = function() {
        db_is_open(self$db)
    },
    property = function(name, error_if_missing = FALSE) {
        db_property(self$db, name, error_if_missing)
    },
    get = function(key, readoptions = NULL) {
        db_get(self$db, key, readoptions)
    },
    mget = function(keys, readoptions = NULL) {
        db_mget(self$db, keys, readoptions)
    },
    mget_prefix = function(starts_with, readoptions = NULL) {
        db_mget_prefix(self$db, starts_with, readoptions)
    },    
    put = function(key, value, writeoptions = NULL) {
        db_put(self$db, key, value, writeoptions)
        invisible(self)
    },
    mput = function(keys, values, writeoptions = NULL) {
        db_mput(self$db, keys, values, writeoptions)
        invisible(self)
    },
    delete = function(key, writeoptions = NULL) {
        db_delete(self$db, key, writeoptions)
        invisible(self)
    },
    mdelete = function(keys, writeoptions = NULL) {
        db_mdelete(self$db, keys, writeoptions)
        invisible(self)
    },
    write = function(keys, values, writeoptions = NULL) {
        db_write(self$db, keys, values, writeoptions)
        invisible(self)
    },
    exists = function(key, readoptions = NULL) {
        db_exists(self$db, key, readoptions)
    },
    keys = function(starts_with = NULL, readoptions = NULL) {
        db_keys(self$db, starts_with, readoptions)
    },
    keys_len = function(starts_with = NULL, readoptions = NULL) {
        db_keys_len(self$db, starts_with, readoptions)
    },
    iterator = function(readoptions = NULL) {
        R6_bedrockdb_iterator$new(self$db, readoptions)
    },
    snapshot = function() {
        db_snapshot(self$db)
    },
    snapshot_release = function(snapshot, error_if_released = FALSE) {
        db_snapshot_release(self$db, snapshot, error_if_released);
    },
    approximate_sizes = function(start, limit) {
        db_approximate_sizes(self$db, start, limit)
    },
    compact_range = function(start = NULL, limit = NULL) {
        db_compact_range(self$db, start, limit)
        invisible(self)
    }
))

R6_bedrockdb_iterator <- R6::R6Class("rbedrock_db_iterator", public = list(it = NULL, 
    initialize = function(db, readoptions) {
        self$it <- db_iter_create(db, readoptions)
    },
    destroy = function(error_if_destroyed = FALSE) {
        ret <- iter_destroy(self$it, error_if_destroyed)
        invisible(ret)
    },
    valid = function() {
        iter_valid(self$it)
    },
    isnil = function() {
        iter_isnil(self$it)
    },
    seek_to_first = function() {
        iter_seek_to_first(self$it)
        invisible(self)
    },
    seek_to_last = function() {
        iter_seek_to_last(self$it)
        invisible(self)
    },
    seek = function(key) {
        iter_seek(self$it, key)
        invisible(self)
    },
    move_next = function(error_if_invalid = FALSE) {
        iter_next(self$it, error_if_invalid)
        invisible(self)
    },
    move_prev = function(error_if_invalid = FALSE) {
        iter_prev(self$it, error_if_invalid)
        invisible(self)
    },
    key = function(error_if_invalid = FALSE) {
        iter_key(self$it, error_if_invalid)
    },
    value = function(error_if_invalid = FALSE) {
        iter_value(self$it, error_if_invalid)
    }
))
