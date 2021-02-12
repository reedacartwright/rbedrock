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

#' Open a Minecraft: Bedrock Edition world for reading and writing.
#'
#' @param path The path to a world folder. If the path does not exist, it is 
#'   assumed to be the base name of a world folder in the local minecraftWorlds
#'   directory.
#' @return On success, an R6 class of type "bedrockdb" that contains and open handle to the
#'   leveldb database that contains the world information.
#' @examples
#' \dontrun{db <- bedrockdb("x7fuXRc8AAA=")
#' o <- db$get("Overworld")
#' read_nbt(o)
#' db$close()}
#'
#' @export
bedrockdb <- function(path, create_if_missing = FALSE, error_if_exists = NULL, paranoid_checks = NULL, 
    write_buffer_size = 4194304, max_open_files = NULL, block_size = NULL, use_compression = 4, 
    cache_capacity = 41943040, bloom_filter_bits_per_key = 10) {
    R6_bedrockdb$new(path, create_if_missing, error_if_exists, paranoid_checks, write_buffer_size, 
        max_open_files, block_size, use_compression, cache_capacity, bloom_filter_bits_per_key)
}

#' @importFrom R6 R6Class
R6_bedrockdb <- R6::R6Class("bedrockdb", public = list(db = NULL, path = NULL, levelname = NULL, 
    mtime = NULL, initialize = function(path, ...) {
        path <- .fixup_path(path)
        namefile <- paste0(path, "/levelname.txt")
        self$levelname <- readLines(namefile, 1L, warn = FALSE)
        self$mtime <- as.character(file.mtime(namefile))
        self$path <- paste0(path, "/db")
        self$db <- bedrock_leveldb_open(self$path, ...)
    },
    close = function(error_if_closed = FALSE) {
        ret <- bedrock_leveldb_close(self$db, error_if_closed)
        invisible(ret)
    },
    destroy = function() {
        self$close()
        ret <- bedrock_leveldb_destroy(self$path)
        invisible(ret)
    },
    property = function(name, error_if_missing = FALSE) {
        bedrock_leveldb_property(self$db, name, error_if_missing)
    },
    get = function(key, as_raw = NULL, error_if_missing = FALSE, readoptions = NULL) {
        bedrock_leveldb_get(self$db, .from_strkey(key), as_raw, error_if_missing, 
            readoptions)
    },
    mget = function(keys, as_raw = NULL, missing_value = NULL, missing_report = FALSE,
        readoptions = NULL) {
        y <- bedrock_leveldb_mget(self$db, .from_strkey(keys), as_raw, missing_value, 
            missing_report, readoptions)
        rlang::set_names(y, keys)
    },
    put = function(value, key, writeoptions = NULL) {
        ret <- bedrock_leveldb_put(self$db, .from_strkey(key), value, writeoptions)
        invisible(self)
    },
    # values comes before keys so we can pass a named list to this function
    mput = function(values, keys = names(values), writeoptions = NULL) {
        ret <- bedrock_leveldb_mput(self$db, .from_strkey(keys), values, writeoptions)
        invisible(self)
    },
    delete = function(key, report = FALSE, readoptions = NULL, writeoptions = NULL) {
        bedrock_leveldb_delete(self$db, .from_strkey(key), report, readoptions, 
            writeoptions)
    },
    exists = function(key, readoptions = NULL) {
        bedrock_leveldb_exists(self$db, .from_strkey(key), readoptions)
    },
    keys = function(readoptions = NULL) {
        # use the c-level function here for speed
        bedrock_leveldb_strkeys(self$db, readoptions)
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
    }
))

R6_bedrockdb_iterator <- R6::R6Class("bedrockdb_iterator", public = list(it = NULL, 
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
        bedrock_leveldb_iter_seek(self$it, .from_strkey(key))
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
    key = function(error_if_invalid = FALSE, as_raw = FALSE) {
        key <- bedrock_leveldb_iter_key(self$it, as_raw = TRUE, error_if_invalid)
        if(as_raw) {
            return(key)
        }
        return(.to_strkey(key))
    },
    value = function(as_raw = NULL, error_if_invalid = FALSE) {
        bedrock_leveldb_iter_value(self$it, as_raw, error_if_invalid)
    }
))

R6_bedrockdb_writebatch <- R6::R6Class("bedrockdb_writebatch", public = list(ptr = NULL, 
    db = NULL, initialize = function(db) {
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
        bedrock_leveldb_writebatch_put(self$ptr, .from_strkey(key), value)
        invisible(self)
    },
    mput = function(key, value) {
        bedrock_leveldb_writebatch_mput(self$ptr, .from_strkey(key), value)
        invisible(self)
    },
    delete = function(key) {
        bedrock_leveldb_writebatch_delete(self$ptr, .from_strkey(key))
        invisible(self)
    },
    write = function(writeoptions = NULL) {
        bedrock_leveldb_write(self$db, self$ptr, writeoptions)
        invisible(self)
    }
))
