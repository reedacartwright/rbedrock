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

##' @export
bedrockdb <- function(path, create_if_missing = FALSE, error_if_exists = NULL, paranoid_checks = NULL, 
    write_buffer_size = 4194304, max_open_files = NULL, block_size = NULL, use_compression = 4, 
    cache_capacity = 41943040, bloom_filter_bits_per_key = 10) {
    R6_bedrockdb$new(path, create_if_missing, error_if_exists, paranoid_checks, write_buffer_size, 
        max_open_files, block_size, use_compression, cache_capacity, bloom_filter_bits_per_key)
}

##' @importFrom R6 R6Class
R6_bedrockdb <- R6::R6Class("bedrockdb", public = list(db = NULL, path = NULL, levelname = NULL, 
    mtime = NULL, initialize = function(path, ...) {
        if (file.exists(path)) {
            path <- normalizePath(path)
        } else {
            wpath <- paste0(worlds_path(), "/", path)
            if (file.exists(wpath)) {
                path <- normalizePath(wpath)
            }
        }
        namefile <- paste0(path, "/levelname.txt")
        self$levelname <- readLines(namefile, 1L, warn = FALSE)
        self$mtime <- as.character(file.mtime(namefile))
        self$path <- paste0(path, "/db")
        self$db <- bedrock_leveldb_open(self$path, ...)
    },
    close = function(error_if_closed = FALSE) {
        bedrock_leveldb_close(self$db, error_if_closed)
    },
    destroy = function() {
        self$close()
        bedrock_leveldb_destroy(self$path)
    },
    property = function(name, error_if_missing = FALSE) {
        bedrock_leveldb_property(self$db, name, error_if_missing)
    },
    get = function(key, as_raw = NULL, error_if_missing = FALSE, readoptions = NULL) {
        bedrock_leveldb_get(self$db, from_bedrockdb_key(key), as_raw, error_if_missing, 
            readoptions)
    },
    mget = function(key, as_raw = NULL, missing_value = NULL, missing_report = TRUE, 
        readoptions = NULL) {
        bedrock_leveldb_mget(self$db, from_bedrockdb_key(key), as_raw, missing_value, 
            missing_report, readoptions)
    },
    put = function(key, value, writeoptions = NULL) {
        bedrock_leveldb_put(self$db, from_bedrockdb_key(key), value, writeoptions)
    },
    mput = function(key, value, writeoptions = NULL) {
        bedrock_leveldb_mput(self$db, from_bedrockdb_key(key), value, writeoptions)
    },
    delete = function(key, report = FALSE, readoptions = NULL, writeoptions = NULL) {
        bedrock_leveldb_delete(self$db, from_bedrockdb_key(key), report, readoptions, 
            writeoptions)
    },
    exists = function(key, readoptions = NULL) {
        bedrock_leveldb_exists(self$db, from_bedrockdb_key(key), readoptions)
    },
    keys = function(starts_with = NULL, readoptions = NULL) {
        to_bedrockdb_key(bedrock_leveldb_keys(self$db, starts_with, as_raw = TRUE, 
            readoptions))
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
    compact_range = function(start, limit) {
        bedrock_leveldb_compact_range(self$db, start, limit)
    }))

R6_bedrockdb_iterator <- R6::R6Class("bedrockdb_iterator", public = list(it = NULL, 
    initialize = function(db, readoptions) {
        self$it <- bedrock_leveldb_iter_create(db, readoptions)
    },
    destroy = function(error_if_destroyed = FALSE) {
        bedrock_leveldb_iter_destroy(self$it, error_if_destroyed)
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
        to_bedrockdb_key(bedrock_leveldb_iter_key(self$it, as_raw = TRUE, error_if_invalid))
    },
    value = function(as_raw = NULL, error_if_invalid = FALSE) {
        bedrock_leveldb_iter_value(self$it, as_raw, error_if_invalid)
    }))
R6_bedrockdb_writebatch <- R6::R6Class("bedrockdb_writebatch", public = list(ptr = NULL, 
    db = NULL, initialize = function(db) {
        self$db <- db
        self$ptr <- bedrock_leveldb_writebatch_create()
    },
    destroy = function(error_if_destroyed = FALSE) {
        bedrock_leveldb_writebatch_destroy(self$ptr, error_if_destroyed)
    },
    clear = function() {
        bedrock_leveldb_writebatch_clear(self$ptr)
        invisible(self)
    },
    put = function(key, value) {
        bedrock_leveldb_writebatch_put(self$ptr, from_bedrockdb_key(key), value)
        invisible(self)
    },
    mput = function(key, value) {
        bedrock_leveldb_writebatch_mput(self$ptr, from_bedrockdb_key(key), value)
        invisible(self)
    },
    delete = function(key) {
        bedrock_leveldb_writebatch_delete(self$ptr, from_bedrockdb_key(key))
        invisible(self)
    },
    write = function(writeoptions = NULL) {
        bedrock_leveldb_write(self$db, self$ptr, writeoptions)
        invisible(self)
    }))

create_bedrockdb_key <- function(x, z, d, tag, subtag = NA) {
    ret <- stringr::str_c(x, z, d, tag, sep = ":")
    ret <- stringr::str_c("@", ret)
    if (!is.na(subtag)) {
        ret <- stringr::str_c(ret, subtag, sep = "-")
    }
    return(ret)
}

to_bedrockdb_key <- function(key) {
    if (is.character(key)) {
        key <- lapply(key, charToRaw)
    }
    out <- sapply(key, function(k) {
        if (!is.raw(k)) {
            stop("One or more keys is not raw().")
        }
        len <- length(k)
        x <- NA
        z <- NA
        d <- NA
        tag <- NA
        subtag <- NA
        if (len == 9 || len == 10) {
            xz <- readBin(k, "integer", n = 2L, endian = "little")
            x <- xz[1]
            z <- xz[2]
            d <- 0
            tag <- as.integer(k[9])
            subtag <- NA
            if (len == 10) {
                subtag <- as.integer(k[10])
            }
        } else if (len == 13 || len == 14) {
            xz <- readBin(k, "integer", n = 3L, endian = "little")
            x <- xz[1]
            z <- xz[2]
            d <- xz[3]
            tag <- as.integer(k[13])
            subtag <- NA
            if (len == 14) {
                subtag <- as.integer(k[14])
            }
        } else {
            return(rawToChar(k))
        }
        return(create_bedrockdb_key(x, z, d, tag, subtag))
    })
    return(out)
}

from_bedrockdb_key <- function(key) {
    m <- stringr::str_match(key, "^@([^:]+):([^:]+):([^:]+):([^:-]+)(?:-([^:]+))?$|^.*$")
    s <- split(m, seq_len(nrow(m)))
    out <- lapply(s, function(k) {
        if (is.na(k[2])) {
            return(charToRaw(k[1]))
        }
        x <- as.integer(k[2])
        z <- as.integer(k[3])
        d <- as.integer(k[4])
        tag <- as.integer(k[5])
        subtag <- as.integer(k[6])
        r <- writeBin(c(x, z), raw(), size = 4, endian = "little")
        if (d > 0) {
            r <- c(r, writeBin(d, raw(), size = 4, endian = "little"))
        }
        r <- c(r, writeBin(tag, raw(), size = 1, endian = "little"))
        if (!is.na(subtag)) {
            r <- c(r, writeBin(subtag, raw(), size = 1, endian = "little"))
        }
        return(r)
    })
    if (length(out) == 1) {
        return(out[[1]])
    }
    return(out)
}
