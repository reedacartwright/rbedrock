#' Read and write chunk version data
#'
#' ChunkVersion data (tag 44) stores the chunk version number for a chunk.
#'
#' * `get_chunk_version_value()` and `get_chunk_version_data()` load
#' ChunkVersion data from `db`. `get_chunk_version_value()`
#' loads data for a single chunk, and `get_chunk_version_data()` loads
#' data for multiple chunks.
#' * `put_chunk_version_value()` and `put_chunk_version_data()`
#' store ChunkVersion data into `db`.
#'
#' @seealso ChunkVersion
#'
#' @inheritParams ChunkData
#' @param value An integer
#' @param values A (named) vector of ChunkVersion values. If `x` is
#' missing, the names of `values` will be taken as the keys.
#'
#' @return `get_chunk_version_value()` returns a ChunkVersion data
#' value. `get_chunk_version_data()` returns a named vector of
#' ChunkVersion data values.
#'
#' @name ChunkVersion
NULL

# ChunkVersion to GameVersion:
# https://github.com/Amulet-Team/Amulet-Core/blob/2.0/src_/amulet/level/bedrock/_raw/leveldb_chunk_versions.py #nolint

#' @rdname ChunkVersion
#' @export
get_chunk_version_value <- function(x, z, dimension, db = default_db()) {
    val <- get_chunk_value(x, z, dimension, tag = 44L, db = db)
    as.integer(val %||% NA_integer_)
}

#' @rdname ChunkVersion
#' @export
get_chunk_version_data <- function(x, z, dimension, db = default_db()) {
    dat <- get_chunk_data(x, z, dimension, tag = 44L, db = db)
    vapply(dat, function(val) as.integer(val %||% NA_integer_), integer(1L))
}

#' @rdname ChunkVersion
#' @export
put_chunk_version_value <- function(value, x, z, dimension, db = default_db()) {
    value <- as.raw(value)
    put_chunk_value(value, x, z, dimension, tag = 44L, db = db)
}

#' @rdname ChunkVersion
#' @export
put_chunk_version_data <- function(values, x, z, dimension, db = default_db()) {
    values <- lapply(as.list(values), as.raw)
    put_chunk_data(values, x, z, dimension, tag = 44L, db = db)
}
