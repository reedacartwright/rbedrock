#' Read and write legacy chunk version data
#'
#' @description
#' `r lifecycle::badge("superseded")`
#'
#' LegacyChunkVersion data (tag 118) stores the chunk version number for a
#' chunk. In Minecraft version 1.16.100, chunk version data was moved from tag
#' 118 to tag 44.
#'
#' * `get_legacy_chunk_version_value()` and `get_legacy_chunk_version_data()`
#' load LegacyChunkVersion data from `db`. `get_legacy_chunk_version_value()`
#' loads data for a single chunk, and `get_legacy_chunk_version_data()` loads
#' data for multiple chunks.
#' * `put_legacy_chunk_version_value()` and `put_legacy_chunk_version_data()`
#' store LegacyChunkVersion data into `db`.
#'
#' @seealso ChunkVersion
#'
#' @inheritParams ChunkData
#' @param value An integer
#' @param values A (named) vector of LegacyChunkVersion values. If `x` is
#' missing, the names of `values` will be taken as the keys.
#'
#' @return `get_legacy_chunk_version_value()` returns a LegacyChunkVersion data
#' value. `get_legacy_chunk_version_data()` returns a named vector of
#' LegacyChunkVersion data values.
#'
#' @keywords internal
#' @name LegacyChunkVersion
NULL

#' @rdname LegacyChunkVersion
#' @export
get_legacy_chunk_version_value <- function(x, z, dimension, db = default_db()) {
  val <- get_chunk_value(x, z, dimension, tag = 118L, db = db)
  as.integer(val %||% NA_integer_)
}

#' @rdname LegacyChunkVersion
#' @export
get_legacy_chunk_version_data <- function(x, z, dimension, db = default_db()) {
  dat <- get_chunk_data(x, z, dimension, tag = 118L, db = db)
  vapply(dat, function(val) as.integer(val %||% NA_integer_), integer(1L))
}

#' @rdname LegacyChunkVersion
#' @export
put_legacy_chunk_version_value <- function(
  value,
  x,
  z,
  dimension,
  db = default_db()
) {
  value <- as.raw(value)
  put_chunk_value(value, x, z, dimension, tag = 118L, db = db)
}

#' @rdname LegacyChunkVersion
#' @export
put_legacy_chunk_version_data <- function(
  values,
  x,
  z,
  dimension,
  db = default_db()
) {
  values <- lapply(as.list(values), as.raw)
  put_chunk_data(values, x, z, dimension, tag = 118L, db = db)
}
