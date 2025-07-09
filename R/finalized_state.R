#' Load and store FinalizedState data
#'
#' FinalizedState data (tag 54) holds a number which indicates a chunk's state
#' of generation.
#'
#' FinalizedState data contains the following information.
#'
#' | Value | Name | Description |
#' |-------|------|-------------|
#' | 0     | NeedsInstaticking | Chunk needs to be ticked |
#' | 1     | NeedsPopulation | Chunk needs to be populated with mobs |
#' | 2     | Done            | Chunk generation is fully complete |
#'
#' * `get_finalized_state_value()` and `get_finalized_state_data()`
#' load FinalizedState data from `db`. `get_finalized_state_value()`
#' loads data for a single chunk, and `get_finalized_state_data()` loads
#' data for multiple chunks.
#' * `put_finalized_state_value()` and `put_finalized_state_data()`
#' store FinalizedState data into `db`.
#'
#' @inheritParams ChunkData
#' @param value An integer
#' @param values A (named) vector of FinalizedState values. If `x` is
#' missing, the names of `values` will be taken as the keys.
#'
#' @return `get_finalized_state_value()` returns a ChunkVersion data
#' value. `get_finalized_state_data()` returns a named vector of
#' FinalizedState data values.
#'
#' @name FinalizedState
NULL

raw_na <- as.raw(c(0x00, 0x00, 0x00, 0x80))

#' @rdname FinalizedState
#' @export
get_finalized_state_value <- function(x, z, dimension, db = default_db()) {
  value <- get_chunk_value(x, z, dimension, tag = 54L, db = db)
  readBin(value %||% raw_na, integer(), n = 1L, size = 4L, endian = "little")
}

#' @rdname FinalizedState
#' @export
get_finalized_state_data <- function(x, z, dimension, db = default_db()) {
  dat <- get_chunk_data(x, z, dimension, tag = 54L, db = db)
  vapply(
    dat,
    function(value) {
      readBin(
        value %||% raw_na,
        integer(),
        n = 1L,
        size = 4L,
        endian = "little"
      )
    },
    integer(1L)
  )
}

#' @rdname FinalizedState
#' @export
put_finalized_state_value <- function(
  value,
  x,
  z,
  dimension,
  db = default_db()
) {
  value <- writeBin(as.integer(value), raw(), size = 4L, endian = "little")
  put_chunk_value(value, x, z, dimension, tag = 54L, db = db)
}

#' @rdname FinalizedState
#' @export
put_finalized_state_data <- function(
  values,
  x,
  z,
  dimension,
  db = default_db()
) {
  values <- lapply(as.list(values), function(value) {
    writeBin(as.integer(value), raw(), size = 4L, endian = "little")
  })
  put_chunk_data(values, x, z, dimension, tag = 54L, db = db)
}
