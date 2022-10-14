#' Load and store FinalizedState data
#'
#' FinalizedState data (tag 54) holds a number which
#' indicates a chunk's state of generation.
#'
#' FinalizedState data contains the following information.
#'
#' | Value | Name | Description |
#' |-------|------|-------------|
#' | 0     | NeedsInstaticking | Chunk needs to be ticked |
#' | 1     | NeedsPopulation | Chunk needs to be populated with mobs |
#' | 2     | Done            | Chunk generation is fully complete |
#' 
#' `get_finalized_state_data()` and `get_finalized_state_value()` load FinalizedState
#' data from `db`. `get_finalized_statevalue()` supports loading
#' only a single value.
#'
#' `put_finalized_state_value()` and `put_finalized_state_data()` store
#' FinalizedState data into `db`.
#'
#' `read_finalized_state_value()` parses a binary FinalizedState record and
#' `write_finalized_state_value()` converts a FinalizedState value
#' to a raw vector.
#'
#' @param db A bedrockdb object.
#' @param x,z,dimension Chunk coordinates to extract data from.
#'    `x` can also be a character vector of db keys.
#' @param values An integer vector. If `x` is missing, the names of `values` will be taken as the keys.
#' @param value A scalar integer vector.
#' @param rawdata a raw vector.
#'
#' @return `get_finalized_state__data()` returns a integer vector with names.
#' `get_pending_ticks_value()` returns a single integer value.
#'
#' @name FinalizedState
NULL

#' @rdname FinalizedState
#' @export
get_finalized_state_data <- function(x, z, dimension, db) {
    dat <- .get_chunk_data(x, z, dimension, db, tag=54L)
    purrr::map_int(dat, read_finalized_state_value)
}

#' @rdname FinalizedState
#' @export
get_finalized_state_value <- function(x, z, dimension, db) {
    dat <- .get_chunk_value(x, z, dimension, db, tag=54L)
    read_finalized_state_value(dat)
}

#' @rdname FinalizedState
#' @export
put_finalized_state_data <- function(values, x, z, dimension, db) {
    dat <- purrr::map(values, write_finalized_state_value)
    .put_chunk_data(dat, x, z, dimension, db, tag=54L)
}

#' @rdname FinalizedState
#' @export
put_finalized_state_value <- function(value, x, z, dimension, db) {
    value <- write_finalized_state_value(value)
    .put_chunk_value(value, x, z, dimension, db, tag=54L)
}

#' @rdname FinalizedState
#' @export
read_finalized_state_value <- function(rawdata) {
    readBin(rawdata, integer(), n=1L, size=4L, endian="little")
}

#' @rdname FinalizedState
#' @export
write_finalized_state_value <- function(value) {
    stopifnot(rlang::is_scalar_integerish(value))
    writeBin(as.integer(value), raw(), size=4L, endian="little")
}
