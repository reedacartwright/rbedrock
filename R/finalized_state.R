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
#' @name FinalizedState
NULL

#' @description
#' `get_finalized_state_data()` and `get_finalized_state_value()` load
#' FinalizedState data from `db`. `get_finalized_state_data()` will silently
#' drop and keys not representing FinalizedState data.
#' `get_finalized_state_value()` supports loading only a single value.
#' `get_finalized_state_values()` is a synonym for `get_finalized_state_data()`.
#'
#' @param db A bedrockdb object.
#' @param x,z,dimension Chunk coordinates to extract data from.
#'    `x` can also be a character vector of db keys.
#'
#' @return `get_finalized_state_data()` returns a named integer vector
#'         of the values returned by `get_finalized_state_value()`.
#' @rdname FinalizedState
#' @export
get_finalized_state_data <- function(db, x, z, dimension) {
    keys <- .process_key_args(x, z, dimension, tag = 54L)
    dat <- get_values(db, keys)
    purrr::map_int(dat, read_finalized_state_value)
}

#' @rdname FinalizedState
#' @export
get_finalized_state_values <- get_finalized_state_data

#' @rdname FinalizedState
#' @export
get_finalized_state_value <- function(db, x, z, dimension) {
    key <- .process_key_args(x, z, dimension, tag = 54L)
    vec_assert(key, character(), 1L)
    dat <- get_value(db, key)
    read_finalized_state_value(dat)
}

#' @description
#' `put_finalized_state_data()`, `put_finalized_state_values()`, and
#' `put_finalized_state_value()` store FinalizedState data into a `bedrockdb`.
#'
#' @param data A named-vector of key-value pairs for FinalizedState data.
#'
#' @rdname FinalizedState
#' @export
put_finalized_state_data <- function(db, data) {
    .check_chunk_key_tag(names(data), 54L)
    dat <- purrr::map(data, write_finalized_state_value)
    put_data(db, dat)
}

#' @param values An integer vector
#'
#' @rdname FinalizedState
#' @export
put_finalized_state_values <- function(db, x, z, dimension, values) {
    keys <- .process_key_args(x, z, dimension, tag = 54L,
                              stop_if_filtered = TRUE)
    values <- vctrs::vec_recycle(values, length(keys), x_arg = "values")
    values <- purrr::map(values, write_finalized_state_value)
    put_values(db, keys, values)
}

#' @param value A scalar integer vector
#'
#' @rdname FinalizedState
#' @export
put_finalized_state_value <- function(db, x, z, dimension, value) {
    key <- .process_key_args(x, z, dimension, tag = 54L)
    vec_assert(key, character(), 1L)
    value <- write_finalized_state_value(value)
    put_value(db, key, value)
}

#' @description
#' `read_finalized_state_value()` parses a binary FinalizedState record.
#'
#' @param rawdata a raw vector
#'
#' @rdname FinalizedState
#' @export
read_finalized_state_value <- function(rawdata) {
    readBin(rawdata, integer(), n = 1L, size = 4L, endian = "little")
}

#' @description
#' `write_finalized_state_value()` converts a FinalizedState value
#' to a raw vector.
#'
#' @param value a scalar integer
#'
#' @rdname FinalizedState
#' @export
write_finalized_state_value <- function(value) {
    stopifnot(rlang::is_scalar_integerish(value))
    writeBin(as.integer(value), raw(), size = 4L, endian = "little")
}
