#' Load and store Finalization data
#'
#' Finalization data (tag 54) holds a number which
#' indicates a chunk's state of generation.
#'
#' Finalization data contains the following information.
#'
#' | Value | Name | Description |
#' |-------|------|-------------|
#' | 0     | NeedsInstaticking | Chunk needs to be ticked |
#' | 1     | NeedsPopulation | Chunk needs to be populated with mobs |
#' | 2     | Done            | Chunk generation is fully complete |
#' 
#' @name Finalization
NULL

#' @description
#' `get_finalization_data()` loads Finalization data from a `bedrockdb`.
#'  It will silently drop and keys not representing Finalization data.
#'
#' @param db A bedrockdb object.
#' @param x,z,dimension Chunk coordinates to extract data from.
#'    `x` can also be a character vector of db keys.
#'
#' @return `get_finalization_data()` returns a named integer vector
#'         of the values returned by `get_finalization_value()`.
#' @rdname Finalization
#' @export
get_finalization_data <- function(db, x, z, dimension) {
    keys <- .process_key_args(x ,z, dimension, tag=54L)
    dat <- get_values(db, keys)
    purrr::map_int(dat, read_finalization_value)
}

#' @description
#' `get_finalization_value()` loads Finalization data from a `bedrockdb`.
#' It only supports loading a single value.
#'
#' @return `get_finalization_value()` and `read_finalization_value()`
#'         return an integer.
#' @rdname Finalization
#' @export
get_finalization_value <- function(db, x, z, dimension) {
    key <- .process_key_args(x, z, dimension, tag=54L)
    vec_assert(key, character(), 1L)
    dat <- get_value(db, key)
    read_finalization_value(dat)
}

#' @description
#' `put_finalization_data()`, `put_finalization_values()`, and
#' `put_finalization_value()` store Finalization data into a `bedrockdb`.
#'
#' @param data A named-vector of key-value pairs for Finalization data.
#'
#' @rdname Finalization
#' @export
put_finalization_data <- function(db, data) {
    .check_chunk_key_tag(names(data), 54L)
    dat <- purrr::map(data, write_finalization_value)
    put_data(db, dat)
}

#' @param values An integer vector
#'
#' @rdname Finalization
#' @export
put_finalization_values <- function(db, x, z, dimension, values) {
    keys <- .process_key_args(x, z, dimension, tag=54L, stop_if_filtered = TRUE)
    values <- vctrs::vec_recycle(values, length(keys), x_arg="values")
    values <- purrr::map(values, write_finalization_value)
    put_values(db, keys, values)
}

#' @param value A scalar integer vector
#'
#' @rdname Finalization
#' @export
put_finalization_value <- function(db, x, z, dimension, value) {
    key <- .process_key_args(x, z, dimension, tag=54L)
    vec_assert(key, character(), 1L)
    value <- write_finalization_value(value)
    put_value(db, key, value)
}

#' @description
#' `read_finalization_value()` parses a binary Finalization record.
#'
#' @param rawdata a raw vector
#'
#' @rdname Finalization
#' @export
read_finalization_value <- function(rawdata) {
    readBin(rawdata, integer(), n=1L, size=4L, endian="little")
}

#' @description
#' `write_finalization_value()` converts a Finalization value
#' to a raw vector.
#'
#' @param value a scalar integer
#'
#' @rdname Finalization
#' @export
write_finalization_value <- function(value) {
    stopifnot(rlang::is_scalar_integerish(value))
    writeBin(as.integer(value), raw(), size=4L, endian="little")
}
