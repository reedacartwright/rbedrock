#' Load and store RandomTicks NBT data
#'
#' RandomTicks data (tag 59) holds a list of NBT values for
#' random ticks.
#'
#' @name RandomTicks
NULL

#' @description
#' `get_random_ticks_data()` and `get_random_ticks_value()` load RandomTicks
#' data from `db`. `get_random_ticks_data()` will silently drop and keys not
#' representing RandomTicks data. `get_random_ticks_value()` supports loading
#' only a single value. `get_random_ticks_values()` is a synonym for
#' `get_random_ticks_data()`.
#'
#' @param db A bedrockdb object.
#' @param x,z,dimension Chunk coordinates to extract data from.
#'    `x` can also be a character vector of db keys.
#'
#' @return `get_random_ticks_data()` returns a named-list of nbt data.
#' `get_random_ticks_values()` returns a single nbt value.
#'
#' @rdname RandomTicks
#' @export
get_random_ticks_data <- function(db, x, z, dimension) {
    keys <- .process_key_args(x, z, dimension, tag = 59L)
    get_nbt_values(db, keys, simplify = FALSE)
}

#' @rdname RandomTicks
#' @export
get_random_ticks_values <- get_random_ticks_data

#' @rdname RandomTicks
#' @export
get_random_ticks_value <- function(db, x, z, dimension) {
    key <- .process_key_args(x, z, dimension, tag = 59L)
    vec_assert(key, character(), 1L)
    get_nbt_value(db, key, simplify = FALSE)
}

#' @description
#' `put_random_ticks_values()`, `put_random_ticks_value()`, and
#' `put_random_ticks_data()` store RandomTicks data into `db`.
#'
#' @param values A list of nbt objects
#' @param value An nbt object.
#' @param data A named-list specifying key-value pairs.
#' @rdname RandomTicks
#' @export
put_random_ticks_values <- function(db, x, z, dimension, values) {
    keys <- .process_key_args(x, z, dimension, tag = 59L,
                              stop_if_filtered = TRUE)
    put_nbt_values(db, keys, values)
}

#' @rdname RandomTicks
#' @export
put_random_ticks_value <- function(db, x, z, dimension, value) {
    key <- .process_key_args(x, z, dimension, tag = 59L)
    vec_assert(key, character(), 1L)
    put_nbt_value(db, key, value)
}

#' @rdname RandomTicks
#' @export
put_random_ticks_data <- function(db, data) {
    .check_chunk_key_tag(names(data), 59L)
    put_nbt_data(db, data)
}
