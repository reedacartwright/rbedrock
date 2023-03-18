#' Load and store PendingTicks NBT data
#'
#' PendingTicks data (tag 51) holds a list of NBT values for
#' pending ticks.
#'
#' @name PendingTicks
NULL

#' @description
#' `get_pending_ticks_data()` and `get_pending_ticks_value()` load PendingTicks
#' data from `db`. `get_pending_ticks_data()` will silently drop and keys not
#' representing PendingTicks data. `get_pending_ticks_value()` supports loading
#' only a single value. `get_pending_ticks_values()` is a synonym for
#' `get_pending_ticks_data()`.
#'
#' @param db A bedrockdb object.
#' @param x,z,dimension Chunk coordinates to extract data from.
#'    `x` can also be a character vector of db keys.
#'
#' @return `get_pending_ticks_data()` returns a named-list of nbt data.
#' `get_pending_ticks_values()` returns a single nbt value.
#'
#' @rdname PendingTicks
#' @export
get_pending_ticks_data <- function(db, x, z, dimension) {
    keys <- .process_key_args(x, z, dimension, tag = 51L)
    get_nbt_values(db, keys, simplify = FALSE)
}

#' @rdname PendingTicks
#' @export
get_pending_ticks_values <- get_pending_ticks_data

#' @rdname PendingTicks
#' @export
get_pending_ticks_value <- function(db, x, z, dimension) {
    key <- .process_key_args(x, z, dimension, tag = 51L)
    vec_assert(key, character(), 1L)
    get_nbt_value(db, key, simplify = FALSE)
}

#' @description
#' `put_pending_ticks_values()`, `put_pending_ticks_value()`, and
#' `put_pending_ticks_data()` store PendingTicks data into `db`.
#'
#' @param values A list of nbt objects
#' @param value An nbt object.
#' @param data A named-list specifying key-value pairs.
#' @rdname PendingTicks
#' @export
put_pending_ticks_values <- function(db, x, z, dimension, values) {
    keys <- .process_key_args(x, z, dimension, tag = 51L,
                              stop_if_filtered = TRUE)
    put_nbt_values(db, keys, values)
}

#' @rdname PendingTicks
#' @export
put_pending_ticks_value <- function(db, x, z, dimension, value) {
    key <- .process_key_args(x, z, dimension, tag = 51L)
    vec_assert(key, character(), 1L)
    put_nbt_value(db, key, value)
}

#' @rdname PendingTicks
#' @export
put_pending_ticks_data <- function(db, data) {
    .check_chunk_key_tag(names(data), 51L)
    put_nbt_data(db, data)
}
