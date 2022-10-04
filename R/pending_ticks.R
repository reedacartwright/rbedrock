#' Load and store PendingTicks NBT data
#'
#' PendingTicks data (tag 51) holds a list of NBT values for
#' pending ticks.
#'
#' `get_pending_ticks_data()` and `get_pending_ticks_value()` load PendingTicks
#' data from `db`. `get_pending_ticks_value()` supports loading
#' only a single value.
#'
#' `put_pending_ticks_value()` and `put_pending_ticks_data()` store
#' PendingTicks data into `db`.
#'
#' @param db A bedrockdb object.
#' @param x,z,dimension Chunk coordinates to extract data from.
#'    `x` can also be a character vector of db keys.
#' @param values A list of nbt objects. If `x` is missing, the names of `values` will be taken as the keys.
#' @param value An nbt object.
#'
#' @return `get_pending_ticks_data()` returns a named-list of nbt data.
#' `get_pending_ticks_values()` returns a single nbt value.
#' 
#' @name PendingTicks
NULL

#' @rdname PendingTicks
#' @export
get_pending_ticks_data <- function(x, z, dimension, db) {
    keys <- .process_chunk_key_args(x, z, dimension, tag = 51L, assert_validity = TRUE)
    get_nbt_data(keys, db, simplify = FALSE)

}

#' @rdname PendingTicks
#' @export
get_pending_ticks_value <- function(x, z, dimension, db) {
    key <- .process_chunk_key_args(x, z, dimension, tag = 51L, assert_validity = TRUE, assert_scalar = TRUE)
    get_nbt_value(key, db, simplify = FALSE)
}

#' @rdname PendingTicks
#' @export
put_pending_ticks_data <- function(values, db) {
    keys <- .process_chunk_key_args(x, z, dimension, tag = 51L, values = values, assert_validity = TRUE)
    put_nbt_data(values = values, keys=keys, db = db)

}

#' @rdname PendingTicks
#' @export
put_pending_ticks_value <- function(x, z, dimension, value, db) {
    key <- .process_chunk_key_args(x, z, dimension, tag = 51L, assert_validity = TRUE, assert_scalar = TRUE)
    put_nbt_value(value=value, key = key, db = db)
}
