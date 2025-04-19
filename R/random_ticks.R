#' Load and store RandomTicks NBT data
#'
#' RandomTicks data (tag 59) holds a list of NBT values for
#' random ticks.
#'
#' * `get_random_ticks_value()` and `get_random_ticks_data()` load RandomTicks
#' data from `db`. `get_random_ticks_value()` loads data for a single chunk,
#' and `get_random_ticks_data()` loads data for multiple chunks.
#' * `put_random_ticks_value()` and `put_random_ticks_data()` store RandomTicks
#' data for one or multiple chunks into `db`.
#'
#' @inheritParams ChunkNBTData
#'
#' @return `get_random_ticks_value()` returns a list of NBT objects.
#' `get_random_ticks_data()` returns a named list of lists of NBT objects.
#'
#' @name RandomTicks
NULL

#' @rdname RandomTicks
#' @export
get_random_ticks_data <- function(x, z, dimension, db = default_db()) {
    get_chunk_nbt_data(x, z, dimension, tag = 59L, db = db)
}

#' @rdname RandomTicks
#' @export
get_random_ticks_value <- function(x, z, dimension, db = default_db()) {
    get_chunk_nbt_value(x, z, dimension, tag = 59L, db = db)
}

#' @rdname RandomTicks
#' @export
put_random_ticks_data <- function(values, x, z, dimension, db = default_db()) {
    put_chunk_nbt_data(values, x, z, dimension, tag = 59L, db = db)
}

#' @rdname RandomTicks
#' @export
put_random_ticks_value <- function(value, x, z, dimension, db = default_db()) {
    put_chunk_nbt_value(value, x, z, dimension, tag = 59L, db = db)
}
