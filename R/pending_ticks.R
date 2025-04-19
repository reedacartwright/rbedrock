#' Load and store PendingTicks NBT data
#'
#' PendingTicks data (tag 51) holds a list of NBT values for
#' pending ticks.
#'
#' * `get_pending_ticks_value()` and `get_pending_ticks_data()` load
#' PendingTicks data from `db`. `get_pending_ticks_value()` loads data for a
#' single chunk, and `get_pending_ticks_data()` loads data for multiple chunks.
#' * `put_pending_ticks_value()` and `put_pending_ticks_data()` store
#' PendingTicks data for one or multiple chunks into `db`.
#'
#' @inheritParams ChunkNBTData
#'
#' @return `get_pending_ticks_value()` returns a list of NBT objects.
#' `get_pending_ticks_data()` returns a named list of lists of NBT objects.
#'
#' @name PendingTicks
NULL

#' @rdname PendingTicks
#' @export
get_pending_ticks_data <- function(x, z, dimension, db = default_db()) {
    get_chunk_nbt_data(x, z, dimension, tag = 51L, db = db)
}

#' @rdname PendingTicks
#' @export
get_pending_ticks_value <- function(x, z, dimension, db = default_db()) {
    get_chunk_nbt_value(x, z, dimension, tag = 51L, db = db)
}

#' @rdname PendingTicks
#' @export
put_pending_ticks_data <- function(values, x, z, dimension, db = default_db()) {
    put_chunk_nbt_data(values, x, z, dimension, tag = 51L, db = db)
}

#' @rdname PendingTicks
#' @export
put_pending_ticks_value <- function(value, x, z, dimension, db = default_db()) {
    put_chunk_nbt_value(value, x, z, dimension, tag = 51L, db = db)
}
