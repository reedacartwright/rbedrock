#' Load and store PendingBlockTicks NBT data
#'
#' PendingBlockTicks data (tag 51) holds a list of NBT values for
#' pending ticks.
#'
#' @name PendingBlockTicks
NULL

#' @description
#' `get_pending_block_ticks_data()` loads PendingBlockTicks data from a `bedrockdb`.
#' It will silently drop and keys not representing PendingBlockTicks data.
#'
#' @param db A bedrockdb object.
#' @param x,z,dimension Chunk coordinates to extract data from.
#'    `x` can also be a character vector of db keys.
#'
#' @rdname PendingBlockTicks
#' @export
get_pending_block_ticks_data <- function(db, x=get_keys(db), z, dimension) {
    keys <- .process_key_args(x,z,dimension, tag=51L)
    get_nbt_values(db, keys, simplify = FALSE)
}

#' @description
#' `put_pending_block_ticks_data()` stores PendingBlockTicks data into a `bedrockdb`.
#'
#' @param data A named-list of key-value pairs for PendingBlockTicks data.
#'
#' @rdname PendingBlockTicks
#' @export
put_pending_block_ticks_data <- function(db, data) {
    .check_chunk_key_tag(names(data), 51L)
    put_nbt_data(db, data)
}
