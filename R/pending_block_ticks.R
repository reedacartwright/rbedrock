#' Load and store PendingBlockTicks NBT data
#'
#' @description
#' `get_pending_block_ticks_data` loads PendingBlockTicks NBT data from a `bedrockdb`.
#'
#' @param db A bedrockdb object.
#' @param x,z,dimension Chunk coordinates to extract data from.
#'    `x` can also be a character vector of db keys and any keys not
#'    representing PendingBlockTicks data will be silently dropped.
#' @param data A named-list of key-value pairs for PendingBlockTicks NBT data.
#'
#' @export
get_pending_block_ticks_data <- function(db, x=get_keys(db), z, dimension) {
    keys <- .process_key_args(x,z,dimension, tag=51L)
    get_nbt_values(db, keys, simplify = FALSE)
}

#' @description
#' `put_pending_block_ticks_data` stores PendingBlockTicks NBT data into a `bedrockdb`.
#'
#' @export
#' @rdname get_pending_block_ticks_data
put_pending_block_ticks_data <- function(db, data) {
    stopifnot(check_chunk_key_tag(names(data), 51L))
    put_nbt_data(db, data)
}
