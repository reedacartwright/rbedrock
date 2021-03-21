#' Load and store RandomBlockTicks NBT data
#'
#' @description
#' `get_random_block_ticks_data` loads RandomBlockTicks NBT data from a `bedrockdb`.
#'
#' @param db A bedrockdb object.
#' @param x,z,dimension Chunk coordinates to extract data from.
#'    `x` can also be a character vector of db keys and any keys not
#'    representing RandomBlockTicks data will be silently dropped.
#' @param data A named-list of key-value pairs for RandomBlockTicks NBT data.
#'
#' @export
get_random_block_ticks_data <- function(db, x=get_keys(db), z, dimension) {
    keys <- .process_key_args(x,z,dimension, tag=58L)
    get_nbt_values(db, keys, simplify = FALSE)
}

#' @description
#' `put_random_block_ticks_data` stores RandomBlockTicks NBT data into a `bedrockdb`.
#'
#' @export
#' @rdname get_random_block_ticks_data
put_random_block_ticks_data <- function(db, data) {
    stopifnot(check_chunk_key_tag(names(data), 58L))
    put_nbt_data(db, data)
}
