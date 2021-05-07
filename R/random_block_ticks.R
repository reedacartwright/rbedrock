#' Load and store RandomBlockTicks NBT data
#'
#' RandomBlockTicks data (tag 59) holds a list of NBT values for
#' random ticks.
#'
#' @name RandomBlockTicks
NULL

#' @description
#' `get_random_block_ticks_data()` loads RandomBlockTicks data from a `bedrockdb`.
#' It will silently drop and keys not representing RandomBlockTicks data.
#'
#' @param db A bedrockdb object.
#' @param x,z,dimension Chunk coordinates to extract data from.
#'    `x` can also be a character vector of db keys.
#'
#' @rdname RandomBlockTicks
#' @export
get_random_block_ticks_data <- function(db, x=get_keys(db), z, dimension) {
    keys <- .process_key_args(x,z,dimension, tag=58L)
    get_nbt_values(db, keys, simplify = FALSE)
}

#' @description
#' `put_random_block_ticks_data()` stores RandomBlockTicks data into a `bedrockdb`.
#'
#' @param data A named-list of key-value pairs for RandomBlockTicks data.
#'
#' @export
#' @rdname RandomBlockTicks
put_random_block_ticks_data <- function(db, data) {
    .check_chunk_key_tag(names(data), 58L)
    put_nbt_data(db, data)
}
