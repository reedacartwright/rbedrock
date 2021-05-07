#' Load and store BlockEntities NBT data
#'
#' BlockEntities data (tag 49) holds a list of NBT values for
#' entity data associated with specific blocks.
#'
#' @name BlockEntities
NULL

#' @description
#' `get_block_entities_data()` loads BlockEntities data from a `bedrockdb`.
#' It will silently drop and keys not representing BlockEntities data.
#'
#' @param db A bedrockdb object.
#' @param x,z,dimension Chunk coordinates to extract data from.
#'    `x` can also be a character vector of db keys.
#'
#' @rdname BlockEntities
#' @export
get_block_entities_data <- function(db, x=get_keys(db), z, dimension) {
    keys <- .process_key_args(x,z,dimension, tag=49L)
    get_nbt_values(db, keys, simplify = FALSE)
}

#' @description
#' `put_block_entities_data()` stores BlockEntities data into a `bedrockdb`.
#'
#' @param data A named-list of key-value pairs for BlockEntities data.
#'
#' @rdname BlockEntities
#' @export
put_block_entities_data <- function(db, data) {
    .check_chunk_key_tag(names(data), 49L)
    put_nbt_data(db, data)
}
