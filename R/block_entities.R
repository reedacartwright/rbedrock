#' Load and store BlockEntities NBT data
#'
#' @description
#' `get_block_entities_data` loads BlockEntities NBT data from a `bedrockdb`.
#'
#' @param db A bedrockdb object.
#' @param x,z,dimension Chunk coordinates to extract data from.
#'    `x` can also be a character vector of db keys and any keys not
#'    representing BlockEntities data will be silently dropped.
#' @param data A named-list of key-value pairs for BlockEntities NBT data.
#'
#' @export
get_block_entities_data <- function(db, x=get_keys(db), z, dimension) {
    keys <- .process_key_args(x,z,dimension, tag=49L)
    get_nbt_values(db, keys, simplify = FALSE)
}

#' @description
#' `put_block_entities_data` stores BlockEntities NBT data into a `bedrockdb`.
#'
#' @export
#' @rdname get_block_entities_data
put_block_entities_data <- function(db, data) {
    stopifnot(check_chunk_key_tag(names(data), 49L))
    put_nbt_data(db, data)
}
