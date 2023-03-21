#' Load and store BlockEntity NBT data
#'
#' BlockEntity data (tag 49) holds a list of NBT values for
#' entity data associated with specific blocks.
#'
#' `get_block_entity_data()` and `get_block_entity_value()` load BlockEntity
#' data from `db`. `get_block_entity_value()` supports loading
#' only a single value.
#'
#' `put_block_entity_value()`, and `put_block_entity_data()` store BlockEntity
#' data into `db`.
#'
#' @param db A bedrockdb object.
#' @param x,z,dimension Chunk coordinates to extract data from.
#'    `x` can also be a character vector of db keys.
#' @param values A list of nbt objects. If `x` is missing, the names of `values`
#' will be taken as the keys.
#' @param value An nbt object.
#'
#' @return `get_block_entity_data()` returns a named-list of nbt data.
#' `get_block_entity_value()` returns a single nbt value.
#'
#' @name BlockEntity
NULL

#' @rdname BlockEntity
#' @export
get_block_entity_data <- function(x, z, dimension, db) {
    .get_chunk_nbt_data(x, z, dimension, db, tag = 49L)
}

#' @rdname BlockEntity
#' @export
get_block_entity_value <- function(x, z, dimension, db) {
    .get_chunk_nbt_value(x, z, dimension, db, tag = 49L)
}

#' @rdname BlockEntity
#' @export
put_block_entity_data <- function(values, x, z, dimension, db) {
    .put_chunk_nbt_data(values, x, z, dimension, db, tag = 49L)
}

#' @rdname BlockEntity
#' @export
put_block_entity_value <- function(value, x, z, dimension, db) {
    .put_chunk_nbt_value(value, x, z, dimension, db, tag = 49L)
}
