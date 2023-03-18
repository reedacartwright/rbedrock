#' Load and store BlockEntity NBT data
#'
#' BlockEntity data (tag 49) holds a list of NBT values for
#' entity data associated with specific blocks.
#'
#' @name BlockEntity
NULL

#' @description
#' `get_block_entity_data()` and `get_block_entity_value()` load BlockEntity
#' data from `db`. `get_block_entity_data()` will silently drop and keys not
#' representing BlockEntity data. `get_block_entity_value()` supports loading
#' only a single value. `get_block_entity_values()` is a synonym for
#' `get_block_entity_data()`.
#'
#' @param db A bedrockdb object.
#' @param x,z,dimension Chunk coordinates to extract data from.
#'    `x` can also be a character vector of db keys.
#'
#' @return `get_block_entity_data()` returns a named-list of nbt data.
#' `get_block_entity_values()` returns a single nbt value.
#'
#' @rdname BlockEntity
#' @export
get_block_entity_data <- function(db, x, z, dimension) {
    keys <- .process_key_args(x, z, dimension, tag = 49L)
    get_nbt_values(db, keys, simplify = FALSE)
}

#' @rdname BlockEntity
#' @export
get_block_entity_values <- get_block_entity_data

#' @rdname BlockEntity
#' @export
get_block_entity_value <- function(db, x, z, dimension) {
    key <- .process_key_args(x, z, dimension, tag = 49L)
    vec_assert(key, character(), 1L)
    get_nbt_value(db, key, simplify = FALSE)
}

#' @description
#' `put_block_entity_values()`, `put_block_entity_value()`, and
#' `put_block_entity_data()` store BlockEntity data into `db`.
#'
#' @param values A list of nbt objects
#' @param value An nbt object.
#' @param data A named-list specifying key-value pairs.
#' @rdname BlockEntity
#' @export
put_block_entity_values <- function(db, x, z, dimension, values) {
    keys <- .process_key_args(x, z, dimension, tag = 49L,
                              stop_if_filtered = TRUE)
    put_nbt_values(db, keys, values)
}

#' @rdname BlockEntity
#' @export
put_block_entity_value <- function(db, x, z, dimension, value) {
    key <- .process_key_args(x, z, dimension, tag = 49L)
    vec_assert(key, character(), 1L)
    put_nbt_value(db, key, value)
}

#' @rdname BlockEntity
#' @export
put_block_entity_data <- function(db, data) {
    .check_chunk_key_tag(names(data), 49L)
    put_nbt_data(db, data)
}
