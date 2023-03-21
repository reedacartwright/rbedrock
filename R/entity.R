#' Load and store Entity NBT data
#'
#' Entity data (tag 50) holds a list of NBT values for mobs and other entities
#' in the game. After 1.18.30, entity data was migrated to a new actor digest
#' format and no longer saved with chunk data.
#'
#' `get_entity_data()` and `get_entity_value()` load Entity
#' data from `db`. `get_entity_value()` supports loading
#' only a single value.
#'
#' `put_entity_value()`, and `put_entity_data()` store Entity
#' data into `db`.
#'
#' @param db A bedrockdb object.
#' @param x,z,dimension Chunk coordinates to extract data from.
#'    `x` can also be a character vector of db keys.
#' @param values A list of nbt objects. If `x` is missing, the names of `values`
#' will be taken as the keys.
#' @param value An nbt object.

#' @return `get_entity_data()` returns a named-list of nbt data.
#' `get_entity_value()` returns a single nbt value.
#'
#' @name Entity
NULL

#' @rdname Entity
#' @export
get_entity_data <- function(x, z, dimension, db) {
    .get_chunk_nbt_data(x, z, dimension, db, tag = 50L)
}

#' @rdname Entity
#' @export
get_entity_value <- function(x, z, dimension, db) {
    .get_chunk_nbt_value(x, z, dimension, db, tag = 50L)
}

#' @rdname Entity
#' @export
put_entity_data <- function(values, x, z, dimension, db) {
    .put_chunk_nbt_data(values, x, z, dimension, db, tag = 50L)
}

#' @rdname Entity
#' @export
put_entity_value <- function(value, x, z, dimension, db) {
    .put_chunk_nbt_value(value, x, z, dimension, db, tag = 50L)
}
