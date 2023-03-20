#' Load and store Entity NBT data
#'
#' Entity data (tag 50) holds a list of NBT values for mobs and other entities
#' in the game. After 1.18.30, entity data was migrated to a new actor digest
#' format and no longer saved with chunk data.
#'
#' @name Entity
NULL

#' @description
#' `get_entity_data()` and `get_entity_value()` load Entity
#' data from `db`. `get_entity_data()` will silently drop and keys not
#' representing Entity data. `get_entity_value()` supports loading
#' only a single value. `get_entity_values()` is a synonym for
#' `get_entity_data()`.
#'
#' @param db A bedrockdb object.
#' @param x,z,dimension Chunk coordinates to extract data from.
#'    `x` can also be a character vector of db keys.
#'
#' @return `get_entity_data()` returns a named-list of nbt data.
#' `get_entity_values()` returns a single nbt value.
#'
#' @rdname Entity
#' @export
get_entity_data <- function(db, x, z, dimension) {
    keys <- .process_key_args(x, z, dimension, tag = 50L)
    get_nbt_values(db, keys, simplify = FALSE)
}

#' @rdname Entity
#' @export
get_entity_values <- get_entity_data

#' @rdname Entity
#' @export
get_entity_value <- function(db, x, z, dimension) {
    key <- .process_key_args(x, z, dimension, tag = 50L)
    vec_assert(key, character(), 1L)
    get_nbt_value(db, key, simplify = FALSE)
}

#' @description
#' `put_entity_values()`, `put_entity_value()`, and
#' `put_entity_data()` store BlockEntity data into `db`.
#'
#' @param values A list of nbt objects
#' @param value An nbt object.
#' @param data A named-list specifying key-value pairs.
#' @rdname Entity
#' @export
put_entity_values <- function(db, x, z, dimension, values) {
    keys <- .process_key_args(x, z, dimension, tag = 50L,
                              stop_if_filtered = TRUE)
    put_nbt_values(db, keys, values)
}

#' @rdname Entity
#' @export
put_entity_value <- function(db, x, z, dimension, value) {
    key <- .process_key_args(x, z, dimension, tag = 50L)
    vec_assert(key, character(), 1L)
    put_nbt_value(db, key, value)
}

#' @rdname Entity
#' @export
put_entity_data <- function(db, data) {
    .check_chunk_key_tag(names(data), 50L)
    put_nbt_data(db, data)
}
