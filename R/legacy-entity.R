#' Load and store Entity NBT data
#'
#' @description
#' `r lifecycle::badge("superseded")`
#'
#' Entity data (tag 50) holds a list of NBT values for mobs and other entities
#' in the game. After 1.18.30, entity data was migrated to a new actor digest
#' format and no longer saved with chunk data.
#'
#' * `get_entity_value()` and `get_entity_data()` load Entity
#' data from `db`. `get_entity_value()` loads data for a single chunk,
#' and `get_entity_data()` loads data for multiple chunks.
#' * `put_entity_value()` and `put_entity_data()` store Entity
#' data for one or multiple chunks into `db`.
#'
#' @inheritParams ChunkNBTData
#'
#' @return `get_entity_value()` returns a list of NBT objects.
#' `get_entity_data()` returns a named list of lists of NBT objects.
#'
#' @keywords internal
#' @name Entity
NULL

#' @rdname Entity
#' @export
get_entity_data <- function(x, z, dimension, db = default_db()) {
  get_chunk_nbt_data(x, z, dimension, tag = 50L, db = db)
}

#' @rdname Entity
#' @export
get_entity_value <- function(x, z, dimension, db = default_db()) {
  get_chunk_nbt_value(x, z, dimension, tag = 50L, db = db)
}

#' @rdname Entity
#' @export
put_entity_data <- function(values, x, z, dimension, db = default_db()) {
  put_chunk_nbt_data(values, x, z, dimension, tag = 50L, db = db)
}

#' @rdname Entity
#' @export
put_entity_value <- function(value, x, z, dimension, db = default_db()) {
  put_chunk_nbt_value(value, x, z, dimension, tag = 50L, db = db)
}
