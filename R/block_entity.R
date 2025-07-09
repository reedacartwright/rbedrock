#' Load and store BlockEntity NBT data
#'
#' BlockEntity data (tag 49) holds a list of NBT values for
#' entity data associated with specific blocks.
#'
#' * `get_block_entity_value()` and `get_block_entity_data()` load BlockEntity
#' data from `db`. `get_block_entity_value()` loads data for a single chunk,
#' and `get_block_entity_data()` loads data for multiple chunks.
#' * `put_block_entity_value()` and `put_block_entity_data()` store BlockEntity
#' data for one or multiple chunks into `db`.
#'
#' @inheritParams ChunkNBTData
#'
#' @return `get_block_entity_value()` returns a list of NBT objects.
#' `get_block_entity_data()` returns a named list of lists of NBT objects.
#'
#' @name BlockEntity
NULL

#' @rdname BlockEntity
#' @export
get_block_entity_data <- function(x, z, dimension, db = default_db()) {
  get_chunk_nbt_data(x, z, dimension, tag = 49L, db = db)
}

#' @rdname BlockEntity
#' @export
get_block_entity_value <- function(x, z, dimension, db = default_db()) {
  get_chunk_nbt_value(x, z, dimension, tag = 49L, db = db)
}

#' @rdname BlockEntity
#' @export
put_block_entity_data <- function(values, x, z, dimension, db = default_db()) {
  put_chunk_nbt_data(values, x, z, dimension, tag = 49L, db = db)
}

#' @rdname BlockEntity
#' @export
put_block_entity_value <- function(value, x, z, dimension, db = default_db()) {
  put_chunk_nbt_value(value, x, z, dimension, tag = 49L, db = db)
}
