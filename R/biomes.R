#' Read and write Biomes data
#'
#' Biomes data is stored as the second map in the [Data3D] data (tag 43).
#'
#' * `get_biomes_value()` and `get_biomes_data()` load Biomes data from `db`.
#' `get_biomes_value()` loads data for a single chunk, and `get_biomes_data()`
#' loads data for multiple chunks.
#' * `put_biomes_value()` and `put_biomes_data()` store biomes data into `db`.
#'
#' @seealso LegacyBiomes
#'
#' @inheritParams ChunkData
#' @param value An array of biome ids.
#' @param values A (named) list of Biomes data values. If `x` is missing, the
#'     names of `values` will be taken as the keys.
#' @param return_names return biome names instead of biome ids.
#' @param missing_height if there is no existing height data, use this value
#'    for the chunk.
#'
#' @return `get_biomes_value()` returns a Biomes data value.
#' `get_biomes_data()` returns a named list of Biomes data values.
#' Biomes data values are 16x384x16 arrays containing biome data.
#'
#' @name Biomes
NULL

#' @rdname Biomes
#' @export
get_biomes_value <- function(
  x,
  z,
  dimension,
  db = default_db(),
  return_names = TRUE
) {
  val <- get_data3d_value(x, z, dimension, db = db)
  get_biomes_impl(val, return_names = return_names)
}

#' @rdname Biomes
#' @export
get_biomes_data <- function(
  x,
  z,
  dimension,
  db = default_db(),
  return_names = TRUE
) {
  dat <- get_data3d_data(x, z, dimension, db = db)
  lapply(dat, get_biomes_impl, return_names = return_names)
}

#' @rdname Biomes
#' @export
put_biomes_value <- function(
  value,
  x,
  z,
  dimension,
  db = default_db(),
  missing_height = 0L
) {
  if (is.character(value)) {
    value <- biome_id(value)
    value[is.na(value)] <- 0
  }
  new_value <- get_data3d_value(x, z, dimension, db = db)
  new_value$biome_map <- value
  put_data3d_value(new_value, x, z, dimension, db = db)
}

#' @rdname Biomes
#' @export
put_biomes_data <- function(
  values,
  x,
  z,
  dimension,
  db = default_db(),
  missing_height = 0L
) {
  keys <- process_chunk_key_args(x, z, dimension, tag = 45L, values = values)
  new_data <- get_data3d_data(keys, db = db)
  for (i in seq_along(new_data)) {
    if (is.null(new_data[[i]])) {
      new_data[[i]] <- list(height_map = missing_height)
    }
    value <- values[[i]]
    if (is.character(value)) {
      value <- biome_id(value)
      value[is.na(value)] <- 0
    }
    new_data[[i]]$biome_map <- value
  }
  put_data3d_data(new_data, x, z, dimension, db = db)
}

get_biomes_impl <- function(x, return_names) {
  biome_map <- x[["biome_map", exact = TRUE]]
  if (!is.null(biome_map) && isTRUE(return_names)) {
    biome_map[] <- biome_name(biome_map)
  }
  biome_map
}

#' Bedrock biome data
#'
#' Information about biomes used in Bedrock edition. Generated from the
#' PyMCTranslate project. Colors are generated from the cubiomes project.
#'
#' @format
#' A data.frame with `r nrow(biome_df)` rows and `r ncol(biome_df)` columns.
#'
#' \describe{
#'   \item{bedrock_id}{The numeric id of the biome.}
#'   \item{bedrock_name}{The name of the biome.}
#'   \item{java_name}{The name of the equivalent biome in Java edition.}
#'   \item{universal_name}{The universal name used for the biome in Amulet.}
#'   \item{color}{The color used when mapping biomes.}
#' }
#'
#' @source
#' * <https://github.com/gentlegiantJGC/PyMCTranslate/>
#' * <https://github.com/Cubitect/cubiomes/>
"biome_df"

globalVariables("biome_df")

#' @rdname Biomes
#' @export
biome_id <- function(value) {
  biome_df$bedrock_id[match(value, biome_df$bedrock_name)]
}

#' @rdname Biomes
#' @export
biome_name <- function(value) {
  biome_df$bedrock_name[match(value, biome_df$bedrock_id)]
}
