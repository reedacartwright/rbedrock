#' Read and write legacy biomes data
#'
#' @description
#' `r lifecycle::badge("superseded")`
#'
#' Legacy Biomes data is stored as the second map in the [Data2D] data (tag 45).
#'
#' * `get_legacy_biomes_value()` and `get_legacy_biomes_data()` load legacy
#' biomes data from `db`. `get_legacy_biomes_value()` loads data for a single
#' chunk, and `get_legacy_biomes_data()` loads data for multiple chunks.
#' * `put_legacy_biomes_value()` and `put_legacy_biomes_data()` store legacy
#' biomes data into `db`.
#'
#' @inheritParams ChunkData
#' @param value A 16x16 matrix of biome ids.
#' @param values A (named) list of Data2D values. If `x` is missing, the names
#'    of `values` will be taken as the keys.
#' @param return_names return biome names instead of biome ids.
#' @param missing_height if there is no existing height data, use this value
#'    for the chunk.
#'
#' @return `get_legacy_biomes_value()` returns a legacy biomes data value.
#' `get_legacy_biomes_data()` returns a named list of legacy biomes data values.
#' Legacy biomes data values are 16x16 matrices containing biome data.
#'
#'
#' @keywords internal
#' @name LegacyBiomes
NULL

#' @rdname LegacyBiomes
#' @export
get_legacy_biomes_value <- function(x, z, dimension, db = default_db(),
                                    return_names = TRUE) {
    val <- get_data2d_value(x, z, dimension, db = db)
    get_legacy_biomes_impl(val, return_names = return_names)
}

#' @rdname LegacyBiomes
#' @export
get_legacy_biomes_data <- function(x, z, dimension, db = default_db(),
                                   return_names = TRUE) {
    dat <- get_data2d_data(x, z, dimension, db = db)
    lapply(dat, get_legacy_biomes_impl, return_names = return_names)
}

#' @rdname LegacyBiomes
#' @export
put_legacy_biomes_value <- function(value, x, z, dimension, db = default_db(),
                                    missing_height = 0L) {
    if (is.character(value)) {
        value <- biome_id(value)
        value[is.na(value)] <- 0
    }
    new_value <- get_data2d_value(x, z, dimension, db = db)
    new_value$biome_map <- value
    put_data2d_value(new_value, x, z, dimension, db = db)
}

#' @rdname LegacyBiomes
#' @export
put_legacy_biomes_data <- function(values, x, z, dimension, db = default_db(),
                                   missing_height = 0L) {
    keys <- process_chunk_key_args(x, z, dimension, tag = 45L, values = values)
    new_data <- get_data2d_data(keys, db = db)
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
    put_data2d_data(new_data, x, z, dimension, db = db)
}

get_legacy_biomes_impl <- function(x, return_names) {
    biome_map <- x[["biome_map", exact = TRUE]]
    if (!is.null(biome_map) && isTRUE(return_names)) {
        biome_map[] <- .BIOME_LIST_INV[biome_map + 1]
    }
    biome_map
}
