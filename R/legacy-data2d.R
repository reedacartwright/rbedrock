#' Read and write Data2D data
#'
#' @description
#' `r lifecycle::badge("superseded")`
#'
#' Data2D data (tag 45) stores information about surface heights and biomes in a
#' chunk. Data2D data is 768 bytes long and consists of a 256 int16s (heights)
#' followed by 256 uint8s (biomes). The game no longer uses it after 1.18.
#'
#' * `get_data2d_value()` and `get_data2d_data()` load Data2D
#' data from `db`. `get_data2d_value()` loads data for a single chunk,
#' and `get_data2d_data()` loads data for multiple chunks.
#' * `put_data2d_value()` and `put_data2d_data()` store Data2D
#' data into `db`.
#' * `write_data2d_value()` encodes Data2D data into a raw vector.
#' `read_data2d_value()` decodes binary Data2D data.
#'
#' @inheritParams ChunkData
#' @param value A Data2D value.
#' @param values A (named) list of Data2D values. If `x` is missing, the names
#' of `values` will be taken as the keys.
#' @param rawvalue A raw vector.
#'
#' @return `get_data2d_value()` returns a Data2D value. `get_data2d_data()`
#' returns a named list of Data2D values. Data2D values are lists containing
#' two elements. The `height_map` element is a 16x16 matrix containing height
#' data. The `biome_map` element is a 16x16 matrix containing biome data.
#'
#' @keywords internal
#' @name Data2D
NULL

#' @rdname Data2D
#' @export
get_data2d_data <- function(x, z, dimension, db = default_db()) {
    dat <- get_chunk_data(x, z, dimension, tag = 45L, db = db)
    lapply(dat, read_data2d_value)
}

#' @rdname Data2D
#' @export
get_data2d_value <- function(x, z, dimension, db = default_db()) {
    val <- get_chunk_value(x, z, dimension, tag = 45L, db = db)
    read_data2d_value(val)
}

#' @rdname Data2D
#' @export
put_data2d_data <- function(values, x, z, dimension, db = default_db()) {
    values <- lapply(values, write_data2d_value)
    put_chunk_data(values, x, z, dimension, tag = 45L, db = db)
}

#' @rdname Data2D
#' @export
put_data2d_value <- function(value, x, z, dimension, db = default_db()) {
    value <- write_data2d_value(value)
    put_chunk_value(value, x, z, dimension, tag = 45L, db = db)
}

#' @rdname Data2D
#' @export
read_data2d_value <- function(rawvalue) {
    if (is.null(rawvalue)) {
        return(NULL)
    }
    vec_assert(rawvalue, raw(), 768L)

    h <- readBin(rawvalue[1L:512L], integer(), n = 256L, size = 2L,
                 endian = "little", signed = TRUE)
    b <- readBin(rawvalue[513L:768L], integer(), n = 256L, size = 1L,
                 endian = "little", signed = FALSE)
    dim(h) <- c(16L, 16L)
    dim(b) <- c(16L, 16L)

    list(height_map = h, biome_map = b)
}

#' @rdname Data2D
#' @export
write_data2d_value <- function(value) {
    if (is.null(value)) {
        return(NULL)
    }

    height_map <- value[["height_map", exact = TRUE]]
    biome_map <- value[["biome_map", exact = TRUE]]

    height_map <- as.integer(height_map)
    biome_map <- as.integer(biome_map)

    height_map <- vec_recycle(height_map, 256, x_arg = "height_map")
    biome_map <- vec_recycle(biome_map, 256, x_arg = "biome_map")

    h <- writeBin(height_map, raw(), size = 2L, endian = "little")
    b <- writeBin(biome_map, raw(), size = 1L, endian = "little")

    c(h, b)
}
