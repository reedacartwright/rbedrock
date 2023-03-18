#' Read and write Data2D data
#'
#' Data2D data (tag 45) stores information about surface heights and biomes in a
#' chunk. Data2D data is 768 bytes long and consists of a 256 int16s (heights)
#' followed by 256 uint8s (biomes).
#'
#' @name Data2D
#'
#' @examples
#' heights <- matrix(63,16,16)
#' biomes <- matrix(1,16,16)
#' # Pass heights and biomes as separate parameters
#' dat <- write_data2d_value(heights, biomes)
#' # Pass them as a list.
#' obj <- list(height_map = heights, biome_map = biomes)
#' dat <- write_data2d_value(obj)
#' # Pass them as scalars
#' dat <- write_data2d_value(63, 1)
NULL

#' @description
#' `get_data2d_data()` loads Data2D data from a `bedrockdb`.
#'  It will silently drop and keys not representing Data2D data.
#'
#' @param db A bedrockdb object.
#' @param x,z,dimension Chunk coordinates to extract data from.
#'    `x` can also be a character vector of db keys.
#'
#' @return `get_data2d_data()` returns a list of the of the values returned by
#'         `get_data2d_value()`.
#'
#' @rdname Data2D
#' @export
get_data2d_data <- function(db, x, z, dimension) {
    keys <- .process_key_args(x, z, dimension, tag = 45L)
    dat <- get_values(db, keys)
    purrr::map(dat, read_data2d_value)
}

#' @rdname Data2D
#' @export
get_data2d_values <- get_data2d_data


#' @description
#' `get_data2d_value()` loads Data2D data from a `bedrockdb`.
#' It only supports loading a single value.
#'
#' @return `get_data2d_value()` returns a list with components "height_map"
#' and "biome_map".
#' @rdname Data2D
#' @export
get_data2d_value <- function(db, x, z, dimension) {
    key <- .process_key_args(x, z, dimension, tag = 45L)
    vec_assert(key, character(), 1L)
    dat <- get_value(db, key)
    read_data2d_value(dat)
}

#' @description
#' `read_data2d_value` decodes binary Data2D data.
#'
#' @param rawdata A raw vector.
#'
#' @rdname Data2D
#' @export
read_data2d_value <- function(rawdata) {
    if (is.null(rawdata)) {
        return(NULL)
    }
    vec_assert(rawdata, raw(), 768L)

    h <- readBin(rawdata[1L:512L], integer(), n = 256L, size = 2L,
                 endian = "little", signed = TRUE)
    b <- readBin(rawdata[513L:768L], integer(), n = 256L, size = 1L,
                 endian = "little", signed = FALSE)
    dim(h) <- c(16L, 16L)
    dim(b) <- c(16L, 16L)

    list(height_map = h, biome_map = b)
}

#' @description
#' `put_data2d_data()`, `put_data2d_values()`, and
#' `put_data2d_value()` store Data2D data into a `bedrockdb`.
#'
#' @param data A named-vector of key-value pairs for Data2D data.
#'
#' @rdname Data2D
#' @export
put_data2d_data <- function(db, data) {
    stopifnot(all(.get_tag_from_chunk_key(names(data)) == 45L))
    dat <- purrr::map(data, write_data2d_value)
    put_data(db, dat)
}

#' @param height_maps,biome_maps Lists of height and biome data.
#' Values will be recycled if necessary to match the number of keys
#' to be written to. If `biome_maps` is missing, `height_maps` should
#' be in the same format as returned by `get_data2d_data()`.
#'
#' @rdname Data2D
#' @export
put_data2d_values <- function(db, x, z, dimension, height_maps, biome_maps) {
    keys <- .process_key_args(x, z, dimension, tag = 45L,
                              stop_if_filtered = TRUE)
    if (missing(biome_maps)) {
        values <- vec_recycle(height_maps, length(keys), x_arg = "height_maps")
        values <- purrr::map(values, write_data2d_value)
    } else {
        h <- vec_recycle(height_maps, length(keys), x_arg = "height_maps")
        b <- vec_recycle(biome_maps, length(keys), x_arg = "biome_maps")
        values <- purrr::map2(h, b, write_data2d_value)
    }
    put_values(db, keys, values)
}

#' @param height_map,biome_map 16x16 arrays containing height and biome data.
#' Values will be recycled if necessary. If `biome_map` is missing, `height-map`
#' should be a list a `list()` with both "height_map" and "biome_map" elements.
#'
#' @rdname Data2D
#' @export
put_data2d_value <- function(db, x, z, dimension, height_map, biome_map) {
    key <- .process_key_args(x, z, dimension, tag = 45L)
    vec_assert(key, character(), 1L)
    value <- write_data2d_value(height_map, biome_map)
    put_value(db, key, value)
}

#' @description
#' `write_data2d_value` encodes Data2D data into a raw vector.
#'
#' @rdname Data2D
#' @export
write_data2d_value <- function(height_map, biome_map) {
    # support passing a list
    if (missing(biome_map)) {
        if (is.null(height_map)) {
            return(NULL)
        }
        object <- height_map
        height_map <- object$height_map
        biome_map <- object$biome_map
    } else if (is.null(height_map) && is.null(biome_map)) {
        return(NULL)
    }
    if (is.null(height_map) || is.null(biome_map)) {
        abort("Invalid Data2D data.")
    }

    height_map <- vec_cast(c(height_map), integer(), x_arg = "height_map")
    biome_map <- vec_cast(c(biome_map), integer(), x_arg = "biome_map")
    height_map <- vec_recycle(height_map, 256, x_arg = "height_map")
    biome_map <- vec_recycle(biome_map, 256, x_arg = "biome_map")

    h <- writeBin(height_map, raw(), size = 2L, endian = "little")
    b <- writeBin(biome_map, raw(), size = 1L, endian = "little")

    c(h, b)
}
