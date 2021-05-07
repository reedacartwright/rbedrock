#' Read and write 2DMaps data
#'
#' 2DMaps data (tag 45) stores information about surface heights and biomes in a
#' chunk. 2DMaps data is 768 bytes long and consists of a 256 int16s (heights)
#' followed by 256 uint8s (biomes).
#'
#' @name Maps2D
#'
#' @examples
#' heights <- matrix(63,16,16)
#' biomes <- matrix(1,16,16)
#' # Pass heights and biomes as separate parameters
#' write_2dmaps_value(heights, biomes)
#' # Pass them as a list.
#' obj <- list(height_map = heights, biome_map = biomes)
#' write_2dmaps_value(obj)
#'
NULL

#' @description
#' `get_2dmaps_data()` loads 2DMaps data from a `bedrockdb`.
#'  It will silently drop and keys not representing 2DMaps data.
#'
#' @param db A bedrockdb object.
#' @param x,z,dimension Chunk coordinates to extract data from.
#'    `x` can also be a character vector of db keys.
#'
#' @return `get_2dmaps_data()` returns a list of the of the values returned by 
#'         `get_2dmaps_value()`.
#'
#' @rdname Maps2D
#' @export
get_2dmaps_data <- function(db, x, z, dimension) {
    keys <- .process_key_args(x,z,dimension, tag=45L)
    dat <- get_values(db, keys)
    purrr::map(dat, read_2dmaps_value)
}

#' @description
#' `get_2dmaps_value()` loads 2DMaps data from a `bedrockdb`.
#' It only supports loading a single value.
#'
#' @return `get_2dmaps_value()` and `read_2dmaps_value()`
#'         return a list with components "height_map" and "biome_map".
#' @rdname Maps2D
#' @export
get_2dmaps_value <- function(db, x, z, dimension) {
    key <- .process_key_args(x, z, dimension, tag=45L)
    vec_assert(key, character(), 1L)
    dat <- get_value(db, key)
    read_2dmaps_value(dat)
}

#' @description
#' `read_2dmaps_value` decodes binary 2DMaps data.
#'
#' @param rawdata A raw vector.
#'
#' @rdname Maps2D
#' @export
read_2dmaps_value <- function(rawdata) {
    if(is.null(rawdata)) {
        return(NULL)
    }
    vec_assert(rawdata, raw(), 768L)    
    .read_2dmaps_value_impl(rawdata)
}

.read_2dmaps_value_impl <- function(x) {
    h <- readBin(x[1L:512L], integer(), n=256L, size=2L, endian="little", signed = TRUE)
    b <- readBin(x[513L:768L], integer(), n=256L, size=1L, endian="little", signed = FALSE)
    dim(h) <- c(16L,16L)
    dim(b) <- c(16L,16L)
    list(height_map = h, biome_map = b)
}

#' @description
#' `put_2dmaps_data()`, `put_2dmaps_values()`, and
#' `put_2dmaps_value()` store 2DMaps data into a `bedrockdb`.
#'
#' @param data A named-vector of key-value pairs for 2DMaps data.
#'
#' @rdname Maps2D
#' @export
put_2dmaps_data <- function(db, data) {
    stopifnot(all(.get_tag_from_chunk_key(names(data)) == 45L))
    dat <- purrr::map(data, write_2dmaps_value)
    put_data(db, dat)
}

#' @param values A list of lists.
#'
#' @rdname Maps2D
#' @export
put_2dmaps_values <- function(db, x, z, dimension, values) {
    keys <- .process_key_args(x, z, dimension, tag=45L, stop_if_filtered = TRUE)
    values <- vctrs::vec_recycle(values, length(keys), x_arg="values")
    values <- purrr::map(values, write_2dmaps_value)
    put_values(db, keys, values)
}

#' @param value A list.
#'
#' @rdname Maps2D
#' @export
put_2dmaps_value <- function(db, x, z, dimension, value) {
    key <- .process_key_args(x, z, dimension, tag=45L)
    vec_assert(key, character(), 1L)
    value <- write_2dmaps_value(value)
    put_value(db, key, value)
}

#' @description
#' `write_2dmaps_value` encodes 2dMaps data into a raw vector.
#'
#' @param height_map If `biome_map` is missing, a `list` with both "height_map"
#'                   and "biome_map" values. Otherwise, a 16x16 array of height
#'                   data for a chunk.
#' @param biome_map  A 16x16 array of biome data for a chunk
#'
#' @rdname Maps2D
#' @export
write_2dmaps_value <- function(height_map, biome_map) {
    # support passing a list
    if(missing(biome_map)) {
        if(is.null(height_map)) {
            return(NULL)
        }
        object <- height_map
        stopifnot(rlang::is_list(object, n = 2) && rlang::has_name(object, "height_map")
                                                && rlang::has_name(object, "biome_map"))       
        height_map <- object[["height_map"]]
        biome_map <- object[["biome_map"]]
    }
    height_map <- vec_recycle(as.integer(height_map), 256, x_arg="height_map")
    biome_map <- vec_recycle(as.integer(biome_map), 256, x_arg="biome_map")

    .write_2dmaps_value_impl(height_map, biome_map)
}

.write_2dmaps_value_impl <- function(h, b) {
    h <- writeBin(h, raw(), size = 2L, endian="little")
    b <- writeBin(b, raw(), size = 1L, endian="little")
    c(h,b)
}
