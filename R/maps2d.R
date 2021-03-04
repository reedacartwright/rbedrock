#' Read and Write 2dmaps data.
#'
#' @description
#' `get_2dmaps` retrieves a 2dMap data from a `bedrockdb`.
#'
#' @param db A bedrockdb object.
#' @param x,z,dimension Chunk coordinates to extract 2dMaps data from.
#'    `x` can also be a character vector of db keys and any keys not
#'    representing 2dMaps data will be silently dropped.
#' @param rawdata A raw vector.
#' @param object For `read_2dmaps_data` a list of rawdata.
#'               For `write_2dmaps_data` a list of lists containing "height_map" and "biome_map" values.
#'
#' @param height_map If `biome_map` is missing, a `list` with both "height_map"
#'                   and "biome_map" values. Otherwise, a 16x16 array of height data for a chunk.
#' @param biome_map  A 16x16 array of biome data for a chunk
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
#' @export
get_2dmaps <- function(db, x, z, dimension) {
    keys <- .process_key_args(x,z,dimension, tag=45L)
    dat <- get_values(db, keys)
    read_2dmaps_data(dat)
}

#' @description
#' `read_2dmaps_data` parses a list of raw vector of 2dMaps data.
#'
#' @rdname get_2dmaps
#' @export
read_2dmaps_data <- function(object) {
     purrr::map(object, read_2dmaps_value);
}

#' @description
#' `read_2dmaps_value` parses a raw vector of 2dMaps data.
#'
#' @rdname get_2dmaps
#' @export
read_2dmaps_value <- function(rawdata) {
    if(is.null(rawdata)) return(NULL)
    
    stopifnot(rlang::is_raw(rawdata, n = 768L))
    
    .read_2dmaps_data_impl(rawdata)
}

.read_2dmaps_data_impl <- function(x) {
    h <- readBin(x[1L:512L], integer(), n=256L, size=2L, endian="little", signed = TRUE)
    b <- readBin(x[513L:768L], integer(), n=256L, size=1L, endian="little", signed = FALSE)
    dim(h) <- c(16L,16L)
    dim(b) <- c(16L,16L)
    list(height_map = h, biome_map = b)
}

#' @description
#' `write_2dmaps_data` converts list of 2dMaps data into a list of raw vector.
#'
#' @rdname get_2dmaps
#' @export
write_2dmaps_data <- function(object) {
    purrr::map(object, write_2dmaps_value);
}

#' @description
#' `write_2dmaps_value` converts 2dMaps data into a raw vector.
#'
#' @rdname get_2dmaps
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
    height_map <- as.integer(height_map)
    biome_map <- as.integer(biome_map)

    stopifnot(length(height_map) == 256L && length(biome_map) == 256L)

    .write_2dmaps_value_impl(height_map, biome_map)
}

.write_2dmaps_value_impl <- function(h, b) {
    h <- writeBin(h, raw(), size = 2L, endian="little")
    b <- writeBin(b, raw(), size = 1L, endian="little")
    c(h,b)
}
