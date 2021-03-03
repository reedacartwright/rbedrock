#' Read and Write 2dmaps data.
#'
#' @description
#' `get_2dmaps` retrieves a 2dMap data from a `bedrockdb`.
#'
#' @param db A bedrockdb object.
#' @param x,z,dimension Chunk coordinates to extract 2dMaps data from.
#'    x can also be a character vector of db keys and any keys not
#'    representing 2dMaps data will be silently dropped.
#' @param rawval A `raw` vector containing binary 2dMaps data.
#' @param object A list containing height and biome biome. If `biome_map` is specified,
#'             this can be a 16x16 array of height data for a chunk.
#' @param biome_map  A 16x16 array of biome data for a chunk
#'
#' @export
get_2dmaps <- function(db, x, z, dimension) {
    keys <- .process_key_args(x,z,dimension, tag=45L)
    dat <- get_values(db, keys) %>% purrr::compact()
    dat %>% purrr::map(read_2dmaps_data)
}

#' @description
#' `read_2dmaps_data` parses a raw vector of 2dMaps data.
#'
#' @rdname get_2dmaps
#' @export
read_2dmaps_data <- function(rawval) {
    con <- rawConnection(rawval)
    on.exit(close(con))

    h <- readBin(con, integer(), n=256L, size=2L, endian="little", signed = TRUE)
    b <- readBin(con, integer(), n=256L, size=1L, endian="little", signed = FALSE)
    dim(h) <- c(16,16)
    dim(b) <- c(16,16)
    list(height_map = h, biome_map = b)
}

#' @description
#' `write_2dmaps_data` converts 2dMaps data into a raw vector.
#'
#' @rdname get_2dmaps
#' @export
write_2dmaps_data <- function(object, biome_map) {
    # support passing a list
    if(missing(biome_map) && is.list(object)) {
        biome_map <- object$biome_map
        height_map <- object$height_map
    } else {
        height_map <- object
    }

    con <- rawConnection(raw(0), "wb")
    on.exit(close(con))

    height_map <- as.integer(height_map)
    biome_map <- as.integer(biome_map)

    stopifnot(length(height_map) == 256L && length(biome_map) == 256L)
    writeBin(height_map, con, size = 2L, endian="little")
    writeBin(biome_map, con, size = 1L, endian="little")
    
    rawConnectionValue(con)
}
