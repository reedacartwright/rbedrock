#' @export
get_2dmaps <- function(db, x, z, dimension) {
    k <- .process_strkey_args(x,z,dimension, tag=45L)
    dat <- db$mget(k, as_raw = TRUE) %>% purrr::compact()
    dat %>% purrr::map(read_2dmaps_data)
}

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

#' @export
write_2dmaps_data <- function(height_map, biome_map) {
    # support passing a list
    if(missing(biome_map) && is.list(height_map)) {
        biome_map <- height_map$biome_map
        height_map <- height_map$height_map
    }

    con <- rawConnection(raw(0), "wb")
    on.exit(close(con))

    height_map <- as.integer(height_map)
    biome_map <- as.integer(biome_map)
    if(length(height_map) != 256 || length(biome_map) != 256) {
        stop("length of both height_maps and biomes_ids must be 256")
    }
    writeBin(height_map, con, size = 2L, endian="little")
    writeBin(biome_map, con, size = 1L, endian="little")
    
    rawConnectionValue(con)
}
