#' Get biome information from a world.
#'
#' @param db A bedrockdb object.
#' @param x,z,dimension Chunk coordinates to extract HSA data from.
#'    x can also be a character vector of db keys and any keys not
#'    representing biome data will be silently dropped.
#' @param return_names if set to true, this will return character vectors containing
#'    the names of each biome instead of biome ids.
#' @return An list of arrays containing biome information of each key.
#'
#' @export
get_biomes <- function(db, x, z, dimension, return_names=TRUE) {
    k <- .process_strkey_args(x,z,dimension, tag=45L)
    
    dat <- db$mget(k, as_raw = TRUE) %>% purrr::compact()

    biomes <- dat %>% purrr::map(function(x) {
        y <- .read_2dmaps(x)$biome_map
        if(return_names) {
            y <- .BIOME_LIST_INV[y+1]
        }
        dim(y) <- c(16,16)
        y
    })

    biomes
}

#' Put biome information into the world
#'
#' put_biomes updates the biome information of chunks.
#' It will preserve any existing height data or use
#' the missing_height value if no such data exists.
#'
#' @param db A bedrockdb object.
#' @param values A list of character or integer vectors. Each element of
#'    the list must contain 256 values or an error will be raised.
#' @param x,z,dimension Chunk coordinates to write biome data to.
#'    `x` can also be a character vector of db keys and any keys not
#'    representing biome data (tag 47) will be silently dropped.
#'    `x` defaults to `names(values)`    
#'
#' @param missing_height if there is no existing height data, use this value for the chunk.
#' @export
put_biomes <- function(db, values, x = names(values), z, dimension, missing_height=0L) {
    k <- .process_strkey_args(x, z, dimension, tag=45L, stop_if_filtered = TRUE)

    if(length(k) != length(values)) {
        stop("put_biomes: keys and values have different lengths")
    }

    dat <- db$mget(k, as_raw = TRUE)
    h <- dat %>% purrr::map(function(x) {
        if(is.null(x)) {
            rep(missing_height, 256L)
        } else {
            .read_2dmaps(x)$height_map
        }
    })
    dat2 <- purrr::map2(h, values, function(x,y) {
        if(is.character(y)) {
            y <- .BIOME_LIST[y]
            if(any(is.na(y))) {
                stop("biome list contains unknown biome")
            }
        }
        .write_2dmaps(x,y)
    })
    
    db$mput(dat2)
}

.read_2dmaps <- function(con) {
    if (is.character(con)) {
        con <- file(con, "rb")
        on.exit(close(con))
    } else if (is.raw(con)) {
        con <- rawConnection(con, "rb")
        on.exit(close(con))
    }
    h <- readBin(con, integer(), n=256L, size=2L, endian="little", signed = TRUE)
    b <- readBin(con, integer(), n=256L, size=1L, endian="little", signed = FALSE)

    list(height_map = h, biome_map = b)
}

.write_2dmaps <- function(height_map, biome_map, con = raw(0)) {
    # support passing a list
    if(missing(biome_map) && is.list(height_map)) {
        biome_map <- height_map$biome_map
        height_map <- height_map$height_map
    }

    con_is_raw_connection <- FALSE
    if(is.character(con)) {
        con <- file(con, "wb")
        on.exit(close(con))
    } else if(is.raw(con)) {
        con_is_raw_connection <- TRUE
        con <- rawConnection(raw(0), "wb")
        on.exit(close(con))
    }

    height_map <- as.integer(height_map)
    biome_map <- as.integer(biome_map)
    if(length(height_map) != 256 || length(biome_map) != 256) {
        stop("length of both height_maps and biomes_ids must be 256")
    }
    writeBin(height_map, con, size = 2L, endian="little")
    writeBin(biome_map, con, size = 1L, endian="little")
    
    if(!con_is_raw_connection) {
        return()
    }
    rawConnectionValue(con)
}

# this lists was generated from a running instance of
# bedrock dedicated server 1.16.0
.BIOME_LIST <- c(
    ocean = 0L,
    plains = 1L,
    desert = 2L,
    extreme_hills = 3L,
    forest = 4L,
    taiga = 5L,
    swampland = 6L,
    river = 7L,
    hell = 8L,
    the_end = 9L,
    frozen_river = 11L,
    ice_plains = 12L,
    ice_mountains = 13L,
    mushroom_island = 14L,
    mushroom_island_shore = 15L,
    beach = 16L,
    desert_hills = 17L,
    forest_hills = 18L,
    taiga_hills = 19L,
    extreme_hills_edge = 20L,
    jungle = 21L,
    jungle_hills = 22L,
    jungle_edge = 23L,
    deep_ocean = 24L,
    stone_beach = 25L,
    cold_beach = 26L,
    birch_forest = 27L,
    birch_forest_hills = 28L,
    roofed_forest = 29L,
    cold_taiga = 30L,
    cold_taiga_hills = 31L,
    mega_taiga = 32L,
    mega_taiga_hills = 33L,
    extreme_hills_plus_trees = 34L,
    savanna = 35L,
    savanna_plateau = 36L,
    mesa = 37L,
    mesa_plateau_stone = 38L,
    mesa_plateau = 39L,
    warm_ocean = 40L,
    deep_warm_ocean = 41L,
    lukewarm_ocean = 42L,
    deep_lukewarm_ocean = 43L,
    cold_ocean = 44L,
    deep_cold_ocean = 45L,
    frozen_ocean = 46L,
    deep_frozen_ocean = 47L,
    bamboo_jungle = 48L,
    bamboo_jungle_hills = 49L,
    sunflower_plains = 129L,
    swampland_mutated = 134L,
    ice_plains_spikes = 140L,
    roofed_forest_mutated = 157L,
    cold_taiga_mutated = 158L,
    savanna_mutated = 163L,
    savanna_plateau_mutated = 164L,
    soulsand_valley = 178L,
    crimson_forest = 179L,
    warped_forest = 180L,
    basalt_deltas = 181L
)

# invert the list
.BIOME_LIST_INV <- character()
.BIOME_LIST_INV[.BIOME_LIST+1] <- names(.BIOME_LIST)

#' List Minecraft Bedrock Edition biomes.
#'
#' @return A table containing biome names and numeric ids.
#'
#' @export
list_biomes <- function() {
    tibble::tibble(name=names(.BIOME_LIST), id=.BIOME_LIST)
}
