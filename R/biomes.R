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
        y <- read_2dmaps_data(x)$biome_map
        if(return_names) {
            y <- .BIOME_LIST_INV[y+1]
        }
        dim(y) <- c(16,16)
        # y[x+1,z+1] is biome of x,z
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
            read_2dmaps_data(x)$height_map
        }
    })
    dat2 <- purrr::map2(h, values, function(x,y) {
        if(is.character(y)) {
            y <- .BIOME_LIST[y]
            if(any(is.na(y))) {
                stop("biome list contains unknown biome")
            }
        }
        write_2dmaps_data(x,y)
    })
    
    db$mput(dat2)
}


# these lists was generated from a running instance of
# bedrock dedicated server 1.16.100.04
.BIOME_LIST <- c(
    plains = 1L,
    desert = 2L,
    extreme_hills = 3L,
    forest = 4L,
    taiga = 5L,
    swampland = 6L,
    river = 7L,
    hell = 8L,
    the_end = 9L,
    legacy_frozen_ocean = 10L,
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
    ocean = 0L,
    deep_ocean = 24L,
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
    cold_taiga_mutated = 158L,
    savanna_mutated = 163L,
    savanna_plateau_mutated = 164L,
    roofed_forest_mutated = 157L,
    desert_mutated = 130L,
    flower_forest = 132L,
    taiga_mutated = 133L,
    jungle_mutated = 149L,
    jungle_edge_mutated = 151L,
    mesa_bryce = 165L,
    mesa_plateau_stone_mutated = 166L,
    mesa_plateau_mutated = 167L,
    birch_forest_mutated = 155L,
    birch_forest_hills_mutated = 156L,
    redwood_taiga_mutated = 160L,
    extreme_hills_mutated = 131L,
    extreme_hills_plus_trees_mutated = 162L,
    redwood_taiga_hills_mutated = 161L,
    soulsand_valley = 178L,
    crimson_forest = 179L,
    warped_forest = 180L,
    basalt_deltas = 181L
)

# Some biomes don't have a color in BDS, and their color was copied from Java/Amidst.
# Ocean colors in BDS are very similar or identical, and their colors were copied as
# well.
.BIOME_COLORS = c(
    plains = '#8db360',
    desert = '#fa9418',
    extreme_hills = '#606060',
    forest = '#056621',
    taiga = '#0b6659',
    swampland = '#07f9b2',
    river = '#0000ff',
    hell = '#ff0000',
    the_end = '#8080ff',
    legacy_frozen_ocean = '#9090a0',
    frozen_river = '#a0a0ff',
    ice_plains = '#ffffff',
    ice_mountains = '#a0a0a0',
    mushroom_island = '#ff00ff',
    mushroom_island_shore = '#a000ff',
    beach = '#fade55',
    desert_hills = '#d25f12',
    forest_hills = '#22551c',
    taiga_hills = '#163933',
    extreme_hills_edge = '#72789a',
    jungle = '#537b09',
    jungle_hills = '#2c4205',
    jungle_edge = '#628b17',
    stone_beach = '#a2a284',
    cold_beach = '#faf0c0',
    birch_forest = '#307444', # color was missing
    birch_forest_hills = '#1f5f32', # color was missing
    roofed_forest = '#40511a',
    cold_taiga = '#31554a',
    cold_taiga_hills = '#243f36',
    mega_taiga = '#596651',
    mega_taiga_hills = '#454f3e',
    extreme_hills_plus_trees = '#507050',
    savanna = '#bdb25f',
    savanna_plateau = '#a79d64',
    mesa = '#d94515',
    mesa_plateau_stone = '#b09765',
    mesa_plateau = '#ca8c65',
    ocean = '#000070', # color changed
    deep_ocean = '#000030', # color changed
    warm_ocean = '#0000ac', # color changed
    deep_warm_ocean = '#000050', # color changed
    lukewarm_ocean = '#000090', # color changed
    deep_lukewarm_ocean = '#000040', # color changed
    cold_ocean = '#202070', # color changed
    deep_cold_ocean = '#202038', # color changed
    frozen_ocean = '#7070d6', # color changed
    deep_frozen_ocean = '#404090', # color changed
    bamboo_jungle = '#537b09',
    bamboo_jungle_hills = '#2c4205',
    sunflower_plains = '#8db360',
    swampland_mutated = '#b2f907',
    ice_plains_spikes = '#d2ffff',
    cold_taiga_mutated = '#4a5531',
    savanna_mutated = '#e5da87', # color was missing
    savanna_plateau_mutated = '#cfc58c', # color was missing
    roofed_forest_mutated = '#1a5140',
    desert_mutated = '#1894fa',
    flower_forest = '#6a7425',
    taiga_mutated = '#59660b',
    jungle_mutated = '#097b53',
    jungle_edge_mutated = '#178b62',
    mesa_bryce = '#1545d9',
    mesa_plateau_stone_mutated = '#6597b0',
    mesa_plateau_mutated = '#658cca',
    birch_forest_mutated = '#589c6c', # color was missing
    birch_forest_hills_mutated = '#47875a', # color was missing
    redwood_taiga_mutated = '#596651',
    extreme_hills_mutated = '#606060',
    extreme_hills_plus_trees_mutated = '#507050',
    redwood_taiga_hills_mutated = '#3e4f45',
    soulsand_valley = '#5b4538',
    crimson_forest = '#941818',
    warped_forest = '#167e86',
    basalt_deltas = '#685f70'
)

# invert the list
.BIOME_LIST_INV <- character()
.BIOME_LIST_INV[.BIOME_LIST+1] <- names(.BIOME_LIST)

#' List Minecraft Bedrock Edition biomes.
#'
#' @param x A character vector containing biome name.
#'
#' @return \code{list_biomes} returns a table containing biome names and numeric ids.
#'         \code{biome_id} returns the numeric ids corresponding to input names.
#'
#' @export
list_biomes <- function() {
    tibble::tibble(name=names(.BIOME_LIST), id=.BIOME_LIST, color=.BIOME_COLORS[name])
}

#' @rdname list_biomes
#' @export
biome_id <- function(x) {
    .BIOME_LIST[x]
}
