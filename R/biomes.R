#' Read and write biomes data.
#'
#' Biomes data is stored as the second map in Data3D data (tag 43).
#' Legacy Biomes data is stored as the second map in the Data2D data (tag 45).
#'
#' @name Biomes
NULL

#' @description
#' `get_biomes_data()` and `get_biomes_value()` load Biomes
#' data from `db`. `get_biomes_data()` will silently drop keys not
#' representing Data3D data. `get_biomes_value()` supports loading
#' only a single value. `get_biomes_values()` is a synonym for
#' `get_biomes_data()`.
#'
#' @param db A bedrockdb object.
#' @param x,z,dimension Chunk coordinates to extract data from.
#'    `x` can also be a character vector of db keys.
#'
#' @param return_names return biome names instead of biome ids.
#'
#' @return `get_biomes_value()` returns an array with 3 dimensions.
#' `get_biomes_data()` returns a list of the of the values returned by
#' `get_biomes_value()`.
#'
#' @rdname Biomes
#' @export
get_biomes_data <- function(db, x, z, dimension, return_names = TRUE) {
    dat <- get_data3d_data(db, x, z, dimension)
    purrr::map(dat, .get_biomes_impl, return_names = return_names)
}

#' @rdname Biomes
#' @export
get_biomes_values <- get_biomes_data

#' @rdname Biomes
#' @export
get_biomes_value <- function(db, x, z, dimension, return_names = TRUE) {
    dat <- get_data3d_value(db, x, z, dimension)
    .get_biomes_impl(dat, return_names = return_names)
}

#' @description
#' `put_biomes_data()` `put_biomes_values()`, and `put_biomes_value()` update
#' the biome information of chunks. They preserve any existing height data.
#'
#' @param data A list of character or integer vectors.
#' @param values a list of arrays containing biome names or ids.
#' @param value an array containing biome names or ids.
#' @param missing_height if there is no existing height data, use this value
#'    for the chunk.
#'
#' @rdname Biomes
#' @export
put_biomes_data <- function(db, data, missing_height = -64L) {
    put_biomes_values(db, names(data), values = data,
                      missing_height = missing_height)
}

#' @rdname Biomes
#' @export
put_biomes_values <- function(db, x, z, dimension, values,
                              missing_height = -64L) {
    keys <- .process_key_args(x, z, dimension, tag = 43L,
                              stop_if_filtered = TRUE)
    values <- vctrs::vec_recycle(values, length(keys), x_arg = "values")

    dat <- get_data3d_data(db, keys)

    dat2 <- purrr::map2(dat, values, function(d, value) {
        h <- d$height_map %||% missing_height

        if (is.character(value)) {
            value <- biome_id(value)
            if (any(is.na(value))) {
                abort("`values` contains unknown biome")
            }
        }
        list(height_map = h, biome_map = value)
    })

    put_data3d_data(db, dat2)
}

#' @rdname Biomes
#' @export
put_biomes_value <- function(db, x, z, dimension, value,
                             missing_height = -64L) {
    key <- .process_key_args(x, z, dimension, tag = 43L)
    vec_assert(key, character(), 1L)

    d <- get_data3d_value(db, key)
    h <- d$height_map %||% missing_height

    if (is.character(value)) {
        value <- biome_id(value)
        if (any(is.na(value))) {
            abort("`value` contains unknown biome")
        }
    }
    put_data3d_value(db, key, height_map = h, biome_map = value)
}

#' @description
#' `get_legacy_biomes_*()` and `put_legacy_biomes_*()` behave similar to
#' the equivalent non-legacy functions. They get or put 2d biome data.
#'
#' @rdname Biomes
#' @export
get_legacy_biomes_data <- function(db, x, z, dimension, return_names = TRUE) {
    dat <- get_data2d_data(db, x, z, dimension)
    purrr::map(dat, .get_biomes_impl, return_names = return_names)
}

#' @rdname Biomes
#' @export
get_legacy_biomes_values <- get_legacy_biomes_data

#' @rdname Biomes
#' @export
get_legacy_biomes_value <- function(db, x, z, dimension, return_names = TRUE) {
    dat <- get_data2d_value(db, x, z, dimension)
    .get_biomes_impl(dat, return_names = return_names)
}

#' @rdname Biomes
#' @export
put_legacy_biomes_data <- function(db, data, missing_height = 0L) {
    put_legacy_biomes_values(db, names(data), values = data,
                             missing_height = missing_height)
}

#' @rdname Biomes
#' @export
put_legacy_biomes_values <- function(db, x, z, dimension, values,
                                     missing_height = 0L) {
    keys <- .process_key_args(x, z, dimension, tag = 45L,
                              stop_if_filtered = TRUE)
    values <- vctrs::vec_recycle(values, length(keys), x_arg = "values")

    dat <- get_data2d_data(db, keys)

    dat2 <- purrr::map2(dat, values, function(d, value) {
        h <- d$height_map %||% missing_height

        if (is.character(value)) {
            value <- biome_id(value)
            if (any(is.na(value))) {
                abort("`values` contains unknown biome")
            }
        }
        list(height_map = h, biome_map = value)
    })

    put_data2d_data(db, dat2)
}

#' @rdname Biomes
#' @export
put_legacy_biomes_value <- function(db, x, z, dimension, value,
                                    missing_height = 0L) {
    key <- .process_key_args(x, z, dimension, tag = 45L)
    vec_assert(key, character(), 1L)

    d <- get_data2d_value(db, key)
    h <- d$height_map %||% missing_height

    if (is.character(value)) {
        value <- biome_id(value)
        if (any(is.na(value))) {
            abort("`value` contains unknown biome")
        }
    }
    put_data2d_value(db, key, height_map = h, biome_map = value)
}

# these lists was generated from a running instance of
# bedrock dedicated server 1.16.100.04
.BIOME_LIST <- c( # nolint: object_name_linter
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
    basalt_deltas = 181L,
    lofty_peaks = 182L,
    snow_capped_peaks = 183L,
    snowy_slopes = 184L,
    mountain_grove = 185L,
    mountain_meadow = 186L,
    lush_caves = 187L,
    dripstone_caves = 188L,
    stony_peaks = 189L
)

# Some biomes don't have a color in BDS, and their color was copied from
# Java/Amidst. Ocean colors in BDS are very similar or identical, and their
# colors were copied as well.
# Amidst Colors: https://git.io/JYD2c
.BIOME_COLORS <- c( # nolint: object_name_linter
    plains = "#8db360",
    desert = "#fa9418",
    extreme_hills = "#606060",
    forest = "#056621",
    taiga = "#0b6659",
    swampland = "#07f9b2",
    river = "#0000ff",
    hell = "#ff0000",
    the_end = "#8080ff",
    legacy_frozen_ocean = "#9090a0",
    frozen_river = "#a0a0ff",
    ice_plains = "#ffffff",
    ice_mountains = "#a0a0a0",
    mushroom_island = "#ff00ff",
    mushroom_island_shore = "#a000ff",
    beach = "#fade55",
    desert_hills = "#d25f12",
    forest_hills = "#22551c",
    taiga_hills = "#163933",
    extreme_hills_edge = "#72789a",
    jungle = "#537b09",
    jungle_hills = "#2c4205",
    jungle_edge = "#628b17",
    stone_beach = "#a2a284",
    cold_beach = "#faf0c0",
    birch_forest = "#307444", # color was missing
    birch_forest_hills = "#1F5F32", # color was missing
    roofed_forest = "#40511a",
    cold_taiga = "#31554a",
    cold_taiga_hills = "#243f36",
    mega_taiga = "#596651",
    mega_taiga_hills = "#454f3e",
    extreme_hills_plus_trees = "#507050",
    savanna = "#bdb25f",
    savanna_plateau = "#a79d64",
    mesa = "#d94515",
    mesa_plateau_stone = "#b09765",
    mesa_plateau = "#ca8c65",
    ocean = "#000070", # color changed
    deep_ocean = "#000030", # color changed
    warm_ocean = "#0000ac", # color changed
    deep_warm_ocean = "#000050", # color changed
    lukewarm_ocean = "#000090", # color changed
    deep_lukewarm_ocean = "#000040", # color changed
    cold_ocean = "#202070", # color changed
    deep_cold_ocean = "#202038", # color changed
    frozen_ocean = "#7070d6", # color changed
    deep_frozen_ocean = "#404090", # color changed
    bamboo_jungle = "#537b09",
    bamboo_jungle_hills = "#2c4205",
    sunflower_plains = "#8db360",
    swampland_mutated = "#b2f907",
    ice_plains_spikes = "#d2ffff",
    cold_taiga_mutated = "#4a5531",
    savanna_mutated = "#E5DA87", # color was missing
    savanna_plateau_mutated = "#CFC58C", # color was missing
    roofed_forest_mutated = "#1a5140",
    desert_mutated = "#1894fa",
    flower_forest = "#6a7425",
    taiga_mutated = "#59660b",
    jungle_mutated = "#097b53",
    jungle_edge_mutated = "#178b62",
    mesa_bryce = "#1545d9",
    mesa_plateau_stone_mutated = "#6597b0",
    mesa_plateau_mutated = "#658cca",
    birch_forest_mutated = "#589C6C", # color was missing
    birch_forest_hills_mutated = "#47875A", # color was missing
    redwood_taiga_mutated = "#596651",
    extreme_hills_mutated = "#606060",
    extreme_hills_plus_trees_mutated = "#507050",
    redwood_taiga_hills_mutated = "#3e4f45",
    soulsand_valley = "#5b4538",
    crimson_forest = "#941818",
    warped_forest = "#167e86",
    basalt_deltas = "#685f70",
    lofty_peaks = "#909090",
    snow_capped_peaks = "#ffccd2",
    snowy_slopes = "#ffffff",
    mountain_grove = "#0c0c0c",
    mountain_meadow = "#60b38d",
    lush_caves = "#097b53",
    dripstone_caves = "#1545d9",
    stony_peaks = "#606060"
)

# invert the list
.BIOME_LIST_INV <- character() # nolint: object_name_linter
.BIOME_LIST_INV[.BIOME_LIST + 1] <- names(.BIOME_LIST) # nolint: object_name_linter

#' List Minecraft Bedrock Edition biomes.
#'
#' @param x A character vector containing biome name.
#'
#' @export
list_biomes <- function() {
    n <- names(.BIOME_LIST)
    tibble::tibble(name = n, id = .BIOME_LIST, color = .BIOME_COLORS[n])
}

#' @rdname list_biomes
#' @export
biome_id <- function(x) {
    .BIOME_LIST[x]
}

.get_biomes_impl <- function(x, return_names) {
    if (is.null(x$biome_map)) {
        return(NULL)
    }
    y <- x$biome_map
    if (isTRUE(return_names)) {
        y[] <- .BIOME_LIST_INV[y + 1]
    }
    y
}
