#' Read and write Data3D data
#'
#' Data3D data (tag 43) stores information about surface heights and biomes in a
#' chunk.
#'
#' `get_data3d_data()` and `get_data3d_value()` load Data3D data from `db`.
#' `get_data3d_value()` only supports loading a single value.'
#'
#' `put_data3d_data()` and `put_data3d_value()` store Data3D data into `db`.
#'
#' `read_data3d_value()` decodes binary Data3D data. `write_data3d_value()`
#' encodes Data3D data into a raw vector.
#'
#' @param db A bedrockdb object.
#' @param x,z,dimension Chunk coordinates to extract data from.
#'    `x` can also be a character vector of db keys.
#' @param height_maps,biome_maps Lists of height and biome data.
#' Values will be recycled if necessary to match the number of keys
#' to be written to. If `biome_maps` is missing, `height_maps` should
#' be in the same format as returned by `get_data3d_data()`.
#' If `x` is missing, the names of `height_maps` will be taken as the keys
#' @param height_map 16x16 array containing height data.
#' Values will be recycled if necessary. If `biome_map` is missing, `height-map`
#' should be a list a `list()` with both "height_map" and "biome_map" elements.
#' @param biome_map 16xNx16 array containing biome data.
#' @param rawdata A raw vector.
#'
#' @return `get_data3d_value()` returns a list with components "height_map"
#' and "biome_map". `get_data3d_data()` returns a list of the of the values
#' returned by `get_data3d_value()`.
#'
#' @name Data3D
NULL

#' @rdname Data3D
#' @export
get_data3d_data <- function(x, z, dimension, db) {
    keys <- .process_chunk_key_args(x, z, dimension, tag = 43L,
        assert_validity = TRUE)
    dat <- get_data(keys, db)
    purrr::map(dat, read_data3d_value)
}

#' @rdname Data3D
#' @export
get_data3d_value <- function(x, z, dimension, db) {
    key <- .process_chunk_key_args(x, z, dimension, tag = 43L,
        assert_validity = TRUE, assert_scalar = TRUE)
    dat <- get_value(key, db)
    read_data3d_value(dat)
}

#' @rdname Data3D
#' @export
put_data3d_data <- function(height_maps, biome_maps, x, z, dimension, db) {
    keys <- .process_chunk_key_args(x, z, dimension, tag = 43L,
        values = height_maps, assert_validity = TRUE)
    if(missing(biome_maps)) {
        values <- vec_recycle(height_maps, length(keys), x_arg="height_maps")
        values <- purrr::map(values, write_data3d_value)
    } else {
        h <- vec_recycle(height_maps, length(keys), x_arg="height_maps")
        b <- vec_recycle(biome_maps, length(keys), x_arg="biome_maps")
        values <- purrr::map2(h, b, write_data3d_value)
    }
    put_values(values, keys, db)
}

#' @rdname Data3D
#' @export
put_data3d_value <- function(height_map, biome_map, x, z, dimension, db) {
    key <- .process_chunk_key_args(x, z, dimension, tag = 43L,
        assert_validity = TRUE, assert_scalar = TRUE)
    value <- write_data3d_value(height_map, biome_map)
    put_value(value, key, db)
}

#' @rdname Data3D
#' @export
read_data3d_value <- function(rawdata) {
    if(is.null(rawdata)) {
        return(NULL)
    }
    vec_assert(rawdata, raw())    
    h <- readBin(rawdata[1:512], integer(), n=256L, size=2L, endian="little", signed = TRUE)
    dim(h) <- c(16L,16L)
    b <- .Call(Cread_chunk_biomes, rawdata[-(1:512)])
    # trim trailing null values
    pos <- purrr::detect_index(b, ~!is_null(.$values), .dir = "backward")
    if(pos == 0) {
        return(NULL)
    }
    b <- b[seq.int(pos)]

    a <- purrr::map(b, function(x) {
        # apply palette
        if(is_null(x$values)) {
            array(NA_integer_, c(16,16,16))
        } else {
            array(x$palette[x$values], dim(x$values))
        }
    })
    b <- array(0L, c(16,length(a)*16,16))
    for(i in seq_along(a)) {
        b[,16*(i-1)+(1:16),] <- a[[i]]
    }
    list(height_map = h, biome_map = b)
}

#' @rdname Data3D
#' @export
write_data3d_value <- function(height_map, biome_map) {
    # support passing a list
    if(missing(biome_map)) {
        if(is.null(height_map)) {
            return(NULL)
        }
        object <- height_map
        height_map <- object$height_map
        biome_map <- object$biome_map
    } else if(is.null(height_map) && is.null(biome_map)) {
        return(NULL)
    }
    if(is.null(height_map) || is.null(biome_map)) {
        abort("Invalid Data3D data.")
    }

    height_map <- vec_cast(c(height_map), integer(), x_arg="height_map")
    height_map <- vec_recycle(height_map, 256, x_arg="height_map")

    biome_map <- vec_cast(c(biome_map), integer(), x_arg="biome_map")
    
    # reshape biome_map
    if(length(biome_map) == 1L) {
        biome_map <- array(biome_map, c(16L,16L*24L,16L))
    } else if(length(biome_map) == 256L) {
        biome_map <- aperm(array(biome_map, c(16L,16L,16L*24L)), c(1L,3L,2L))
    } else if(length(biome_map) %% 4096 != 0) {
        abort("Invalid biome_map dimensions.")
    } else {
        biome_map <- array(biome_map, c(16L, length(biome_map)/256L, 16L))
    }

    values_list <- rep(list(integer(0L)), 25)
    palette_list <- rep(list(integer(0L)), 25)
    for(i in seq.int(length(biome_map) %/% 4096)) {
        j <- (1:16) + (i-1)*16
        id <- c(biome_map[1:16, j, 1:16])
        palette_list[[i]] <- vec_unique(id)
        values_list[[i]] <- match(id, palette_list[[i]])
    }

    h <- writeBin(height_map, raw(), size = 2L, endian="little")
    b <- .Call(Cwrite_chunk_biomes, values_list, palette_list)
    c(h,b)
}
