#' Read and write Data3D data
#'
#' Data3D data (tag 43) stores information about surface heights and biomes in a
#' chunk.
#'
#' @name Data3D
#'
NULL

#' @description
#' `get_data3d_data()` loads Data3D data from `db`.
#'  It will silently drop keys not representing Data3D data.
#'
#' @param db A bedrockdb object.
#' @param x,z,dimension Chunk coordinates to extract data from.
#'    `x` can also be a character vector of db keys.
#'
#' @return `get_data3d_data()` returns a list of the of the values returned by
#'         `get_data3d_value()`.
#'
#' @rdname Data3D
#' @export
get_data3d_data <- function(db, x, z, dimension) {
    keys <- .process_key_args(x, z, dimension, tag = 43L)
    dat <- get_values(db, keys)
    purrr::map(dat, read_data3d_value)
}

#' @rdname Data3D
#' @export
get_data3d_values <- get_data3d_data

#' @description
#' `get_data3d_value()` loads Data3D data from `db`.
#' It only supports loading a single value.
#'
#' @return `get_data3d_value()` returns a list with components "height_map"
#' and "biome_map".
#' @rdname Data3D
#' @export
get_data3d_value <- function(db, x, z, dimension) {
    key <- .process_key_args(x, z, dimension, tag = 43L)
    vec_assert(key, character(), 1L)
    dat <- get_value(db, key)
    read_data3d_value(dat)
}

#' @description
#' `put_data3d_data()`, `put_data3d_values()`, and
#' `put_data3d_value()` store Data3D data into `db`.
#'
#' @param data A named-vector of key-value pairs for Data3D data.
#'
#' @rdname Data3D
#' @export
put_data3d_data <- function(db, data) {
    put_data3d_values(db, x = names(data), height_maps = data)
}

#' @param height_maps,biome_maps Lists of height and biome data.
#' Values will be recycled if necessary to match the number of keys
#' to be written to. If `biome_maps` is missing, `height_maps` should
#' be in the same format as returned by `get_data3d_data()`.
#'
#' @rdname Data3D
#' @export
put_data3d_values <- function(db, x, z, dimension, height_maps, biome_maps) {
    keys <- .process_key_args(x, z, dimension, tag = 43L,
                              stop_if_filtered = TRUE)
    if (missing(biome_maps)) {
        values <- vec_recycle(height_maps, length(keys), x_arg = "height_maps")
        values <- purrr::map(values, write_data3d_value)
    } else {
        h <- vec_recycle(height_maps, length(keys), x_arg = "height_maps")
        b <- vec_recycle(biome_maps, length(keys), x_arg = "biome_maps")
        values <- purrr::map2(h, b, write_data3d_value)
    }
    put_values(db, keys, values)
}

#' @param height_map 16x16 array containing height data.
#' Values will be recycled if necessary. If `biome_map` is missing, `height-map`
#' should be a list a `list()` with both "height_map" and "biome_map" elements.
#' @param biome_map 16xNx16 array containing biome data.
#'
#' @rdname Data3D
#' @export
put_data3d_value <- function(db, x, z, dimension, height_map, biome_map) {
    key <- .process_key_args(x, z, dimension, tag = 43L)
    vec_assert(key, character(), 1L)
    value <- write_data3d_value(height_map, biome_map)
    put_value(db, key, value)
}

#' @description
#' `read_data3d_value()` decodes binary Data3D data.
#'
#' @param rawdata A raw vector.
#'
#' @rdname Data3D
#' @export
read_data3d_value <- function(rawdata) {
    if (is.null(rawdata)) {
        return(NULL)
    }
    vec_assert(rawdata, raw())
    h <- readBin(rawdata[1:512], integer(), n = 256L, size = 2L,
                 endian = "little", signed = TRUE)
    dim(h) <- c(16L, 16L)
    b <- .Call(Cread_chunk_biomes, rawdata[-(1:512)])
    # trim trailing null values
    pos <- purrr::detect_index(b, ~!is_null(.$values), .dir = "backward")
    if (pos == 0) {
        return(NULL)
    }
    b <- b[seq.int(pos)]

    a <- purrr::map(b, function(x) {
        # apply palette
        if (is_null(x$values)) {
            array(NA_integer_, c(16, 16, 16))
        } else {
            array(x$palette[x$values], dim(x$values))
        }
    })
    b <- array(0L, c(16, length(a) * 16, 16))
    for (i in seq_along(a)) {
        b[, 16 * (i - 1) + (1:16), ] <- a[[i]]
    }
    list(height_map = h, biome_map = b)
}

.reshape_biome_map <- function(value) {
    if (length(value) == 1L) {
        return(array(value, c(16L, 16L * 24L, 16L)))
    } else if (length(value) == 256L) {
        return(aperm(array(value, c(16L, 16L, 16L * 24L)),
                     c(1L, 3L, 2L)))
    } else if (length(value) %% 4096 != 0) {
        abort("Invalid biome_map dimensions.")
    }
    array(value, c(16L, length(value) / 256L, 16L))
}

#' @description
#' `write_data3d_value` encodes Data3D data into a raw vector.
#'
#' @rdname Data3D
#' @export
write_data3d_value <- function(height_map, biome_map) {
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
        abort("Invalid Data3D data.")
    }

    height_map <- vec_cast(c(height_map), integer(), x_arg = "height_map")
    height_map <- vec_recycle(height_map, 256, x_arg = "height_map")

    biome_map <- vec_cast(c(biome_map), integer(), x_arg = "biome_map")

    # reshape biome_map
    biome_map <- .reshape_biome_map(biome_map)

    values_list <- rep(list(integer(0L)), 25)
    palette_list <- rep(list(integer(0L)), 25)
    for (i in seq.int(length(biome_map) %/% 4096)) {
        j <- (1:16) + (i - 1) * 16
        id <- c(biome_map[1:16, j, 1:16])
        palette_list[[i]] <- vec_unique(id)
        values_list[[i]] <- match(id, palette_list[[i]])
    }

    h <- writeBin(height_map, raw(), size = 2L, endian = "little")
    b <- .Call(Cwrite_chunk_biomes, values_list, palette_list)
    c(h, b)
}
