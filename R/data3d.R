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
    height_map <- readBin(rawdata[1:512], integer(), n = 256L, size = 2L,
                          endian = "little", signed = TRUE)
    dim(height_map) <- c(16L, 16L)

    b <- .Call(Cread_chunk_biomes, rawdata[-(1:512)])
    # Validate Biome Data
    if (length(b) == 0 || is.null(b[[1]])) {
        abort("Value does not contain at least one subchunk of biome data.")
    }
    # Enlarge list to length 24 if necessary.
    if (length(b) < 24) {
        b[24] <- list(NULL)
    }
    # Validate Biome Data
    hasdata <- !sapply(b, is.null)
    n <- length(hasdata)
    if (sum(hasdata[-1] != hasdata[-n]) > 1) {
        abort("Value contains empty biome data between valid subchunks.")
    }
    # Trim biome data
    if (n > 24) {
        if (any(hasdata[25:n])) {
            msg <- sprintf("Trimming biome data from %d to 24 subchunks.",
                           length(b))
            warn(msg)
        }
        b <- b[1:24]
        hasdata <- hasdata[1:24]
    }

    # Fill biome array
    biome_map <- array(NA_integer_, c(16, 16, 24 * 16))

    # Subchunks with data
    ii <- which(hasdata)
    for (i in ii) {
        bb <- b[[i]]
        biome_map[, , 16 * (i - 1) + (1:16)] <- bb$palette[bb$values]
    }
    # Subchunks without data copy from the highest y level of
    # subchunks with data
    i <- max(ii)
    if (i < 24) {
        y <- 16 * i
        biome_map[, , (y + 1):(16 * 24)] <- biome_map[, , y]
    }
    # reshape from x,z,y to x,y,z
    biome_map <- aperm(biome_map, c(1, 3, 2))
    list(height_map = height_map, biome_map = biome_map)
}

reshape_biome_map <- function(value) {
    # returns biome_map in x,z,y order
    n <- length(value)
    if (n == 1 || n == 256) {
        array(value, c(16, 16, 16 * 24))
    } else if (n > 0 && n %% 256 == 0) {
        ny <- length(value) %/% 256
        if (ny == 16 * 24) {
            value <- array(value, c(16, ny, 16))
            aperm(value, c(1, 3, 2))
        } else if (ny > 16 * 24) {
            value <- array(value, c(16, ny, 16))
            aperm(value[, , 1:(16 * 24)], c(1, 3, 2))
        } else {
            v <- array(NA_integer_, c(16, 16, 16 * 24))
            v[, , 1:ny] <- aperm(value, c(1, 3, 2))
            v[, , (ny + 1):(16 * 24)] <- v[, , ny]
            v
        }
    } else {
        abort("Invalid biome_map dimensions.")
    }
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
    biome_map <- reshape_biome_map(biome_map)

    # identify y levels with repetitive biomes
    y <- (16 * 24):2
    o <- sapply(y, function(x) any(biome_map[, , x] != biome_map[, , x - 1]))
    m <- match(TRUE, o)
    m <- if (is.na(m)) 1 else y[m]
    # y levels m to 384 are identical.
    # chunks 1:mm need to be written
    mm <- ((m - 1) %/% 16) + 1

    values_list <- rep(list(integer(0L)), 24)
    palette_list <- rep(list(integer(0L)), 24)
    for (i in 1:mm) {
        j <- (1:16) + (i - 1) * 16
        id <- c(biome_map[, , j])
        palette_list[[i]] <- vec_unique(id)
        values_list[[i]] <- match(id, palette_list[[i]])
    }

    h <- writeBin(height_map, raw(), size = 2L, endian = "little")
    b <- .Call(Cwrite_chunk_biomes, values_list, palette_list)
    c(h, b)
}
