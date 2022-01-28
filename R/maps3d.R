#' Read and write 3DMaps data
#'
#' 3DMaps data (tag 43) stores information about surface heights and biomes in a
#' chunk.
#'
#' @name Maps3D
#'
NULL

#' @description
#' `get_3dmaps_data()` loads 3DMaps data from a `bedrockdb`.
#'  It will silently drop keys not representing 3DMaps data.
#'
#' @param db A bedrockdb object.
#' @param x,z,dimension Chunk coordinates to extract data from.
#'    `x` can also be a character vector of db keys.
#'
#' @return `get_3dmaps_data()` returns a list of the of the values returned by 
#'         `get_3dmaps_value()`.
#'
#' @rdname Maps3D
#' @export
get_3dmaps_data <- function(db, x, z, dimension) {
    keys <- .process_key_args(x,z,dimension, tag=43L)
    dat <- get_values(db, keys)
    purrr::map(dat, read_3dmaps_value)
}

#' @rdname Maps3D
#' @export
get_3dmaps_values <- get_3dmaps_data

#' @description
#' `get_3dmaps_value()` loads 3DMaps data from a `bedrockdb`.
#' It only supports loading a single value.
#'
#' @return `get_3dmaps_value()` returns a list with components "height_map"
#' and "biome_map".
#' @rdname Maps3D
#' @export
get_3dmaps_value <- function(db, x, z, dimension) {
    key <- .process_key_args(x, z, dimension, tag=43L)
    vec_assert(key, character(), 1L)
    dat <- get_value(db, key)
    read_3dmaps_value(dat)
}

#' @description
#' `put_3dmaps_data()`, `put_3dmaps_values()`, and
#' `put_3dmaps_value()` store 3DMaps data into a `bedrockdb`.
#'
#' @param data A named-vector of key-value pairs for 3DMaps data.
#'
#' @rdname Maps3D
#' @export
put_3dmaps_data <- function(db, data) {
    put_3dmaps_values(db, x=names(data), height_maps=data)
}

#' @param height_maps,biome_maps Lists of height and biome data.
#' Values will be recycled if necessary to match the number of keys
#' to be written to. If `biome_maps` is missing, `height_maps` should
#' be in the same format as returned by `get_3dmaps_data()`.
#'
#' @rdname Maps3D
#' @export
put_3dmaps_values <- function(db, x, z, dimension, height_maps, biome_maps) {
    keys <- .process_key_args(x, z, dimension, tag=43L, stop_if_filtered = TRUE)
    if(missing(biome_maps)) {
        values <- vec_recycle(height_maps, length(keys), x_arg="height_maps")
        values <- purrr::map(values, write_3dmaps_value)
    } else {
        h <- vec_recycle(height_maps, length(keys), x_arg="height_maps")
        b <- vec_recycle(biome_maps, length(keys), x_arg="biome_maps")
        values <- purrr::map2(h, b, write_3dmaps_value)
    }
    put_values(db, keys, values)
}

#' @param height_map 16x16 array containing height data.
#' Values will be recycled if necessary. If `biome_map` is missing, `height-map`
#' should be a list a `list()` with both "height_map" and "biome_map" elements.
#' @param biome_map 16xNx16 array containing biome data.
#'
#' @rdname Maps3D
#' @export
put_3dmaps_value <- function(db, x, z, dimension, height_map, biome_map) {
    key <- .process_key_args(x, z, dimension, tag=43L)
    vec_assert(key, character(), 1L)
    value <- write_3dmaps_value(height_map, biome_map)
    put_value(db, key, value)
}

#' @description
#' `get_cnc_biomes_data()` loads 3D Biomes data from a `bedrodckdb`.
#' It will silently drop keys not holding 3D biome data.
#'
#' @return `get_cnc_biomes_data()` returns a list of the of the values returned by 
#'         `get_cnc_biomes_value()`.
#' @rdname Maps3D
#' @inheritParams get_biomes_data
#' @export
get_cnc_biomes_data <- function(db, x, z, dimension, return_names = TRUE) {
    dat <- get_3dmaps_data(db, x, z, dimension)
    purrr::map(dat, .get_biomes_impl, return_names = return_names)
}

#' @rdname Maps3D
#' @export
get_cnc_biomes_values <- get_cnc_biomes_data

#' @description
#' `get_cnc_biomes_value()` loads 3D biome data from a `bedrockdb`.
#' It only supports loading a single value.
#'
#' @return `get_cnc_biomes_value()` returns an array.
#'
#' @rdname Maps3D
#' @export
get_cnc_biomes_value <- function(db, x, z, dimension, return_names = TRUE) {
    dat <- get_3dmaps_value(db, x, z, dimension)
    .get_biomes_impl(dat, return_names = return_names)
}

#' @description
#' `put_cnc_biomes_data()` `put_cnc_biomes_values()`, and `put_cnc_biomes_value()` update
#' the biome information of chunks. They preserve any existing height data.
#'
#' @param data A list of character or integer vectors. Each element of
#'    the list must contain 256 values or an error will be raised.
#' @param missing_height if there is no existing height data, use this value
#'    for the chunk.
#'
#' @rdname Maps3D
#' @export
put_cnc_biomes_data <- function(db, data, missing_height = -64L) {
    put_cnc_biomes_values(db, names(data), values=data, missing_height = missing_height)
}

#' @param values a list of arrays containing biome names or ids.
#'
#' @rdname Maps3D
#' @export
put_cnc_biomes_values <- function(db, x, z, dimension, values,
    missing_height = -64L) {
    keys <- .process_key_args(x, z, dimension, tag=43L, stop_if_filtered = TRUE)
    values <- vctrs::vec_recycle(values, length(keys), x_arg="values")

    dat <- get_3dmaps_data(db, keys)

    dat2 <- purrr::map2(dat, values, function(d, value) {
        h <- d$height_map %||% missing_height

        if(is.character(value)) {
            value <- biome_id(value)
            if(any(is.na(value))) {
                abort("`values` contains unknown biome")                 
            }
        }
        list(height_map = h, biome_map = value)
    })

    put_3dmaps_data(db, dat2)
}

#' @description
#' `get_cnc_biomes_value()` loads 3D biome data from a `bedrockdb`.
#' It only supports loading a single value.
#'
#' @param value an array containing biome names or ids.
#'
#' @rdname Maps3D
#' @export
put_cnc_biomes_value <- function(db, x, z, dimension, value,
    missing_height = -64L) {
    key <- .process_key_args(x, z, dimension, tag=43L)
    vec_assert(key, character(), 1L)

    d <- get_3dmaps_value(db, key)
    h <- d$height_map %||% missing_height

    if(is.character(value)) {
        value <- biome_id(value)
        if(any(is.na(value))) {
            abort("`value` contains unknown biome")
        }
    }
    put_3dmaps_value(db, key, height_map = h, biome_map = value)
}

#' @description
#' `read_3dmaps_value()` decodes binary 3DMaps data.
#'
#' @param rawdata A raw vector.
#'
#' @rdname Maps3D
#' @export
read_3dmaps_value <- function(rawdata) {
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

#' @description
#' `write_3dmaps_value` encodes 3DMaps data into a raw vector.
#'
#' @rdname Maps3D
#' @export
write_3dmaps_value <- function(height_map, biome_map) {
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
        abort("Invalid 3DMaps data.")
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

.get_biomes_impl <- function(x, return_names) {
    if(is.null(x$biome_map)) {
        return(NULL)
    }
    y <- x$biome_map
    if(isTRUE(return_names)) {
        y[] <- .BIOME_LIST_INV[y+1]
    }
    y
}
