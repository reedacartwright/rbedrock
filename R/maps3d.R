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
#' `get_cnc_biomes_data()` loads 3D Biomes data from a `bedrodckdb`.
#' It will silently drop keys not holding 3D biome data.
#'
#' @return `get_cnc_biomes_data()` returns a list of the of the values returned by 
#'         `get_cnc_biomes__value()`.
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
    a <- purrr::map(b, function(x) {
        # apply palette
        array(x$palette[x$values], dim(x$values))
    })
    b <- array(0L, c(16,length(a)*16,16))
    for(i in seq_along(a)) {
        b[,16*(i-1)+(1:16),] <- a[[i]]
    }
    list(height_map = h, biome_map = b)
}

.get_biomes_impl <- function(x, return_names) {
    if(is.null(x)) {
        return(NULL)
    }
    y <- x$biome_map
    if(isTRUE(return_names)) {
        y[] <- .BIOME_LIST_INV[y+1]
    }
    y
}