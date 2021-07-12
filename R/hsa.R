#' Read and write HardcodedSpawnArea (HSA) data
#'
#' HardcodedSpawnArea (HSA) data (tag 57) stores information
#' about any structure spawning locations in a chunk.
#' An HSA is defined by a bounding box that specifies
#' the location of an HSA in a chunk and a tag that
#' specifies the type: 1 = NetherFortress, 2 = SwampHut,
#' 3 = OceanMonument, and 5 = PillagerOutpost.
#'
#' @name HSA
#' @examples
#' dbpath <- rbedrock_example_world("example1.mcworld")
#' db <- bedrockdb(dbpath)
#' # view all HSA in a world
#' hsa <- get_hsa_data(db)
#' hsa
#' # add an HSA to a world
#' dat <- data.frame(x1 = 0, x2 = 15, z1 = 0, z2 = 15,
#'                   y1 = 40, y2 = 60, tag = "SwampHut")
#' put_hsa_data(db, dat, merge = TRUE)
#' close(db)
NULL

#' @description
#' `get_hsa_data()` loads HardcodedSpawnArea data from a `bedrockdb`.
#'  It will silently drop and keys not representing HSA data.
#'
#' @param db A bedrockdb object.
#' @param x,z,dimension Chunk coordinates to extract data from.
#'    `x` can also be a character vector of db keys.
#'
#' @return `get_hsa_data()` returns a table in the same format
#'         as `get_hsa_value()`.
#' @rdname HSA
#' @export
get_hsa_data <- function(db, x=get_keys(db), z, dimension) {
    keys <- .process_key_args(x,z,dimension, tag=57L)
    dat <- get_values(db, keys)
    hsa <- purrr::map_dfr(dat, read_hsa_value, .id="key")

    hsa$dimension <- .get_dimension_from_chunk_key(hsa$key)
    hsa <- dplyr::relocate(hsa, "key", .after = dplyr::last_col())
    hsa
}

#' @description
#' `get_hsa_value()` loads HSA data from a `bedrockdb`.
#' It only supports loading a single value.
#'
#' @return `get_hsa_value()` and `read_hsa_value()`
#'         return a table with columns indicating the
#'         coordinates of the HSA bounding box and the
#'         location of the HSS at the center of the bounding
#'         box. `get_hsa_value()` also records the dimension
#'         of the bounding box.
#' @rdname HSA
#' @export
get_hsa_value <- function(db, x, z, dimension) {
    key <- .process_key_args(x,z,dimension, tag=57L)
    vec_assert(key, character(), 1L)
    dat <- get_value(db, key)
    hsa <- read_hsa_value(dat)
    hsa$dimension <- .get_dimension_from_chunk_key(key)
    hsa$key <- key
    hsa
}

.HSA_LIST = c(
   "NetherFortress",
   "SwampHut",
   "OceanMonument",
   "4",  # removed cat HSA
   "PillagerOutpost",
   "6" # removed cat HSA
)

#' @description
#' `read_hsa_value()` decodes HSA data.
#'
#' @param rawdata A scalar raw.
#'
#' @rdname HSA
#' @export
read_hsa_value <- function(rawdata) {
    if(is.null(rawdata)) {
        hsa <- tibble::tibble(
            tag = character(0L),
            x1 = integer(0L), y1 = integer(0L), z1 = integer(0L),
            x2 = integer(0L), y2 = integer(0L), z2 = integer(0L),
            xspot = integer(0L), yspot = integer(0L), zspot = integer(0L)
        )
        return(hsa)
    }
    sz <- readBin(rawdata, integer(), n=1L, size= 4L, endian = "little")
    vec_assert(rawdata, raw(), sz*25L+4)
    
    rawdata <- rawdata[-c(1:4)]
    stopifnot(length(rawdata) == sz*25L)
    mat <- matrix(0L, nrow=sz, ncol=7)
    for(i in 1:sz) {
        aabb <- readBin(rawdata, integer(), n = 6, size = 4, endian = "little")
        tag <- as.raw(rawdata[25])
        mat[i,] <- c(tag, aabb)
        rawdata <- rawdata[-c(1:25)]
    }
    # store results in a tibble
    hsa <- tibble::tibble(
        tag = .HSA_LIST[mat[,1]],
        x1 = mat[,2], y1 = mat[,3], z1 = mat[,4],
        x2 = mat[,5], y2 = mat[,6], z2 = mat[,7])
    # include HSS information.
    hsa$xspot <- (hsa$x1 + hsa$x2 + 1L) %/% 2L
    hsa$yspot <- pmax.int(hsa$y1, hsa$y2) -
        ifelse(hsa$tag %in% .HSA_LIST[c(2,5)], 4L, 1L)
    hsa$zspot <- (hsa$z1 + hsa$z2 + 1L) %/% 2L

    hsa
}

#' @description
#' `put_hsa_data()` puts HSA data into a `bedrockdb`.
#' HSA bounding boxes will be split across chunks and 
#'
#' @param data A table containing HSA coordinates.
#' @param merge Merge the new HSAs with existing HSAs.
#'
#' @rdname HSA
#' @export
put_hsa_data <- function(db, data, merge = TRUE) {
    # create HSA data
    x1 <- pmin(data$x1,data$x2)
    x2 <- pmax(data$x1,data$x2)
    y1 <- pmin(data$y1,data$y2)
    y2 <- pmax(data$y1,data$y2)
    z1 <- pmin(data$z1,data$z2)
    z2 <- pmax(data$z1,data$z2)
    tag <- data$tag
    if(is.character(tag)) {
        tag <- match(tag, .HSA_LIST)
        stopifnot(all(!is.na(tag)))
    }
    dimension <- data$dimension
    if(is.null(dimension)) {
        dimension <- ifelse(tag == 1L, 1L, 0L)
    }
    dat <- NULL
    for(i in seq_along(x1)) {
        # identify which chunks the row overlaps
        a <- seq.int(x1[i] %/% 16L, x2[i] %/% 16L)
        b <- seq.int(z1[i] %/% 16L, z2[i] %/% 16L)
        chunks <- tidyr::expand_grid(x=a, z=b, d=dimension[i])
        chunks$key <- create_chunk_keys(chunks$x, chunks$z, chunks$d, 57L)
        # identify intersection between hsa and chunks.
        a1 <- pmax(chunks$x*16L, x1[i])
        b1 <- pmax(chunks$z*16L, z1[i])
        a2 <- pmin(chunks$x*16L+15L, x2[i])
        b2 <- pmin(chunks$z*16L+15L, z2[i])
        # construct table of hsa
        dati <- tibble::tibble(
            tag = tag[i],
            x1 = a1, y1 = y1[i], z1 = b1,
            x2 = a2, y2 = y2[i], z2 = b2,
            key = chunks$key
            )
        dat <- dplyr::bind_rows(dat, dati)
    }
    if(is.null(dat)) {
        return(dat)
    }
    # merge existing values
    ret <- dat
    if(isTRUE(merge)) {
        mdat <- get_hsa_data(db, unique(dat$key))
        mdat <- mdat[,c("tag","x1","y1","z1","x2","y2","z2","key")]
        mdat$tag <- match(mdat$tag, .HSA_LIST)
        dat <- dplyr::bind_rows(mdat,dat)
    }
    # split hsa by chunk and store
    dat <- split(dat, dat$key)
    dat <- purrr::map(dat, write_hsa_value)
    put_data(db, dat)
    ret$tag <- .HSA_LIST[ret$tag]
    invisible(ret)
}

#' @description
#' `put_hsa_values()` and `put_hsa_value()` store HSA data
#' into a `bedrockdb`.
#'
#' @param values A list of tables containing HSA coordinates and tags.
#'
#' @rdname HSA
#' @export
put_hsa_values <- function(db, x, z, dimension, values) {
    keys <- .process_key_args(x, z, dimension, tag=57L, stop_if_filtered = TRUE)
    stopifnot(length(keys) == length(values))
    values <- purrr::map(values, write_hsa_value)
    put_values(db, keys, values)
}

#' @param value A table containing HSA coordinates
#'
#' @rdname HSA
#' @export
put_hsa_value <- function(db, x, z, dimension, value) {
    key <- .process_key_args(x, z, dimension, tag=57L)
    vec_assert(key, character(), 1L)
    value <- write_hsa_value(value)
    put_value(db, key, value)
}

#' @description
#' `write_hsa_value()` encodes HSA data.
#'
#' @export
#' @rdname HSA
write_hsa_value <- function(value) {
    len <- nrow(value)
    ret <- raw(4L + 25L*len)
    ret[1:4] <- writeBin(as.integer(len), raw(), size = 4, endian = "little")
    hsa <- value[, c("x1","y1","z1","x2","y2","z2","tag")]
    if(is.character(hsa$tag)) {
        hsa$tag <- match(hsa$tag, .HSA_LIST)
        stopifnot(all(!is.na(hsa$tag)))
    }
    for (i in 1:len) {
        pos <- i*25L - 21L
        n <- as.integer(hsa[i,])
        ret[pos+1:24] <- writeBin(n[1:6], raw(), size = 4, endian = "little")
        ret[pos+25L] <- writeBin(n[7], raw(), size = 1, endian = "little")
    }
    ret
}