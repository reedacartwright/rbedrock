#' Read and write HardcodedSpawnArea (HSA) data
#'
#' @description
#' `r lifecycle::badge("superseded")`
#'
#' HardcodedSpawnArea (HSA) data (tag 57) stores information about any structure
#' spawning locations in a chunk. An HSA is defined by a bounding box that
#' specifies the location of an HSA in a chunk and a tag that
#' specifies the type:
#'
#' | Value | Name |
#' |-------|------|
#' | 1     | NetherFortress |
#' | 2     | SwampHut |
#' | 3     | OceanMonument |
#' | 4     | Removed Cat HSA |
#' | 5     | PillagerOutpost |
#' | 6     | Removed Cat HSA |
#'
#' As of version 1.21.10, HSA data is no longer saved to the world db.
#'
#' * `get_hsa_value()` and `get_hsa_data()` load HardcodedSpawnArea data from
#' `db`. `get_hsa_value()` loads data for a single chunk, and `get_hsa_data()`
#' loads data for multiple chunks.
#' * `put_hsa_value()` and `put_hsa_data()` store HardcodedSpawnArea data into
#' `db`.
#'
#' @inheritParams ChunkData
#' @param value A HardcodedSpawnArea value
#' @param values A (named) vector of HardcodedSpawnArea values. If `x` is
#' missing, the names of `values` will be taken as the keys.
#'
#' @return `get_hsa_value()` returns a HardcodedSpawnArea data value.
#' `get_hsa_data()` returns a named vector of HardcodedSpawnArea data values.
#' HardcodedSpawnArea data values are integer matrices.
#'
#' @keywords internal
#' @name HardcodedSpawnArea
NULL

#' @rdname HardcodedSpawnArea
#' @export
get_hsa_value <- function(x, z, dimension, db = default_db()) {
    value <- get_chunk_value(x, z, dimension, tag = 57L, db = db)
    read_hsa_value_impl(value)
}

#' @rdname HardcodedSpawnArea
#' @export
get_hsa_data <- function(x, z, dimension, db = default_db()) {
    dat <- get_chunk_data(x, z, dimension, tag = 57L, db = db)
    lapply(dat, read_hsa_value_impl)
}

#' @rdname HardcodedSpawnArea
#' @export
put_hsa_value <- function(value, x, z, dimension, db = default_db()) {
    value <- write_hsa_value_impl(value)
    put_chunk_value(value, x, z, dimension, tag = 57L, db = db)
}

#' @rdname HardcodedSpawnArea
#' @export
put_hsa_data <- function(values, x, z, dimension, db = default_db()) {
    values <- lapply(values, write_hsa_value_impl)
    put_chunk_data(values, x, z, dimension, tag = 57L, db = db)
}

read_hsa_value_impl <- function(rawvalue) {
    if (is.null(rawvalue)) {
        return(NULL)
    }
    sz <- readBin(rawvalue, integer(), n = 1L, size = 4L, endian = "little")
    vec_assert(rawvalue, raw(), sz * 25L + 4)

    rawvalue <- rawvalue[-c(1:4)]
    stopifnot(length(rawvalue) == sz * 25L)
    mat <- matrix(0L, nrow = sz, ncol = 7)
    for (i in 1:sz) {
        aabb <- readBin(rawvalue, integer(), n = 6, size = 4, endian = "little")
        tag <- as.raw(rawvalue[25])
        mat[i, ] <- c(aabb, tag)
        rawvalue <- rawvalue[-c(1:25)]
    }
    colnames(mat) <- c("x1", "y1", "z1", "x2", "y2", "z2", "tag")

    # nolint start
    # Document how HSS are calculated for posterity
    # hsa$xspot <- (hsa$x1 + hsa$x2 + 1L) %/% 2L
    # hsa$yspot <- pmax.int(hsa$y1, hsa$y2) -
    #     ifelse(hsa$tag %in% .HSA_LIST[c(2, 5)], 4L, 1L)
    # hsa$zspot <- (hsa$z1 + hsa$z2 + 1L) %/% 2L
    # nolint end
    mat
}

write_hsa_value_impl <- function(value) {
    len <- nrow(value)
    ret <- raw(4L + 25L * len)
    ret[1:4] <- writeBin(as.integer(len), raw(), size = 4, endian = "little")
    for (i in 1:len) {
        pos <- i * 25L - 21L
        n <- as.integer(value[i, ])
        ret[pos + 1:24] <- writeBin(n[1:6], raw(), size = 4, endian = "little")
        ret[pos + 25L] <- writeBin(n[7], raw(), size = 1, endian = "little")
    }
    ret
}
