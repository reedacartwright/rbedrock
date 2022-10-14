#' Load and store HardcodedSpawnArea (HSA) data
#'
#' HardcodedSpawnArea (HSA) data (tag 57) stores information
#' about structure spawning locations in a chunk.
#' An HSA is defined by a bounding box that specifies
#' the location of an HSA in a chunk and a tag that
#' specifies the type: 1 = NetherFortress, 2 = SwampHut,
#' 3 = OceanMonument, and 5 = PillagerOutpost.
#'
#' `get_hsa_data()` and `get_hsa_value()` load HardcodedSpawnArea data from `db`.
#' `get_hsa_value()` only supports loading a single value.
#'
#' `put_hsa_data()` and `put_hsa_value()` puts HSA data into `db`.
#'
#' `read_hsa_value()` decodes HSA data. `write_hsa_value()` encodes HSA data.
#'
#' @param db A bedrockdb object.
#' @param x,z,dimension Chunk coordinates to extract data from.
#'    `x` can also be a character vector of db keys.
#' @param values A table or a list of tables containing HSA coordinates and tags.
#'   If `values` is a table, the HSA bounding boxes will be split across chunks as needed.
#' @param rawdata A scalar raw.
#' @param value A table containing HSA coordinates
#' @param merge Merge the new HSAs with existing HSAs.
#' @param simplify If `TRUE`, simplify a list of tables into one table.
#' @param pretty If `TRUE`, return as a tibble with extra information.
#'
#' @return `get_hsa_data()` returns a table in the same format
#'         as `get_hsa_value()`.
#' `get_hsa_value()` and `read_hsa_value()`
#'         return a table with columns indicating the
#'         coordinates of the HSA bounding box and the
#'         location of the HSS at the center of the bounding
#'         box. `get_hsa_value()` also records the dimension
#'         of the bounding box.
#'
#' @examples
#' dbpath <- rbedrock_example_world("example1.mcworld")
#' db <- bedrockdb(dbpath)
#' # view all HSA in a world
#' keys <- grep(":57$", get_keys(db), value=TRUE)
#' hsa <- get_hsa_data(keys, db = db)
#' hsa
#' # add an HSA to a world
#' dat <- data.frame(x1 = 0, x2 = 15, z1 = 0, z2 = 15,
#'                   y1 = 40, y2 = 60, tag = "SwampHut")
#' put_hsa_data(dat, db = db, merge = TRUE)
#' close(db)
#' @name HardcodedSpawnArea
NULL

#' @rdname HardcodedSpawnArea
#' @export
get_hsa_data <- function(x, z, dimension, db, simplify = TRUE) {
    dat <- .get_chunk_data(x, z, dimension, db, tag = 57L)
    hsa <- purrr::map(dat, read_hsa_value, pretty = simplify)
    if(isTRUE(simplify)) {
        hsa <- dplyr::bind_rows(hsa, .id = "key")
        hsa$dimension <- .get_dimension_from_chunk_key(hsa$key)
        hsa <- dplyr::relocate(hsa, "key", .after = dplyr::last_col())
    }
    hsa
}

#' @rdname HardcodedSpawnArea
#' @export
get_hsa_value <- function(x, z, dimension, db) {
    dat <- .get_chunk_value(x, z, dimension, db, tag = 57L)
    read_hsa_value(dat)
}

.HSA_LIST = c(
   "NetherFortress",
   "SwampHut",
   "OceanMonument",
   "4",  # removed cat HSA
   "PillagerOutpost",
   "6" # removed cat HSA
)

#' @rdname HardcodedSpawnArea
#' @export
read_hsa_value <- function(rawdata, pretty = TRUE) {
    if(is.null(rawdata)) {
        mat <- data.frame(matrix(integer(0L),0,7))
    } else {
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
    }
    hsa <- as.data.frame(mat)
    names(hsa) <- c("tag", "x1", "y1", "z1", "x2", "y2", "z2")
    if(isTRUE(pretty)) {
        # store results in a tibble
        hsa <- tibble::as_tibble(hsa)
        # prettify tag
        hsa$tag <- .HSA_LIST[mat[,1]]
        # include HSS information.
        hsa$xspot <- (hsa$x1 + hsa$x2 + 1L) %/% 2L
        hsa$yspot <- pmax.int(hsa$y1, hsa$y2) -
            ifelse(hsa$tag %in% .HSA_LIST[c(2,5)], 4L, 1L)
        hsa$zspot <- (hsa$z1 + hsa$z2 + 1L) %/% 2L        
    }
    hsa
}

#' @rdname HardcodedSpawnArea
#' @export
put_hsa_data <- function(values, x, z, dimension, db, merge = TRUE) {
    # if values is a data.frame, split HSAs across chunks as necessary
    if(is.data.frame(values)) {
        # create HSA data
        x1 <- pmin(values$x1, values$x2)
        x2 <- pmax(values$x1, values$x2)
        y1 <- pmin(values$y1, values$y2)
        y2 <- pmax(values$y1, values$y2)
        z1 <- pmin(values$z1, values$z2)
        z2 <- pmax(values$z1, values$z2)
        tag <- values$tag
        dimension <- values$dimension
        if(is.character(tag)) {
            tag <- match(tag, .HSA_LIST)
            stopifnot(all(!is.na(tag)))
        }
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
            dati <- data.frame(
                tag = tag[i],
                x1 = a1, y1 = y1[i], z1 = b1,
                x2 = a2, y2 = y2[i], z2 = b2,
                key = chunks$key
                )
            dat <- dplyr::bind_rows(dat, dati)
        }
        # split by chunk
        datkeys <- dat$key
        dat$key <- NULL
        values <- split(dat, datkeys)
    }
    keys <- .process_chunk_key_args(x, z, dimension, tag = 57L,
        values = values, assert_validity = TRUE)

    # save HSA for returning
    ret <- set_names(values, keys)

    # merge existing values
    if(isTRUE(merge)) {
        mdat <- get_hsa_data(keys, db = db, simplify = FALSE)
        values <- purrr::map(set_names(keys), function(k) {
            dplyr::bind_rows(mdat[[k]], values[[k]])
        })
    }

    # store
    dat <- purrr::map(values, write_hsa_value)
    put_data(dat, keys, db = db)
    invisible(ret)
}

#' @rdname HardcodedSpawnArea
#' @export
put_hsa_value <- function(value, x, z, dimension, db) {
    value <- write_hsa_value(value)
    .put_chunk_value(value, x, z, dimension, tag=57L, db = db)
}

#' @export
#' @rdname HardcodedSpawnArea
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
