#' Read and write HardcodedSpawnArea (HSA) data
#'
#' HardcodedSpawnArea (HSA) data (tag 57) stores information
#' about any structure spawning locations in a chunk.
#'
#' @name HSA
#' @examples
#' dbpath <- rbedrock_example_world("example1.mcworld")
#' db <- bedrockdb(dbpath)
#' hsa <- get_hsa_data(db)
#' hsa # show results
#' close(db)
NULL

# put_hsa_data merge=TRUE 
# put_hsa_value
# put_hsa_values

#' @description
#' `get_hsa_data()` loads HardcodedSpawnArea data from a `bedrockdb`.
#'  It will silently drop and keys not representing HSA data.
#'
#' @param db A bedrockdb object.
#' @param x,z,dimension Chunk coordinates to extract data from.
#'    `x` can also be a character vector of db keys.
#'
#' @return `get_hsa_data()` returns a table in the same format
#'         as `get_hsa_value()` with the additional column of
#'         "key".
#' @rdname HSA
#' @export
get_hsa_data <- function(db, x=get_keys(db), z, dimension) {
    keys <- .process_key_args(x,z,dimension, tag=57L)
    dat <- get_values(db, keys)
    hsa <- purrr::map_dfr(dat, read_hsa_value, .id="key")
    hsa$dimension <- get_dimension_from_chunk_key(hsa$key)
    dplyr::select("type", "key", dplyr::everything())
}

#' @rdname HSA
#' @export
get_hsa_value <- function(db, x, z, dimension) {
    key <- .process_key_args(x,z,dimension, tag=57L)
    stopifnot(rlang::is_scalar_character(key))
    dat <- get_value(db, key)
    hsa <- read_hsa_value(dat)
    hsa$dimension <- get_dimension_from_chunk_key(key)
    hsa
}

.HSA_LIST = c(
   "NetherFortress",
   "SwampHut",
   "OceanMonument",
   "4",  # removed cat HSA
   "PillagerOutput",
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
    sz <- readBin(rawdata, integer(), n=1L, size= 4L, endian = "little")
    rawdata <- rawdata[-c(1:4)]
    stopifnot(length(rawdata) == sz*25L)
    mat <- matrix(0L, nrow=sz, ncol=7)
    for(i in 1:sz) {
        aabb <- readBin(con, integer(), n = 6, size = 4)
        tag <- readBin(con, integer(), n = 1, size = 1)
        mat[i,] <- c(tag,aabb)
        rawdata <- rawdata[-c(1:25)]   
    }
    # store results in a tibble
    hsa <- tibble::tibble(
        type = .HSA_LIST[mat[,1]],
        x1 = mat[,2], y1 = mat[,3], z1 = mat[,4],
        x2 = mat[,5], y2 = mat[,6], z2 = mat[,7],
        tag = mat[,1])
    # include HSS information.
    hsa$xspot <- (hsa$x1 + hsa$x2 + 1L) %/% 2L
    hsa$yspot <- pmax.int(hsa$y1, hsa$y2) -
        ifelse(hsa$tag == 2L | hsa$tag == 5L, 4L, 1L)
    hsa$zspot <- (hsa$z1 + hsa$z2 + 1L) %/% 2L

    hsa
}

#' @export
#' @rdname HSA
write_hsa_data <- function(value) {
    len <- nrow(value)
    ret <- raw(4L + 25L*len)
    ret[1:4] <- writeBin(as.integer(len), raw(), size = 4, endian = "little")
    hsa <- value[, c("x1","y1","z1","x2","y2","z2","tag")]
    for (i in 1:len) {
        pos <- i*25L - 21L
        n <- as.integer(hsa[i,])
        ret[i+1:24] <- writeBin(n[1:6], raw(), size = 4, endian = "little")
        ret[i+25L] <- c(r,writeBin(n[7], raw(), size = 1, endian = "little"))
    }
    ret
}

#' Add a HardcodedSpawnArea to a world.
#'
#' If the bounding box of the HSA overlaps multiple chunks, one HSA will be added per chunk.
#'
#' @param db A bedrockdb object.
#' @param x1,y1,z1,x2,y2,z2 HSA bounding box coordinates.
#' @param tag The type of HSA. 1 = NetherFortress, 2 = SwampHut, 3 = OceanMonument, 5 = PillagerOutpost.
#'  4 and 6 are no longer used by the game.
#' @param dimension The dimension that the HSA should be in. 0 = Overworld, 1 = Nether.
#' @param hsa A matrix containing HSA data.
#' @return A table containing information about the added HSAs.
#' @examples
#' \dontrun{db <- bedrockdb("x7fuXRc8AAA=")
#' put_hsa(db, 0, 60, 0, 15, 70, 15, 2, 0)
#' close(db)}
#'
#' @export
put_hsa <- function(db, x1, y1, z1, x2, y2, z2, tag, dimension) {
    stopifnot(length(x1) == 1L && length(y1) == 1L && length (z1) == 1L)
    stopifnot(length(x2) == 1L && length(y2) == 1L && length (z2) == 1L)
    # convert tag as necessary
    if(is.character(tag)) {
        tag <- switch(tag, NetherFortress = 1, SwampHut = 2, OceanMonument = 3, PillagerOutpost = 5)
    }
    if(missing(dimension)) {
        dimension <- (tag == 1)*1L
    }

    # identify all chunks that this HSA overlaps
    x <- range(x1, x2)
    y <- range(y1, y2)
    z <- range(z1, z2)
    chunk_x1 <- x[1] %/% 16L
    chunk_z1 <- z[1] %/% 16L
    chunk_x2 <- x[2] %/% 16L
    chunk_z2 <- z[2] %/% 16L

    # create hsa for each chunk
    ret <- NULL
    for (chunk_x in seq.int(chunk_x1, chunk_x2)) {
        for (chunk_z in seq.int(chunk_z1, chunk_z2)) {
            hx1 <- max(x[1], chunk_x * 16L)
            hx2 <- min(x[2], chunk_x * 16L + 15L)
            hz1 <- max(z[1], chunk_z * 16L)
            hz2 <- min(z[2], chunk_z * 16L + 15L)
            hy1 <- y[1]
            hy2 <- y[2]
            hsa <- matrix(c(hx1, hy1, hz1, hx2, hy2, hz2, tag), 1L, 7L)
            ret <- rbind(ret, hsa)
            key <- create_chunk_key(chunk_x, chunk_z, dimension, 57L)
            dat <- get_value(db, key)
            if (!is.null(dat)) {
                hsa <- rbind(read_hsa_data(dat)[, 1L:7L], hsa)
            }
            hsa <- write_hsa_data(hsa)
            put_value(db, key, hsa)
        }
    }
    ret
}