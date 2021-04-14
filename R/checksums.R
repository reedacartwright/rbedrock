#' Load and store Checksums data
#'
#' Checksums data (tag 59) holds checksums for several chunk records.
#' These records are 2DMaps (tag 45), SubchunkBlocks (tag 47),
#' BlockEntities (tag 49), and Entities (tag 50).
#' 
#' @name Checksums
NULL

#' @description
#' `get_checksums_data()` loads Checksums data from a `bedrockdb`.
#'  It will silently drop and keys not representing Checksums data.
#'
#' @param db A bedrockdb object.
#' @param x,z,dimension Chunk coordinates to extract data from.
#'    `x` can also be a character vector of db keys.
#'
#' @return `get_checksums_data()` returns a named-list of the values returned
#'          by `get_checksums_value()`.
#' @rdname Checksums
#' @export
get_checksums_data <- function(db, x=get_keys(db), z, dimension) {
    keys <- .process_key_args(x,z,dimension, tag=59L)
    dat <- get_values(db, keys)
    purrr::map(dat, read_checksums_value)
}

#' @description
#' `get_checksums_value()` loads Checksums data from a `bedrockdb`.
#' It only supports loading a single value.
#'
#' @return `get_checksums_value()` and `read_checksums_value()`
#'         return a character vector.
#'         The names of the character vector indicate which
#'         chunk record (tag and subtag) the checksum is for.
#' @rdname Checksums
#' @export
get_checksums_value <- function(db, x, z, dimension) {
    key <- .process_key_args(x,z,dimension, tag=59L)
    stopifnot(rlang::is_scalar_character(key))

    dat <- get_value(db, key)
    read_checksums_value(dat)
}

#' @description
#' `update_checksums_data()` recalculates Checksums data.
#' It calculates checksums for the specified chunks'
#' SubchunkBlocks, 2DMaps, BlockEntities, and Entities
#' records in `db` and updates the Checksums record to match.
#'
#' @rdname Checksums
#' @export
update_checksums_data <- function(db, x, z, dimension) {
    keys <- .process_key_args(x,z,dimension, tag=59L, stop_if_filtered = TRUE)
    purrr::map(keys, .update_checksums_value_impl, db=db)
    invisible(db)
}

#' @description
#' `read_checksums_value()` parses a binary Checksums record
#' into a list of checksums.
#'
#' @param rawdata a raw vector holding binary Checksums data
#'
#' @rdname Checksums
#' @export
read_checksums_value <- function(rawdata) {
    sz <- readBin(rawdata, integer(), n=1L, size= 4L, endian = "little")
    rawdata <- rawdata[-c(1:4)]
    stopifnot(length(rawdata) == sz*11L)
    ret <- list()
    for(i in 1:sz) {
        tag <- readBin(rawdata, integer(), n=1L, size=2L, endian = "little")
        subtag <- as.integer(rawdata[3])
        hash <- rawdata[4:11]
        hash <- paste0(rev(as.character(hash)), collapse="")
        k <- ifelse(tag == 47L, paste(tag, subtag, sep="-"), as.character(tag))
        ret[[k]] <- hash
        rawdata <- rawdata[-c(1:11)]
    }
    storage.mode(ret) <- "character"
    ret
}

#' @description
#' `write_checksums_value()` converts Checksums from a named list into 
#' binary format.
#'
#' @param object a named character vector in the same format as returned by `read_checksums_value()`.
#'
#' @return `write_checksums_value()` returns a raw vector.
#' @rdname Checksums
#' @export
write_checksums_value <- function(object) {
    parsed_names <- stringr::str_match(names(object), .CHUNK_KEY_TAG_MATCH)
    stopifnot(!any(is.na(parsed_names[,1])))
    tag <- as.integer(parsed_names[,2])
    subtag <- as.integer(parsed_names[,3])
    # check for NAs introduced by coercion
    stopifnot(all(is.na(tag) == is.na(parsed_names[,2]) & is.na(subtag) == is.na(parsed_names[,3])))

    subtag[is.na(subtag)] <- 0L

    hash <- purrr::map(object, function(x){
        h <- strsplit(x, character(0L))[[1]]
        h <- paste(h[c(TRUE,FALSE)], h[c(FALSE,TRUE)], sep="")
        as.raw(as.hexmode(rev(h)))
    })

    ret <- writeBin(length(object), raw(), size=4L, endian="little")
    for(i in 1:length(object)) {
        ret <- c(ret,
            writeBin(tag[i], raw(), size=2L, endian="little"),
            as.raw(subtag[i]),
            hash[[i]])
    }
    ret
}

.update_checksums_value_impl <- function(db, key) {
    # 45, 47, 49, 50 all need to be updated if they exist
    stem <- stringr::str_replace(key, ":59$", "")
    chunk_keys <- get_keys(db, stem)
    chunk_keys <- stringr::str_subset(chunk_keys, ":(?:47-[^-:]+|45|49|50)$")

    dat <- get_values(db, chunk_keys)
    obj <- purrr::map(dat, .checksum_impl)
    names(obj) <- .trim_stem_from_chunk_key(names(obj))

    val <- write_checksums_value(obj)
    put_value(db, key, val)
    invisible()
}

.checksum_impl <- function(x) {
    # this matches the algorithm used in BDS
    digest::digest(x, algo="xxhash64", serialize=FALSE, raw=TRUE)
}
