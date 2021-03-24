#' Read and write chunk version data
#'
#' ChunkVersion data (tag 44) and ChunkVersionLegacy data (tag 118)
#' store the version number of a chunk.
#'
#' @name ChunkVersion
NULL

#' @description
#' `get_chunk_versions()` retrieves chunk versions from a `bedrockdb`.
#' It will silently drop and keys not representing ChunkVersion data.
#'
#' @param db A bedrockdb object.
#' @param x,z,dimension Chunk coordinates to extract version data from.
#'    `x` can also be a character vector of db keys.
#' @param include_legacy If true, `ChunkVersionLegacy` tags will be included.
#'
#' @rdname ChunkVersion
#' @export
get_chunk_versions <- function(db, x, z, dimension, include_legacy = TRUE) {
    tags <- 44L
    if(isTRUE(include_legacy)) {
        tags <- c(tags, 118L)
    }

    keys <- .process_key_args2(x,z,dimension, tag=tags)
    dat <- get_values(db, keys)
    
    # remove any legacy tags if the current tag is not null
    legkey <- stringr::str_subset(keys, ":118$")
    k <- stringr::str_replace(legkey, ":118$", ":44")
    b <- !purrr::map_lgl(dat[k], is.null)
    dat[legkey[b]] <- NULL
    # remove any current tags that are null if the legacy tag is not null
    curkey <- stringr::str_subset(keys, ":44$")
    b <- purrr::map_lgl(dat[curkey], is.null)
    nullkey <- curkey[b]
    k <- stringr::str_replace(nullkey, ":44$", ":118")
    b <- !purrr::map_lgl(dat[k], is.null)
    dat[nullkey[b]] <- NULL

    read_chunk_version_data(dat)
}

#' @description
#' `read_chunk_version_data()` parses a list of raw scalars of chunk version data.
#'
#' @param object For `read_chunk_version_data`, this is a list of raw values.
#'               For `write_chunk_version_data`, this is a list of integers.
#'
#' @rdname ChunkVersion
#' @export
read_chunk_version_data <- function(object) {
    purrr::map(object, read_chunk_version_value);
}

#' @description
#' `read_chunk_version_value()` parses a raw scalar of chunk version data.
#'
#' @param rawdata A scalar raw.
#'
#' @rdname ChunkVersion
#' @export
read_chunk_version_value <- function(rawdata) {
    stopifnot(rlang::is_scalar_raw(rawdata))

    as.integer(rawdata)
}

#' @description
#' `write_chunk_version_data()` converts a list of chunk versions into binary form.
#'
#' @rdname ChunkVersion
#' @export
write_chunk_version_data <- function(object) {
    purrr::map(object, write_chunk_version_value);
}

#' @description
#' `write_chunk_version_value()` converts a chunk version to binary form.
#'
#' @param num A scalar integer.
#'
#' @rdname ChunkVersion
#' @export
write_chunk_version_value <- function(num) {
    stopifnot(rlang::is_scalar_integerish(num))

    as.raw(num)
}

