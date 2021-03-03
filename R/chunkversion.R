#' Read and write chunk version data
#'
#' @description
#' `get_chunk_versions` retrieves chunk versions from a `bedrockdb`.
#'
#' @param db A bedrockdb object.
#' @param x,z,dimension Chunk coordinates to extract version data from.
#'    x can also be a character vector of db keys and any keys not
#'    representing version data will be silently dropped.
#' @param include_legacy If true, `ChunkVersionLegacy` tags will be included.
#' @param object For `read_chunk_version_data`, this is a list of raw values.
#'               For `write_chunk_version_data`, this is a list of integers.
#'
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

    read_version_data(dat)
}

#' @description
#' `read_chunk_version_data` parses chunk version data.
#'
#' @rdname get_chunk_versions
#' @export
read_chunk_version_data <- function(object) {
    .apply_func(object, function(x) {
        stopifnot(is.raw(x))
        as.integer(x)
    })
}

#' @description
#' `write_chunk_version_data` converts chunk version data into binary form.
#'
#' @rdname get_chunk_versions
#' @export
write_chunk_version_data <- function(object) {
    .apply_func(object, function(x) {
        as.raw(x)
    })
}
