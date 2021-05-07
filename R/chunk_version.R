#' Read and write chunk version data
#'
#' ChunkVersion data (tag 44) and ChunkVersionLegacy data (tag 118)
#' store the version number of a chunk. In Minecraft version 1.16.100,
#' chunk version data was moved from tag 118 to tag 44.
#'
#' @name ChunkVersion
NULL

#' @description
#' `get_chunk_version_data()` retrieves chunk versions from a `bedrockdb`.
#' It will silently drop and keys not representing ChunkVersion data.
#'
#' @param db A bedrockdb object.
#' @param x,z,dimension Chunk coordinates to extract version data from.
#'    `x` can also be a character vector of db keys.
#' @param include_legacy If true, `ChunkVersionLegacy` tags will be included.
#'
#' @rdname ChunkVersion
#' @export
get_chunk_version_data <- function(db, x, z, dimension, include_legacy = TRUE) {
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

    purrr::map_int(dat, read_chunk_version_value)
}

#' @rdname ChunkVersion
#' @export
get_chunk_version_value <- function(db, x, z, dimension, include_legacy = TRUE) {
    key <- .process_key_args(x, z, dimension, tag=44L)
    vec_assert(key, character(), 1L)
    val <- get_value(db, key)
    if(is.null(val) && isTRUE(include_legacy)) {
        key <- .process_key_args(x, z, dimension, tag=118L)
        vec_assert(key, character(), 1L)
        val <- get_value(db, key)
    }
    read_chunk_version_value(val)
}

#' @description
#' `put_chunk_version_data()`, `put_chunk_version_values()`, and
#' `put_chunk_version_value()` store Finalization data into a `bedrockdb`.
#' `put_chunk_version_data()` supports writing both ChunkVersion and
#' ChunkVersionLegacy tags.
#' `put_chunk_version_values()` and `put_chunk_version_value()`
#' only support ChunkVersion tags.
#'
#' @param data A named-vector of key-value pairs for Finalization data.
#'
#' @rdname ChunkVersion
#' @export
put_chunk_version_data <- function(db, data) {
    .check_chunk_key_tag(names(data), c(44L,118L))
    dat <- purrr::map(data, write_chunk_version_value)
    put_data(db, dat)
}

#' @param values An integer vector
#'
#' @rdname ChunkVersion
#' @export
put_chunk_version_values <- function(db, x, z, dimension, values) {
    keys <- .process_key_args(x, z, dimension, tag=44L, stop_if_filtered = TRUE)
    values <- vctrs::vec_recycle(values, length(keys), x_arg="values")
    values <- purrr::map(values, write_chunk_version_value)
    put_values(db, keys, values)
}

#' @param value A scalar integer vector
#'
#' @rdname ChunkVersion
#' @export
put_chunk_version_value <- function(db, x, z, dimension, value) {
    key <- .process_key_args(x, z, dimension, tag=44L)
    vec_assert(key, character(), 1L)
    value <- write_chunk_version_value(value)
    put_value(db, key, value)
}

#' @description
#' `read_chunk_version_value()` decodes ChunkVersion data.
#'
#' @param rawdata A scalar raw.
#'
#' @rdname ChunkVersion
#' @export
read_chunk_version_value <- function(rawdata) {
    if(is.null(rawdata)) {
        return(NULL)
    }
    vec_assert(rawdata, raw())
    as.integer(rawdata)
}

#' @description
#' `write_chunk_version_value()` encodes ChunkVersion data.
#'
#' @param num A scalar integer.
#'
#' @rdname ChunkVersion
#' @export
write_chunk_version_value <- function(num) {
    stopifnot(rlang::is_scalar_integerish(num))
    as.raw(num)
}
