#' Read and write chunk version data
#'
#' Version data (tag 44) and LegacyVersion data (tag 118)
#' store the version number of a chunk. In Minecraft version 1.16.100,
#' chunk version data was moved from tag 118 to tag 44.
#'
#' @name ChunkVersion
NULL

#' @description
#' `get_chunk_version_data()` and `get_chunk_version_value()` load Version
#' data from `db`. `get_chunk_version_data()` will silently drop and keys not
#' representing Version data. `get_chunk_version_value()` supports loading
#' only a single value. `get_chunk_version_values()` is a synonym for
#' `get_chunk_version_data()`.
#'
#' @param db A bedrockdb object.
#' @param x,z,dimension Chunk coordinates to extract version data from.
#'    `x` can also be a character vector of db keys.
#'
#' @rdname ChunkVersion
#' @export
get_chunk_version_data <- function(db, x, z, dimension) {
    keys <- .process_chunk_version_key_args(x, z, dimension)
    dat <- get_values(db, keys)
    # if any results are null, fallback to reading legacy key
    null_val <- purrr::map_lgl(dat, is.null)
    legkeys <- keys[null_val] %>% str_replace(":44$", ":118")
    dat[null_val] <- get_values(db, legkeys)

    # read versions
    purrr::map_int(dat, read_chunk_version_value)
}

#' @rdname ChunkVersion
#' @export
get_chunk_version_values <- get_chunk_version_data

#' @rdname ChunkVersion
#' @export
get_chunk_version_value <- function(db, x, z, dimension) {
    key <- .process_chunk_version_key_args(x, z, dimension)
    vec_assert(key, character(), 1L)
    val <- get_value(db, key)
    if (is.null(val)) {
        key <- str_replace(key, ":44$", ":118")
        val <- get_value(db, key)
    }
    read_chunk_version_value(val)
}

.process_chunk_version_key_args <- function(x, z, dimension) { # nolint: object_length_linter
    if (missing(z) && is.character(x)) {
        # replace legacy tags with new
        x <- str_replace(x, ":118$", ":44")
        x <- unique(x)
    }
    .process_key_args(x, z, dimension, tag = 44L)
}

#' @description
#' `put_chunk_version_data()`, `put_chunk_version_values()`, and
#' `put_chunk_version_value()` store Version data into a `bedrockdb`.
#'
#' @param data A named-vector of key-value pairs for Version data.
#'
#' @rdname ChunkVersion
#' @export
put_chunk_version_data <- function(db, data) {
    .check_chunk_key_tag(names(data), 44L)
    dat <- purrr::map(data, write_chunk_version_value)
    put_data(db, dat)
}

#' @param values An integer vector
#'
#' @rdname ChunkVersion
#' @export
put_chunk_version_values <- function(db, x, z, dimension, values) {
    keys <- .process_key_args(x, z, dimension, tag = 44L,
                              stop_if_filtered = TRUE)
    values <- vec_recycle(values, length(keys), x_arg = "values")
    values <- purrr::map(values, write_chunk_version_value)
    put_values(db, keys, values)
}

#' @param value A scalar integer vector
#'
#' @rdname ChunkVersion
#' @export
put_chunk_version_value <- function(db, x, z, dimension, value) {
    key <- .process_key_args(x, z, dimension, tag = 44L)
    vec_assert(key, character(), 1L)
    value <- write_chunk_version_value(value)
    put_value(db, key, value)
}

#' @description
#' `read_chunk_version_value()` decodes Version data.
#'
#' @param rawdata A scalar raw.
#'
#' @rdname ChunkVersion
#' @export
read_chunk_version_value <- function(rawdata) {
    if (is.null(rawdata)) {
        return(NULL)
    }
    vec_assert(rawdata, raw())
    as.integer(rawdata)
}

#' @description
#' `write_chunk_version_value()` encodes Version data.
#'
#' @param num A scalar integer.
#'
#' @rdname ChunkVersion
#' @export
write_chunk_version_value <- function(num) {
    stopifnot(is_scalar_integerish(num))
    as.raw(num)
}
