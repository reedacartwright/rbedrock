#' Read and write chunk version data
#'
#' Version data (tag 44) and LegacyVersion data (tag 118)
#' store the version number of a chunk. In Minecraft version 1.16.100,
#' chunk version data was moved from tag 118 to tag 44.
#'
#' `get_chunk_version_data()` and `get_chunk_version_value()` load Version
#' data from `db`. `get_chunk_version_value()` supports loading
#' only a single value.
#'
#' `put_chunk_version_data()` and
#' `put_chunk_version_value()` store Version data into a `bedrockdb`.
#'
#' `read_chunk_version_value()` decodes Version data and
#' `write_chunk_version_value()` encodes Version data.
#' data from `db`. `get_chunk_version_data()` will silently drop and keys not
#' representing Version data. `get_chunk_version_value()` supports loading
#' only a single value. `get_chunk_version_values()` is a synonym for
#' `get_chunk_version_data()`.
#'
#' @param db A bedrockdb object.
#' @param x,z,dimension Chunk coordinates to extract version data from.
#'    `x` can also be a character vector of db keys.
#' @param values An integer vector. If `x` is missing, the names of `values`
#' will be taken as the keys.
#' @param value A scalar integer vector.
#' @param rawdata A scalar raw.
#' @param num A scalar integer.
#'
#'
#' @name ChunkVersion
NULL

#' @rdname ChunkVersion
#' @export
get_chunk_version_data <- function(x, z, dimension, db) {
    dat <- .get_chunk_data(x, z, dimension, db, tag = 44L)
    keys <- names(dat)
    # if any results are null, fallback to reading legacy key
    null_val <- purrr::map_lgl(dat, is.null)
    legkeys <- str_replace(keys[null_val], ":44$", ":118")
    dat[null_val] <- get_data(legkeys, db = db)

    # read versions
    purrr::map_int(dat, read_chunk_version_value)
}

#' @rdname ChunkVersion
#' @export
get_chunk_version_value <- function(x, z, dimension, db) {
    key <- .process_chunk_key_args(x, z, dimension, tag = 44L,
        assert_validity = TRUE, assert_scalar = TRUE)
    val <- get_value(key, db)
    if (is.null(val)) {
        key <- str_replace(key, ":44$", ":118")
        val <- get_value(key, db = db)
    }
    read_chunk_version_value(val)
}

#' @rdname ChunkVersion
#' @export
put_chunk_version_data <- function(values, x, z, dimension, db) {
    keys <- .process_chunk_key_args(x, z, dimension, tag = 44L,
        values = values, assert_validity = TRUE)
    values <- vec_recycle(values, length(keys), x_arg = "values")
    values <- purrr::map(values, write_chunk_version_value)
    put_data(values, keys, db = db)
}

#' @rdname ChunkVersion
#' @export
put_chunk_version_value <- function(value, x, z, dimension, db) {
    key <- .process_chunk_key_args(x, z, dimension, tag = 44L,
        assert_validity = TRUE, assert_scalar = TRUE)
    value <- write_chunk_version_value(value)
    put_value(value, key, db = db)
}

#' @rdname ChunkVersion
#' @export
read_chunk_version_value <- function(rawdata) {
    if (is.null(rawdata)) {
        return(NULL)
    }
    vec_assert(rawdata, raw())
    as.integer(rawdata)
}

#' @rdname ChunkVersion
#' @export
write_chunk_version_value <- function(num) {
    stopifnot(is_scalar_integerish(num))
    as.raw(num)
}
