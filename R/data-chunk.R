#' Load and store raw chunk data
#'
#' * `get_chunk_value()` and `get_chunk_data()` load raw chunk data
#' from `db`. `get_chunk_value()` loads data for a single chunk,
#' and `get_chunk_data()` loads data for multiple chunks.
#' * `put_chunk_value()` and `put_chunk_data()` store chunk
#' data for one or multiple chunks into `db`.
#'
#' @param db A bedrockdb object.
#' @param x,z,dimension Chunk coordinates to extract data from.
#'    `x` can also be a character vector of db keys.
#' @param value A raw vector.
#' @param values A list of raw vectors.

#' @return `get_chunk_value()` returns a single raw vector.
#' `get_chunk_data()` returns a named list of raw data.
#'
#' @name ChunkData
#' @keywords internal
NULL

#' @rdname ChunkData
get_chunk_value <- function(x, z, dimension, tag, subtag, db) {
  key <- process_chunk_key_args(x, z, dimension, tag, subtag)
  b <- check_chunk_key_args(key, tag)
  b[-1] <- FALSE
  get_value(key[b], db = db)
}

#' @rdname ChunkData
get_chunk_data <- function(x, z, dimension, tag, subtag, db) {
  keys <- process_chunk_key_args(x, z, dimension, tag, subtag)
  b <- check_chunk_key_args(keys, tag)
  ret <- vector("list", length(keys))
  names(ret) <- keys
  ret[b] <- get_data(keys[b], db = db)
  ret
}

#' @rdname ChunkData
put_chunk_value <- function(value, x, z, dimension, tag, subtag, db) {
  key <- process_chunk_key_args(x, z, dimension, tag, subtag)
  b <- check_chunk_key_args(key, tag)
  b[-1] <- FALSE
  if (length(b) > 0 && b[1]) {
    # if multiple keys were passed, assume multiple values were passed too
    if (length(b) > 1) {
      value <- value[[1]]
      key <- key[[1]]
    }
    put_value(value, key, db = db)
  }
  invisible(b)
}

#' @rdname ChunkData
put_chunk_data <- function(values, x, z, dimension, tag, subtag, db) {
  keys <- process_chunk_key_args(x, z, dimension, tag, subtag, values = values)
  b <- check_chunk_key_args(keys, tag)
  put_data(values[b], keys[b], db = db)
  invisible(b)
}

process_chunk_key_args <- function(
  x,
  z,
  dimension,
  tag,
  subtag,
  values = NULL
) {
  if (missing(x) && !is.null(names(values))) {
    # if x is missing use names from values
    x <- names(values)
  } else if (!missing(z)) {
    # if z is not missing, create keys from x, z, and dimension
    x <- create_chunk_keys(x, z, dimension, tag, subtag)
  }
  x
}

check_chunk_key_args <- function(keys, tag) {
  if (!missing(tag)) {
    tag <- tag[[1]]
    if (!is.na(tag)) {
      ktag <- c(extract_chunk_key_components(keys, 4))
      return(!is.na(ktag) & ktag == tag)
    }
  }
  rep(TRUE, length(keys))
}

#' Load and store NBT chunk data
#'
#' * `get_chunk_nbt_value()` and `get_chunk_nbt_data()` load NBT data for a
#' chunk from `db`. `get_chunk_nbt_value()` loads NBT data for a single chunk,
#' and `get_chunk_nbt_data()` loads data for multiple chunks.
#' * `put_chunk_nbt_value()` and `put_chunk_nbt_data()` store NBT data for
#' one or multiple chunks into `db`.
#'
#' @inheritParams ChunkData
#' @param value A list of NBT objects.
#' @param values A (named) list of list of NBT objects.
#'
#' @return `get_chunk_nbt_value()` returns a list of NBT objects.
#' `get_chunk_nbt_data()` returns a named list of lists of NBT objects.
#'
#' @name ChunkNBTData
#' @keywords internal
NULL

#' @rdname ChunkNBTData
get_chunk_nbt_data <- function(x, z, dimension, tag, subtag, db) {
  dat <- get_chunk_data(x, z, dimension, tag, subtag, db)
  read_nbt_data(dat, simplify = FALSE)
}

#' @rdname ChunkNBTData
get_chunk_nbt_value <- function(x, z, dimension, tag, subtag, db) {
  val <- get_chunk_value(x, z, dimension, tag, subtag, db)
  read_nbt(val, simplify = FALSE)
}

#' @rdname ChunkNBTData
put_chunk_nbt_data <- function(values, x, z, dimension, tag, subtag, db) {
  values <- write_nbt_data(values)
  put_chunk_data(values, x, z, dimension, tag, subtag, db)
}

#' @rdname ChunkNBTData
put_chunk_nbt_value <- function(value, x, z, dimension, tag, subtag, db) {
  value <- write_nbt(value)
  put_chunk_value(value, x, z, dimension, tag, subtag, db)
}
