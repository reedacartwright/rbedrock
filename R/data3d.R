#' Read and write Data3D data
#'
#' Data3D data (tag 43) stores information about surface heights and biomes in a
#' chunk.
#'
#' * `get_data3d_value()` and `get_data3d_data()` load Data3D
#' data from `db`. `get_data3d_value()` loads data for a single chunk,
#' and `get_data3d_data()` loads data for multiple chunks.
#' * `put_data3d_value()` and `put_data3d_data()` store Data3D
#' data into `db`.
#' * `write_data3d_value()` encodes Data3D data into a raw vector.
#' `read_data3d_value()` decodes binary Data3D data.
#'
#' @seealso Data2D
#'
#' @inheritParams ChunkData
#' @param value A Data3D value.
#' @param values A (named) list of Data3D values. If `x` is missing, the names
#' of `values` will be taken as the keys.
#' @param rawvalue A raw vector.
#'
#' @return `get_data3d_value()` returns a Data3D value. `get_data3d_data()`
#' returns a named list of Data3D values. Data3D values are lists containing
#' two elements. The `height_map` element is a 16x16 matrix containing height
#' data. The `biome_map` element is a 16x384x16 array containing biome data.
#'
#' @name Data3D
NULL

#' @rdname Data3D
#' @export
get_data3d_value <- function(x, z, dimension, db = default_db()) {
  value <- get_chunk_value(x, z, dimension, tag = 43L, db = db)
  read_data3d_value(value)
}

#' @rdname Data3D
#' @export
get_data3d_data <- function(x, z, dimension, db = default_db()) {
  dat <- get_chunk_data(x, z, dimension, tag = 43L, db = db)
  lapply(dat, read_data3d_value)
}

#' @rdname Data3D
#' @export
put_data3d_value <- function(value, x, z, dimension, db = default_db()) {
  value <- write_data3d_value(value)
  put_chunk_value(value, x, z, dimension, tag = 43L, db = db)
}

#' @rdname Data3D
#' @export
put_data3d_data <- function(values, x, z, dimension, db = default_db()) {
  values <- lapply(values, write_data3d_value)
  put_chunk_data(values, x, z, dimension, tag = 43L, db = db)
}

#' @rdname Data3D
#' @useDynLib rbedrock R_read_chunk_biomes
#' @export
read_data3d_value <- function(rawvalue) {
  if (is.null(rawvalue)) {
    return(NULL)
  }
  stopifnot(is.raw(rawvalue))
  height_map <- readBin(
    rawvalue[1:512],
    integer(),
    n = 256L,
    size = 2L,
    endian = "little",
    signed = TRUE
  )
  dim(height_map) <- c(16L, 16L)

  b <- .Call(R_read_chunk_biomes, rawvalue[-(1:512)])
  # Validate Biome Data
  if (length(b) == 0 || is.null(b[[1]])) {
    abort("Value does not contain at least one subchunk of biome data.")
  }
  # Enlarge list to length 24 if necessary.
  if (length(b) < 24) {
    b[24] <- list(NULL)
  }
  # Validate Biome Data
  hasdata <- !sapply(b, is.null)
  n <- length(hasdata)
  if (sum(hasdata[-1] != hasdata[-n]) > 1) {
    abort("Value contains empty biome data between valid subchunks.")
  }
  # Trim biome data
  if (n > 24) {
    if (any(hasdata[25:n])) {
      msg <- sprintf("Trimming biome data from %d to 24 subchunks.", length(b))
      warn(msg)
    }
    b <- b[1:24]
    hasdata <- hasdata[1:24]
  }

  # Fill biome array
  biome_map <- array(NA_integer_, c(16, 16, 24 * 16))

  # Subchunks with data
  ii <- which(hasdata)
  for (i in ii) {
    bb <- b[[i]]
    biome_map[, , 16 * (i - 1) + (1:16)] <- bb$palette[bb$values]
  }
  # Subchunks without data copy from the highest y level of
  # subchunks with data
  i <- max(ii)
  if (i < 24) {
    y <- 16 * i
    biome_map[, , (y + 1):(16 * 24)] <- biome_map[, , y]
  }
  # reshape from x,z,y to x,y,z
  biome_map <- aperm(biome_map, c(1, 3, 2))
  list(height_map = height_map, biome_map = biome_map)
}

reshape_biome_map <- function(value) {
  # returns biome_map in x,z,y order
  n <- length(value)
  if (n == 1 || n == 256) {
    array(value, c(16, 16, 16 * 24))
  } else if (n > 0 && n %% 256 == 0) {
    ny <- length(value) %/% 256
    if (ny == 16 * 24) {
      value <- array(value, c(16, ny, 16))
      aperm(value, c(1, 3, 2))
    } else if (ny > 16 * 24) {
      value <- array(value, c(16, ny, 16))
      aperm(value[, , 1:(16 * 24)], c(1, 3, 2))
    } else {
      v <- array(NA_integer_, c(16, 16, 16 * 24))
      v[, , 1:ny] <- aperm(value, c(1, 3, 2))
      v[, , (ny + 1):(16 * 24)] <- v[, , ny]
      v
    }
  } else {
    abort("Invalid biome_map dimensions.")
  }
}

#' @rdname Data3D
#' @useDynLib rbedrock R_write_chunk_biomes
#' @export
write_data3d_value <- function(value) {
  height_map <- value[["height_map", exact = TRUE]]
  biome_map <- value[["biome_map", exact = TRUE]]

  height_map <- as.integer(height_map)
  biome_map <- as.integer(biome_map)

  height_map <- rac_recycle(height_map, 256)
  biome_map <- reshape_biome_map(biome_map)

  # identify y levels with repetitive biomes
  y <- (16 * 24):2
  o <- sapply(y, function(x) any(biome_map[, , x] != biome_map[, , x - 1]))
  m <- match(TRUE, o)
  m <- if (is.na(m)) 1 else y[m]
  # y levels m to 384 are identical.
  # chunks 1:mm need to be written
  mm <- ((m - 1) %/% 16) + 1

  values_list <- rep(list(integer(0L)), 24)
  palette_list <- rep(list(integer(0L)), 24)
  for (i in 1:mm) {
    j <- (1:16) + (i - 1) * 16
    id <- c(biome_map[, , j])
    palette_list[[i]] <- unique(id)
    values_list[[i]] <- match(id, palette_list[[i]])
  }

  h <- writeBin(height_map, raw(), size = 2L, endian = "little")
  b <- .Call(R_write_chunk_biomes, values_list, palette_list)
  c(h, b)
}
