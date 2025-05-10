#' Load and store Checksums data
#'
#' @description
#' `r lifecycle::badge("superseded")`
#'
#' Checksums data (tag 59) holds checksums for several chunk records.
#' These records are 2DMaps (tag 45), SubchunkBlocks (tag 47),
#' BlockEntities (tag 49), and Entities (tag 50). Checksums are no longer
#' used by the game.
#'
#' * `get_checksums_value()` and `get_checksums_data()` load Checksums
#' data from `db`. `get_checksums_value()` loads data for a single chunk,
#' and `get_checksums_data()` loads data for multiple chunks.
#' * `update_checksums_value()` and `updates_checksums_data()`
#' recalculate Checksums data from `db` and update `db`.
#' * `write_checksums_value()` encodes Checksums data into a raw vector.
#' `read_checksums_value()` decodes binary Checksums data.
#'
#' @inheritParams ChunkData
#' @param value A Checksums value.
#' @param rawvalue A raw vector.
#' @param key A key prefix.
#'
#' @return `get_checksums_value()` returns a Checksums value.
#' `get_checksums_data()` returns a named list of Checksums values.
#' Checksums values are named character vectors.
#'
#' @keywords internal
#' @name Checksums
NULL

#' @rdname Checksums
#' @export
get_checksums_value <- function(x, z, dimension, db = default_db()) {
    key <- process_chunk_key_args(x, z, dimension, tag = 59L)
    b <- check_chunk_key_args(key, tag = 59L)
    b[-1] <- FALSE
    val <- get_value(key[b], db = db)
    read_checksums_value(val, key[b])
}

#' @rdname Checksums
#' @export
get_checksums_data <- function(x, z, dimension, db = default_db()) {
    dat <- get_chunk_data(x, z, dimension, tag = 59L, db = db)
    mapply(read_checksums_value, dat, names(dat), SIMPLIFY = FALSE)
}

#' @rdname Checksums
#' @export
update_checksums_value <- function(x, z, dimension, db = default_db()) {
    key <- process_chunk_key_args(x, z, dimension, tag = 59L)
    b <- check_chunk_key_args(key, tag = 59L)
    b[-1] <- FALSE
    update_checksums_value_impl(key[b], db = db)
}

#' @rdname Checksums
#' @export
update_checksums_data <- function(x, z, dimension, db = default_db()) {
    keys <- process_chunk_key_args(x, z, dimension, tag = 59L)
    b <- check_chunk_key_args(keys, tag = 59L)
    lapply(keys[b], update_checksums_value_impl, db = db)
    invisible(b)
}

#' @rdname Checksums
#' @export
read_checksums_value <- function(rawvalue, key) {
    stem <- get_stem_from_chunk_key(key)
    if (is.null(rawvalue)) {
        return(NULL)
    }
    sz <- readBin(rawvalue, integer(), n = 1L, size = 4L, endian = "little")
    stopifnot(is.raw(rawvalue) && length(rawvalue) == sz * 11 + 4)
    if (sz == 0) {
        return(character())
    }
    rawdata <- split(rawvalue[-c(1:4)], rep(seq_len(sz), each = 11))

    ret <- character()
    for (i in seq_along(rawdata)) {
        x <- rawdata[[i]]
        tag <- readBin(x, integer(), n = 1L, size = 2L, endian = "little")
        subtag <- as.integer(x[3])
        hash <- x[4:11]
        hash <- paste0(rev(as.character(hash)), collapse = "")
        k <- paste(stem, tag, sep = ":")
        if (tag == 47L) {
            k <- paste(k, subtag, sep = ":")
        }
        ret[[k]] <- hash
    }
    ret
}

#' @rdname Checksums
#' @export
write_checksums_value <- function(value) {
    n <- names(value)
    m <- extract_chunk_key_components(n, 4:5)
    tag <- m[, 1]
    subtag <- m[, 2]
    subtag[is.na(subtag)] <- 0L

    ret <- writeBin(length(value), raw(), size = 4L, endian = "little")
    for (i in seq_along(value)) {
        rtag <- writeBin(tag[i], raw(), size = 2L, endian = "little")
        ret <- c(ret, rtag, as.raw(subtag[i]),
                 checksum_impl_raw(value[[i]]))
    }
    ret
}

update_checksums_value_impl <- function(key, db) {
    # 45, 47, 49, 50 all need to be updated if they exist
    stem <- get_stem_from_chunk_key(key)
    chunk_keys <- get_keys(key_prefix(stem), db = db)
    tags <- get_tag_from_chunk_key(chunk_keys)
    chunk_keys <- chunk_keys[tags %in% c(45, 47, 49, 50)]

    dat <- get_data(chunk_keys, db = db)
    obj <- lapply(dat, checksum_impl)
    val <- write_checksums_value(obj)
    put_chunk_value(val, key, db = db)
}

checksum_impl_raw <- function(x) {
    if (is.raw(x)) {
        return(x)
    }
    x <- strsplit(x, character(0L))[[1]]
    x <- paste0(x[c(TRUE, FALSE)], x[c(FALSE, TRUE)])
    x <- strtoi(x, base = 16L)
    as.raw(rev(x))
}

checksum_impl <- function(x) {
    # this matches the algorithm used in BDS
    y <- digest::digest(x, algo = "xxhash64", serialize = FALSE, raw = TRUE)
    checksum_impl_raw(y)
}
