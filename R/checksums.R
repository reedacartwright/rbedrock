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
#' `get_checksums_values()` is a synonym for `get_checksums_data()`.
#'
#' @param db A bedrockdb object.
#' @param x,z,dimension Chunk coordinates to extract data from.
#'    `x` can also be a character vector of db keys.
#'
#' @return `get_checksums_data()` returns a named-list of the values returned
#'          by `get_checksums_value()`.
#' @rdname Checksums
#' @export
get_checksums_data <- function(db, x, z, dimension) {
    keys <- .process_key_args(x, z, dimension, tag = 59L)
    dat <- get_values(db, keys)
    purrr::map(dat, read_checksums_value)
}

#' @rdname Checksums
#' @export
get_checksums_values <- get_checksums_data

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
    key <- .process_key_args(x, z, dimension, tag = 59L)
    vec_assert(key, character(), 1)

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
    keys <- .process_key_args(x, z, dimension, tag = 59L,
                              stop_if_filtered = TRUE)
    purrr::map(keys, .update_checksums_value_impl, db = db)
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
    if (is.null(rawdata)) {
        return(NULL)
    }
    sz <- readBin(rawdata, integer(), n = 1L, size = 4L, endian = "little")
    vec_assert(rawdata, raw(), sz * 11 + 4)
    if (sz == 0) {
        return(character())
    }
    rawdata <- rawdata[-c(1:4)] %>% split(rep(seq_len(sz), each = 11))
    ret <- purrr::map(rawdata, function(x) {
        tag <- readBin(x, integer(), n = 1L, size = 2L, endian = "little")
        subtag <- as.integer(x[3])
        hash <- x[4:11]
        hash <- paste0(rev(as.character(hash)), collapse = "")
        k <- ifelse(tag == 47L, paste(tag, subtag, sep = ":"),
                    as.character(tag))
        list(hash, k)
    })
    ret <- purrr::transpose(ret)

    rlang::set_names(purrr::as_vector(ret[[1]]), ret[[2]])
}

#' @description
#' `write_checksums_value()` converts Checksums from a named list into
#' binary format.
#'
#' @param object a named character vector in the same format as returned by
#'        `read_checksums_value()`.
#'
#' @return `write_checksums_value()` returns a raw vector.
#' @rdname Checksums
#' @export
write_checksums_value <- function(object) {
    n <- names(object)
    if (any(.is_chunk_key(n))) {
        m <- .extract_chunk_key_components(n, 4:5)
    } else if (length(n)) {
        m <- str_split(n, fixed(":"), simplify = TRUE)
        mode(m) <- "integer"
    } else {
        m <- array(integer(), c(0, 2))
    }
    tag <- m[, 1]
    subtag <- m[, 2] %|% 0L
    if (any(is.na(tag))) {
        abort("Invalid chunk key passed to write_chunksums_value.")
    }

    hash <- purrr::map(object, function(x) {
        h <- strsplit(x, character(0L))[[1]]
        h <- paste(h[c(TRUE, FALSE)], h[c(FALSE, TRUE)], sep = "")
        as.raw(as.hexmode(rev(h)))
    })

    ret <- writeBin(length(object), raw(), size = 4L, endian = "little")
    retp <- purrr::map(seq_along(object), function(i) {
        c(writeBin(tag[i], raw(), size = 2L, endian = "little"),
            as.raw(subtag[i]), hash[[i]])
    })
    c(ret, purrr::list_c(retp))
}

.update_checksums_value_impl <- function(db, key) {
    # 45, 47, 49, 50 all need to be updated if they exist
    stem <- stringr::str_replace(key, ":59$", "")
    chunk_keys <- get_keys(db, starts_with = stem)
    chunk_keys <- stringr::str_subset(chunk_keys, ":(?:47:[^:]+|45|49|50)$")

    dat <- get_values(db, chunk_keys)
    obj <- purrr::map(dat, .checksum_impl)

    val <- write_checksums_value(obj)
    put_value(db, key, val)
    invisible()
}

.checksum_impl <- function(x) {
    # this matches the algorithm used in BDS
    digest::digest(x, algo = "xxhash64", serialize = FALSE, raw = TRUE)
}
