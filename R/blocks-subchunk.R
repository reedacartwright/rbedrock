#' Load and store SubChunkBlocks data
#'
#' SubChunkBlocks data (tag 47) stores information about the blocks in a world.
#' Each chunk is divided into multiple 16x16x16 subchunks, and the blocks for
#' each subchunk are stored separately. Blocks are stored per subchunk in a
#' palette-based format. Block storage is separated into multiple layers where
#' each layer has its own block palette and block ids. In practices subchunks
#' have either one or two layers, and the extra layer is most-often used to
#' store water for water-logged blocks.
#'
#' The format description can be found at
#' <https://gist.github.com/Tomcc/a96af509e275b1af483b25c543cfbf37>.
#'
#' * `get_subchunk_blocks_value()` and `get_subchunk_blocks_data()` load
#'   SubChunkBlocks data from `db`. `get_subchunk_blocks_value()` loads data
#'   for a single subchunk, and `get_subchunk_blocks_data()` loads data for
#'   multiple subchunks.
#' * `put_subchunk_blocks_value()` and `put_subchunk_blocks_data()` store
#'   SubChunkBlocks data into `db`.
#' * `write_subchunk_blocks_value()` encodes SubChunkBlocks data into a raw
#'   vector. `read_subchunk_blocks_value()` decodes binary SubChunkBlocks data.
#' * `subchunk_blocks_value_as_array()` converts SubChunkBlocks data into a
#'   character array.
#' * `subchunk_origins()` returns a matrix containing the block coordinate of
#'   the lower NW corner of subchunk keys.
#' * `subchunk_coords()` determines the block coordinates of blocks based on
#'   their array indexes and their subchunk origins.
#'
#' @inheritParams ChunkData
#' @param subchunk Subchunk indexes
#' @param value A SubChunkBlocks data value
#' @param values A (named) list of SubChunkBlocks data values. If `x` is
#'   missing, the names of `values` will be taken as the keys.
#' @param keys A character vector of keys.
#' @param version Which format of subchunk data to use
#' @param names_only  A logical scalar. Return only the names of the blocks,
#' ignoring block states.
#' @param extra_block A logical scalar. Append the extra block layer to the
#' output (separated by ";"). This is mostly useful if you have waterlogged
#' blocks. If the extra block is air, it will not be appended.
#' @param r A character array
#' @param rawvalue A raw vector
#' @param subchunk_position Optional, an integer. When reading a value, it will
#' be used if the value's position attribute is missing. When writing a value,
#' it will be used in place of the value's position attribute.
#' @param ind Numeric vector or a named list of numeric vectors containing
#'            indexes for blocks in a subchunk.
#' @param origins A matrix of subchunk origins.
#'
#' @return `get_subchunk_blocks_value()` returns a SubChunkBlocks data value.
#' `get_biomes_data()` returns a named list of SubChunkBlocks data values.
#'
#' @name SubChunkBlocks
NULL

#' @rdname SubChunkBlocks
#' @export
get_subchunk_blocks_value <- function(x, z, dimension, subchunk,
                                      db = default_db()) {
    key <- process_chunk_key_args(x, z, dimension, 47L, subchunk)
    subtag <- get_subtag_from_chunk_key(key)
    b <- check_chunk_key_args(key, 47L)
    b[-1] <- FALSE
    value <- get_value(key[b], db = db)
    read_subchunk_blocks_value(value[b], subtag[b])
}

#' @rdname SubChunkBlocks
#' @export
get_subchunk_blocks_data <- function(x, z, dimension, subchunk,
                                     db = default_db()) {
    keys <- process_chunk_key_args(x, z, dimension, 47L, subchunk)
    subtags <- get_subtag_from_chunk_key(keys)
    b <- check_chunk_key_args(keys, 47L)
    ret <- vector("list", length(keys))
    names(ret) <- keys
    values <- get_data(keys[b], db = db)
    ret[b] <- mapply(read_subchunk_blocks_value, values, subtags[b],
                     SIMPLIFY = FALSE)
    ret
}

#' @rdname SubChunkBlocks
#' @export
put_subchunk_blocks_value <- function(value, x, z, dimension, subchunk,
                                      db = default_db(),
                                      version = 9L) {
    key <- process_chunk_key_args(x, z, dimension, 47L, subchunk)
    subtag <- get_subtag_from_chunk_key(key)
    b <- check_chunk_key_args(key, 47L)
    b[-1] <- FALSE
    if (length(b) > 0 && b[1]) {
        # if multiple keys were passed, assume multiple values were passed too
        if (length(b) > 1) {
            value <- value[[1]]
            key <- key[[1]]
            subtag <- subtag[[1]]
        }
        value <- write_subchunk_blocks_value(value, subchunk_position = subtag,
                                             version = version)
        put_value(value, key, db = db)
    }
    invisible(b)
}

#' @rdname SubChunkBlocks
#' @export
put_subchunk_blocks_data <- function(values, x, z, dimension, subchunk,
                                     db = default_db(),
                                     version = 9L) {
    version <- rep(version, length.out = length(values))

    keys <- process_chunk_key_args(x, z, dimension, 47L, subchunk,
                                   values = values)
    subtags <- get_subtag_from_chunk_key(keys)
    b <- check_chunk_key_args(keys, 47L)
    values <- mapply(write_subchunk_blocks_value, values[b], subtags[b],
                     version[b], SIMPLIFY = FALSE)

    put_data(values, keys[b], db = db)
    invisible(b)
}

#' @rdname SubChunkBlocks
#' @useDynLib rbedrock R_read_subchunk_blocks
#' @export
read_subchunk_blocks_value <- function(rawvalue,
                                       subchunk_position = NA_integer_) {
    if (is.null(rawvalue)) {
        return(NULL)
    }
    stopifnot(is.raw(rawvalue))
    x <- .Call(R_read_subchunk_blocks, rawvalue)
    for (i in seq_along(x)) {
        x[[i]]$values <- aperm(x[[i]]$values, c(1, 3, 2))
        x[[i]]$palette <- from_rnbt(x[[i]]$palette)
    }
    o <- attr(x, "subchunk_position", exact = TRUE)
    if (is.null(o) || is.na(o)) {
        attr(x, "subchunk_position") <- subchunk_position
    }
    x
}

#' @rdname SubChunkBlocks
#' @useDynLib rbedrock R_write_subchunk_blocks
#' @export
write_subchunk_blocks_value <- function(value, subchunk_position,
                                        version = 9L) {
    if (is.null(value)) {
        return(NULL)
    }
    if (!is.list(value)) {
        stop("subchunk blocks value must be a list.")
    }
    version <- as.integer(version)
    if (version != 8L && version != 9L) {
        stop(sprintf("subchunk blocks `version` = %d not supported.", version))
    }
    if (version >= 9L) {
        # identify subchunk
        if (missing(subchunk_position)) {
            subchunk_position <- attr(value, "subchunk_position")
        }
        subchunk_position <- as.integer(subchunk_position)
        if (length(subchunk_position) != 1 || is.na(subchunk_position)) {
            stop("subchunk block format 9 requires a valid subchunk position.")
        }
    }
    values <- lapply(value, function(x) {
        x <- as.integer(x[["values", exact = TRUE]])
        if (length(x) != 16 * 16 * 16) {
            stop("subchunk block value is malformed.")
        }
        dim(x) <- c(16, 16, 16)
        aperm(x, c(1, 3, 2))
    })
    palettes <- lapply(value, function(x) {
        x <- x[["palette", exact = TRUE]]
        if (is.null(x) || !all_nbt_values(x)) {
            stop("subchunk block value is malformed.")
        }
        to_rnbt(x)
    })

    .Call(R_write_subchunk_blocks, values, palettes, version, subchunk_position)
}

#' @rdname SubChunkBlocks
#' @export
subchunk_blocks_value_as_array <- function(value, names_only = FALSE,
                                           extra_block = !names_only) {
    if (is.null(value)) {
        return(array("minecraft:air", c(16L, 16L, 16L)))
    }
    pal_str <- lapply(value, function(x) {
        blocks_str(x[["palette", exact = TRUE]], names_only = names_only)
    })
    val1 <- value[[1]][["values", exact = TRUE]]
    blocks <- pal_str[[1]][val1]
    dim(blocks) <- dim(val1)
    if (isTRUE(extra_block) && length(value) >= 2) {
        val2 <- value[[2]][["values", exact = TRUE]]
        blocks2 <- paste0(";", pal_str[[2]][val2])
        blocks2[blocks2 == ";minecraft:air"] <- ""
        blocks[] <- paste0(blocks, blocks2)
    }
    attr(blocks, "subchunk_position") <- attr(value, "subchunk_position",
                                              exact = TRUE)
    blocks
}

#' @rdname SubChunkBlocks
#' @export
subchunk_blocks_array_as_value <- function(r) {
    stopifnot(is.character(r))
    s <- strsplit(r, ";", fixed = TRUE)
    # main block
    a <- vapply(s, `[[`, character(1L), 1L)
    # extra block
    b <- vapply(s, character(1L), FUN = function(x) {
        if (!is.na(x[2])) x[[2]] else "minecraft:air"
    })
    u1 <- unique(a)
    v1 <- match(a, u1)
    dim(v1) <- c(16L, 16L, 16L)
    ret <- list()
    ret[[1]] <- list(values = v1, palette = blocks_nbt(u1))
    if (any(b != "minecraft:air")) {
        u2 <- unique(b)
        v2 <- match(b, u2)
        dim(v2) <- c(16L, 16L, 16L)
        ret[[2]] <- list(values = v2, palette = blocks_nbt(u2))
    }
    attr(ret, "subchunk_position") <- attr(r, "subchunk_position", exact = TRUE)
    ret
}

#' @rdname SubChunkBlocks
#' @export
subchunk_origins <- function(keys) {
    pos <- extract_chunk_key_components(keys, which = c(1, 5, 2))
    pos * 16L
}

#' @rdname SubChunkBlocks
#' @export
subchunk_coords <- function(ind, origins = subchunk_origins(names(ind))) {
    f <- function(x, y) {
        t(as.vector(y) - 1L + t(arrayInd(x, c(16, 16, 16))))
    }
    if (is.list(ind)) {
        lapply(seq_along(ind), function(j) {
            f(ind[[j]], origins[j, , drop = FALSE])
        })
    } else {
        f(ind, origins)
    }
}
