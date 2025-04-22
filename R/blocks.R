#' Read and write Blocks data
#'
#' Blocks data stores information about blocks in a world and their properties.
#' Blocks data is stored per-subchunk as [SubChunkBlocks] data. These functions
#' are wrappers around a SubChunkBlocks functions to make it easy to save and
#' load blocks for an entire chunk.
#'
#' * `get_blocks_value()` and `get_blocks_data()` load Blocks
#' data from `db`. `get_blocks_value()` loads data for a single chunk,
#' and `get_blocks_data()` loads data for multiple chunks.
#' * `put_blocks_value()` and `put_blocks_data()` store Blocks
#' data into `db`.
#'
#' @seealso SubChunkBlocks
#'
#' @inheritParams ChunkData
#' @param value A 16x384x16 character array.
#' @param values A (named) list of Blocks values. If `x` is missing, the names
#' of `values` will be taken as the keys.
#' @param names_only  A logical scalar. Return only the names of the blocks,
#' ignoring block states.
#' @param extra_block A logical scalar. Append the extra block layer to the
#' output (separated by ";"). This is mostly useful if you have waterlogged
#' blocks. If the extra block is air, it will not be appended.
#'
#' @return `get_blocks_value()` returns a Blocks value. `get_blocks_data()`
#' returns a named list of Blocks values.
#'
#' @name Blocks
NULL

#' @rdname Blocks
#' @export
get_blocks_value <- function(x, z, dimension, db = default_db(),
                             names_only = FALSE,
                             extra_block = !names_only) {
    key <- process_chunk_key_args(x, z, dimension, 47L)
    b <- check_chunk_key_args(key, 47L)
    b[-1] <- FALSE
    get_blocks_value_impl(key[b], db = db, names_only = names_only,
                          extra_block = extra_block)
}

#' @rdname Blocks
#' @export
get_blocks_data <- function(x, z, dimension, db = default_db(),
                            names_only = FALSE,
                            extra_block = !names_only) {
    keys <- process_chunk_key_args(x, z, dimension, 47L)
    b <- check_chunk_key_args(keys, 47L)
    ret <- vector("list", length(keys))
    names(ret) <- keys
    ret[b] <- lapply(keys[b], get_blocks_value_impl, db = db,
                     names_only = names_only,
                     extra_block = extra_block)
    ret
}

#' @rdname Blocks
#' @export
put_blocks_value <- function(value, x, z, dimension, db = default_db()) {
    key <- process_chunk_key_args(x, z, dimension, 47L)
    b <- check_chunk_key_args(key, 47L)
    b[-1] <- FALSE
    if (length(b) > 0 && b[1]) {
        # if multiple keys were passed, assume multiple values were passed too
        if (length(b) > 1) {
            value <- value[[1]]
            key <- key[[1]]
        }
        put_blocks_value_impl(value, key, db = db)
    }
    invisible(b)
}

#' @rdname Blocks
#' @export
put_blocks_data <- function(values, x, z, dimension, db = default_db()) {
    keys <- process_chunk_key_args(x, z, dimension, 47L,
                                   values = values)
    b <- check_chunk_key_args(keys, 47L)
    mapply(values[b], keys[b], FUN = put_blocks_value_impl,
           MoreArgs = list(db = db), SIMPLIFY = FALSE)
    invisible(b)
}

get_blocks_value_impl <- function(prefix, db, names_only, extra_block) {
    if (length(prefix) != 1L) {
        return(NULL)
    }
    prefix <- get_stem_from_chunk_key(prefix)
    prefix <- paste0(prefix, ":47")
    p <- split_chunk_stems(prefix)
    dimension <- p[3]

    dat <- get_data(key_prefix(prefix), db = db)
    if (length(dat) == 0) {
        return(NULL)
    }
    dat <- lapply(dat, read_subchunk_blocks_value)
    dat <- lapply(dat, subchunk_blocks_value_as_array,
                  names_only = names_only, extra_block = extra_block)

    # adjust subtag
    pos <- get_subtag_from_chunk_key(names(dat)) + 4 * (dimension == 0)

    # Allocate array
    mat <- array("minecraft:air", c(16, 384, 16))

    # Copy Data
    for (i in seq_along(dat)) {
        if (pos[i] >= 0 && pos[i] < 24) {
            mat[, 16 * pos[i] + 1:16, ] <- dat[[i]]
        }
    }
    attr(mat, "origin") <- c(p[1], -4 * (dimension == 0), p[2]) * 16L
    mat
}

put_blocks_value_impl <- function(value, prefix, db) {
    if (length(prefix) != 1L) {
        return(FALSE)
    }
    prefix <- get_stem_from_chunk_key(prefix)
    prefix <- paste0(prefix, ":47")
    p <- split_chunk_stems(prefix)
    dimension <- p[3]

    # identify bottom location of value
    ymin <- chunk_origin(value)[2] %||% (-64L * (dimension == 0))

    if (ymin %% 16L != 0 || length(value) %% 4096 != 0) {
        stop("Invalid Blocks value. Value must be evenly divisible into subchunks.") # nolint
    }

    # identify subchunk positions and filter based on dimension
    # y ranges
    # - overworld: [-64, 320)
    # - nether: [0, 128)
    # - the end: [0, 256)
    offset <- ymin %/% 16
    pos <- seq(offset, length.out = length(value) / 4096)
    if (dimension == 0) {
        pos <- pos[pos >= -4 & pos < 20]
    } else if (dimension == 1) {
        pos <- pos[pos >= 0 & pos < 8]
    } else if (dimension == 2) {
        pos <- pos[pos >= 0 & pos < 16]
    }

    # union of old keys and new keys for this chunk
    old_keys <- get_keys(key_prefix(prefix), db = db)
    new_keys <- paste(prefix, pos, sep = ":")
    keys <- union(new_keys, old_keys)
    # build new data
    data <- vector("list", length(keys))
    names(data) <- keys
    for (i in seq_along(pos)) {
        y <- pos[[i]] - offset
        subchunk <- value[, y * 16L + (1:16), ]
        # skip empty chunks
        if (all(subchunk == "minecraft:air")) {
            next
        }
        data[[new_keys[i]]] <- subchunk_blocks_array_as_value(subchunk)
    }
    # identify NULL blocks to delete
    empty <- vapply(data, is.null, logical(1L))
    empty_keys <- names(empty)[empty]

    # encode data
    values <- mapply(write_subchunk_blocks_value, data[!empty], pos[!empty],
                     SIMPLIFY = FALSE)

    batch <- db$writebatch()
    batch$mdelete(chrkeys_to_rawkeys(empty_keys))
    batch$mput(chrkeys_to_rawkeys(names(values)), values)
    batch$write()
    # clean up
    batch$destroy()

    invisible(TRUE)
}

#' Get or set the coordinates of the origin of a chunk
#'
#' @param x an array of block data
#' @param value an integer vector
#' @export
chunk_origin <- function(x) {
    attr(x, "origin", exact = TRUE)
}

#' @export
#' @rdname chunk_origin
`chunk_origin<-` <- function(x, value) {
    attr(x, "origin") <- value
    x
}

#' Locate the coordinates of blocks in a chunk
#'
#' @param blocks A character array containing block data.
#' @param pattern The pattern to look for. Passed to [base::grep].
#' @param negate  If `TRUE`, return non-matching elements.
#'
#' @examples
#' dbpath <- rbedrock_example_world("example1.mcworld")
#' db <- bedrockdb(dbpath)
#' blocks <- get_blocks_value(db, x=37, z=10, dimension=0)
#' locate_blocks(blocks, "ore")
#' close(db)
#'
#' @export
locate_blocks <- function(blocks, pattern, negate = FALSE) {
    ind <- grep(pattern, blocks, invert = negate)

    aind <- arrayInd(ind, dim(blocks))
    coords <- chunk_origin(blocks) + t(aind) - 1
    if (length(coords) == 0) {
        dim(coords) <- c(3, 0)
    }
    ret <- tibble::tibble(x = coords[1, ],
                          y = coords[2, ],
                          z = coords[3, ],
                          block = as.character(blocks[ind]))
    dplyr::arrange(ret, .data$y, .data$x, .data$z)
}

#' @description
#' `subchunk_origins()` returns a matrix containing the block coordinate of the
#' lower NW corner of subchunk keys
#' @param keys A character vector of database keys.
#' @export
#' @rdname SubChunkBlocks
subchunk_origins <- function(keys) {
    pos <- extract_chunk_key_components(keys, which = c(1, 5, 2))
    pos * 16L
}

#' @description
#' `subchunk_coords()` determines the block coordinates of blocks based on their
#' array indexes and their subchunk origins.
#'
#' @param ind Numeric vector or a named list of numeric vectors containing
#'            indexes for blocks in a subchunk.
#' @param origins A matrix of subchunk origins.
#'
#' @return `subchunk_coords()` returns a 3-column matrix of block coordinates.
#' @export
#' @rdname SubChunkBlocks
subchunk_coords <- function(ind, origins = subchunk_origins(names(ind))) {
    if (is.character(origins)) {
        origins <- subchunk_origins(origins)
    }

    f <- function(x, y) t(y - 1L + t(arrayInd(x, c(16, 16, 16))))
    if (is.list(ind)) {
        o <- purrr::array_tree(origins, 1)
        purrr::map2(ind, o, f)
    } else {
        f(ind, as.vector(origins))
    }
}

#' Extract or replace chunk blocks from an array
#'
#' Convenience wrappers around `[` to extract or replace blocks from an array
#' based on block coordinates.
#'
#' @param x Object from which to extract element(s) or in which to replace
#' element(s).
#' @param drop if `TRUE` the result is coerced to the lowest possible dimension.
#' @param origin the origin of the chunk array, used for mapping coordinates to
#' indices
#' @param ... block coordinates specifying elements to extract or replace. Can
#' be numeric, logical, or missing. If numeric, the coordinates will be mapped
#' to indices unless there is a single, non-matrix argument.
#' @param value An array-like R object of similar class as x
#'
#' @export
chunk_blocks <- function(x, ..., drop = TRUE, origin = chunk_origin(x)) {
    args <- rlang::dots_list(..., .named = NULL,
                             .preserve_empty = TRUE,
                             .ignore_empty = "none")
    args <- chunk_blocks_apply_offsets(args, dim(x), origin)

    # if this fails the error message is gnarly, see rlang::exec docs
    rlang::exec(`[`, x, !!!args, drop = drop)
}

#' @rdname chunk_blocks
#' @export
`chunk_blocks<-` <- function(x, ..., origin = chunk_origin(x), value) {
    args <- rlang::dots_list(..., .named = NULL,
                             .preserve_empty = TRUE,
                             .ignore_empty = "none")
    args <- chunk_blocks_apply_offsets(args, dim(x), origin)

    rlang::exec(`[<-`, x, !!!args, value = value)
}

chunk_blocks_apply_offsets <- function(args, dims, origin) {
    if (length(args) == length(dims)) {
        for (i in seq_along(args)) {
            if (is.numeric(args[[i]])) {
                ii <- args[[i]] - origin[i] + 1L
                if (any(ii < 1L, na.rm = TRUE)) {
                    rlang::abort("subscript out of bounds")
                }
                args[[i]] <- ii
            }
        }
    } else if (length(args) == 1L) {
        if (is.matrix(args[[1]]) && is.numeric(args[[1]])) {
            # adjust indices
            args[[1]] <- sweep(args[[1]], 2, origin) + 1L
        }
    } else if (length(args) != 0L) {
        rlang::abort("incorrect number of dimensions")
    }

    args
}
