#' Load block data from one or more chunks
#'
#' @description
#' These functions return block data as strings containing the
#' block name and block states. The strings' format is
#' `blockname@@state1=value1@@state2=value2` etc.
#' Blocks may have 0 or more states.
#'
#' @param db A bedrockdb object.
#' @param x,z,dimension Chunk coordinates to extract data from.
#'    `x` can also be a character vector of db keys.
#'
#' @param names_only  A logical scalar. Return only the names of the blocks,
#' ignoring block states.
#' @param extra_block A logical scalar. Append the extra block layer to the
#' output (separated by ";"). This is mostly useful if you have waterlogged
#' blocks. If the extra block is air, it will not be appended.
#' @param min_subchunk,max_subchunk The minimum and maximum subchunks of the
#' returned array. Set to `TRUE` to use dimension defaults. Set to `FALSE` to
#' use the values from chunk data.
#'
#' @return `get_chunk_blocks_data()` returns a list of the of the values
#' returned by `read_chunk_blocks_value()`.
#' @export
get_chunk_blocks_data <- function(db, x, z, dimension,
                                  names_only = FALSE,
                                  extra_block = !names_only,
                                  min_subchunk = TRUE,
                                  max_subchunk = !names_only) {
    starts_with <- .process_key_args_prefix(x, z, dimension)
    starts_with <- vec_unique(starts_with)

    dat <- purrr::map(starts_with, function(x) {
        .get_chunk_blocks_value_impl(db,
                                     prefix = x,
                                     names_only = names_only,
                                     extra_block = extra_block,
                                     min_subchunk = min_subchunk,
                                     max_subchunk = max_subchunk)
    })

    set_names(dat, starts_with)
}

#' @description
#' `get_chunk_blocks_value()` is an alias for `get_chunk_blocks_data()`
#' @export
#' @rdname get_chunk_blocks_data
get_chunk_blocks_values <- get_chunk_blocks_data

#' @description
#' `get_chunk_blocks_value()` loads block data from a `bedrockdb`.
#' It only supports loading a single value.
#'
#' @return `get_chunk_blocks_value()`
#' return a 16xNx16 character array. The axes represent the `x`, `y`, and `z`
#' dimensions in that order. The size of the y-axis is based on the highest
#' subchunk in the coordinate. Missing subchunks are considered air.
#'
#' @rdname get_chunk_blocks_data
#' @export
get_chunk_blocks_value <- function(db, x, z, dimension,
                                   names_only = FALSE,
                                   extra_block = !names_only,
                                   min_subchunk = TRUE,
                                   max_subchunk = !names_only) {
    starts_with <- .process_key_args_prefix(x, z, dimension)
    starts_with <- vec_unique(starts_with)

    .get_chunk_blocks_value_impl(db, prefix = starts_with,
        names_only = names_only,
        extra_block = extra_block,
        min_subchunk = min_subchunk,
        max_subchunk = max_subchunk
    )
}

#' @description
#' `put_chunk_blocks_data()`, `put_chunk_blocks_values()`, and
#' `put_chunk_blocks_value()` stores block data into a `bedrockdb`.
#'
#' @param data A named list of 16xNx16 character() arrays
#' @param version Which format of subchunk data to use
#'
#' @rdname get_chunk_blocks_data
#' @export
put_chunk_blocks_data <- function(db, data, version = 9L) {
    put_chunk_blocks_values(db, names(data), values =  data, version = version)
}

#' @param values A list of 16xNx16 character() arrays
#'
#' @rdname get_chunk_blocks_data
#' @export
put_chunk_blocks_values <- function(db, x, z, dimension, values,
                                    version = 9L) {
    keys <- .process_key_args_prefix(x, z, dimension)
    values <- vec_recycle(values, length(keys), x_arg = "values")
    f <- function(x, y) {
        .put_chunk_blocks_value_impl(db, x, y, version = version)
    }
    purrr::walk2(keys, values, f)
}

#' @param value A 16xNx16 character array
#'
#' @rdname get_chunk_blocks_data
#' @export
put_chunk_blocks_value <- function(db, x, z, dimension, value, version = 9L) {
    key <- .process_key_args_prefix(x, z, dimension)
    vec_assert(key, character(), 1L)
    .put_chunk_blocks_value_impl(db, key, value, version = version)
}

.get_chunk_blocks_value_impl <- function(db, prefix, names_only,
                                         extra_block, min_subchunk,
                                         max_subchunk) {
    p <- split_chunk_stems(prefix)
    dimension <- p[3]

    prefix <- paste0(prefix, ":47")
    dat <- .get_subchunk_blocks_data_impl(db, prefix = prefix,
                                          names_only = names_only,
                                          extra_block = extra_block)

    # calculate lowest and highest subchunks
    pos <- purrr::map_int(dat, attr, "offset")
    if (is.numeric(min_subchunk)) {
        bottom <- as.integer(min_subchunk)
    } else if (isTRUE(min_subchunk)) {
        bottom <- if (dimension == 0) -4 else 0
    } else if (length(pos) > 0L) {
        bottom <- min(pos)
    } else {
        bottom <- NA
    }
    if (is.numeric(max_subchunk)) {
        top <- as.integer(max_subchunk)
    } else if (isTRUE(max_subchunk)) {
        top <- if (dimension == 0) 19 else if (dimension == 1) 7 else 15
    } else  if (length(pos) > 0L) {
        top <- max(pos)
    } else {
        top <- NA
    }
    if (is.na(top) || is.na(bottom)) {
        return(NULL)
    }

    # Allocate array
    height <- 16 * (top - bottom + 1)
    mat <- array("minecraft:air", c(16, height, 16))

    # Copy Data
    for (i in seq_along(pos)) {
        if (pos[i] >= bottom && pos[i] <= top) {
            mat[, ((pos[i] - bottom) * 16) + 1:16, ] <- dat[[i]]
        }
    }
    attr(mat, "origin") <- c(p[1], bottom, p[2]) * 16L
    mat
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

.valid_blocks_value <- function(value) {
    if (!is.character(value) || length(value) %% 16L != 0L) {
        return(FALSE)
    }
    d <- dim(value)
    if (length(d) != 3L || d[1] != 16L || d[3] != 16L) {
        return(FALSE)
    }
    TRUE
}

.is_blocks_prefix <- function(x) {
    grepl("^chunk:-?[0-9]+:-?[0-9]+:[0-9]+:47$", x)
}

.put_chunk_blocks_value_impl <- function(db, prefix, value, version = 9L) {
    if (!.valid_blocks_value(value)) {
        abort("`value` must be a 16 x 16*N x 16 character array.")
    }
    if (!.is_blocks_prefix(prefix)) {
        prefix_ <- paste0(prefix, ":47")
        if (!.is_blocks_prefix(prefix_)) {
            msg <- sprintf("`%s` is not a valid blocks prefix", prefix)
            abort(msg)
        }
        prefix <- prefix_
    }

    origin <- chunk_origin(value)
    if (is.null(origin)) {
        abort("`value` must have an origin.")
    }

    bottom <- origin[2] %/% 16L

    # identify existing chunk data.
    old_keys <- get_keys(prefix, db = db)

    # construct new chunk data
    data <- list()
    subtags <- seq.int(bottom, length.out = (dim(value)[2] %/% 16L))
    new_keys <- str_c(prefix, ":", subtags)
    for (s in seq_along(subtags)) {
        subchunk <- value[, (s - 1L) * 16L + (1:16), ]
        # skip empty chunks
        if (all(subchunk == "minecraft:air")) {
            next
        }
        # extract and encode subchunk data
        data[[new_keys[s]]] <-
            write_subchunk_blocks_value(subchunk,
                                        version = version,
                                        missing_offset = subtags[s])
    }
    put_keys <- names(data)
    del_keys <- old_keys[!(old_keys %in% put_keys)]
    # write a batch
    batch <- db$writebatch()
    batch$mdelete(chrkeys_to_rawkeys(del_keys))
    batch$mput(chrkeys_to_rawkeys(put_keys), data)
    batch$write()
    # clean up
    batch$destroy()

    invisible()
}

#' Locate the coordinates of blocks in a chunk
#'
#' @param blocks A character array containing block data.
#' @param pattern The pattern to look for. Passed to `stringr::str_detect`.
#' @param negate  If `TRUE`, return non-matching elements.
#'
#' @examples
#' dbpath <- rbedrock_example_world("example1.mcworld")
#' db <- bedrockdb(dbpath)
#' blocks <- get_chunk_blocks_value(db, x=37, z=10, dimension=0)
#' locate_blocks(blocks, "ore")
#' close(db)
#'
#' @export
locate_blocks <- function(blocks, pattern, negate = FALSE) {
    ind <- which(str_detect(blocks, pattern, negate))

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
#' `get_subchunk_blocks_from_chunk()` loads SubchunkBlocks data from a
#' `bedrockdb`. It supports efficiently loading subchunk block data from a
#' single chunk.
#'
#' @return `get_subchunk_blocks_from_chunk()` returns a list of the of the
#' values returned by `read_subchunk_blocks_value()`.

#' @rdname SubChunkBlocks
#' @export
get_subchunk_blocks_from_chunk <- function(db, x, z, dimension,
                                           names_only = FALSE,
                                           extra_block = !names_only) {
    starts_with <- .process_key_args_prefix(x, z, dimension)
    vec_assert(starts_with, character(), 1L)
    starts_with <- str_c(starts_with, ":47")

    .get_subchunk_blocks_data_impl(db, prefix = starts_with,
                                   names_only = names_only,
                                   extra_block = extra_block)
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

.chunk_blocks_apply_offsets <- function(args, dims, origin) {
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
    args <- .chunk_blocks_apply_offsets(args, dim(x), origin)

    # if this fails the error message is gnarly, see rlang::exec docs
    rlang::exec(`[`, x, !!!args, drop = drop)
}

#' @rdname chunk_blocks
#' @export
`chunk_blocks<-` <- function(x, ..., origin = chunk_origin(x), value) {
    args <- rlang::dots_list(..., .named = NULL,
                             .preserve_empty = TRUE,
                             .ignore_empty = "none")
    args <- .chunk_blocks_apply_offsets(args, dim(x), origin)

    rlang::exec(`[<-`, x, !!!args, value = value)
}
