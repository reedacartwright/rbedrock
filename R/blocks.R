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
                                     starts_with = x,
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

    .get_chunk_blocks_value_impl(db, starts_with,
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

.get_chunk_blocks_value_impl <- function(db, starts_with, names_only,
                                         extra_block, min_subchunk,
                                         max_subchunk) {

    p <- .split_chunk_stems(starts_with)
    dimension <- p[3]

    starts_with <- paste0(starts_with, ":47")
    dat <- .get_subchunk_blocks_data_impl(db, starts_with = starts_with,
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
    old_keys <- get_keys(db, starts_with = prefix)

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

#' Load and store SubchunkBlocks data
#'
#' @description
#' SubchunkBlocks data (tag 47) holds information about the blocks in a
#' subchunks. Each chunk is divided into multiple 16x16x16 subchunks, and each
#' subchunk is stored separately and indicated by the use of the subtag.
#' Blocks are stored in a palette-based format. Subchunks can have two layers
#' of blocks, and the extra layer is most-often used to store water for
#' water-logged blocks.
#'
#' These functions return block data as strings containing the
#' block name and block states. The strings' format is
#' `blockname@@state1=value1@@state2=value2` etc.
#' Blocks may have 0 or more states.
#'
#' @details
#' If a subchunk contains only air it will not be stored in the database, and
#' missing subchunks are considered air.
#
#' @name SubchunkBlocks
NULL

#' @description
#' `get_subchunk_blocks_data()` loads SubchunkBlocks data from a `bedrockdb`.
#'  It will silently drop and keys not representing SubchunkBlocks data.
#' `get_subchunk_blocks_values()` is a synonym for `get_subchunk_blocks_data()`.
#'
#' @param db A bedrockdb object.
#' @param x,z,dimension Chunk coordinates to extract data from.
#'    `x` can also be a character vector of db keys.
#' @param subchunk Subchunk indexes to extract data from.
#'
#' @param names_only  A logical scalar. Return only the names of the blocks,
#' ignoring block states.
#' @param extra_block A logical scalar. Append the extra block layer to the
#' output (separated by ";"). This is mostly useful if you have waterlogged
#' blocks. If the extra block is air, it will not be appended.
#'
#' @return `get_subchunk_blocks_data()` returns a list of the of the values
#' returned by `read_subchunk_blocks_value()`.
#'
#' @rdname SubchunkBlocks
#' @export
get_subchunk_blocks_data <- function(db, x, z, dimension, subchunk,
                                     names_only = FALSE,
                                     extra_block = !names_only) {
    keys <- .process_key_args(x, z, dimension, tag = 47L, subtag = subchunk)

    .get_subchunk_blocks_data_impl(db, keys,
                                   names_only = names_only,
                                   extra_block = extra_block)
}

#' @rdname SubchunkBlocks
#' @export
get_subchunk_blocks_values <- get_subchunk_blocks_data

.get_subchunk_blocks_data_impl <- function(db, keys, starts_with,
                                           readoptions = NULL,
                                           names_only = FALSE,
                                           extra_block = !names_only) {
    dat <- get_values(db, keys, starts_with, readoptions)
    offsets <- .get_subtag_from_chunk_key(names(dat))
    ret <- purrr::map2(dat, offsets, read_subchunk_blocks_value,
                       names_only = names_only, extra_block = extra_block)
    ret
}

#' @description
#' `get_subchunk_blocks_value()` loads SubchunkBlocks data from a `bedrockdb`.
#' It only supports loading a single value.
#'
#' @return `get_subchunk_blocks_value()` and `read_subchunk_blocks_value()`
#' return a 16x16x16 character array. The axes represent the `x`, `y`, and `z`
#' dimensions in that order.
#'
#' @rdname SubchunkBlocks
#' @export
get_subchunk_blocks_value <- function(db, x, z, dimension, subchunk,
                                      names_only = FALSE,
                                      extra_block = !names_only) {
    key <- .process_key_args(x, z, dimension, tag = 47L, subtag = subchunk)
    vec_assert(key, character(), 1L)

    dat <- get_value(db, key)
    offset <- .get_subtag_from_chunk_key(key)

    read_subchunk_blocks_value(dat, offset, names_only = names_only,
                               extra_block = extra_block)
}

#' @description
#' `get_subchunk_blocks_from_chunk()` loads SubchunkBlocks data from a
#' `bedrockdb`. It supports efficiently loading subchunk block data from a
#' single chunk.
#'
#' @return `get_subchunk_blocks_from_chunk()` returns a list of the of the
#' values returned by `read_subchunk_blocks_value()`.

#' @rdname SubchunkBlocks
#' @export
get_subchunk_blocks_from_chunk <- function(db, x, z, dimension,
                                           names_only = FALSE,
                                           extra_block = !names_only) {
    starts_with <- .process_key_args_prefix(x, z, dimension)
    vec_assert(starts_with, character(), 1L)
    starts_with <- str_c(starts_with, ":47")

    .get_subchunk_blocks_data_impl(db, starts_with = starts_with,
                                   names_only = names_only,
                                   extra_block = extra_block)
}

#' @description
#' `put_subchunk_blocks_data()`, `put_subchunk_blocks_values()`, and
#' `put_subchunk_blocks_value()` store SubchunkBlocks data into a `bedrockdb`.
#'
#' @param data A named list of 16x16x16 character() arrays
#' @param version Which format of subchunk data to use
#'
#' @rdname SubchunkBlocks
#' @export
put_subchunk_blocks_data <- function(db, data, version = 9L) {
    put_subchunk_blocks_values(db, names(data), values = data,
                               version = version)
}

#' @param values A list of 16x16x16 character() arrays
#'
#' @rdname SubchunkBlocks
#' @export
put_subchunk_blocks_values <- function(db, x, z, dimension, subchunk, values,
                                       version = 9L) {
    keys <- .process_key_args(x, z, dimension, tag = 47L, subtag = subchunk,
                              stop_if_filtered = TRUE)
    values <- vec_recycle(values, length(keys), x_arg = "values")
    offsets <- .get_subtag_from_chunk_key(keys)
    values <- purrr::map2(values, offsets,
                          ~write_subchunk_blocks_value(.x, version = version,
                                                       missing_offset = .y))
    put_values(db, keys, values)
}

#' @param value A 16x16x16 character array
#'
#' @rdname SubchunkBlocks
#' @export
put_subchunk_blocks_value <- function(db, x, z, dimension, subchunk, value,
                                      version = 9L) {
    key <- .process_key_args(x, z, dimension, tag = 47L, subtag = subchunk)
    offset <- .get_subtag_from_chunk_key(key)
    vec_assert(key, character(), 1L)
    value <- write_subchunk_blocks_value(value, version = version,
                                         missing_offset = offset)
    put_value(db, key, value)
}

#' @description
#' `read_subchunk_blocks_value()` decodes binary SubchunkBlock data.
#'
#' @param rawdata a raw vector holding binary SubchunkBlock data
#' @param missing_offset subchunk offset to use if one is not found in `rawdata`
#'
#' @return `read_subchunk_blocks_value()` returns a 16x16x16 character array.
#' The axes represent the `x`, `y`, and `z` dimensions in that order.
#'
#' @rdname SubchunkBlocks
#' @export
read_subchunk_blocks_value <- function(rawdata, missing_offset = NA,
                                       names_only = FALSE,
                                       extra_block = !names_only) {
    if (is.null(rawdata)) {
        return(NULL)
    }
    subchunk <- read_subchunk_layers_value(rawdata)
    offset <- attr(subchunk, "offset") %|% missing_offset
    blocks <- purrr::map(subchunk, function(x) {
        pal <- purrr::map_chr(x[["palette"]], .block_string,
                              names_only = names_only)
        array(pal[x[["values"]]], dim = dim(x[["values"]]))
    })
    if (isTRUE(extra_block) && length(blocks) >= 2) {
        ret <- str_c(blocks[[1]], blocks[[2]], sep = ";") %>%
            str_replace(";minecraft:air$", "")
        dim(ret) <- dim(blocks[[1]])
    } else {
        ret <- blocks[[1]]
    }
    structure(ret, offset = offset)
}

#' @param object A 16x16x16 character array.
#' @rdname SubchunkBlocks
#' @export
write_subchunk_blocks_value <- function(object, version = 9L,
                                        missing_offset = NA_integer_) {
    if (!is_character(object, n = 16 * 16 * 16)) {
        abort("`object` is not a character vector of length 4096")
    }
    # check to see if we have extra blocks and split as needed
    s <- str_split(object, fixed(";"))
    n <- max(purrr::map_int(s, length))
    # construct palettes and maps
    block_layers <- purrr::map(seq_len(n), function(i) {
        layer <- purrr::map_chr(s, i, .default = "minecraft:air")
        u <- vec_unique(layer)
        list(values = match(layer, u), palette = purrr::map(u, .block_nbt))
    })
    # copy attribute
    attr(block_layers, "offset") <- attr(object, "offset")
    write_subchunk_layers_value(block_layers, version = version,
                                missing_offset = missing_offset)
}

#' Load and store SubchunkBlocks layers
#'
#' @description
#' `get_subchunk_layers_data()` loads SubchunkBlocks data from a `bedrockdb`.
#'  It will silently drop and keys not representing SubchunkBlocks data.
#' `get_subchunk_layers_values()` is a synonym for `get_subchunk_layers_data()`.
#'
#' @param db A bedrockdb object.
#' @param x,z,dimension Chunk coordinates to extract data from.
#'    `x` can also be a character vector of db keys.
#' @param subchunk Subchunk indexes to extract data from.
#'
#' @return `get_subchunk_layers_data()` returns a list of the of the values
#'          returned by `read_subchunk_layers_value()`.
#'
#' @keywords internal
#' @export
get_subchunk_layers_data <- function(db, x, z, dimension, subchunk) {
    keys <- .process_key_args(x, z, dimension, tag = 47L, subtag = subchunk)
    dat <- get_values(db, keys)
    purrr::map(dat, read_subchunk_layers_value)
}

#' @rdname get_subchunk_layers_data
#' @export
get_subchunk_layers_values <- get_subchunk_layers_data

#' @description
#' `get_subchunk_layers_value()` loads SubchunkBlocks data from a `bedrockdb`.
#' It only supports loading a single value.
#'
#' @return `get_subchunk_layers_value()` and `read_subchunk_layers_value()`
#' return a list of block layers. Each block layer is a 16x16x16 array of
#' integers associated with a block palette. The block palette is stored in the
#' "palette" attribute of the array.
#'
#' @rdname get_subchunk_layers_data
#' @export
get_subchunk_layers_value <- function(db, x, z, dimension, subchunk) {
    key <- .process_key_args(x, z, dimension, tag = 47L, subtag = subchunk)
    vec_assert(key, character(), 1L)

    dat <- get_value(db, key)
    read_subchunk_layers_value(dat)
}

#' @description
#' `get_subchunk_layers_value()` loads SubchunkBlocks data from a `bedrockdb`.
#' It supports efficiently loading subchunk block data from a single chunk.
#'
#' @return `get_subchunk_layers_value()` returns a list of the of the values
#' returned by `read_subchunk_layers_value()`.

#' @rdname get_subchunk_layers_data
#' @export
get_subchunk_layers_from_chunk <- function(db, x, z, dimension) {
    starts_with <- .process_key_args_prefix(x, z, dimension)
    vec_assert(starts_with, character(), 1L)
    starts_with <- str_c(starts_with, ":47")

    dat <- get_values(db, starts_with = starts_with)
    purrr::map(dat, read_subchunk_layers_value)
}

#' @description
#' `put_subchunk_layers_data()`, `put_subchunk_layers_values()`, and
#' `put_subchunk_layers_value()` store SubchunkBlocks data into a `bedrockdb`.
#'
#' @param data A named-vector of key-value pairs for SubchunkBlocks data.
#'
#' @rdname get_subchunk_layers_data
#' @export
put_subchunk_layers_data <- function(db, data, ...) {
    put_subchunk_layers_values(db, names(data), values = data)
}

#' @param values A list of lists of 16x16x16 integer indexes with associated
#' block_palettes.
#'
#' @rdname get_subchunk_layers_data
#' @export
put_subchunk_layers_values <- function(db, x, z, dimension, subchunk,
                                       values, ...) {
    keys <- .process_key_args(x, z, dimension, tag = 47L, subtag = subchunk,
                              stop_if_filtered = TRUE)
    values <- vec_recycle(values, length(keys), x_arg = "values")
    values <- purrr::map(values, write_subchunk_layers_value, ...)
    put_values(db, keys, values)
}

#' @param value A list of 16x16x16 integer indexes with associated
#' block_palettes.
#'
#' @rdname get_subchunk_layers_data
#' @export
put_subchunk_layers_value <- function(db, x, z, dimension, subchunk,
                                      value, ...) {
    key <- .process_key_args(x, z, dimension, tag = 47L, subtag = subchunk)
    vec_assert(key, character(), 1L)
    value <- write_subchunk_layers_value(value, ...)
    put_value(db, key, value)
}

#' @description
#' `read_subchunk_layers_value()` decodes binary SubchunkBlock data
#' into index-mapped arrays and associated block palettes.
#'
#' @rdname get_subchunk_layers_data
#' @export
read_subchunk_layers_value <- function(rawdata) {
    if (is.null(rawdata)) {
        return(NULL)
    }
    vec_assert(rawdata, raw())
    x <- .Call(Cread_subchunk_blocks, rawdata)
    for (i in seq_along(x)) {
        x[[i]]$values <- aperm(x[[i]]$values, c(1, 3, 2))
        x[[i]]$palette <- from_rnbt(x[[i]]$palette)
    }
    x
}

#' @description
#' `write_subchunk_layers_value()` encode SubchunkBlock data
#' into binary form.
#'
#' @param missing_offset subchunk offset to use if one is not found in `rawdata`
#'
#' @rdname get_subchunk_layers_data
#' @export
write_subchunk_layers_value <- function(object, version = 9L,
                                        missing_offset = NA_integer_) {
    if (is_null(object)) {
        return(NULL)
    }
    if (!is_list(object)) {
        abort("`object` must be a list.")
    }
    if (version != 8L && version != 9L) {
        abort(str_glue("`version` {version} not supported."))
    }
    if (version >= 9L) {
        # identify offset
        offset <- attr(object, "offset") %||% missing_offset
        offset <- vec_cast(offset, integer())
        if (is.na(offset)) {
            abort("subchunk format 9 requires a valid subchunk offset.")
        }
    }
    values <- purrr::map(object, function(x) {
        x <- as.integer(x[["values"]])
        if (length(x) != 16 * 16 * 16) {
            abort("an element of `object` is malformed")
        }
        dim(x) <- c(16, 16, 16)
        aperm(x, c(1, 3, 2))
    })
    palette <- purrr::map(object, function(x) {
        if (!has_name(x, "palette") ||
                any(purrr::map_lgl(x[["palette"]], is_nbt) == FALSE)) {
            abort("an element of `object` is malformed")
        }
        to_rnbt(x[["palette"]])
    })

    .Call(Cwrite_subchunk_blocks, values, palette, version, offset)
}

.block_string <- function(x, names_only = FALSE) {
    # convert block information in a palette entry into a string
    block_name <- payload(x$name)
    states <- payload(x$states)
    if (length(states) == 0L || isTRUE(names_only)) {
        return(block_name)
    }
    states <- purrr::imap(states, function(x, y) {
        xtag <- get_nbt_tag(x)
        p <- payload(x)
        if (xtag == 1) {
            tolower(as.character(as.logical(p)))
        } else if (xtag == 3 || xtag == 8) {
            as.character(p)
        } else {
            msg <- str_glue(
                "Block State '{y}' has NBT tag '{xtag}'. ",
                "Possible loss of information when converting to a string."
            )
            rlang::warn(msg)
            as.character(p)
        }
    })
    states <- str_c(names(states), states,
                    sep = "=", collapse = "@")
    str_c(block_name, states, sep = "@")
}

# nolint start: object_name_linter
.BIT_STATES <- c("active", "age_bit", "allow_underwater_bit", "attached_bit",
    "big_dripleaf_head", "bloom", "brewing_stand_slot_a_bit",
    "brewing_stand_slot_b_bit", "brewing_stand_slot_c_bit",
    "button_pressed_bit", "can_summon", "color_bit", "conditional_bit",
    "coral_hang_type_bit", "covered_bit", "crafting", "dead_bit",
    "disarmed_bit", "door_hinge_bit", "drag_down", "end_portal_eye_bit",
    "explode_bit", "extinguished", "hanging", "head_piece_bit", "in_wall_bit",
    "infiniburn_bit", "item_frame_map_bit", "item_frame_photo_bit", "lit",
    "no_drop_bit", "occupied_bit", "ominous", "open_bit", "output_lit_bit",
    "output_subtract_bit", "persistent_bit", "powered_bit", "rail_data_bit",
    "stability_check", "stripped_bit", "suspended_bit", "toggle_bit",
    "top_slot_bit", "triggered_bit", "update_bit", "upper_block_bit",
    "upside_down_bit", "wall_post_bit")

.INTEGER_STATES <- c("age", "bite_counter", "block_light_level", "books_stored",
    "brushed_progress", "candles", "cluster_count", "composter_fill_level",
    "coral_direction", "coral_fan_direction", "deprecated", "direction",
    "facing_direction", "fill_level", "ground_sign_direction",
    "growing_plant_age", "growth", "height", "honey_level",
    "huge_mushroom_bits", "kelp_age", "liquid_depth", "moisturized_amount",
    "multi_face_direction_bits", "propagule_stage", "rail_direction",
    "redstone_signal", "repeater_delay", "respawn_anchor_charge", "rotation",
    "sculk_sensor_phase", "stability", "trial_spawner_state",
    "twisting_vines_age", "vine_direction_bits", "weeping_vines_age",
    "weirdo_direction")

.STRING_STATES <- c("attachment", "bamboo_leaf_size", "bamboo_stalk_thickness",
    "big_dripleaf_tilt", "cauldron_liquid", "chemistry_table_type",
    "chisel_type", "color", "coral_color", "cracked_state", "damage",
    "dirt_type", "double_plant_type", "dripstone_thickness", "flower_type",
    "lever_direction", "minecraft:block_face", "minecraft:cardinal_direction",
    "minecraft:facing_direction", "minecraft:vertical_half",
    "monster_egg_stone_type", "new_leaf_type", "new_log_type",
    "old_leaf_type", "old_log_type", "orientation", "pillar_axis",
    "portal_axis", "prismarine_block_type", "sand_stone_type", "sand_type",
    "sapling_type", "sea_grass_type", "sponge_type", "stone_brick_type",
    "stone_slab_type", "stone_slab_type_2", "stone_slab_type_3",
    "stone_slab_type_4", "stone_type", "structure_block_type",
    "structure_void_type", "tall_grass_type", "torch_facing_direction",
    "turtle_egg_count", "vault_state", "wall_block_type",
    "wall_connection_type_east", "wall_connection_type_north",
    "wall_connection_type_south", "wall_connection_type_west", "wood_type")
# nolint end

.as_bit <- function(x, strict = FALSE) {
    true_values <- c("true", "TRUE", "1", "T", "t")
    false_values <- c("false", "FALSE", "0", "F", "f")
    if (isTRUE(strict)) {
        true_values <- true_values[1:2]
        false_values <- false_values[1:2]
    }
    if (x %in% true_values) {
        return(1L)
    }
    if (x %in% false_values) {
        return(0L)
    }
    NA_integer_
}

.block_state_nbt <- function(state, name) {
    vec_assert(state, character(), size = 1L)
    if (name %in% .STRING_STATES) {
        return(nbt_string(state))
    } else if (name %in% .INTEGER_STATES) {
        p <- as.integer(state)
        if (is.na(p)) {
            msg <- str_glue("Block State '{name}={state}' could not ",
                            "be converted to an integer.")
            rlang::warn(msg)
        }
        return(nbt_int(p))
    } else if (name %in% .BIT_STATES) {
        p <- .as_bit(state)
        if (is.na(p)) {
            msg <- str_glue("Block State '{name}={state}' could not ",
                            "be converted to a boolean bit.")
            rlang::warn(msg)
        }
        return(nbt_byte(p))
    }
    msg <- str_glue("Unknown Block State '{name}={state}' ",
                    "converted to an ")

    p <- suppressWarnings(as.integer(state))
    if (!is.na(p)) {
        msg <- str_glue(msg, "nbt_int.")
        ret <- nbt_int(p)
    } else {
        p <- .as_bit(state, strict = TRUE)
        if (!is.na(p)) {
            msg <- str_glue(msg, "nbt_byte.")
            ret <- nbt_byte(as.integer(p))
        } else {
            msg <- str_glue(msg, "nbt_string.")
            ret <- nbt_string(state)
        }
    }
    rlang::warn(msg)
    ret
}

.block_nbt <- function(x) {
    # convert block string back into palette information
    s <- str_split(x, fixed("@"))[[1]]
    name <- s[1]
    if (length(s) > 1) {
        s <- str_split(s[-1], fixed("="))
        s <- purrr::transpose(s)
        s[[1]] <- tolower(s[[1]])
        states <- rlang::set_names(s[[2]], s[[1]])
        states <- nbt_compound(!!!purrr::imap(states, .block_state_nbt))
    } else {
        states <- nbt_compound()
    }

    nbt_compound(name = nbt_string(name),
                 states = states,
                 version = nbt_int(0x1100010)) # 1.16.0.16 ??
}

#' @description
#' `subchunk_origins()` returns a matrix containing the block coordinate of the
#' lower NW corner of subchunk keys
#' @param keys A character vector of database keys.
#' @export
#' @rdname SubchunkBlocks
subchunk_origins <- function(keys) {
    pos <- .extract_chunk_key_components(keys, which = c(1, 5, 2))
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
#' @rdname SubchunkBlocks
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
