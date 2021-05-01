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
#'
#' @return `get_chunk_blocks_data()` returns a list of the of the values
#' returned by `read_chunk_blocks_value()`.
#' @export
get_chunk_blocks_data <- function(db, x, z, dimension,
        names_only = FALSE, extra_block = FALSE) {
    keys <- .process_key_args_prefix(x, z, dimension)
    keys <- vec_unique(keys)

    dat <- purrr::map(keys, ~.get_chunk_blocks_value_impl(db,
        prefix = .,
        names_only = names_only,
        extra_block = extra_block
    ))

    rlang::set_names(dat, keys)
}

#' @description
#' `get_chunk_blocks_value()` loads block data from a `bedrockdb`.
#' It only supports loading a single value.
#'
#' @return `get_chunk_blocks_value()`
#' return a 16xNx16 character array. The axes represent the `x`, `y`, and `z`
#' dimensions in that order.
#'
#' @rdname get_chunk_blocks_data
#' @export
get_chunk_blocks_value <- function(db, x, z, dimension,
        names_only = FALSE, extra_block = FALSE) {
    keys <- .process_key_args_prefix(x, z, dimension)
    keys <- vec_unique(keys)
    vec_assert(keys, character(), 1L)

    .get_chunk_blocks_value_impl(db, keys,
        names_only = names_only,
        extra_block = extra_block
    )
}

.get_chunk_blocks_value_impl <- function(db, prefix, ...) {
    prefix <- stringr::str_c(prefix, ":47", collapse="")
    keys <- get_keys(db, starts_with = prefix)
    dat <- get_subchunk_blocks_data(db, keys, ...)
    pos <- .get_subtag_from_chunk_key(names(dat))
    max_y <- 16*max(pos)+16
    mat <- array("minecraft:air", c(16, max_y, 16))
    for(i in seq_along(pos)) {
        mat[,(pos[i]*16)+1:16,] <- dat[[i]]
    }
    mat
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
        names_only = FALSE, extra_block = FALSE) {
    keys <- .process_key_args(x,z,dimension, tag=47L, subtag = subchunk)
    dat <- get_values(db, keys)
    ret <- purrr::map(dat, read_subchunk_blocks_value, names_only = names_only,
        extra_block = extra_block)
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
        names_only = FALSE, extra_block = FALSE) {
    key <- .process_key_args(x,z,dimension, tag=47L, subtag = subchunk)
    vec_assert(key, character(), 1L)

    dat <- get_value(db, key)
    
    read_subchunk_blocks_value(dat, names_only = names_only,
        extra_block = extra_block)
}

#' @description
#' `read_subchunk_blocks_value()` decodes binary SubchunkBlock data.
#'
#' @param rawdata a raw vector holding binary SubchunkBlock data
#'
#' @return `read_subchunk_blocks_value()` returns a 16x16x16 character array.
#' The axes represent the `x`, `y`, and `z` dimensions in that order.
#' 
#' @rdname SubchunkBlocks
#' @export
read_subchunk_blocks_value <- function(rawdata, names_only = FALSE,
        extra_block = FALSE) {
    if(is.null(rawdata)) {
        return(NULL)
    }
    blocks <- .read_subchunk(rawdata)
    pal <- block_palette(blocks[[1]]) %>%
        purrr::map_chr(.block_string, names_only = names_only)
    b <- array(pal[blocks[[1]]], dim = dim(blocks[[1]]))
    if(isTRUE(extra_block) && length(blocks) >= 2) {
        pal2 <- block_palette(blocks[[2]]) %>%
            purrr::map_chr(.block_string, names_only = names_only)
        pal2[pal2 == 'minecraft:air'] <- NA_character_
        b2 <- array(pal2[blocks[[2]]], dim = dim(blocks[[2]]))
        b[] <- stringr::str_c(b, b2, sep=";") %|% b
    }
    b
}

#' Load and store SubchunkBlocks layers
#'
#' @description
#' `get_subchunk_layers_data()` loads SubchunkBlocks data from a `bedrockdb`.
#'  It will silently drop and keys not representing SubchunkBlocks data.
#'
#' @param db A bedrockdb object.
#' @param x,z,dimension Chunk coordinates to extract data from.
#'    `x` can also be a character vector of db keys.
#' @param subchunk Subchunk indexes to extract data from.
#'
#' @param layer an integer vector specifying which layers of storage to return
#'              or `NULL` to return all layers
#'
#' @param simplify a logical scalar indicating whether to simplify single-layer
#'                 results to an array instead of a list containing one array
#'
#' @return `get_subchunk_layers_data()` returns a list of the of the values
#'          returned by `read_subchunk_layers_value()`.
#'
#' @keywords internal
#' @export
get_subchunk_layers_data <- function(db, x, z, dimension, subchunk, layer = 1L,
        simplify = TRUE) {
    keys <- .process_key_args(x,z,dimension, tag=47L, subtag = subchunk)
    dat <- get_values(db, keys)
    ret <- purrr::map(dat, read_subchunk_layers_value, layer = layer,
        simplify = simplify)
    ret
}

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
get_subchunk_layers_value <- function(db, x, z, dimension, subchunk, layer = 1L,
        simplify = TRUE) {
    key <- .process_key_args(x,z,dimension, tag=47L, subtag = subchunk)
    vec_assert(key, character(), 1L)

    dat <- get_value(db, key)
    read_subchunk_layers_value(dat, layer = layer, simplify = simplify)
}

#' @description
#' `read_subchunk_layers_value()` decodes binary SubchunkBlock data
#' into index-mapped arrays and associated block palettes.
#'
#' @rdname get_subchunk_layers_data
#' @export
read_subchunk_layers_value <- function(rawdata, layer = 1L, simplify = TRUE) {
    if(is.null(rawdata)) {
        return(NULL)
    }
    blocks <- .read_subchunk(rawdata)
    # subset the storage layers
    if (!is.null(layer)) {
        blocks <- blocks[layer]
    }
    # Simplify if requested
    if (isTRUE(simplify) && length(blocks) == 1) {
        blocks <- blocks[[1]]
    }
    blocks
}

#' @description
#' `block_palette()` returns the NBT palette associated with a block storage layer.
#'
#' @param object A storage layer returned from `read_subchunk` or `get_subchunk`.
#' @rdname get_subchunk_layers_data
#' @export
block_palette <- function(object) {
    attr(object, 'palette')
}

.block_string <- function(x, names_only = FALSE) {
    # convert block information in a palette entry into a string
    block_name <- payload(x$name)
    states <- payload(x$states)
    if(length(states) == 0L || isTRUE(names_only)) {
        return(block_name)
    }
    states <- stringr::str_c(names(states), as.character(states),
        sep="=", collapse="@")
    stringr::str_c(block_name, states, sep="@")
}

.read_subchunk <- function(rawdata) {
    vec_assert(rawdata, raw())
    .Call(Cread_subchunk, rawdata)
}

#' @description
#' `subchunk_origins()` returns a matrix containing the block coordinate of the
#' lower NW corner of subchunk keys
#' @param keys A character vector of database keys.
#' @export
#' @rdname SubchunkBlocks
subchunk_origins <- function(keys) {
    pos <- .split_chunk_keys(keys)[,c(2,6,3), drop = FALSE]
    mode(pos) <- "integer"
    pos*16L
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
subchunk_coords <- function(ind, origins=subchunk_origins(names(ind))) {
    if(is.character(origins)) {
        origins <- subchunk_origins(origins)
    }
    
    f <- function(x,y) t(y - 1L + t(arrayInd(x,c(16,16,16))))
    if(is.list(ind)) {
        o <- purrr::array_tree(origins,1)
        purrr::map2(ind, o, f)
    } else {
        f(ind, as.vector(origins))
    }
}
