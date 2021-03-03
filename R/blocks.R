#' Get block information stored in subchunks.
#'
#' Reads block information from a bedrockdb. `get_subchunk_blocks` reads data from multiple
#' subchunks and returns a named list containing a three-dimensional character array with
#' axes `x`, `y`, and `z`.
#' Each block is represented by a string containing block name and block states in the format
#' `blockname@@state1=value1@@state2=value2...`. Blocks may have 0 or mode states.
#'
#' @param db A bedrockdb object.
#' @param x,z,dimension,subchunk Subchunk Coordinates to extract block data from.
#'    x can also be a character vector of db keys and any keys not
#'    representing subchunk data will be silently dropped.
#' @param rawval A raw vector containing binary subchunk data
#' @param names_only Return only the names of the blocks, ignoring block states.
#' @param extra_block Append the extra block layer to the output (separated by ';'').
#'    This is mostly useful if you have waterlogged blocks. If the extra block is air,
#'    it will not be appended.
#' @param storage Which storage layer(s) to get. If `storage = NULL`, all storage layers will be
#'    returned.
#' @param simplify Return a single storage layer instead of a list containing a single storage layer.
#' @param object A storage layer returned from `read_subchunk` or `get_subchunk`.
#' @param ind Numeric vector or a named list of numeric vectors containing indexes for blocks in a subchunk.
#' @param origins A matrix of subchunk origins.
#'
#' @return `get_subchunk_blocks` returns a list of character arrays.
#'         `get_subchunk` and `read_subchunk` return a list of numeric arrays or a single array.
#'         Each array contains block ids with a block palette stored in attribute 'palette'.
#'         `palette` returns the palette.
#'
#' @name get_blocks
NULL

#' @rdname get_blocks
#' @export
get_subchunk_blocks <- function(db, x, z, dimension, subchunk, names_only = FALSE, extra_block = FALSE) {
    keys <- .process_key_args(x,z,dimension, tag=47L, subtag = subchunk)
    dat <- get_values(db, keys)
    ret <- purrr::map(dat, read_subchunk_blocks, names_only = names_only, extra_block = extra_block)
    ret
}

#' @rdname get_blocks
#' @export
get_subchunk <- function(db, x, z, dimension, subchunk, storage=1, simplify=TRUE) {
    keys <- .process_key_args(x,z,dimension, tag=47L, subtag = subchunk)
    dat <- get_values(db, keys)
    ret <- purrr::map(dat, read_subchunk, storage=storage, simplify = simplify)
    ret
}

#' @rdname get_blocks
#' @export
read_subchunk <- function(rawval, storage=1, simplify=TRUE) {
    if(is.null(rawval)) {
        return(NULL)
    }
    blocks <- .read_subchunk(rawval)
    # subset the storage layers
    if (!is.null(storage)) {
        blocks <- blocks[storage]
    }
    # Simplify if requested
    if (simplify && length(blocks) == 1) {
        blocks <- blocks[[1]]
    }
    blocks
}

#' @rdname get_blocks
#' @export
read_subchunk_blocks <- function(rawval, names_only = FALSE, extra_block = FALSE) {
    if(is.null(rawval)) {
        return(NULL)
    }
    blocks <- .read_subchunk(rawval)
    pal <- block_palette(blocks[[1]]) %>% purrr::map_chr(.block_string, names_only = names_only)
    b <- array(pal[blocks[[1]]], dim = dim(blocks[[1]]))
    if(isTRUE(extra_block) && length(blocks) >= 2) {
        pal2 <- block_palette(blocks[[2]]) %>% purrr::map_chr(.block_string, names_only = names_only)
        pal2[pal2 == 'minecraft:air'] <- ""
        b2 <- array(pal2[blocks[[2]]], dim = dim(blocks[[2]]))
        b[] <- stringr::str_c(b, ifelse(b2 == "", '', ';'), b2)
    }
    b
}

#' @rdname get_blocks
#' @export
block_palette <- function(object) {
    attr(object, 'palette')
}

.block_string <- function(x, names_only = FALSE) {
    # convert block information in a palette entry into a string
    if(length(x$states) == 0L || names_only) {
        return(x$name)
    }
    states <- stringr::str_c(names(x$states),x$states,sep='=',collapse='@')
    stringr::str_c(x$name, states, sep="@")
}

.read_subchunk <- function(rawval) {
    .Call(Cread_subchunk, rawval)
}

#' @export
#' @rdname get_blocks
subchunk_coords <- function(ind, origins=subchunk_origins(names(ind))) {
    f <- function(x,y) t(y - 1L + t(arrayInd(x,c(16,16,16))))
    if(is.list(ind)) {
        o <- purrr::array_tree(origins,1)
        purrr::map2(ind, o, f)
    } else {
        f(ind,origins)
    }
}
