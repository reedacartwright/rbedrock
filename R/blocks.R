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
#' @param con A connection, raw vector, or character vector specifying a path to read
#'    binary subchunk data from.
#' @param names_only Return only the names of the blocks, ignoring block states.
#' @param extra_block Append the extra block layer to the output (separated by ';'').
#'    This is mostly useful if you have waterlogged blocks. If the extra block is air,
#'    it will not be appended.
#' @param storage Which storage layer(s) to get. If `storage = NULL`, all storage layers will be
#'    returned.
#' @param simplify Return a single storage layer instead of a list containing a single storage layer.
#' @param object A storage layer returned from `read_subchunk` or `get_subchunk`.
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
    k <- .process_strkey_args(x,z,dimension, tag=47L, subtag = subchunk)

    dat <- db$mget(k) %>% purrr::map(function(rawval) {
        if(is.null(rawval)) {
            return(NULL)
        }
        blocks <- .read_subchunk(rawval)
        pal <- block_palette(blocks[[1]]) %>% purrr::map_chr(.block_string, names_only = names_only)
        b <- array(pal[blocks[[1]]], dim = dim(blocks[[1]]))
        if(extra_block && length(blocks) >= 2) {
            pal2 <- block_palette(blocks[[2]]) %>% purrr::map_chr(.block_string, names_only = names_only)
            pal2[pal2 == 'minecraft:air'] <- ""
            b2 <- array(pal2[blocks[[2]]], dim = dim(blocks[[2]]))
            b[] <- stringr::str_c(b, ifelse(b2 == "", '', ';'), b2)
        }
        b
    })
    dat
}

# get_chunk_blocks <- function(db, x, z, dimension) {
    
# }

#' @rdname get_blocks
#' @export
get_subchunk <- function(db, x, z, dimension, subchunk, storage=1, simplify=TRUE) {
    k <- .process_strkey_args(x,z,dimension, tag=47L, subtag = subchunk)

    dat <- db$mget(k) %>% purrr::map(read_subchunk, storage=storage, simplify = simplify)
    dat
}

#' @rdname get_blocks
#' @export
read_subchunk <- function(con, storage=1, simplify=TRUE) {
    if(is.null(con)) {
        return(NULL)
    }
    blocks <- .read_subchunk(con)
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

# https://gist.github.com/Tomcc/a96af509e275b1af483b25c543cfbf37
# [version:byte][num_storages:byte][block storage1]...[blockStorageN]

.read_subchunk <- function(con) {
    if (is.character(con)) {
        con <- file(con, "rb")
        on.exit(close(con))
    } else if (is.raw(con)) {
        con <- rawConnection(con)
        on.exit(close(con))
    }

    # read data format version
    version <- readBin(con, integer(), n=1L, size=1L, endian="little", signed = FALSE)
    # currently we only support version 8
    stopifnot(version == 8)
    # read how many layers of blocks we have
    storage_count <- readBin(con, integer(), n=1L, size=1L, endian="little", signed = FALSE)
    out <- list()
    for (i in seq.int(storage_count)) {
        flag <- readBin(con, integer(), n=1L, size=1L, endian="little", signed = FALSE)
        isRuntime <- flag %% 2
        stopifnot(isRuntime == 0) # check for "persistent storage"
        bitsPerBlock <- flag %/% 2
        blocksPerWord <- floor(32 / bitsPerBlock)
        wordCount <- ceiling(4096 / blocksPerWord)
        idMask <- 2^bitsPerBlock-1
        words <- readBin(con, integer(), n=wordCount, size=4L, endian="little")
        # convert to numeric and fix na values to work around
        # R's handling of 32-bit data
        words <- as.numeric(words)
        words[is.na(words)] <- -2147483648
        paletteSize <- readBin(con, integer(), n=1L, size=4L, endian="little")
        palette <- .read_nbt_compound_payload(con, paletteSize)
        ids <- array(0L, c(blocksPerWord, wordCount))
        for(j in seq.int(blocksPerWord)) {
            k <- bitops::bitShiftR(words, bitsPerBlock*(j-1))
            ids[j,] <- bitops::bitAnd(k,idMask)+1L
        }
        # Find the id of a block use: ids[y,z,x]
        blocks <- array(as.integer(ids[1:4096]), c(16,16,16))
        blocks <- aperm(blocks, c(3,1,2))
        out[[i]] <- structure(blocks, palette = palette)
    }
    out
}

#' @export
subchunk_origin <- function(keys) {
    m <- keys %>% subset_chunk_keys() %>% split_chunk_keys()
    m <- m[m[,5] == "47", , drop = FALSE]
    xyz <- matrix(as.integer(m[,c(2,6,3)])*16L,ncol=3)
    xyz <- xyz %>% purrr::array_branch(1)
    names(xyz) <- m[,1]
    xyz
}

#' @export
subchunk_coords <- function(offsets, origins=subchunk_origin(names(offsets))) {
    if(is.list(offsets)) {
        purrr::map2(offsets, origins, ~sweep(.x, 2, .y, "+")-1)        
    } else {
        sweep(offsets, 2, origins, "+")-1
    }
}

# (word >> ((position % blocksPerWord) * bitsPerBlock)) & ((1 << bitsPerBlock) - 1);