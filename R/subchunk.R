

#' @export
read_subchunk <- function(con, storage=1, names_only=TRUE, simplify=TRUE, keep_palette=FALSE) {
    blocks <- .read_subchunk(con)
    # subset the storage layers
    if (!is.null(storage)) {
        blocks <- blocks[storage]
    }
    # apply palette if needed
    if (!keep_palette || names_only) {
        blocks <- purrr::map(blocks, function(x) {
            pal <- attr(x,'palette')
            array(pal[x], c(16,16,16))
        })
    }
    # Convert to names only if requested
    if (names_only) {
        blocks <- purrr::map(blocks, function(x) {
            array(purrr::map_chr(x, pluck, 'name'), c(16,16,16))
        })
    }
    # Simplify if requested
    if (simplify && length(blocks) == 1) {
        blocks <- blocks[[1]]
    }
    blocks
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