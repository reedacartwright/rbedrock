
# https://gist.github.com/Tomcc/a96af509e275b1af483b25c543cfbf37

# [version:byte][num_storages:byte][block storage1]...[blockStorageN]

#' @export
read_subchunk <- function(con, storage=1, names_only=TRUE, simplify=TRUE) {
    blocks <- .read_subchunk(con)
    if (!is.null(storage)) {
        blocks <- blocks[storage]
    }
    if (names_only) {
        blocks <- purrr::modify_depth(blocks, 2, ~.$name)
    }
    if (simplify && length(blocks) == 1) {
        b <- simplify(blocks[[1]])
        dim(b) <- c(16,16,16)
        # b[y,z,x]
        return(b)
    }
    return(blocks)
}

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
        palleteSize <- readBin(con, integer(), n=1L, size=4L, endian="little")
        pallete <- read_nbt(con, palleteSize)
        ids <- array(0L, c(blocksPerWord, wordCount))
        for(j in seq.int(blocksPerWord)) {
            k <- bitops::bitShiftR(words, bitsPerBlock*(j-1))
            ids[j,] <- bitops::bitAnd(k,idMask)+1L
        }
        o <- pallete[ids[1:4096]]
        dim(o) <- c(16,16,16)
        out[[i]] <- o
    }
    out
}

#' @export
subchunk_origin <- function(keys) {
    m <- keys %>% subset_chunk_keys() %>% split_chunk_keys()
    m <- m[m[,5] == "47",]
    xyz <- matrix(as.integer(m[,c(2,6,3)])*16L,ncol=3)
    xyz <- xyz %>% purrr::array_branch(1)
    names(xyz) <- m[,1]
    xyz
}

#' @export
subchunk_coords <- function(offsets, origins=subchunk_origin(names(offsets))) {
    purrr::map2(offsets, origins, ~sweep(.x, 2, .y, "+")-1)
}

# (word >> ((position % blocksPerWord) * bitsPerBlock)) & ((1 << bitsPerBlock) - 1);