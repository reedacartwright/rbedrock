#' @export
split_chunk_keys <- function(keys) {
    stringr::str_match(keys, "^@([^:]+):([^:]+):([^:]+):([^:-]+)(?:-([^:]+))?$")
}

#' @export
chunk_pos <- function(keys) {
    pos <- split_chunk_keys(keys)[,2:3, drop = FALSE]
    mode(pos) <- "integer"
    pos
}

# List of Tags that identify the contents of a chunk key.
.CHUNK_TAGS <- c(
    "ChunkVersion" = 44L,
    "2DMaps" = 45L,
    "2DMapsLegacy" = 46L,
    "SubchunkBlocks" = 47L,
    "48" = 48L, # removed
    "BlockEntities" = 49L,
    "Entities"= 50L,
    "PendingBlockTicks" = 51L,
    "52" = 52L, # removed
    "BiomeStates" = 53L,
    "Finalization" = 54L,
    "55" = 55L, # removed
    "BorderBlocks" = 56L, # Education edition
    "HardcodedSpawnAreas" = 57L,
    "RandomBlockTicks" = 58L,
    "Checksums" = 59L, # introduced in 1.16
    "ChunkVersionLegacy" = 118L,

    # future proofing
    "33" = 33L,
    "34" = 34L,
    "35" = 35L,
    "36" = 36L,
    "37" = 37L,
    "38" = 38L,
    "39" = 39L,
    "40" = 40L,
    "41" = 41L,
    "42" = 42L,
    "43" = 43L,
    "60" = 60L,
    "60" = 60L,
    "61" = 61L,
    "62" = 62L,
    "63" = 63L,
    "64" = 64L
)
.CHUNK_TAGS_INV <- rep(NA_character_, 128)
.CHUNK_TAGS_INV[.CHUNK_TAGS] <- names(.CHUNK_TAGS)

chunk_tag_str <- function(tags) {
    tags <- as.integer(tags)
    .CHUNK_TAGS_INV[tags]
}

chunk_tag_int <- function(str) {
    str <- as.character(str)
    .CHUNK_TAGS[str]
}

get_chunk_tag_from_key <- function(keys) {
    m <- stringr::str_match(keys, "^@[^:]+:[^:]+:[^:]+:([^:-]+)(?:-[^:]+)?$")
    as.integer(m[,2])
}

get_chunk_tag_str <- function(keys) {
    chunk_tag_str(get_chunk_tag_from_key(keys))
}

is_chunk_key <- function(keys) {
    stringr::str_detect(keys, "^@[^:]+:[^:]+:[^:]+:[^:-]+(?:-[^:]+)?$")
}

subset_chunk_keys <- function(keys, negate = FALSE) {
   stringr::str_subset(keys, "^@[^:]+:[^:]+:[^:]+:[^:-]+(?:-[^:]+)?$", negate = negate)
}

.create_strkey <- function(x, z, d, tag, subtag = NA_integer_) {
    ret <- stringr::str_c(x, z, d, tag, sep = ":")
    ret <- stringr::str_c("@", ret)
    if(length(subtag) < length(ret)) {
        subtag <- rep(subtag, length.out = length(ret))
    }
    ret <- ifelse(is.na(subtag), ret, stringr::str_c(ret, subtag, sep = "-"))
    ret
}

.process_strkey_args <- function(x, z, d, tag, subtag = NA_integer_, stop_if_filtered = FALSE) {
    # is z is missing then x should contain keys as strings
    if(missing(z) && is.character(x)) {
        # if tag exists, we are going to filter on data type
        if(!missing(tag)) {
            if(length(tag) != 1) {
                stop("when filtering keys in x, tag must have length 1")
            }
            ktag <- get_chunk_tag_from_key(x)
            b <- !is.na(ktag) & ktag == tag
            if(stop_if_filtered && any(!b)) {
                stop(paste0("Some keys passed to .process_strkeys_args are not of type ", tag))
            }
            x <- x[b]
        }
        return(x)
    }
    .create_strkey(x, z, d, tag, subtag)
}

# this can be very very inefficient compared to a c-level api
.to_strkey <- function(key) {
    if (is.raw(key)) {
        key <- list(key)
    } else if (is.character(key)) {
        key <- lapply(key, charToRaw)
    }
    out <- sapply(key, function(k) {
        if (!is.raw(k)) {
            stop("One or more keys is not raw().")
        }
        len <- length(k)
        x <- NA
        z <- NA
        d <- NA
        tag <- NA
        subtag <- NA
        if (len == 9 || len == 10) {
            xz <- readBin(k, integer(), n = 2L, endian = "little")
            x <- xz[1]
            z <- xz[2]
            d <- 0
            tag <- as.integer(k[9])
            subtag <- NA
            if (len == 10) {
                subtag <- as.integer(k[10])
            }
            if (!(tag %in% .CHUNK_TAGS)) {
                return(.raw_to_strkey(k))
            }
        } else if (len == 13 || len == 14) {
            xz <- readBin(k, integer(), n = 3L, endian = "little")
            x <- xz[1]
            z <- xz[2]
            d <- xz[3]
            tag <- as.integer(k[13])
            subtag <- NA
            if (len == 14) {
                subtag <- as.integer(k[14])
            }
            if (!(tag %in% .CHUNK_TAGS) || d > 2) {
                return(.raw_to_strkey(k))
            }
        } else {
            return(.raw_to_strkey(k))
        }
        return(.create_strkey(x, z, d, tag, subtag))
    })
    return(out)
}

.from_strkey <- function(key, simplify=TRUE) {
    if(is.raw(key)) {
        return(key)
    }
    m <- stringr::str_match(key, "^@([^:]+):([^:]+):([^:]+):([^:-]+)(?:-([^:]+))?$|^.*$")
    s <- split(m, seq_len(nrow(m)))
    out <- lapply(s, function(k) {
        if (is.na(k[2])) {
            return(.strkey_to_raw(k[1]))
        }
        x <- as.integer(k[2])
        z <- as.integer(k[3])
        d <- as.integer(k[4])
        tag <- as.integer(k[5])
        subtag <- as.integer(k[6])
        r <- writeBin(c(x, z), raw(), size = 4, endian = "little")
        if (d > 0) {
            r <- c(r, writeBin(d, raw(), size = 4, endian = "little"))
        }
        r <- c(r, writeBin(tag, raw(), size = 1, endian = "little"))
        if (!is.na(subtag)) {
            r <- c(r, writeBin(subtag, raw(), size = 1, endian = "little"))
        }
        return(r)
    })
    if (length(out) == 1 && simplify) {
        return(out[[1]])
    }
    return(out)
}

# percent encode raw values
.raw_to_strkey <- function(key) {
    if(!is.raw(key)) {
        stop("key passed to .raw_to_strkey must have type raw")
    }
    b <- 0x20 < key & key < 0x7F # graphical characters
    b <- b & key != charToRaw("%") & key != charToRaw("@") # % and @
    y <- character(length(key))
    y[b] <- vapply(key[b], rawToChar, "")
    y[!b] <- vapply(key[!b], function(x) paste0("%", toupper(as.character(x))), "")
    paste0(y,collapse="")
}

# percent decode raw values
# copied with modifications from utils::URLDecode
.strkey_to_raw <- function(key) {
    if(!(is.character(key) && length(key) == 1)) {
        stop("key passed to .strkey_to_raw must be a character vector of length 1")
    }
    x <- charToRaw(key)
    pc <- charToRaw("%")
    out <- raw(0L)
    i <- 1L
    while (i <= length(x)) {
        if (x[i] != pc) {
            out <- c(out, x[i])
            i <- i + 1L
        }
        else {
            y <- as.integer(x[i + 1L:2L])
            y[y > 96L] <- y[y > 96L] - 32L
            y[y > 57L] <- y[y > 57L] - 7L
            y <- sum((y - 48L) * c(16L, 1L))
            out <- c(out, as.raw(as.character(y)))
            i <- i + 3L
        }
    }
    out
}

.create_key_prefix <- function(x, z, dimension, simplify=TRUE) {
    stopifnot(length(x) == length(z) && length(x) == length(dimension))

    # convert coordinates to raw values and bind them together
    rx <- writeBin(as.integer(x), raw(), size = 4, endian = "little")
    rz <- writeBin(as.integer(z), raw(), size = 4, endian = "little")
    rd <- writeBin(as.integer(dimension), raw(), size = 4, endian = "little")
    mx <- matrix(rx, nrow=4)
    mz <- matrix(rz, nrow=4)
    md <- matrix(rd, nrow=4)
    m <- rbind(mx,mz,md)

    # split matrix into a list of columns
    s <- split(m, col(m))
    names(s) <- NULL

    # strip dimension data for overworld
    ret <- lapply(s, function(x) {
        if(all(x[9:12] == 0x00)) {
            x <- x[1:8]
        }
        x
    })
    if(simplify && length(ret) == 1) {
        ret <- ret[[1]]
    }
    ret
}
