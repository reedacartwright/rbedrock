chunk_tag <- function(tags) {
    dplyr::recode(tags,
        `45` = "2DMaps",
        `46` = "2DMapsLegacy",
        `47` = "SubchunkBlocks",
        `48` = "48", # removed
        `49` = "BlockEntities",
        `50` = "Entities",
        `51` = "PendingBlockTicks",
        `52` = "52", # removed
        `53` = "BiomeStates",
        `54` = "Finalization",
        `55` = "55", # removed
        `56` = "BorderBlocks", # Education edition
        `57` = "HardcodedSpawnAreas",
        `58` = "RandomBlockTicks",
        `118` = "ChunkVersion"
    )
}

chunk_tag_as_int <- function(str) {
    dplyr::recode(str,
        "2DMaps" = 45,
        "2DMapsLegacy" = 46,
        "SubchunkBlocks" = 47,
        "48" = 48, # removed
        "BlockEntities" = 49,
        "Entities"= 50,
        "PendingBlockTicks" = 51,
        "52" = 52, # removed
        "BiomeStates" = 53,
        "Finalization" = 54,
        "55" = 55, # removed
        "BorderBlocks" = 56, # Education edition
        "HardcodedSpawnAreas" = 57,
        "RandomBlockTicks" = 58,
        "ChunkVersion" = 118

    )
}

get_chunk_tag <- function(keys) {
    m <- stringr::str_match(keys, "^@[^:]+:[^:]+:[^:]+:([^:-]+)(?:-[^:]+)?$")
    
    chunk_tag(m[,2])
}

is_chunk_key <- function(keys) {
    stringr::str_detect(keys, "^@[^:]+:[^:]+:[^:]+:[^:-]+(?:-[^:]+)?$")
}

subset_chunk_keys <- function(keys, negate = FALSE) {
   stringr::str_subset(keys, "^@[^:]+:[^:]+:[^:]+:[^:-]+(?:-[^:]+)?$", negate = negate)
}

split_chunk_keys <- function(keys) {
    stringr::str_match(keys, "^@([^:]+):([^:]+):([^:]+):([^:-]+)(?:-([^:]+))?$")
}

.create_strkey <- function(x, z, d, tag, subtag = NA) {
    ret <- stringr::str_c(x, z, d, tag, sep = ":")
    ret <- stringr::str_c("@", ret)
    if (!is.na(subtag)) {
        ret <- stringr::str_c(ret, subtag, sep = "-")
    }
    return(ret)
}

# this can be very very inefficient compared to a c-level api
.to_strkey <- function(key) {
    if (is.character(key)) {
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
            xz <- readBin(k, "integer", n = 2L, endian = "little")
            x <- xz[1]
            z <- xz[2]
            d <- 0
            tag <- as.integer(k[9])
            subtag <- NA
            if (len == 10) {
                subtag <- as.integer(k[10])
            }
            if (!(tag %in% c(45:58,118))) {
                return(rawToChar(k))
            }
        } else if (len == 13 || len == 14) {
            xz <- readBin(k, "integer", n = 3L, endian = "little")
            x <- xz[1]
            z <- xz[2]
            d <- xz[3]
            tag <- as.integer(k[13])
            subtag <- NA
            if (len == 14) {
                subtag <- as.integer(k[14])
            }
            if (!(tag %in% c(45:58,118))) {
                return(rawToChar(k))
            }
        } else {
            return(rawToChar(k))
        }
        return(.create_strkey(x, z, d, tag, subtag))
    })
    return(out)
}

.from_strkey <- function(key) {
    m <- stringr::str_match(key, "^@([^:]+):([^:]+):([^:]+):([^:-]+)(?:-([^:]+))?$|^.*$")
    s <- split(m, seq_len(nrow(m)))
    out <- lapply(s, function(k) {
        if (is.na(k[2])) {
            return(charToRaw(k[1]))
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
    if (length(out) == 1) {
        return(out[[1]])
    }
    return(out)
}
