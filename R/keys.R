#' Convert between key types.
#'
#' rbedrock represents database keys two different ways. `chrkeys` are a human-readable
#' format understood by most functions. `rawkeys` are used internally, by the methods
#' of `bedrockdb` objects and `bedrock_leveldb_*` functions.
#'
#' @param keys a character vector of chrkeys or a list or rawkeys 
#'  
#' @return `chrkeys_to_rawkeys` returns a list of raw vectors.
#'
#'         `rawkeys_to_chrkeys` returns a character vector.
#'
#' @export
chrkeys_to_rawkeys <- function(keys) {
    if(!is.null(keys)) {
        keys <- as.character(keys)
    }
    .Call(Cchrkeys_to_rawkeys, keys)
}

#' @rdname chrkeys_to_rawkeys
#' @export
rawkeys_to_chrkeys <- function(keys) {
    if(is.raw(keys)) {
        keys <- list(keys)
    }
    .Call(Crawkeys_to_chrkeys, keys)
}

#' Create a database key from chunk information.
#'
#' @param x Chunk x-coordinate.
#' @param z Chunk z-coordinate.
#' @param d Chunk dimension.
#' @param tag The type of information the key holds.
#' @param subtag The subchunk the key refers to. (Only used if \code{tag==47}).
#' @return The database key corresponding to the inputs.
#' @examples
#' create_chunk_key(0, 0, 0, 47, 1)
#'
#' @export
create_chunk_key <- function(x, z, d, tag, subtag = NA_integer_) {
    if(is.character(tag)) {
        tag <- chunk_tag_int(tag)
    } 
    ret <- stringr::str_c(x, z, d, tag, sep = ":")
    ret <- stringr::str_c("@", ret)
    if(length(subtag) < length(ret)) {
        subtag <- rep(subtag, length.out = length(ret))
    }
    ret <- ifelse(is.na(subtag), ret, stringr::str_c(ret, subtag, sep = "-"))
    ret
}

#' Extract information from chunk keys.
#'
#' @param keys A character vector of database keys.
#' @return
#'   `parse_chunk_keys` returns a tibble containing information extracted
#'   from chunk keys. Keys that do not contain chunk data are dropped.
#'   `chunk_pos` return a matrix containing chunk coordinates.
#'   `chunk_origins` returns the block position of the NW corner of chunks.
#'   `subchunk_origins` returns the block position of the lower NW corner of subchunk.
#' @examples
#' parse_chunk_keys("@@0:0:0:47-1")
#' @export
parse_chunk_keys <- function(keys) {
    if (!is.character(keys)) {
        stop("keys must be a character vector.")
    }
    m <- keys %>% subset_chunk_keys() %>% split_chunk_keys()

    tibble::tibble(key = m[, 1],
        x = as.integer(m[, 2]),
        z = as.integer(m[, 3]),
        dimension = as.integer(m[, 4]),
        tag = chunk_tag_str(m[, 5]),
        subtag = as.integer(m[, 6]),
    )
}

#' @export
#' @rdname parse_chunk_keys
chunk_pos <- function(keys) {
    pos <- split_chunk_keys(keys)[,2:3, drop = FALSE]
    mode(pos) <- "integer"
    pos
}

#' @export
#' @rdname parse_chunk_keys
chunk_origins <- function(keys) {
    pos <- chunk_pos(keys)
    pos*16L
}

#' @export
#' @rdname parse_chunk_keys
subchunk_origins <- function(keys) {
    pos <- split_chunk_keys(keys)[,c(2,6,3), drop = FALSE]
    mode(pos) <- "integer"
    pos <- pos*16L
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

get_tag_from_chunk_key <- function(keys, as_string = FALSE) {
    m <- stringr::str_match(keys, "^@[^:]+:[^:]+:[^:]+:([^:-]+)(?:-[^:]+)?$")
    res <- as.integer(m[,2])
    if(as_string) {
        res <- chunk_tag_str(res)
    }
    res
}

.CHUNK_KEY_RE = "^@[^:]+:[^:]+:[^:]+:[^:-]+(?:-[^:]+)?$"
.CHUNK_KEY_MATCH = "^@([^:]+):([^:]+):([^:]+):([^:-]+)(?:-([^:]+))?$"

is_chunk_key <- function(keys) {
    stringr::str_detect(keys, .CHUNK_KEY_RE)
}

subset_chunk_keys <- function(keys, negate = FALSE) {
   stringr::str_subset(keys, .CHUNK_KEY_RE, negate = negate)
}

split_chunk_keys <- function(keys) {
    stringr::str_match(keys, .CHUNK_KEY_MATCH)
}

.process_key_args <- function(x, z, d, tag, subtag = NA_integer_, stop_if_filtered = FALSE) {
    # is z is missing then x should contain keys as strings
    if(missing(z) && is.character(x)) {
        # if tag exists, we are going to filter on data type
        if(!missing(tag)) {
            if(length(tag) != 1) {
                stop("when filtering keys in x, tag must have length 1")
            }
            ktag <- get_tag_from_chunk_key(x)
            b <- !is.na(ktag) & ktag == tag
            if(stop_if_filtered && any(!b)) {
                stop(paste0("Some keys passed to .process_keys_args are not of type ", tag))
            }
            x <- x[b]
        }
        return(x)
    }
    create_chunk_key(x, z, d, tag, subtag)
}

.process_key_args2 <- function(x, z, d, tag, subtag = NA_integer_, stop_if_filtered = FALSE) {
    # is z is missing then x should contain keys as strings
    if(missing(z) && is.character(x)) {
        # if tag exists, we are going to filter on data type
        if(!missing(tag)) {
            if(length(tag) == 0L) {
                stop("when filtering keys in x, tag must not be empty")
            }
            ktag <- get_tag_from_chunk_key(x)
            b <- !is.na(ktag) & ktag %in% tag
            if(stop_if_filtered && any(!b)) {
                stop(paste0("Some keys passed to .process_keys_args2 were filtered based on tag."))
            }
            x <- x[b]
        }
        return(x)
    }
    # create keys and interleave tags.
    ret <- character(0L)
    for(atag in tag) {
        keys <- create_chunk_key(x, z, d, atag, subtag)
        ret <- rbind(ret,keys)
    }
    as.vector(ret)
}


#' @importFrom utils head
.create_rawkey_prefix <- function(starts_with) {
    if(is.null(starts_with)) {
        return(NULL)
    }
    stopifnot(length(starts_with) == 1L && is.character(starts_with))

    if(stringr::str_starts(starts_with, pattern=stringr::fixed("@"))) {
        # Chunk-key prefixes must refer to a chunk
        v <- stringr::str_count(starts_with, stringr::fixed(":"))
        if(v < 2) {
            stop("Argument 'starts_with' does not identify a chunk")
        }
        if(v == 2) {
            #append a dummy tag
            starts_with <- paste0(starts_with, ":44")
            res <- chrkeys_to_rawkeys(starts_with)[[1]]
            # strip last byte
            return(head(res,-1))
        }
    }
    chrkeys_to_rawkeys(starts_with)[[1]]
}
