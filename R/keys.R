#' Convert between key types.
#'
#' rbedrock represents database keys two different ways. `chrkeys` are a human-readable
#' format understood by most functions. `rawkeys` are used internally, by the methods
#' of `bedrockdb` objects and `bedrock_leveldb_*` functions.
#'
#' @param keys a character vector of chrkeys or a list or rawkeys
#'  
#' @return `chrkeys_to_rawkeys()` returns a list of raw vectors.
#'
#'         `rawkeys_to_chrkeys()` returns a character vector.
#'
#' @keywords internal
#' @export
chrkeys_to_rawkeys <- function(keys) {
    keys <- vec_cast(keys, character())
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

#' Read and manipulate chunk keys
#'
#' @description
#' Chunk keys are keys to chunk data. A chunk key has a format which indicates
#' the chunk it holds data for and the type of data it holds. This format is
#' either `@@x:z:d:t` or `@@x:z:d:t-s`, where `x` and `z` indicates the 
#' coordinates of the chunk in chunk space, `d` indicates the dimension of
#' the chunk, and `t` and `s` indicate the tag and subtag of the chunk.
#'
#' @name chunk_keys
NULL

#' @description
#' `parse_chunk_keys()` splits chunk keys into their individual elements and
#' returns a table with the results. Keys that do not contain chunk data are
#' silently dropped.
#' 
#' @param keys A character vector of database keys.
#'
#' @rdname chunk_keys
#' @examples
#' parse_chunk_keys("@@0:0:0:47-1")
#' @export
parse_chunk_keys <- function(keys) {
    vec_assert(keys, character())
    m <- keys %>% .subset_chunk_keys() %>% .split_chunk_keys()

    tibble::tibble(key = m[, 1],
        x = as.integer(m[, 2]),
        z = as.integer(m[, 3]),
        dimension = as.integer(m[, 4]),
        tag = chunk_tag_str(as.integer(m[, 5])),
        subtag = as.integer(m[, 6])
    )
}

#' @description
#' `create_chunk_keys()` returns a vector of chunk keys formed from its
#' arguments.
#'
#' @param x Chunk x-coordinate
#' @param z Chunk z-coordinate
#' @param dimension dimension
#' @param tag The type of chunk data.
#' @param subtag The subchunk the key refers to (Only used for tag 47).
#' @examples
#' create_chunk_keys(0, 0, 0, 47, 1)
#'
#' @rdname chunk_keys
#' @export
create_chunk_keys <- function(x, z, dimension, tag, subtag) {
    if(is.character(tag)) {
        tag <- chunk_tag_int(tag)
    }
    if(missing(subtag)) {
        subtag <- NA_character_
    }
    args <- vec_recycle_common(x,z,dimension,tag,subtag)
    tag <- str_c(args[[4]], args[[5]], sep="-") %|% as.character(args[[4]])
    ret <- str_glue("@{args[[1]]}:{args[[2]]}:{args[[3]]}:{tag}")
    as.character(ret)
}

#' @description
#' `chunk_positions()` returns a matrix containing the chunk coordinates of keys.
#' @export
#' @rdname chunk_keys
chunk_positions <- function(keys) {
    pos <- .split_chunk_keys(keys)[,2:3, drop = FALSE]
    mode(pos) <- "integer"
    pos
}

#' @description
#' `chunk_origins()` returns a matrix containing the block coordinate of the NW 
#' corner of keys.
#' @export
#' @rdname chunk_keys
chunk_origins <- function(keys) {
    pos <- chunk_positions(keys)
    pos*16L
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


#' @description
#' `chunk_tag_str()` and `chunk_tag_int()` convert between integer and character
#' representations of chunk tags.
#' @param tags a vector
#' @export
#' @rdname chunk_keys
chunk_tag_str <- function(tags) {
    tags <- vec_cast(tags, integer())
    .CHUNK_TAGS_INV[tags]
}

#' @export
#' @rdname chunk_keys
chunk_tag_int <- function(tags) {
    tags <- vec_cast(tags, character())
    unname(.CHUNK_TAGS[tags])
}

.get_tag_from_chunk_key <- function(keys, as_string = FALSE) {
    m <- str_match(keys, "^@[^:]+:[^:]+:[^:]+:([^:-]+)(?:-[^:]+)?$")
    res <- as.integer(m[,2])
    if(as_string) {
        res <- chunk_tag_str(res)
    }
    res
}

.get_subtag_from_chunk_key <- function(keys) {
    m <- str_match(keys, "^@[^:]+:[^:]+:[^:]+:[^:-]+-([^:]+)$")
    as.integer(m[,2])
}

.get_dimension_from_chunk_key <- function(keys) {
    m <- str_match(keys, "^@[^:]+:[^:]+:([^:]+):[^:-]+(?:-[^:]+)?$")
    as.integer(m[,2])
}

.trim_stem_from_chunk_key <- function(keys) {
    str_replace(keys, "^@[^:]+:[^:]+:[^:]+:", "")
}

.get_stem_from_chunk_key <- function(keys) {
    str_extract(keys, "^@[^:]+:[^:]+:[^:]+")
}

.check_chunk_key_tag <- function(keys, tag, subtag, silent = FALSE) {
    if(missing(subtag)) {
        subtag <- if(tag == 47L) "(?:-[^:-]+)?" else ""
    }
    b <- .is_chunk_key(keys, tag=tag, subtag=subtag)
    isgood <- all(b)
    if(isFALSE(silent) && !isgood) {
        abort(str_glue("Invalid key: tag is not {tag}."))
    }
    isgood
}

.CHUNK_KEY_RE = "^@[^:]+:[^:]+:[^:]+:[^:-]+(?:-[^:-]+)?$"
.CHUNK_KEY_MATCH = "^@([^:]+):([^:]+):([^:]+):([^:-]+)(?:-([^:-]+))?$"
.CHUNK_KEY_TAG_MATCH = "^([^:-]+)(?:-([^:-]+))?$"

.is_chunk_key <- function(keys, tag = "[^:-]+", subtag = "(?:-[^:-]+)?") {
    re <- str_c("^@[^:]+:[^:]+:[^:]+:", tag, subtag)
    str_detect(keys, re)
}

.subset_chunk_keys <- function(keys, negate = FALSE) {
   str_subset(keys, .CHUNK_KEY_RE, negate = negate)
}

.split_chunk_keys <- function(keys) {
    str_match(keys, .CHUNK_KEY_MATCH)
}

.process_key_args <- function(x, z, d, tag, subtag,
    stop_if_filtered = FALSE) {
    # is z is missing then x should contain keys as strings
    if(missing(z) && is.character(x)) {
        # if tag exists, we are going to filter on data type
        if(!missing(tag)) {
            vec_assert(tag, size = 1)
            if(missing(subtag)) {
                subtag <- if(tag == 47L) "(?:-[^:-]+)?" else ""
            }
            vec_assert(subtag, size = 1)

            b <- .is_chunk_key(x, tag=tag, subtag=subtag)
            isgood <- all(b)
            if(stop_if_filtered && !isgood) {
                abort(str_c("Some keys passed to .process_keys_args are not of type ", tag))
            }
            x <- x[b]
        }
        return(x)
    }
    create_chunk_keys(x, z, d, tag, subtag)
}

.process_key_args2 <- function(x, z, d, tag, subtag = NA_integer_,
    stop_if_filtered = FALSE) {
    # is z is missing then x should contain keys as strings
    if(missing(z) && is.character(x)) {
        # if tag exists, we are going to filter on data type
        if(!missing(tag)) {
            vec_assert(tag, size = 1L)
            ktag <- .get_tag_from_chunk_key(x)
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
        keys <- create_chunk_keys(x, z, d, atag, subtag)
        ret <- rbind(ret,keys)
    }
    as.vector(ret)
}

.process_key_args_prefix <- function(x, z, d, stop_if_filtered = FALSE) {
    # is z is missing then x should contain keys as strings
    if(missing(z) && is.character(x)) {
        x <- .get_stem_from_chunk_key(x)
        b <- !is.na(x)
        if(stop_if_filtered && any(!b)) {
            stop("Some keys passed to .process_key_arg_prefix are not chunk keys.")
        }
        return(x[b])
    }
    args <- vec_recycle_common(x, z, d)
    ret <- str_glue("@{args[[1]]}:{args[[2]]}:{args[[3]]}")
    as.character(ret)
}

#' @importFrom utils head
.create_rawkey_prefix <- function(starts_with) {
    if(is.null(starts_with)) {
        return(NULL)
    }
    vec_assert(starts_with, character(), 1L)

    if(str_starts(starts_with, pattern=fixed("@"))) {
        # Chunk-key prefixes must refer to a chunk
        v <- str_count(starts_with, fixed(":"))
        if(v < 2) {
            abort("Argument 'starts_with' does not identify a chunk")
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
