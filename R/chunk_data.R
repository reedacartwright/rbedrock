#' Read and manipulate chunk keys
#'
#' @description
#' Chunk keys are keys to chunk data. A chunk key has a format which indicates
#' the chunk it holds data for and the type of data it holds. This format is
#' either `chunk:x:z:d:t` or `chunk:x:z:d:t:s`, where `x` and `z` indicates the
#' coordinates of the chunk in chunk space, `d` indicates the dimension of
#' the chunk, and `t` and `s` indicate the tag and subtag of the chunk.
#'
#' @name chunk_keys
NULL

#' @description
#' `parse_chunk_keys()` splits chunk keys into their individual elements and
#' returns a table with the results.
#'
#' @param keys A character vector of database keys.
#'
#' @rdname chunk_keys
#' @examples
#' parse_chunk_keys("chunk:0:0:0:44")
#' parse_chunk_keys("chunk:0:0:0:47:1")
#' @export
parse_chunk_keys <- function(keys) {
    vec_assert(keys, character())
    m <- extract_chunk_key_components(keys)

    tibble::tibble(key = keys,
        x = m[, 1],
        z = m[, 2],
        dimension = m[, 3],
        tag = chunk_tag_str(m[, 4]),
        subtag = m[, 5]
    )
}

#' @description
#' `create_chunk_keys()` returns a vector of chunk keys formed from its
#' arguments.
#'
#' @param x Chunk x-coordinate.
#' @param z Chunk z-coordinate.
#' @param dimension Dimension.
#' @param tag The type of chunk data.
#' @param subtag The subchunk the key refers to (Only used for tag 47).
#' @examples
#' create_chunk_keys(0, 0, 0, 47, 1)
#'
#' @rdname chunk_keys
#' @export
create_chunk_keys <- function(x, z, dimension, tag, subtag) {
    if (is.character(tag)) {
        tag <- chunk_tag_int(tag)
    }
    if (missing(subtag)) {
        subtag <- NA_character_
    }
    args <- vec_recycle_common(x, z, dimension, tag, subtag)
    tag <- ifelse(is.na(args[[5]]), as.character(args[[4]]),
                  paste(args[[4]], args[[5]], sep = ":"))
    paste("chunk", args[[1]], args[[2]], args[[3]], tag, sep = ":")
}

#' @description
#' `chunk_positions()` returns a matrix containing the chunk coordinates of
#' keys.
#' @export
#' @rdname chunk_keys
chunk_positions <- function(keys) {
    extract_chunk_key_components(keys, which = 1:2)
}

#' @description
#' `chunk_origins()` returns a matrix containing the block coordinate of the NW
#' corner of keys.
#' @export
#' @rdname chunk_keys
chunk_origins <- function(keys) {
    pos <- chunk_positions(keys)
    pos * 16L
}

# nolint start: line_length_linter
# List of Tags that identify the contents of a chunk key.
# Most names are consistent with Creator Documentation
# https://docs.microsoft.com/en-us/minecraft/creator/documents/actorstorage#non-actor-data-chunk-key-ids
# nolint end

# nolint start: object_name_linter
.CHUNK_TAGS_CHR <- c(
    "Data3D" = 43L, # introduced in 1.18
    "Version" = 44L,
    "Data2D" = 45L,
    "Data2DLegacy" = 46L,
    "SubChunkBlocks" = 47L,
    "LegacyTerrain" = 48L, # removed
    "BlockEntity" = 49L,
    "Entity" = 50L,
    "PendingTicks" = 51L,
    "LegacyBlockExtraData" = 52L, # removed
    "BiomeState" = 53L,
    "FinalizedState" = 54L,
    "ConversionData" = 55L, # removed
    "BorderBlocks" = 56L, # Education edition
    "HardcodedSpawners" = 57L,
    "RandomTicks" = 58L,
    "Checksums" = 59L, # introduced in 1.16
    "GenerationSeed" = 60L, # introduced in 1.18
    # introduced in 1.18, not used any more (?)
    "GeneratedPreCavesAndCliffsBlending" = 61L,
    # introduced in 1.18, not used any more (?)
    "BlendingBiomeHeight" = 62L,
    "MetaDataHash" = 63L,
    "BlendingData" = 64L,
    "ActorDigestVersion" = 65L,
    "LegacyVersion" = 118L # replaced by 44
)
.CHUNK_TAGS_INV <- as.character(1:128)  # nolint: object_name_linter
.CHUNK_TAGS_INV[.CHUNK_TAGS_CHR] <- names(.CHUNK_TAGS_CHR)
.CHUNK_TAGS <- setNames(1:128, .CHUNK_TAGS_INV)
# nolint end

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

is_chunk_key <- function(keys) {
    startsWith(keys, "chunk:")
}

subset_chunk_keys <- function(keys, negate = FALSE) {
    grep("^chunk:", keys, value = TRUE, invert = negate)
}

# re <- "^chunk:([^:]+):([^:]+):([^:]+):([^:]+)(?:([^:]+))?$" # nolint

extract_chunk_key_components <- function(keys, which = 1:5) {
    b <- startsWith(keys, "chunk:")
    m <- strsplit(keys[b], ":", fixed = TRUE)
    m <- sapply(m, `[`, 2:6)
    if (length(m) > 0L) {
        m <- m[which, , drop = FALSE]
    } else {
        m <- character(0L)
        dim(m) <- c(length(which), 0L)
    }
    ret <- matrix(NA_character_, ncol = length(keys), nrow = nrow(m))
    ret[, b] <- m
    mode(ret) <- "integer"
    t(ret)
}

split_chunk_stems <- function(keys) {
    extract_chunk_key_components(keys, 1:3)
}

get_tag_from_chunk_key <- function(keys, as_string = FALSE) {
    res <- c(extract_chunk_key_components(keys, 4))
    if (as_string) {
        res <- chunk_tag_str(res)
    }
    res
}

get_subtag_from_chunk_key <- function(keys) {
    c(extract_chunk_key_components(keys, 5))
}

get_dimension_from_chunk_key <- function(keys) {
    c(extract_chunk_key_components(keys, 3))
}

trim_stem_from_chunk_key <- function(keys) {
    sub("^chunk:[^:]+:[^:]+:[^:]+:", "", keys)
}

get_stem_from_chunk_key <- function(keys) {
    m <- regexpr("^chunk:[^:]+:[^:]+:[^:]+", keys)
    regmatches(keys, m = m)
}

check_chunk_key_tag <- function(keys, tag, subtag, silent = FALSE) {
    vec_assert(tag, size = 1)
    if (!missing(subtag)) {
        vec_assert(subtag, size = 1)
        m <- extract_chunk_key_components(keys, 4:5)
        b <- (m[, 1] == tag) & (m[, 2] == subtag)
    } else {
        m <- extract_chunk_key_components(keys, 4)
        b <- (m[, 1] == tag)
    }
    # keys that aren't chunk keys or are malformed may have NA here
    b <- b & !is.na(b)
    isgood <- isTRUE(all(b))
    if (isFALSE(silent) && !isgood) {
        abort(paste0("Invalid key found: tag is not ", tag, "."))
    }
    b
}

is_valid_chunk_key <- function(x) {
    grepl("^chunk:-?[0-9]+:-?[0-9]+:-?[0-9]+:-?[0-9]+(:-?[0-9]+)?$", x)
}

is_valid_chunk_key_prefix <- function(x) {
    grepl("^chunk:-?[0-9]+:-?[0-9]+:-?[0-9]+$", x)
}


is_valid_chunk_key_prefix_with_tag <- function(x) { #nolint: object_length_linter
    grepl("^chunk:-?[0-9]+:-?[0-9]+:-?[0-9]+:-?[0-9]+$", x)
}

process_chunk_key_args <- function(x, z, dimension, tag, subtag,
                                   values = NULL) {
    if (missing(x) && !is.null(names(values))) {
        # if x is missing use names from values
        x <- names(values)
    } else if (!missing(z)) {
        # if z is not missing, create keys from x, z, and dimension
        x <- create_chunk_keys(x, z, dimension, tag, subtag)
    }
    x
}

check_chunk_key_args <- function(keys, tag) {
    if (!missing(tag)) {
        tag <- tag[[1]]
        if (!is.na(tag)) {
            ktag <- c(extract_chunk_key_components(keys, 4))
            return(!is.na(ktag) & ktag == tag)
        }
    }
    rep(TRUE, length(keys))
}

#' Load and store raw chunk data
#'
#' * `get_chunk_value()` and `get_chunk_data()` load raw chunk data
#' from `db`. `get_chunk_value()` loads data for a single chunk,
#' and `get_chunk_data()` loads data for multiple chunks.
#' * `put_chunk_value()` and `put_chunk_data()` store chunk
#' data for one or multiple chunks into `db`.
#'
#' @param db A bedrockdb object.
#' @param x,z,dimension Chunk coordinates to extract data from.
#'    `x` can also be a character vector of db keys.
#' @param value A raw vector.
#' @param values A list of raw vectors.

#' @return `get_chunk_value()` returns a single raw vector.
#' `get_chunk_data()` returns a named list of raw data.
#'
#' @name ChunkData
#' @keywords internal
NULL

#' @rdname ChunkData
get_chunk_value <- function(x, z, dimension, tag, subtag, db) {
    key <- process_chunk_key_args(x, z, dimension, tag, subtag)
    b <- check_chunk_key_args(key, tag)
    b[-1] <- FALSE
    get_value(key[b], db = db)
}

#' @rdname ChunkData
get_chunk_data <- function(x, z, dimension, tag, subtag, db) {
    keys <- process_chunk_key_args(x, z, dimension, tag, subtag)
    b <- check_chunk_key_args(keys, tag)
    ret <- vector("list", length(keys))
    names(ret) <- keys
    ret[b] <- get_data(keys[b], db = db)
    ret
}

#' @rdname ChunkData
put_chunk_value <- function(value, x, z, dimension, tag, subtag, db) {
    key <- process_chunk_key_args(x, z, dimension, tag, subtag)
    b <- check_chunk_key_args(key, tag)
    b[-1] <- FALSE
    put_value(value[b], key[b], db = db)
    invisible(b)
}

#' @rdname ChunkData
put_chunk_data <- function(values, x, z, dimension, tag, subtag, db) {
    keys <- process_chunk_key_args(x, z, dimension, tag, subtag,
                                   values = values)
    b <- check_chunk_key_args(keys, tag)
    put_data(values[b], keys[b], db = db)
    invisible(b)
}

#' Load and store NBT chunk data
#'
#' * `get_chunk_nbt_value()` and `get_chunk_nbt_data()` load NBT data for a
#' chunk from `db`. `get_chunk_nbt_value()` loads NBT data for a single chunk,
#' and `get_chunk_nbt_data()` loads data for multiple chunks.
#' * `put_chunk_nbt_value()` and `put_chunk_nbt_data()` store NBT data for
#' one or multiple chunks into `db`.
#'
#' @inheritParams ChunkData
#' @param value A list of NBT objects.
#' @param values A (named) list of list of NBT objects.
#'
#' @return `get_chunk_nbt_value()` returns a list of NBT objects.
#' `get_chunk_nbt_data()` returns a named list of lists of NBT objects.
#'
#' @name ChunkNBTData
#' @keywords internal
NULL

#' @rdname ChunkNBTData
get_chunk_nbt_data <- function(x, z, dimension, tag, subtag, db) {
    dat <- get_chunk_data(x, z, dimension, tag, subtag, db)
    read_nbt_data(dat, simplify = FALSE)
}

#' @rdname ChunkNBTData
get_chunk_nbt_value <- function(x, z, dimension, tag, subtag, db) {
    val <- get_chunk_value(x, z, dimension, tag, subtag, db)
    read_nbt(val, simplify = FALSE)
}

#' @rdname ChunkNBTData
put_chunk_nbt_data <- function(values, x, z, dimension, tag, subtag, db) {
    values <- write_nbt_data(values)
    put_chunk_data(values, x, z, dimension, tag, subtag, db)
}

#' @rdname ChunkNBTData
put_chunk_nbt_value <- function(value, x, z, dimension, tag, subtag, db) {
    value <- write_nbt(value)
    put_chunk_value(value, x, z, dimension, tag, subtag, db)
}
