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
#' returns a table with the results. Keys that do not contain chunk data are
#' silently dropped.
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
    keys <- .subset_chunk_keys(keys) 
    m <- str_split(keys, fixed(":"), simplify=TRUE)
    m <- m[,-1, drop=FALSE]
    if(ncol(m) == 4) {
        m <- cbind(m, NA_character_)
    }

    tibble::tibble(key = keys,
        x = as.integer(m[, 1]),
        z = as.integer(m[, 2]),
        dimension = as.integer(m[, 3]),
        tag = chunk_tag_str(as.integer(m[, 4])),
        subtag = as.integer(m[, 5])
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
    if(is.character(tag)) {
        tag <- chunk_tag_int(tag)
    }
    if(missing(subtag)) {
        subtag <- NA_character_
    }
    args <- vec_recycle_common(x,z,dimension,tag,subtag)
    tag <- str_c(args[[4]], args[[5]], sep=":") %|% as.character(args[[4]])

    str_c("chunk", args[[1]], args[[2]], args[[3]], tag, sep=":")
}

#' @description
#' `chunk_positions()` returns a matrix containing the chunk coordinates of keys.
#' @export
#' @rdname chunk_keys
chunk_positions <- function(keys) {
    .extract_chunk_key_components(keys, which=1:2)
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

#' @export
#' @rdname chunk_keys
filter_chunk_keys <- function(keys, tag) {
    b <- .check_chunk_key_tag(keys, tag = tag, silent = TRUE)
    keys[b]
}

# List of Tags that identify the contents of a chunk key.
# Most names are consistent with Creator Documentation
# https://docs.microsoft.com/en-us/minecraft/creator/documents/actorstorage#non-actor-data-chunk-key-ids
.CHUNK_TAGS_CHR <- c(
    "Data3D" = 43L, # introduced in 1.18
    "Version" = 44L,
    "Data2D" = 45L,
    "Data2DLegacy" = 46L,
    "SubChunkBlocks" = 47L,
    "LegacyTerrain" = 48L, # removed
    "BlockEntity" = 49L,
    "Entity"= 50L,
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
    "GeneratedPreCavesAndCliffsBlending" = 61L, # introduced in 1.18, not used any more (?)
    "BlendingBiomeHeight" = 62L, # introduced in 1.18, not used any more (?)
    "MetaDataHash" = 63L,
    "BlendingData" = 64L,
    "ActorDigestVersion" = 65L,
    "LegacyVersion" = 118L # replaced by 44
)
.CHUNK_TAGS_INV <- as.character(1:128)
.CHUNK_TAGS_INV[.CHUNK_TAGS_CHR] <- names(.CHUNK_TAGS_CHR)
.CHUNK_TAGS <- setNames(1:128, .CHUNK_TAGS_INV)

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

.is_chunk_key <- function(keys) {
    str_starts(keys, pattern=fixed("chunk:"))
}

.subset_chunk_keys <- function(keys, negate = FALSE) {
   str_subset(keys, "^chunk:", negate = negate)
}

.extract_chunk_key_components <- function(keys, which=1:5) {
    m <- str_split(keys, fixed(":"), simplify=TRUE)
    if(length(m) == 0) {
        dim(m) <- c(0L,6L)
    }
    is_chunk <- m[,1] == "chunk"
    ret <- m[, which+1, drop=FALSE]
    ret[!is_chunk,] <- NA_character_
    mode(ret) <- "integer"
    ret
}

.split_chunk_stems <- function(keys) {
    .extract_chunk_key_components(keys, 1:3)
}

.get_tag_from_chunk_key <- function(keys, as_string = FALSE) {
    res <- c(.extract_chunk_key_components(keys, 4))
    if(as_string) {
        res <- chunk_tag_str(res)
    }
    res
}

.get_subtag_from_chunk_key <- function(keys) {
    c(.extract_chunk_key_components(keys, 5))
}

.get_dimension_from_chunk_key <- function(keys) {
    c(.extract_chunk_key_components(keys, 3))
}

.trim_stem_from_chunk_key <- function(keys) {
    str_replace(keys, "^chunk:[^:]+:[^:]+:[^:]+:", "")
}

.get_stem_from_chunk_key <- function(keys) {
    str_extract(keys, "^chunk:[^:]+:[^:]+:[^:]+")
}

.check_chunk_key_tag <- function(keys, tag, subtag, silent = FALSE) {
    vec_assert(tag, size = 1)
    if(!missing(subtag)) {
        vec_assert(subtag, size = 1)
        m <- .extract_chunk_key_components(keys, 4:5)
        b <- (m[,1] == tag) & (m[,2] == subtag)
    } else {
        m <- .extract_chunk_key_components(keys, 4)
        b <- (m[,1] == tag)
    }
    # keys that aren't chunk keys or are malformed will may have NA here
    b <- b & !is.na(b)
    isgood <- isTRUE(all(b))
    if(isFALSE(silent) && !isgood) {
        abort(str_glue("One or more keys have a tag that is not {tag}."))
    }
    b
}

.is_valid_chunk_key <- function(keys) {
    str_detect(keys, pattern="^chunk:-?[0-9]+:-?[0-9]+:-?[0-9]+:-?[0-9]+(:-?[0-9]+)?$")
}

.process_chunk_key_args <- function(x, z, dimension, tag, values = NULL, assert_scalar = FALSE,
    assert_validity = FALSE) {
    if(missing(x) && is_named(values)) {
        # if x is missing use names from values
        x <- names(values)
    } else if(!missing(z)) {
        # if z is not missing, create keys from x, z, and dimension
        x <- create_chunk_keys(x, z, dimension, tag)
    }
    vec_assert(x, character(), size = if(isTRUE(assert_scalar)) 1L else NULL)
    if(isTRUE(assert_validity)) {
        if(!missing(tag)) {
            .check_chunk_key_tag(x, tag, silent = FALSE)
        }
        if(!isTRUE(all(.is_valid_chunk_key(x)))) {
            abort("One or more chunk keys is invalid.")
        }
    }
    x
}

.process_key_args <- function(x, z, dimension, tag, subtag,
    stop_if_filtered = FALSE) {
    # is z is missing then x should contain keys as strings
    if(missing(z) && is.character(x)) {
        # if tag exists, we are going to filter on data type
        if(!missing(tag)) {
            b <- .check_chunk_key_tag(x, tag, subtag, silent=!stop_if_filtered)
            x <- x[b]
        }
        return(x)
    }
    create_chunk_keys(x, z, dimension, tag, subtag)
}

.process_key_args_prefix <- function(x, z, dimension, stop_if_filtered = FALSE) {
    # is z is missing then x should contain keys as strings
    if(missing(z) && is.character(x)) {
        x <- .get_stem_from_chunk_key(x)
        b <- !is.na(x)
        if(stop_if_filtered && any(!b)) {
            stop("Some keys passed to .process_key_arg_prefix are not chunk keys.")
        }
        return(x[b])
    }
    args <- vec_recycle_common(x, z, dimension)

    str_c("chunk", args[[1]], args[[2]], args[[3]], sep=":")
}

.get_chunk_data <- function(x, z, dimension, db, tag) {
    keys <- .process_chunk_key_args(x, z, dimension, tag = tag, assert_validity = TRUE)
    get_data(keys, db)

}

.get_chunk_value <- function(x, z, dimension, db, tag) {
    key <- .process_chunk_key_args(x, z, dimension, tag = tag, assert_validity = TRUE, assert_scalar = TRUE)
    get_value(key, db)
}

.put_chunk_data <- function(values, x, z, dimension, db, tag) {
    keys <- .process_chunk_key_args(x, z, dimension, tag = tag, values = values, assert_validity = TRUE)
    put_data(values = values, keys=keys, db = db)
}

.put_chunk_value <- function(value, x, z, dimension, db, tag) {
    key <- .process_chunk_key_args(x, z, dimension, tag = tag, assert_validity = TRUE, assert_scalar = TRUE)
    put_value(value=value, key = key, db = db)
}

.get_chunk_nbt_data <- function(x, z, dimension, db, tag) {
    keys <- .process_chunk_key_args(x, z, dimension, tag = tag, assert_validity = TRUE)
    get_nbt_data(keys, db, simplify = FALSE)

}

.get_chunk_nbt_value <- function(x, z, dimension, db, tag) {
    key <- .process_chunk_key_args(x, z, dimension, tag = tag, assert_validity = TRUE, assert_scalar = TRUE)
    get_nbt_value(key, db, simplify = FALSE)
}

.put_chunk_nbt_data <- function(values, x, z, dimension, db, tag) {
    keys <- .process_chunk_key_args(x, z, dimension, tag = tag, values = values, assert_validity = TRUE)
    put_nbt_data(values = values, keys=keys, db = db)
}

.put_chunk_nbt_value <- function(value, x, z, dimension, db, tag) {
    key <- .process_chunk_key_args(x, z, dimension, tag = tag, assert_validity = TRUE, assert_scalar = TRUE)
    put_nbt_value(value=value, key = key, db = db)
}
