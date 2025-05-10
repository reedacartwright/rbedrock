#' Read and write Actor Digest Data
#'
#' Actor digests store a list of all entities in a chunk; however
#' they are not chunk data and use their own prefix. The key format
#' for actor digest data is acdig:x:z:dimension.
#'
#' @name ActorDigest
NULL

#' @description
#' `get_acdig_data()` and `get_acdig_value()` load ActorDigest
#' data from `db`.  `get_acdig_value()` supports loading
#' only a single value.
#'
#' `put_acdig_data()` and `put_acdig_value()` store ActorDigest data into `db`.
#'
#' `read_acdig_value()` and `write_acdig_value()` decode and encode
#' ActorDigest data respectively.
#'
#' `create_acdig_keys()` creates keys for ActorDigest data.
#'
#' @param db A bedrockdb object.
#' @param x,z,dimension Chunk coordinates to extract data from.
#'    `x` can also be a character vector of db keys.
#' @param value A character vector.
#' @param values A list of character vectors.
#' If `x` is missing, the names of `values` will be taken as the keys.
#' @param rawvalue A raw vector.
#'
#' @return `get_acdig_values()` returns a vector of actor keys.
#' `get_acdig_data()` returns a named list of the of the values
#' returned by `get_acdig_value()`.
#'
#' @seealso [Actors], [Entity]
#'
#' @name ActorDigest
NULL

#' @rdname ActorDigest
#' @export
get_acdig_data <- function(x, z, dimension, db = default_db()) {
    keys <- process_acdig_key_args(x, z, dimension)
    good_key <- is_valid_acdig_key(keys)
    ret <- rep(list(NULL), length(keys))
    names(ret) <- keys
    dat <- get_data(keys[good_key], db = db)
    ret[names(dat)] <- lapply(dat, read_acdig_value)
    ret
}

#' @rdname ActorDigest
#' @export
get_acdig_value <- function(x, z, dimension, db = default_db()) {
    key <- process_acdig_key_args(x, z, dimension, assert_scalar = TRUE)
    if (!is_valid_acdig_key(key)) {
        return(NULL)
    }
    dat <- get_value(key, db = db)
    read_acdig_value(dat)
}

#' @rdname ActorDigest
#' @export
put_acdig_data <- function(values, x, z, dimension, db = default_db()) {
    keys <- process_acdig_key_args(x, z, dimension, values = values,
                                   assert_validity = TRUE)
    values <- lapply(values, write_acdig_value)
    put_data(values, keys, db = db)
}

#' @rdname ActorDigest
#' @export
put_acdig_value <- function(value, x, z, dimension, db = default_db()) {
    key <- process_acdig_key_args(x, z, dimension,
                                  assert_scalar = TRUE,
                                  assert_validity = TRUE)
    value <- write_acdig_value(value)
    put_value(value, key, db = db)
}

#' @rdname ActorDigest
#' @export
read_acdig_value <- function(rawvalue) {
    if (is.null(rawvalue)) {
        return(NULL)
    }
    stopifnot(is.raw(rawvalue))
    if ((length(rawvalue) %% 8) != 0) {
        stop(paste0("Invalid actor digest data. ",
                    "Length of rawdata must be a multiple of 8."))
    }
    m <- matrix(rawvalue, nrow = 8)
    if (ncol(m) == 0) {
        character()
    } else {
        paste0("actor:", toupper(apply(m, 2, paste0, collapse = "")))
    }
}

#' @rdname ActorDigest
#' @export
write_acdig_value <- function(value) {
    if (is.null(value)) {
        return(NULL)
    }
    stopifnot(is.character(value))
    b <- is_valid_actor_key(value)
    if (!isTRUE(all(b))) {
        stop("Invalid actor key.")
    }
    val <- substr(value, 7, nchar(value))
    val <- strsplit(val, character(0L))
    val <- unlist(val)
    val <- paste0(val[c(TRUE, FALSE)], val[c(FALSE, TRUE)])
    val <- strtoi(val, base = 16L)
    as.raw(val)
}

#' @rdname ActorDigest
#' @export
create_acdig_keys <- function(x, z, dimension) {
    args <- rac_recycle_common(list(x, z, dimension))
    paste("acdig", args[[1]], args[[2]], args[[3]], sep = ":")
}

#' Read and write Actor data
#'
#' After 1.18.30, the nbt data of each actor is saved independently in the
#' database, using a key with a prefix and a 16-character storage key:
#' 'actor:0123456789abcdef'. The keys of all actors in a chunk are saved in an
#' [ActorDigest] record, with format acdig:x:z:dimension'.
#'
#' `get_actors_value()` loads Actors data for a single chunk in `db`.
#' `get_actors_data()` loads Actors data from multiple chunks in `db`.
#'
#' `put_actors_value()` and `put_actors_data()` store one/multiple chunks
#' Actors data into `db` and update the chunks' ActorDigests.
#' When storing Actors data, an actor's storage key will be recalculated from
#' the actor's `UniqueID`. The actor's position and dimension are not verified
#' to be in the chunk it is assigned to.
#'
#' @param db A bedrockdb object.
#' @param x,z,dimension Chunk coordinates to extract data from.
#'    `x` can also be a character vector of db keys.
#' @param value A list of nbt actors data
#' @param values A list of character vectors.
#' If `x` is missing, the names of `values` will be taken as the keys.
#'
#' @seealso [ActorDigest], [Entity]
#'
#' @name Actors
NULL

#' @rdname Actors
#' @export
get_actors_data <- function(x, z, dimension, db = default_db()) {
    keys <- get_acdig_data(x, z, dimension, db)
    lapply(keys, function(x) {
        if (is.null(x)) NULL else get_nbt_data(x, db = db)
    })
}

#' @rdname Actors
#' @export
get_actors_value <- function(x, z, dimension, db = default_db()) {
    keys <- get_acdig_value(x, z, dimension, db)
    if (is.null(keys)) {
        return(NULL)
    }
    get_nbt_data(keys, db = db)
}

#' @rdname Actors
#' @export
put_actors_data <- function(values, x, z, dimension, db = default_db()) {
    dig_keys <- process_acdig_key_args(x, z, dimension,
                                       values = values,
                                       assert_validity = TRUE)
    mapply(put_actors_value_impl, values, dig_keys,
           MoreArgs = list(db = db), SIMPLIFY = FALSE)
}

#' @rdname Actors
#' @export
put_actors_value <- function(value, x, z, dimension, db = default_db()) {
    dig_key <- process_acdig_key_args(x, z, dimension,
                                      assert_scalar = TRUE,
                                      assert_validity = TRUE)
    put_actors_value_impl(value, dig_key, db)
}

put_actors_value_impl <- function(value, dig_key, db) {
    ids <- vapply(value, `[[`, character(1L), "UniqueID", USE.NAMES = FALSE)
    storage_keys <- make_storagekeys(ids)
    actor_keys <- read_acdig_value(unlist(storage_keys))

    # update storage keys
    nbt_storage_key <- function(k) {
        obj <- nbt_raw_string(k)
        obj <- nbt_compound(StorageKey = obj)
        obj <- nbt_compound(EntityStorageKeyComponent = obj)
        obj <- nbt_compound(internalComponents = obj)
        obj
    }
    nbt_dat <- lapply(storage_keys, nbt_storage_key)
    value <- utils::modifyList(value, nbt_dat)

    dat <- write_nbt_data(value)
    names(dat) <- actor_keys

    dat[[dig_key]] <- write_acdig_value(actor_keys)

    put_data(dat, db = db)
}

#' @useDynLib rbedrock R_rbedrock_actor_make_storagekeys
make_storagekeys <- function(ids) {
    stopifnot(is.character(ids))
    .Call(R_rbedrock_actor_make_storagekeys, ids)
}

is_acdig_key <- function(keys) {
    startsWith(keys, "acdig:")
}

is_valid_acdig_key <- function(keys) {
    grepl(keys, pattern = "^acdig:-?[0-9]+:-?[0-9]+:[0-2]$")
}

is_actor_key <- function(keys) {
    startsWith(keys, "actor:")
}

is_valid_actor_key <- function(keys) {
    grepl(keys, pattern = "^actor:[0-9a-fA-F]{16}$")
}

process_acdig_key_args <- function(x, z, d, values = NULL,
                                   assert_scalar = FALSE,
                                   assert_validity = FALSE) {
    if (missing(x) && is_named(values)) {
        # if x is missing use names from values
        x <- names(values)
    } else if (!missing(z)) {
        # if z is not missing, create keys from x, z, and d
        x <- create_acdig_keys(x, z, d)
    }
    stopifnot(is.character(x))
    if (isTRUE(assert_scalar)) {
        stopifnot(length(x) == 1L)
    }
    if (isTRUE(assert_validity) && !isTRUE(all(is_valid_acdig_key(x)))) {
        abort("Invalid acdig key.")
    }
    x
}
