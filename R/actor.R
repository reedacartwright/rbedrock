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
#' @param rawdata A raw vector.
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
get_acdig_data <- function(x, z, dimension, db) {
    keys <- .process_acdig_key_args(x, z, dimension)
    good_key <- .is_valid_acdig_key(keys)
    ret <- rep(list(NULL), length(keys))
    names(ret) <- keys
    dat <- get_data(db, keys[good_key])
    ret[names(dat)] <- purrr::map(dat, read_acdig_value)
    ret
}

#' @rdname ActorDigest
#' @export
get_acdig_value <- function(x, z, dimension, db) {
    key <- .process_acdig_key_args(x, z, dimension, assert_scalar = TRUE)
    if (!.is_valid_acdig_key(key)) {
        return(NULL)
    }
    dat <- get_value(db, key)
    read_acdig_value(dat)
}

#' @rdname ActorDigest
#' @export
put_acdig_data <- function(values, x, z, dimension, db) {
    keys <- .process_acdig_key_args(x, z, dimension, values = values,
        assert_validity = TRUE)
    values <- purrr::map(values, write_acdig_value)
    put_values(db, keys, values)
}

#' @rdname ActorDigest
#' @export
put_acdig_value <- function(value, x, z, dimension, db) {
    key <- .process_acdig_key_args(x, z, dimension,
        assert_scalar = TRUE, assert_validity = TRUE)
    value <- write_acdig_value(value)
    put_value(db, key, value)
}

#' @rdname ActorDigest
#' @export
read_acdig_value <- function(rawdata) {
    if (is.null(rawdata)) {
        return(NULL)
    }
    vec_assert(rawdata, raw())
    if ((length(rawdata) %% 8) != 0) {
        abort(str_c("Invalid actor digest data. ",
                    "Length of rawdata must be a multiple of 8."))
    }
    m <- matrix(rawdata, nrow = 8)
    if (ncol(m) == 0) {
        character()
    } else {
        str_c("actor:", toupper(apply(m, 2, str_c, collapse = "")))
    }
}

#' @rdname ActorDigest
#' @export
write_acdig_value <- function(value) {
    if (is.null(value)) {
        return(NULL)
    }
    vec_assert(value, character())
    b <- .is_valid_actor_key(value)
    if (!isTRUE(all(b))) {
        abort("Invalid actor key.")
    }
    dat <- str_remove(value, "^actor:")
    dat <- str_extract_all(dat, "..")
    dat <- purrr::map(dat, strtoi, base = 16L)
    as.raw(purrr::as_vector(dat))
}

#' @rdname ActorDigest
#' @export
create_acdig_keys <- function(x, z, dimension) {
    args <- vec_recycle_common(x, z, dimension)
    str_c("acdig", args[[1]], args[[2]], args[[3]], sep = ":")
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
get_actors_data <- function(x, z, dimension, db) {
    keys <- get_acdig_data(x, z, dimension, db)
    purrr::map(keys, function(x) {
        if (is.null(x)) NULL else get_nbt_data(x, db = db)
    })
}

#' @rdname Actors
#' @export
get_actors_value <- function(x, z, dimension, db) {
    keys <- get_acdig_value(x, z, dimension, db)
    if (is.null(keys)) {
        return(NULL)
    }
    get_nbt_data(keys, db = db)
}

#' @rdname Actors
#' @export
put_actors_data <- function(values, x, z, dimension, db) {
    dig_keys <- .process_acdig_key_args(x, z, dimension,
        values = values, assert_validity = TRUE)
    purrr::map2(values, dig_keys, .put_actors_value_impl, db = db)
}

#' @rdname Actors
#' @export
put_actors_value <- function(value, x, z, dimension, db) {
    dig_key <- .process_acdig_key_args(x, z, dimension,
        assert_scalar = TRUE, assert_validity = TRUE)
    .put_actors_value_impl(value, dig_key, db)
}

.put_actors_value_impl <- function(value, dig_key, db) {
    ids <- purrr::map_dbl(value, "UniqueID")
    class(ids) <- "integer64"
    storage_keys <- .make_storagekeys(ids)
    actor_keys <- read_acdig_value(unlist(storage_keys))

    # update storage keys
    nbt_storage_key <- function(k) {
        obj <- nbt_raw_string(k)
        obj <- nbt_compound(StorageKey = obj)
        obj <- nbt_compound(EntityStorageKeyComponent = obj)
        obj <- nbt_compound(internalComponents = obj)
        obj
    }
    nbt_dat <- purrr::map(storage_keys, nbt_storage_key)
    value <- purrr::list_modify(value, !!!nbt_dat)

    dat <- write_nbt_data(value)
    names(dat) <- actor_keys

    dat[[dig_key]] <- write_acdig_value(actor_keys)

    put_data(dat, db = db)

}

.make_storagekeys <- function(ids) {
    vec_assert(ids, bit64::integer64())
    .Call(rbedrock_actor_make_storagekeys, ids)
}

.is_acdig_key <- function(keys) {
    str_starts(keys, pattern = fixed("acdig:"))
}

.is_valid_acdig_key <- function(keys) {
    str_detect(keys, pattern = "^acdig:-?[0-9]+:-?[0-9]+:[0-2]$")
}

.is_actor_key <- function(keys) {
    str_starts(keys, pattern = fixed("actor:"))
}

.is_valid_actor_key <- function(keys) {
    str_detect(keys, pattern = "^actor:[0-9a-fA-F]{16}$")
}

.process_acdig_key_args <- function(x, z, d, values = NULL,
                                    assert_scalar = FALSE,
                                    assert_validity = FALSE) {
    if (missing(x) && is_named(values)) {
        # if x is missing use names from values
        x <- names(values)
    } else if (!missing(z)) {
        # if z is not missing, create keys from x, z, and d
        x <- create_acdig_keys(x, z, d)
    }
    vec_assert(x, character(), size = if (isTRUE(assert_scalar)) 1L else NULL)
    if (isTRUE(assert_validity) && !isTRUE(all(.is_valid_acdig_key(x)))) {
        abort("Invalid acdig key.")
    }
    x
}
