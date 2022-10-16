#' Read and write ActorDigest data
#'
#' Actor digests store a list of all entities in a chunk; however
#' they are not chunk data and use their own prefix. The key format
#' for actor digest data is acdig:x:z:dimension.
#'
#' `get_acdig_data()` and `get_acdig_value()` load ActorDigest
#' data from `db`. They return `NULL` for any key that is not 
#' does not represent ActorDigest data. `get_acdig_value()` supports loading
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
#' @param values A list of character vectors. If named, the names represent db keys.
#' @param rawdata A raw vector.
#'
#' @return `get_acdig_values()` returns a vector of actor keys.
#' `get_acdig_data()` returns a named list of the of the values
#' returned by `get_acdig_value()`.
#' @name ActorDigest
NULL

#' @rdname ActorDigest
#' @export
get_acdig_data <- function(x, z, dimension, db) {
    keys <- .process_acdig_key_args(x, z, dimension)
    good_key <- .is_valid_acdig_key(keys)
    ret <- rep(list(NULL), length(keys))
    names(ret) <- keys
    dat <- get_data(keys[good_key], db = db)
    ret[names(dat)] <- purrr::map(dat, read_acdig_value)
    ret
}

#' @rdname ActorDigest
#' @export
get_acdig_value <- function(x, z, dimension, db) {
    key <- .process_acdig_key_args(x, z, dimension, assert_scalar=TRUE)
    if(!.is_valid_acdig_key(key)) {
        return(NULL)
    }
    dat <- get_value(key, db = db)
    read_acdig_value(dat)
}

#' @rdname ActorDigest
#' @export
put_acdig_data <- function(values, x, z, dimension, db) {
    keys <- .process_acdig_key_args(x, z, dimension, values=values,
        assert_validity = TRUE)
    values <- purrr::map(values, write_acdig_value)
    put_data(values, keys, db = db)
}

#' @rdname ActorDigest
#' @export
put_acdig_value <- function(value, x, z, dimension, db) {
    key <- .process_acdig_key_args(x, z, dimension,
        assert_scalar=TRUE, assert_validity = TRUE)
    value <- write_acdig_value(value)
    put_value(value, key, db = db)
}

#' @rdname ActorDigest
#' @export
read_acdig_value <- function(rawdata) {
    if(is.null(rawdata)) {
        return(NULL)
    }
    vec_assert(rawdata, raw())
    if((length(rawdata) %% 8) != 0) {
        abort("Invalid actor digest data. Length of rawdata must be a multiple of 8.")
    }
    m <- matrix(rawdata, nrow=8)
    if(ncol(m) == 0) {
        character()
    } else {
        str_c("actor:", toupper(apply(m, 2, str_c, collapse="")))
    }
}

#' @rdname ActorDigest
#' @export
write_acdig_value <- function(value) {
    if(is.null(value)) {
        return(NULL)
    }
    vec_assert(value, character())
    b <- .is_valid_actor_key(value)
    if(!isTRUE(all(b))) {
        abort("Invalid actor key.")
    }
    dat <- str_remove(value, "^actor:")
    dat <- str_extract_all(dat, "..")
    dat <- purrr::map(dat, strtoi, base=16L)
    as.raw(purrr::as_vector(dat))
}

#' @rdname ActorDigest
#' @export
create_acdig_keys <- function(x, z, dimension) {
    args <- vec_recycle_common(x, z, dimension)
    str_c("acdig", args[[1]], args[[2]], args[[3]], sep=":")
}


#' Read and write Actor data
#'
#' @param db A bedrockdb object.
#' @param x,z,dimension Chunk coordinates to extract data from.
#'    `x` can also be a character vector of db keys.
#' #@param value A character vector.
#' #@param values A list of character vectors. If named, the names represent db keys.
#' #@param rawdata A raw vector.
#'
#' @name Actors
NULL

#' @rdname Actors
#' @export
get_actors_data <- function(x, z, dimension, db) {
    keys <- get_acdig_data(x, z, dimension, db)
    purrr::map(keys, ~get_nbt_data(keys=., db=db))
}

#' @rdname Actors
#' @export
get_actors_value <- function(x, z, dimension, db) {
    keys <- get_acdig_value(x, z, dimension, db)
    get_nbt_data(keys, db=db)
}

.is_acdig_key <- function(keys) {
    str_starts(keys, pattern=fixed("acdig:"))
}

.is_valid_acdig_key <- function(keys) {
    str_detect(keys, pattern="^acdig:-?[0-9]+:-?[0-9]+:[0-2]$")
}

.is_actor_key <- function(keys) {
    str_starts(keys, pattern=fixed("actor:"))    
}

.is_valid_actor_key <- function(keys) {
    str_detect(keys, pattern="^actor:[0-9a-fA-F]{16}$")
}

.process_acdig_key_args <- function(x, z, dimension, values = NULL, assert_scalar = FALSE, assert_validity = FALSE) {
    if(missing(x) && is_named(values)) {
        # if x is missing use names from values
        x <- names(values)
    } else if(!missing(z)) {
        # if z is not missing, create keys from x, z, and dimension
        x <- create_acdig_keys(x, z, dimension)
    }
    vec_assert(x, character(), size = if(isTRUE(assert_scalar)) 1L else NULL)
    if(isTRUE(assert_validity) && !isTRUE(all(.is_valid_acdig_key(x)))) {
        abort("Invalid actor key.")
    }
    x
}