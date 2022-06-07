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
#' data from `db`. `get_acdig_data()` will silently drop and keys not
#' representing ActorDigest data. `get_acdig_value()` supports loading
#' only a single value. `get_acdig_values()` is a synonym for 
#' `get_acdig_data()`.
#'
#' `put_acdig_values()`, `put_acdig_value()`, and
#' `put_acdig_data()` store ActorDigest data into `db`.
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
#' @param data A named-list of character vectors.
#' @param rawdata A raw vector.
#'
#' @return `get_acdig_values()` returns a vector of actor keys.
#' `get_acdig_data()` returns a named list of the of the values
#' returned by `get_acdig_value()`.
#' 
#' @rdname ActorDigest
#' @export
get_acdig_data <- function(db, x, z, dimension) {
    keys <- .process_acdig_key_args(x, z, dimension)
    dat <- get_data(db, keys)
    purrr::map(dat, read_acdig_value)
}

#' @rdname ActorDigest
#' @export
get_acdig_values <- get_acdig_data

#' @rdname ActorDigest
#' @export
get_acdig_value <- function(db, x, z, dimension) {
    key <- .process_acdig_key_args(x, z, dimension)
    vec_assert(key, character(), 1L)
    dat <- get_value(db, key)
    read_acdig_value(dat)
}

#' @rdname ActorDigest
#' @export
put_acdig_data <- function(db, data) {
    put_acdig_values(db, x=names(data), values=data) 
}

#' @rdname ActorDigest
#' @export
put_acdig_values <- function(db, x, z, dimension, values) {
    keys <- .process_acdig_key_args(x, z, dimension, stop_if_filtered = TRUE)
    values <- purrr::map(values, write_acdig_value)
    put_values(db, keys, values)
}

#' @rdname ActorDigest
#' @export
put_acdig_value <- function(db, x, z, dimension, value) {
    key <- .process_acdig_key_args(x, z, dimension)
    vec_assert(key, character(), 1L)
    value <- write_acdig_value(value)
    put_value(db, key, value)
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
        str_c("actor:", apply(m, 2, str_c, collapse=""))
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

.process_acdig_key_args <- function(x, z, d, stop_if_filtered = FALSE) {
    # is z is missing then x should contain keys as strings
    if(missing(z) && is.character(x)) {
        # filter 
        b <- .is_valid_acdig_key(x)
        if(isTRUE(stop_if_filtered) && !isTRUE(all(b))) {
            abort("Invalid key. Key is not an acdig key.")            
        }
        return(x[b])
    }
    create_acdig_keys(x, z, d)
}
