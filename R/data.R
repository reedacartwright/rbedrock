#' Get a list of keys stored in a bedrockdb.
#'
#' @param db A `bedrockdb` object
#' @param starts_with A string specifying chunk prefix or string prefix.
#' @param readoptions A `bedrock_leveldb_readoptions` object
#'
#' @return
#' A vector containing all the keys found in the bedrockdb.
#'
#' If `starts_with` is specified, this vector will be filtered for
#' based on the specified prefix.
#' @export
get_keys <- function(db, starts_with = NULL, readoptions = NULL) {
    if(!is_null(starts_with)) {
        starts_with <- as.character(starts_with)
        starts_with_raw <- .create_rawkey_prefix(starts_with)
    }
    else {
        starts_with_raw <- starts_with
    }
    rawkeys <- db$keys(starts_with_raw, readoptions)
    res <- rawkeys_to_chrkeys(rawkeys)
    if(!is.null(starts_with)) {
        # filter out keys from the wrong dimension
        res <- str_subset(res, fixed(starts_with))
    }
    res
}

#' Read values stored in a bedrockdb.
#'
#' @param db A `bedrockdb` object
#' @param keys A character vector of keys.
#' @param prefix A string specifying key prefix.
#' @param key  A single key.
#' @param readoptions A `bedrock_leveldb_readoptions` object
#' 
#' @return `get_data()` returns a named-list of raw vectors. `get_value()` returns a raw vector.
#' @export
get_data <- function(keys, db, readoptions = NULL) {
    if(.is_key_prefix(keys)) {
        starts_with <- .create_rawkey_prefix(as.character(keys))
        dat <- db$mget_prefix(starts_with, readoptions)
        dat <- set_names(dat$values, rawkeys_to_chrkeys(dat$keys))
        return(dat)
    }
    rawkeys <- chrkeys_to_rawkeys(keys)
    dat <- db$mget(rawkeys, readoptions)
    set_names(dat, keys)
}

#' @rdname get_data
#' @export
key_prefix <- function(prefix) {
    if(!is.character(prefix) || is.object(prefix)) {
        prefix <- as.character(prefix)
    }
    structure(prefix, class = c("rbedrock_key_prefix", "character"))
}

.is_key_prefix <- function(x) {
    inherits(x, "rbedrock_key_prefix")
}

#' @rdname get_data
#' @export
get_value <- function(key, db, readoptions = NULL) {
    vec_assert(key, character(), 1L)
    rawkey <- chrkeys_to_rawkeys(key)[[1]]
    db$get(rawkey, readoptions)
}

#' @returns `has_values()` returns a logical vector.
#' @rdname get_data
#' @export
has_values <- function(keys, db, readoptions = NULL) {
    rawkeys <- chrkeys_to_rawkeys(keys)
    dat <- db$exists(rawkeys, readoptions)
    rlang::set_names(dat, keys)
}

#' Write values to a bedrockdb.
#'
#' @param db A `bedrockdb` object
#' @param values A list of raw values. If `keys` is missing, the names of `values` will be taken as the keys.
#' @param keys A character vector of keys.
#' @param writeoptions A `bedrock_leveldb_writeoptions` object
#' @param key  A key that will be used to store data.
#' @param value A raw vector that contains the information to be written.
#' 
#' @return An invisible copy of `db`.
#' @export
put_data <- function(values, keys, db, writeoptions = NULL) {
    if(missing(keys) && is_named(values)) {
        # if keys is missing use names from values
        keys <- names(values)
    } else {
        # recycle values if necessary
        values <- vec_recycle(values, length(keys))
    }
    # convert keys and call mput
    rawkeys <- chrkeys_to_rawkeys(keys)
    db$mput(rawkeys, values, writeoptions)
}

#' @rdname put_data
#' @export
put_value <- function(value, key, db, writeoptions = NULL) {
    vec_assert(key, character(), 1L)
    rawkey <- chrkeys_to_rawkeys(key)[[1]]
    db$put(rawkey, value, writeoptions)
}

#' Remove values from a bedrockdb.
#'
#' @param db A `bedrockdb` object
#' @param keys A character vector of keys.
#' @param report A logical indicating whether to generate a report on deleted keys
#' @param readoptions A `bedrock_leveldb_readoptions` object
#' @param writeoptions A `bedrock_leveldb_writeoptions` object
#'
#' @return If `report == TRUE`, a logical vector indicating which keys were deleted.
#' 
#' @export
delete_values <- function(keys, db, report = FALSE, readoptions = NULL, writeoptions = NULL) {
    rawkeys <- chrkeys_to_rawkeys(keys)
    ret <- db$delete(rawkeys, report, readoptions, writeoptions)
    if(!report) {
        return(invisible(ret))
    }
    ret
}
