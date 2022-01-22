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
    starts_with_raw <- .create_rawkey_prefix(starts_with)
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
#' `get_values()` and `get_data()` are synonyms.
#'
#' @param db A `bedrockdb` object
#' @param keys A character vector of keys.
#' @param starts_with A string specifying chunk prefix or string prefix.
#' @param key  A single key.
#' @param readoptions A `bedrock_leveldb_readoptions` object
#' 
#' @return `get_values()` returns a named-list of raw vectors.
#' @export
get_values <- function(db, keys, starts_with, readoptions = NULL) {
    if(missing(keys)) {
        starts_with <- .create_rawkey_prefix(starts_with)
        dat <- db$mget_prefix(starts_with, readoptions)
        dat <- rlang::set_names(dat$values, rawkeys_to_chrkeys(dat$keys))
        return(dat)
    }
    rawkeys <- chrkeys_to_rawkeys(keys)
    dat <- db$mget(rawkeys, readoptions)
    rlang::set_names(dat, keys)
}

#' @rdname get_values
#' @export
get_data <- get_values

#' @returns `get_value()` returns a raw vector.
#' @rdname get_values
#' @export
get_value <- function(db, key, readoptions = NULL) {
    if(length(key) != 1 || !is.character(key)) {
        stop("key must be a scalar character value")
    }
    rawkey <- chrkeys_to_rawkeys(key)[[1]]
    db$get(rawkey, readoptions)
}

#' @returns `has_values()` returns a logical vector.
#' @rdname get_values
#' @export
has_values <- function(db, keys, readoptions = NULL) {
    rawkeys <- chrkeys_to_rawkeys(keys)
    dat <- db$exists(rawkeys, readoptions)
    rlang::set_names(dat, keys)
}

#' Write values to a bedrockdb.
#'
#' @param db A `bedrockdb` object
#' @param keys A character vector of keys.
#' @param values A list of raw values.
#' @param writeoptions A `bedrock_leveldb_writeoptions` object
#' 
#' @return An invisible copy of `db`.
#' @export
put_values <- function(db, keys, values, writeoptions = NULL) {
    values <- vec_recycle(values, length(keys))
    
    rawkeys <- chrkeys_to_rawkeys(keys)
    db$mput(rawkeys, values, writeoptions)
}

#' @param key  A key that will be used to store data.
#' @param value A raw vector that contains the information to be written.
#' @rdname put_values
#' @export
put_value <- function(db, key, value, writeoptions = NULL) {
    if(length(key) != 1 || !is.character(key)) {
        stop("key must be a scalar character value")
    }
    rawkey <- chrkeys_to_rawkeys(key)[[1]]
    db$put(rawkey, value, writeoptions)
}

#' @param data A named-list of raw values, specifying key-value pairs.
#' @rdname put_values
#' @export
put_data <- function(db, data, writeoptions = NULL) {
    put_values(db, names(data), data)
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
delete_values <- function(db, keys, report = FALSE, readoptions = NULL, writeoptions = NULL) {
    rawkeys <- chrkeys_to_rawkeys(keys)
    ret <- db$delete(rawkeys, report, readoptions, writeoptions)
    if(!report) {
        return(invisible(ret))
    }
    ret
}
