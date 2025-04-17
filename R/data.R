#' Get a list of keys stored in a bedrockdb.
#'
#' @param db A `bedrockdb` object
#' @param prefix A string specifying chunk prefix or string prefix.
#' @param readoptions A `bedrock_leveldb_readoptions` object
#'
#' @return
#' A vector containing all the keys found in the bedrockdb.
#'
#' If `prefix` is specified, this vector will be filtered for
#' based on the specified prefix.
#' @export
get_keys <- function(prefix = NULL, db = default_db(), readoptions = NULL) {
    # support db being passed via the prefix arg
    if (missing(db) && is_bedrockdb(prefix)) {
        db <- prefix
        prefix <- NULL
    }
    if (!is.null(prefix)) {
        prefix <- as.character(prefix)
    }
    prefix_raw <- create_rawkey_prefix(prefix)
    rawkeys <- db$keys(prefix_raw, readoptions)
    res <- rawkeys_to_chrkeys(rawkeys)
    if (!is.null(prefix)) {
        # filter out keys from the wrong dimension
        res <- grep(prefix, res, fixed = TRUE, value = TRUE)
    }
    res
}

#' @rdname get_data
#' @export
key_prefix <- function(prefix) {
    if(!is.character(prefix) || is.object(prefix)) {
        prefix <- as.character(prefix)
    }
    structure(prefix, class = c("rbedrock_key_prefix", "character"))
}

#' @rdname get_data
#' @export
starts_with <- key_prefix

is_key_prefix <- function(x) {
    inherits(x, "rbedrock_key_prefix")
}

#' Read values stored in a bedrockdb.
#'
#' @param db A `bedrockdb` object
#' @param keys A character vector of keys
#' @param prefix A string specifying key prefix
#' @param key  A single key
#' @param readoptions A `bedrock_leveldb_readoptions` object
#'
#' @return get_data()` returns a named-list of raw vectors.
#' `get_value()` returns a raw vector.
#' @export
get_data <- function(keys, db = default_db(), readoptions = NULL) {
    if(is_key_prefix(keys)) {
        rawprefix <- create_rawkey_prefix(as.character(keys))
        dat <- db$mget_prefix(rawprefix, readoptions)
        ret <- dat$values
        names(ret) <- rawkeys_to_chrkeys(dat$keys)
        return(ret)
    }
    rawkeys <- chrkeys_to_rawkeys(keys)
    ret <- db$mget(rawkeys, readoptions)
    names(ret) <- keys
    ret
}

#' @rdname get_data
#' @export
get_value <- function(key, db = default_db(), readoptions = NULL) {
    rawkey <- chrkeys_to_rawkeys_1(key)
    db$get(rawkey, readoptions)
}

#' @returns `has_values()` returns a logical vector.
#' @rdname get_data
#' @export
has_values <- function(keys, db = default_db(), readoptions = NULL) {
    rawkeys <- chrkeys_to_rawkeys(keys)
    dat <- db$exists(rawkeys, readoptions)
    names(dat) <- keys
    dat
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
    vec_assert(key, character(), 1L)
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
#' @param report A logical indicating whether to generate a report on deleted
#'        keys
#' @param readoptions A `bedrock_leveldb_readoptions` object
#' @param writeoptions A `bedrock_leveldb_writeoptions` object
#'
#' @return If `report == TRUE`, a logical vector indicating which keys were
#' deleted.
#'
#' @export
delete_values <- function(db, keys, report = FALSE, readoptions = NULL,
                          writeoptions = NULL) {
    rawkeys <- chrkeys_to_rawkeys(keys)
    ret <- db$delete(rawkeys, report, readoptions, writeoptions)
    if (!report) {
        return(invisible(ret))
    }
    ret
}
