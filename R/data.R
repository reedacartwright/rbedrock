#' Get a list of keys stored in a bedrockdb.
#'
#' Returns a vector containing keys found in the bedrockdb
#' with a specific `prefix`. If `prefix` is `NULL`, it will
#' return all the keys. If `db` is missing, `get_keys()`,
#' will check if `db` was passed as the first argument.
#'
#' @param db A `bedrockdb` object
#' @param prefix A string specifying chunk prefix or string prefix.
#' @param readoptions A `bedrock_leveldb_readoptions` object
#'
#' @return A character vector.
#'
#' @examples
#' dbpath <- rbedrock_example_world("example1.mcworld")
#' db <- bedrockdb(dbpath)
#' # get all keys in the world
#' keys <- get_keys(db = db)
#' # this also works
#' keys <- get_keys(db)
#' # get all the keys in the world with a prefix
#' keys <- get_keys("plain:VILLAGE", db)
#' close(db)
#' @export
get_keys <- function(prefix = NULL, db = the_db(), readoptions = NULL) {
    if(is_bedrockdb(prefix)) {
        db <- prefix
        prefix <- NULL
    }

    db <- maybe_missing(db, the_db())
    bedrockdb_assert_open(db)
    if(!is_null(prefix)) {
        prefix <- as.character(prefix)
        prefix_raw <- .create_rawkey_prefix(prefix)
    }
    else {
        prefix_raw <- NULL
    }
    rawkeys <- db$keys(prefix_raw, readoptions)
    res <- rawkeys_to_chrkeys(rawkeys)
    if(!is.null(prefix)) {
        # filter out keys from the wrong dimension
        res <- str_subset(res, fixed(prefix))
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
    db <- maybe_missing(db, the_db())
    bedrockdb_assert_open(db)

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
    db <- maybe_missing(db, the_db())
    bedrockdb_assert_open(db)
    vec_assert(key, character(), 1L)
    rawkey <- chrkeys_to_rawkeys(key)[[1]]
    db$get(rawkey, readoptions)
}

#' @returns `has_values()` returns a logical vector.
#' @rdname get_data
#' @export
has_values <- function(keys, db, readoptions = NULL) {
    db <- maybe_missing(db, the_db())
    bedrockdb_assert_open(db)
    rawkeys <- chrkeys_to_rawkeys(keys)
    dat <- db$exists(rawkeys, readoptions)
    rlang::set_names(dat, keys)
}

#' @returns `has_values()` returns a logical vector.
#' @rdname get_data
#' @export
has_value <- function(key, db, readoptions = NULL) {
    db <- maybe_missing(db, the_db())
    bedrockdb_assert_open(db)
    rawkey <- chrkeys_to_rawkeys(key)
    db$exists(rawkey, readoptions)
}

#' Store values to a bedrockdb.
#'
#' @param db A `bedrockdb` object
#' @param values A list of raw values. If `keys` is missing, the names of `values` will be taken as the keys.
#' @param keys A character vector of keys.
#' @param writeoptions A `bedrock_leveldb_writeoptions` object
#' @param key  A key that will be used to store data.
#' @param value A raw vector that contains the information to be written.
#' 
#' @seealso [delete_data] for removing key-values pairs from `db`.
#' @seealso [write_data] for applying a batch of `put` and `delete` operations to `db`.
#'
#' @return An invisible copy of `db`.
#' @export
put_data <- function(values, keys, db, writeoptions = NULL) {
    db <- maybe_missing(db, the_db())
    bedrockdb_assert_open(db)
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
    db <- maybe_missing(db, the_db())
    bedrockdb_assert_open(db)
    vec_assert(key, character(), 1L)
    rawkey <- chrkeys_to_rawkeys(key)[[1]]
    db$put(rawkey, value, writeoptions)
}

#' Remove values from a bedrockdb.
#'
#' @param db A `bedrockdb` object
#' @param keys A character vector of keys.
#' @param key A scalar character.
#' @param writeoptions A `bedrock_leveldb_writeoptions` object
#'
#' @export
delete_data <- function(keys, db, writeoptions = NULL) {
    db <- maybe_missing(db, the_db())
    bedrockdb_assert_open(db)
    rawkeys <- chrkeys_to_rawkeys(keys)
    db$mdelete(rawkeys, writeoptions)
    invisible()
}

#' @rdname delete_data
#' @export
delete_value <- function(key, db, writeoptions = NULL) {
    db <- maybe_missing(db, the_db())
    bedrockdb_assert_open(db)
    vec_assert(key, character(), 1L)
    rawkey <- chrkeys_to_rawkeys(key)[[1]]
    db$delete(rawkey, writeoptions)
    invisible()
}

#' Write values to a bedrockdb.
#'
#' `write_data()` commits a batch of write and delete  
#'
#' @param db A `bedrockdb` object
#' @param values A list of raw values. Use `zap()` or `NULL` to indicate that a value should be delete.
#' If `keys` is missing, the names of `values` will be taken as the keys.
#' @param keys A character vector of keys.
#' @param writeoptions A `bedrock_leveldb_writeoptions` object
#' 
#' @return An invisible copy of `db`.
#' @export
write_data <- function(values, keys, db, writeoptions = NULL) {
    db <- maybe_missing(db, the_db())
    bedrockdb_assert_open(db)
    if(missing(keys) && is_named(values)) {
        keys <- names(values)
    } else {
        # recycle values if necessary
        values <- vec_recycle(values, length(keys))
    }
    vec_assert(values, list())

    # convert keys
    rawkeys <- chrkeys_to_rawkeys(keys)
    # convert zaps
    is_z <- purrr::map_lgl(values, rlang::is_zap)
    values[is_z] <- list(NULL)

    db$write(rawkeys, values, writeoptions)
}
