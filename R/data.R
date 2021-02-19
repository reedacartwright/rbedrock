#' @export
get_keys <- function(db, starts_with = NULL, readoptions = NULL) {
    starts_with_raw <- .create_rawkey_prefix(starts_with)
    rawkeys <- db$keys(starts_with_raw, readoptions)
    res <- rawkeys_to_chrkeys(rawkeys)
    if(!is.null(starts_with)) {
        # filter out keys from the wrong dimension
        res <- stringr::str_subset(res, stringr::fixed(starts_with))
    }
    res
}

#' @export
get_values <- function(db, keys, readoptions = NULL) {
    rawkeys <- chrkeys_to_rawkeys(keys)
    dat <- db$mget(rawkeys, readoptions)
    rlang::set_names(dat, keys)
}

#' @export
get_value <- function(db, key, readoptions = NULL) {
    if(length(key) != 1 || !is.character(key)) {
        stop("key must be a scalar character value")
    }
    rawkey <- chrkeys_to_rawkeys(key)[[1]]
    db$get(rawkey, readoptions)
}

#' @export
put_values <- function(db, data, keys = names(data), writeoptions = NULL) {
    rawkeys <- chrkeys_to_rawkeys(keys)
    db$mput(keys, data, writeoptions)
}

#' @export
put_value <- function(db, key, value, writeoptions = NULL) {
    if(length(key) != 1 || !is.character(key)) {
        stop("key must be a scalar character value")
    }
    rawkey <- chrkeys_to_rawkeys(key)[[1]]
    db$put(rawkey, value, writeoptions)
}
