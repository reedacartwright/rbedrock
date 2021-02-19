
#' @export
get_keys <- function(db, starts_with = NULL, readoptions = NULL) {
    if(!is.null(starts_with)) {
        starts_with <- charToRaw(starts_with)
    }
    rawkeys <- db$keys(starts_with, readoptions)
    rawkeys_to_chrkeys(rawkeys)
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
