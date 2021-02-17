
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
rawkeys_to_chrkeys <- function(keys) {
    if(is.raw(keys)) {
        keys <- list(keys)
    }
    .Call(Crawkeys_to_chrkeys, keys)
}

#' @export
chrkeys_to_rawkeys <- function(keys) {
    keys <- as.character(keys)
    .Call(Cchrkeys_to_rawkeys, keys)
}

