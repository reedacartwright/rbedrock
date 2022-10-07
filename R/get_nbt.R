#' Read and Write NBT Data
#'
#' The Named Binary Tag (NBT) format is used by Minecraft for various data types.
#'
#' @description
#' `get_nbt_data()` and `get_nbt_value()` load nbt-formatted data from `db` and parses it.
#'
#' @param db A `bedrockdb` object
#' @param keys A character vector of keys.
#' @param readoptions A `bedrock_leveldb_readoptions` object
#' @param simplify If TRUE, simplifies a list containing a single unnamed `nbtnode`.
#' @export
get_nbt_data <- function(keys, db, readoptions = NULL, simplify = TRUE) {
    dat <- get_data(keys = keys, db = db, readoptions = readoptions)
    read_nbt_data(dat, simplify = simplify)
}

#' @param key  A single key.
#' @rdname get_nbt_data
#' @export
get_nbt_value <- function(key, db, readoptions = NULL, simplify = TRUE) {
    dat <- get_value(key = key, db = db, readoptions = readoptions)
    read_nbt(dat, simplify = simplify)
}

#' @description
#' `put_nbt_value()` and `put_nbt_data()` store nbt data into `db` in binary form.
#'
#' @param value An nbt object.
#' @param values A list of nbt objects
#' @param writeoptions A `bedrock_leveldb_writeoptions` object
#' @rdname get_nbt_data
#' @export
put_nbt_value <- function(value, key, db, writeoptions = NULL) {
    value <- write_nbt(value)
    put_value(value = value, key = key, db = db, writeoptions = writeoptions)
}

#' @rdname get_nbt_data
#' @export
put_nbt_data <- function(values, keys, db, writeoptions = NULL) {
    values <- write_nbt_data(values)
    put_data(values = values, keys = keys, db = db, writeoptions = writeoptions)
}

#' @description
#' `read_nbt` reads NBT data from a `raw` vector.
#'
#' @param rawdata A `raw` vector
#' @rdname get_nbt_data
#' @export
read_nbt <- function(rawdata, simplify = TRUE) {
    res <- read_rnbt(rawdata)
    res <- from_rnbt(res)
    if(isTRUE(simplify) && length(res) == 1L && is.null(attributes(res))) {
        res <- res[[1]]
    }
    res
}

#' @description
#' `read_nbt_data` calls `read_nbt` on each element of a list.
#'
#' @rdname get_nbt_data
#' @export
read_nbt_data <- function(data, simplify = TRUE) {
    purrr::map(data, read_nbt, simplify = simplify)
}

#' @description
#' `write_nbt` encodes NBT data into a `raw` vector.
#'
#' @param object An nbt object or a list of nbt objects
#' @rdname get_nbt_data
#' @export
write_nbt <- function(object) {
    if(is_nbt(object)) {
        object <- list(object)
    }
    object <- to_rnbt(object)
    .Call(Cwrite_nbt, object)
}

#' @description
#' `write_nbt_data` calls `write_nbt` on each element of a list.
#'
#' @rdname get_nbt_data
#' @export
write_nbt_data <- function(data) {
    purrr::map(data, write_nbt)
}
