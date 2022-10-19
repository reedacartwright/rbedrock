#' Read and write NBT data
#'
#' The Named Binary Tag (NBT) format is used by Minecraft for various data types.
#'
#' `get_nbt_data()` and `get_nbt_value()` load nbt-formatted data from `db` and parses it.
#'
#' `put_nbt_value()` and `put_nbt_data()` store nbt data into `db` in binary form.
#'
#' `read_nbt()` reads NBT data from a `raw` vector.
#' `read_nbt_data()` calls `read_nbt()` on each element of a list.
#' `write_nbt()` encodes NBT data into a `raw` vector.
#' `write_nbt_data` calls `write_nbt` on each element of a list.
#'
#' @param db A `bedrockdb` object
#' @param keys A character vector of keys.
#' @param key  A single key.
#' @param readoptions A `bedrock_leveldb_readoptions` object.
#' @param simplify If TRUE, simplifies a list containing a single unnamed `nbtnode`.
#' @param value An nbt object.
#' @param values A list of nbt objects.
#' @param writeoptions A `bedrock_leveldb_writeoptions` object.
#' @param rawvalue  A `raw` vector.
#' @param rawvalues A list of `raw` vectors.
#'
#' @export
get_nbt_data <- function(keys, db, readoptions = NULL, simplify = TRUE) {
    dat <- get_data(keys, db, readoptions)
    read_nbt_data(dat, simplify = simplify)
}

#' @rdname get_nbt_data
#' @export
get_nbt_value <- function(key, db, readoptions = NULL, simplify = TRUE) {
    dat <- get_value(key = key, db = db, readoptions = readoptions)
    read_nbt(dat, simplify = simplify)
}

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

#' @rdname get_nbt_data
#' @export
read_nbt <- function(rawvalue, simplify = TRUE) {
    res <- from_rnbt(read_rnbt(rawvalue))
    if(isTRUE(simplify) && length(res) == 1L && is.null(attributes(res))) {
        res <- res[[1]]
    }
    res
}

#' @rdname get_nbt_data
#' @export
read_nbt_data <- function(rawvalues, simplify = TRUE) {
    purrr::map(rawvalues, read_nbt, simplify = simplify)
}

#' @rdname get_nbt_data
#' @export
write_nbt <- function(value) {
    if(is_nbt(value)) {
        value <- list(value)
    }
    write_rnbt(to_rnbt(value))
}

#' @rdname get_nbt_data
#' @export
write_nbt_data <- function(values) {
    purrr::map(values, write_nbt)
}
