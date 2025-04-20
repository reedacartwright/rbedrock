#' Read and Write NBT Data
#'
#' The Named Binary Tag (NBT) format is used by Minecraft for various data
#' types.
#'
#' `get_nbt_data()` and `get_nbt_value()` load nbt-formatted data from `db`
#' and parses it.
#'
#' `put_nbt_data()` and `put_nbt_value()` store nbt data into `db` in binary
#' form.
#'
#' `read_nbt()` reads NBT data from a `raw` vector.
#'
#' `read_nbt_data()` calls `read_nbt()` on each element of a list.
#'
#' `write_nbt()` encodes NBT data into a `raw` vector.
#'
#' `write_nbt_data()` calls `write_nbt()` on each element of a list.
#'
#' @param db A `bedrockdb` object
#' @param keys A character vector of keys
#' @param key  A single key
#' @param value An nbt object or a list of nbt objects
#' @param values A list of values. Optionally named.
#' @param writeoptions A `bedrock_leveldb_writeoptions` object
#' @param readoptions A `bedrock_leveldb_readoptions` object
#' @param rawvalue A `raw` vector
#' @param rawdata A list of `raw` vectors
#' @param simplify If TRUE, simplifies a list containing a single unnamed
#'        `nbtnode`.
#' @export
get_nbt_data <- function(keys, db = default_db(), readoptions = NULL,
                         simplify = TRUE) {
    dat <- get_data(keys, db = db, readoptions = readoptions)
    read_nbt_data(dat, simplify = simplify)
}

#' @rdname get_nbt_data
#' @export
get_nbt_value <- function(key, db = default_db(), readoptions = NULL,
                          simplify = TRUE) {
    dat <- get_value(key, db = db, readoptions = readoptions)
    read_nbt(dat, simplify = simplify)
}

#' @rdname get_nbt_data
#' @export
put_nbt_data <- function(values, keys, db = default_db(), writeoptions = NULL) {
    dat <- write_nbt_data(values)
    put_data(dat, keys, db = db, writeoptions = writeoptions)
}

#' @rdname get_nbt_data
#' @export
put_nbt_value <- function(value, key, db = default_db(), writeoptions = NULL) {
    dat <- write_nbt(value)
    put_value(dat, key, db = db, writeoptions = writeoptions)
}

#' @rdname get_nbt_data
#' @export
read_nbt <- function(rawvalue, simplify = TRUE) {
    res <- read_rnbt(rawvalue)
    res <- from_rnbt(res)
    if (isTRUE(simplify) && length(res) == 1L && is.null(attributes(res))) {
        res <- res[[1]]
    }
    res
}

#' @rdname get_nbt_data
#' @export
read_nbt_data <- function(rawdata, simplify = TRUE) {
    lapply(rawdata, read_nbt, simplify = simplify)
}

#' @rdname get_nbt_data
#' @export
write_nbt <- function(value) {
    if (is_nbt(value)) {
        value <- list(value)
    }
    value <- to_rnbt(value)
    .Call(Cwrite_nbt, value)
}

#' @rdname get_nbt_data
#' @export
write_nbt_data <- function(values) {
    lapply(values, write_nbt)
}
