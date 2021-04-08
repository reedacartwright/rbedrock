#' Read and Write NBT Data
#'
#' The Named Binary Tag (NBT) format is used by Minecraft for various data types.
#'
#' @description
#' `get_nbt_data()` and `get_nbt_value()` load nbt-formatted data from `db` and parses it.
#' `get_nbt_values()` is a synonym for `get_nbt_data()`.
#'
#'
#' @param db A `bedrockdb` object
#' @param keys A character vector of keys.
#' @param readoptions A `bedrock_leveldb_readoptions` object
#' @param max_elements Maximum number of elements to parse.
#' @param simplify If TRUE, simplifies a list containing a single unnamed `nbtnode`.
#' @export
get_nbt_data <- function(db, keys, readoptions = NULL, max_elements = NULL, simplify=TRUE) {
    dat <- get_values(db, keys, readoptions = readoptions)
    read_nbt_data(dat, max_elements = max_elements, simplify = simplify)
}

#' @param key  A single key.
#' @rdname get_nbt_data
#' @export
get_nbt_value <- function(db, key, readoptions = NULL, max_elements = NULL, simplify=TRUE) {
    dat <- get_value(db, key, readoptions = readoptions)
    read_nbt(dat, max_elements = max_elements, simplify = simplify)
}

#' @rdname get_nbt_data
#' @export
get_nbt_values <- get_nbt_data

#' @description
#' `put_nbt_values`, `put_nbt_value`, and `put_nbt_data` stores nbt data into `db` in binary form.
#'
#' @param values A list of nbt objects
#' @param writeoptions A `bedrock_leveldb_writeoptions` object
#' @rdname get_nbt_data
#' @export
put_nbt_values <- function(db, keys, values, writeoptions = NULL) {
    dat <- write_nbt_data(values)
    put_values(db, keys, dat, writeoptions = writeoptions)
}

#' @param value An nbt object.
#' @rdname get_nbt_data
#' @export
put_nbt_value <- function(db, key, value, writeoptions = NULL) {
    dat <- write_nbt(value)
    put_value(db, key, dat, writeoptions = writeoptions)
}

#' @rdname get_nbt_data
#' @param data A named-list specifying key-value pairs.
#' @export
put_nbt_data <- function(db, data, writeoptions = NULL) {
    dat <- write_nbt_data(data)
    put_data(db, dat, writeoptions = writeoptions)
}

#' @description
#' `read_nbt` reads NBT data from a `raw` vector.
#'
#' @param rawval A `raw` vector of binary data to parse.
#' @rdname get_nbt_data
#' @export
read_nbt <- function(rawval, max_elements = NULL, simplify = TRUE) {
    if(!is.null(max_elements)) {
        stopifnot(length(max_elements) == 1L && !is.na(max_elements))
    }
    res <- .Call(Cread_nbt, rawval, max_elements)
    if(isTRUE(simplify) && length(res) == 1L && is.null(attributes(res))) {
        res <- res[[1]]
    }
    res
}

#' @description
#' `write_nbt` writes a single `nbtnode` or a list of `nbtnodes` into `raw` vector.
#'
#' @param object A single object of class `nbtnode` or a named list of such objects.
#' @rdname get_nbt_data
#' @export
write_nbt <- function (object) {
    con <- rawConnection(raw(), "wb")
    on.exit(close(con))
    if(inherits(object,"nbtnode")) {
        object <- list(object)
    }
    .write_nbt_compound_payload(object, con)

    rawConnectionValue(con)
}

#' @description
#' `read_nbt_data` calls `read_nbt` on each element of a list.
#'
#' @rdname get_nbt_data
#' @export
read_nbt_data <- function(data, max_elements = NULL, simplify=TRUE) {
    purrr::map(data, read_nbt, max_elements = max_elements, simplify = simplify)
}

#' @description
#' `write_nbt_data` calls `write_nbt` on each element of a list.
#'

#' @rdname get_nbt_data
#' @export
write_nbt_data <- function(data) {
    purrr::map(data, write_nbt)
}

#' NBTnode constructor
#'
#' @description
#' `nbtnode` is a convenient wrapper around `new_nbtnode` that supports
#' type conversion.
#' @param payload A nbtnode payload
#' @param tag A numeric tag for the node.
#' @param list_tag A numeric tag for values stored in the payload of an NBT "list" node.
#' @param ... Additional parameters passed to `structure` when creating the object.
#'
#' @export
nbtnode <- function(payload, tag, list_tag = NULL) {
    tag <- as.integer(tag)
    if(tag == 9L) {
        list_tag <- as.integer(list_tag)
    }
    new_nbtnode(payload, tag, list_tag = list_tag)
}

#' @description
#' `new_nbtnode` creates a new object with class `nbtnode`.
#'
#' @rdname nbtnode
#' @export
new_nbtnode <- function(payload, tag, list_tag = NULL, ...) {
    stopifnot(is.integer(tag) && length(tag) == 1L && !is.na(tag))
    if(!is.null(list_tag)) {
        stopifnot(is.integer(list_tag) && length(list_tag) == 1L && !is.na(list_tag))
    }
    cls <- "nbtnode"
    if(bit64::is.integer64(payload)) {
        cls <- c(cls, "integer64")
    }
    structure(payload, class = cls, tag = tag, list_tag = list_tag, ...)
}

#' Read and write an `nbtnode`'s payload
#'
#' @param object An nbtnode
#' @param value A new payload
#'
#' @export
`payload` <- function(object) {
    UseMethod('payload',object)
}

#' @export
`payload.nbtnode` <- function(object) {
    cls <- class(object)
    cls <- cls[cls != 'nbtnode']
    structure(object, class = cls, tag = NULL, list_tag = NULL)
}

#' @rdname payload
#' @export
`payload<-` <- function(object,value) {
    UseMethod('payload<-',object)
}

#' @export
`payload<-.nbtnode` <- function(object,value) {
    object[] <- value
    object
}

.write_nbt_tag <- function (tag, con) {
    writeBin(tag, con, size = 1, endian = "little")
}

.write_nbt_name <- function (name, con) {
    if(is.null(name)) {
        name <- ""
    }
    stopifnot(length(name) == 1L)
    name <- enc2utf8(name)
    len <- nchar(name, type = "bytes")
    writeBin(len, con, size = 2, endian = "little")
    if(len > 0) {
        writeChar(name, con, eos = NULL, useBytes = TRUE)
    }
}

.write_nbt_compound_payload <- function(object, con) {
    for (k in seq_along(object)) {
        name <- names(object)[k]
        value <- object[[k]]
        tag <- attr(value, "tag", exact = TRUE)
        .write_nbt_tag(tag, con)
        .write_nbt_name(name, con)
        .write_nbt_payload(value, tag, con)
    }
}

.write_nbt_unit_payload <- function(object, con, ...) {
    stopifnot(length(object) == 1L)
    writeBin(as.vector(object), con, endian = "little", ...)
}

.write_nbt_array_payload <- function(object, con, ...) {
    len <- length(object)
    writeBin(len, con, size = 4L, endian = "little")
    writeBin(as.vector(object), con, endian = "little", ...)
}

.write_nbt_list_payload <- function(object, con) {
    ntag <- attr(object, "list_tag")
    len <- length(object)
    .write_nbt_tag(ntag, con)
    writeBin(len, con, size = 4L, endian = "little")
    for(v in object) {
        .write_nbt_payload(v, ntag, con)
    }
}

.write_nbt_payload <- function(object, tag, con) {
    switch(tag,
        # BYTE
        .write_nbt_unit_payload(as.integer(object), con, size = 1L),
        # SHORT
        .write_nbt_unit_payload(as.integer(object), con, size = 2L),
        # INT
        .write_nbt_unit_payload(as.integer(object), con, size = 4L),
        # LONG
        .write_nbt_unit_payload(bit64::as.integer64(object), con, size = 8L),
        # FLOAT
        .write_nbt_unit_payload(as.double(object), con, size = 4L),
        # DOUBLE
        .write_nbt_unit_payload(as.double(object), con, size = 8L),
        # BYTEARRAY
        .write_nbt_array_payload(as.integer(object), con, size = 1L),
        # STRING
        .write_nbt_name(as.character(object), con),
        # LIST
        .write_nbt_list_payload(object, con),
        # COMPOUND
        {
            .write_nbt_compound_payload(object, con)
            .write_nbt_tag(0L, con)
        },
        # INTARRAY
        .write_nbt_array_payload(as.integer(object), con, size = 4L),
        # LONGARRAY
        .write_nbt_array_payload(bit64::as.integer64(object), con, size = 8L)
    )
}

# nbt_type = list(
#     END = 0,
#     BYTE = 1,
#     SHORT = 2,
#     INT = 3,
#     LONG = 4,
#     FLOAT = 5,
#     DOUBLE = 6,
#     BYTE_ARRAY = 7,
#     STRING = 8,
#     LIST = 9,
#     COMPOUND = 10,
#     INT_ARRAY = 11,
#     LONG_ARRAY = 12
# )

tag_assert <- function(x, tag, list_tag) {

    invisible(x)
}

#' @export
#' @keywords internal
new_nbt <- function(x = list(), tag = 0L) {
    vec_assert(tag, ptype = integer(), size = 1L)

    if(tag == 0L) {
        vec_assert(x, ptype = list(), size = 0L)
    } else if(tag == 1L || tag == 2L || tag == 3L) {
        vec_assert(x, ptype = integer(), size = 1L)
    } else if(tag == 4L) {
        vec_assert(x, ptype = bit64::integer64(), size = 1L)
    } else if(tag == 5L || tag == 6L) {
        vec_assert(x, ptype = double(), size = 1L)
    } else if(tag == 7L || tag == 11L ) {
        vec_assert(x, ptype = integer())
    } else if(tag == 8L) {
        vec_assert(x, ptype = character(), size = 1L)
    } else if(tag == 12L) {
        vec_assert(x, ptype = bit64::integer64())
    } else if(tag == 10L) {
        vec_assert(x, ptype = list())
        stopifnot(all(sapply(x, is_nbt)))
    } else {
        stopifnot(is_list_of(x))
        validate_list_of(x)
    }

    new_vctr(x, tag = tag, list_tag = list_tag, class = "rbedrock_nbt")
}

#' @export
nbt <- function(x = list(), tag = 0L) {
    tag <- vec_recycle(vec_cast(tag, integer()), 1L, x_arg = "tag")

    new_nbt(x, tag = tag)
}

tag <- function(x) attr(x, "tag")

tag_str <- function(x) {
    chr <- c("END", "BYTE", "SHORT", "INT", "LONG", "FLOAT",
        "DOUBLE", "BYTE_ARRAY", "STRING", "LIST", "COMPOUND",
        "INT_ARRAY", "LONG_ARRAY")
    chr[1L+tag(x)]
}

#' @export
is_nbt <- function(x) {
    inherits(x, "rbedrock_nbt")
}

#' @export
#' @importFrom vctrs vec_ptype_abbr
vec_ptype_abbr.rbedrock_nbt <- function(x, ...) {
    "nbt"
}

#' @export
#' @importFrom vctrs vec_ptype_full
vec_ptype_full.rbedrock_nbt <- function(x, ...) {
    paste0("rbedrock_nbt<", tag_str(x), ">")
}
