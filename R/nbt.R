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

#' Read and Write NBT Data
#'
#' @description
#' `read_nbt` reads NBT data from a `raw` vector.
#'
#' @param rawval A `raw` vector of binary data to parse.
#' @param max_elements Maximum number of elements to parse.
#' @param simplify If TRUE, simplifies a list containing a single unnamed `nbtnode`.
#' @param object A single object of class `nbtnode` or a named list of such objects.
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
#' @rdname read_nbt
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
#''
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
    node[] <- object
    node
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
    .write_nbt_tag(con, ntag)
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
            .write_nbt_tag(con, 0L)
        },
        # INTARRAY
        .write_nbt_array_payload(as.integer(object), con, size = 4L),
        # LONGARRAY
        .write_nbt_array_payload(bit64::as.integer64(object), con, size = 8L)
    )
}
