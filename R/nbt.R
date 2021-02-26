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

#' @export
write_nbt <- function (con, val) {
    ret_val <- FALSE
    if (is.raw(con)) {
        con <- rawConnection(con, "wb")
        on.exit(close(con))
        ret_val <- TRUE
    } else if (is.character(con)) {
        con <- file(con, "wb")
        on.exit(close(con))
    }
    .write_nbt_compound_payload(con, val, writeEnd = FALSE)
    if(ret_val) {
        return(rawConnectionValue(con))
    }
}

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

#' @export
nbtnode <- function(payload, tag, list_tag = NULL) {
    tag <- as.integer(tag)
    if(tag == 9L) {
        list_tag <- as.integer(list_tag)
    }
    new_nbtnode(payload, tag, list_tag = list_tag)
}

#' @export
`payload<-` <- function(node,value) {
    UseMethod('payload<-',node)
}

#' @export
`payload<-.nbtnode` <- function(node,value) {
    node[] <- value
    node
}

#' @export
`payload` <- function(node) {
    UseMethod('payload',node)
}

#' @export
`payload.nbtnode` <- function(node) {
    structure(node, tag = NULL, list_tag = NULL)
}

.write_nbt_tag <- function (con, tag) {
    writeBin(tag, con, size = 1, endian = "little")
}

.write_nbt_name <- function (con, name) {
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

.write_nbt_compound_payload <- function(con, val, writeEnd = TRUE) {
    for (k in seq_along(val)) {
        name <- names(val)[k]
        value <- val[[k]]
        tag <- attr(value, "tag", exact = TRUE)
        .write_nbt_tag(con, tag)
        .write_nbt_name(con, name)
        .write_nbt_payload(con, payload(value), tag)
    }
    if (writeEnd) {
        .write_nbt_tag(con, 0L)
    }
}

.write_nbt_unit_payload <- function(con, val, ...) {
    stopifnot(length(val) == 1L)
    writeBin(as.vector(val), con, endian = "little", ...)
}

.write_nbt_array_payload <- function(con, val, ...) {
    len <- length(val)
    writeBin(len, con, size = 4L, endian = "little")
    writeBin(as.vector(val), con, endian = "little", ...)
}

.write_nbt_list_payload <- function(con, val) {
    ntag <- attr(val, "list_tag")
    len <- length(val)
    .write_nbt_tag(con, ntag)
    writeBin(len, con, size = 4L, endian = "little")
    for(v in val) {
        .write_nbt_payload(con, v, ntag)
    }
}

.write_nbt_payload <- function(con, val, tag) {
    switch(tag,
        # BYTE
        .write_nbt_unit_payload(con, as.integer(val), size = 1L),
        # SHORT
        .write_nbt_unit_payload(con, as.integer(val), size = 2L),
        # INT
        .write_nbt_unit_payload(con, as.integer(val), size = 4L),
        # LONG
        .write_nbt_unit_payload(con, bit64::as.integer64(val), size = 8L),
        # FLOAT
        .write_nbt_unit_payload(con, as.double(val), size = 4L),
        # DOUBLE
        .write_nbt_unit_payload(con, as.double(val), size = 8L),
        # BYTEARRAY
        .write_nbt_array_payload(con, as.integer(val), size = 1L),
        # STRING
        .write_nbt_name(con, as.character(val)),
        # LIST
        .write_nbt_list_payload(con, val),
        # COMPOUND
        .write_nbt_compound_payload(con, val),
        # INTARRAY
        .write_nbt_array_payload(con, as.integer(val), size = 4L),
        # LONGARRAY
        .write_nbt_array_payload(con, bit64::as.integer64(val), size = 8L)
    )
}
