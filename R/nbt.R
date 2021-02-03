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
read_nbt <- function (con) {
    if (is.character(con)) {
        con <- file(con, "rb")
        on.exit(close(con))
    } else if (is.raw(con)) {
        con <- rawConnection(con)
        on.exit(close(con))
    }

    .read_nbt_compound_payload(con)
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
new_nbtnode <- function(payload, tag, ...) {
    stopifnot(is.integer(tag))
    cls <- "nbtnode"
    if(bit64::is.integer64(payload)) {
        cls <- c(cls, "integer64")
    }
    structure(payload, class = cls, tag = tag, ...)
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
    structure(node, tag = NULL)
}

#' @export
nbtnode <- function(payload, tag, list_tag=NULL) {
    tag <- as.integer(tag)
    if(tag == 9L && !is.null(list_tag)) {
        attr(payload,"list_tag") <- list_tag
    }
    new_nbtnode(payload, tag)
}

.read_nbt_tag <- function (con) {
    tag <- readBin(con, integer(), size = 1)
    if(length(tag) == 0L) {
        return(0)
    }
    tag
}

.read_nbt_name <- function (con) {
    len <- readBin(con, integer(), size = 2, endian = "little")
    stopifnot(length(len) == 1L)
    name <- readChar(con, len, useBytes = TRUE)
    stopifnot(nchar(name, type="bytes") == len)
    Encoding(name) <- "UTF-8"
    name
}

.read_nbt_compound_payload <- function(con, len = NULL) {
    nbt <- list()
    k <- 1
    repeat {
        tag <- .read_nbt_tag(con)
        if(tag == 0) {
            break
        }
        name <- .read_nbt_name(con)
        payload <- .read_nbt_payload(con, tag)
        nbt[[k]] <- new_nbtnode(payload, tag)
        if(nchar(name) > 0) {
            names(nbt)[k] <- name
        }
        if (!is.null(len) && k == len) {
            break
        }
        k <- k+1
    }
    nbt
}

.read_nbt_unit_payload <- function(con, what, ...) {
    out <- readBin(con, what, n = 1L, endian = "little", ...)
    
    stopifnot(length(out) == 1L)

    if (bit64::is.integer64(what)) {
        oldClass(out) <- "integer64"
    }
    out
}

.read_nbt_array_payload <- function(con, what, ...) {
    len <- readBin(con, integer(), size = 4L, endian = "little")
    stopifnot(length(len) > 0)

    out <- readBin(con, what, n = len, endian = "little", ...)
    
    stopifnot(length(out) == len)

    if(bit64::is.integer64(what)) {
        oldClass(out) <- "integer64"
    }
    out
}

.read_nbt_list_payload <- function (con) {
    ntag <- .read_nbt_tag(con)
    len <- readBin(con, integer(), size = 4L, endian = "little")
    stopifnot((length(len) > 0) && (ntag > 0) == (len > 0) )
    out <- list()
    attr(out,'list_tag') <- ntag
    for(i in seq_len(len)) {
        out[[i]] <- .read_nbt_payload(con,ntag)
    }
    out
}

.read_nbt_payload <- function(con, tag) {
    val <- switch(tag,
        # BYTE
        .read_nbt_unit_payload(con, integer(), size = 1L),
        # SHORT
        .read_nbt_unit_payload(con, integer(), size = 2L),
        # INT
        .read_nbt_unit_payload(con, integer(), size = 4L),
        # LONG
        .read_nbt_unit_payload(con, bit64::integer64(), size = 8L),
        # FLOAT
        .read_nbt_unit_payload(con, numeric(), size = 4L),
        # DOUBLE
        .read_nbt_unit_payload(con, double(), size = 8L),
        # BYTEARRAY
        .read_nbt_array_payload(con, integer(), size = 1L),
        # STRING
        .read_nbt_name(con),
        # LIST
        .read_nbt_list_payload(con),
        # COMPOUND
        .read_nbt_compound_payload(con),
        # INTARRAY
        .read_nbt_array_payload(con, integer(), size = 4L),
        # LONGARRAY
        .read_nbt_array_payload(con, bit64::integer64(), size = 8L)
    )
    val
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
