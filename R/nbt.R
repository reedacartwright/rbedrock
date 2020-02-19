##' @export
read_nbt <- function (con, len = NULL) {
    if (is.character(con)) {
        con <- file(con, "rb")
        on.exit(close(con))
    } else if (is.raw(con)) {
        con <- rawConnection(con)
        on.exit(close(con))
    }
    .read_nbt_compound_payload(con,len)
}

##' @export
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

.read_nbt_type <- function (con) {
    type <- readBin(con, integer(), size = 1)
    if(length(type) == 0L) {
        return(0)
    }
    return(type)
}

.write_nbt_type <- function (con, type) {
    writeBin(type, con, size = 1, endian = "little")
}

.read_nbt_name <- function (con) {
    len <- readBin(con, integer(), size = 2, endian = "little")
    stopifnot(length(len) == 1L)
    name <- readChar(con, len, useBytes = TRUE)
    stopifnot(nchar(name, type="bytes") == len)
    return(name)
}

.write_nbt_name <- function (con, name) {
    #stopifnot(length(name) == 1L)
    if(is.null(name)) {
        name <- ""
    }
    len <- nchar(name, type = "bytes")
    writeBin(len, con, size = 2, endian = "little")
    if(len > 0) {
        writeChar(name, con, eos = NULL, useBytes = TRUE)
    }
}

.read_nbt_compound_payload <- function(con, len = NULL) {
    nbt <- list()
    k = 1
    repeat {
        type <- .read_nbt_type(con)
        if(type == 0) {
            break
        }
        name <- .read_nbt_name(con)
        value <- .read_nbt_payload(con, type)
        nbt[[k]] <- value
        names(nbt)[k] <- name
        if (!is.null(len) && k == len) {
            break
        }
        k <- k+1
    }
    nbt
}

.write_nbt_compound_payload <- function(con, val, writeEnd = TRUE) {
    for (k in seq_along(val)) {
        name <- names(val)[k]
        value <- val[[k]]
        type <- attr(value, "nbt_type", exact = TRUE)
        .write_nbt_type(con, type)
        .write_nbt_name(con, name)
        .write_nbt_payload(con, value, type)
    }
    if (writeEnd) {
        .write_nbt_type(con, 0L)
    }
}

.read_nbt_unit_payload <- function(con, what, ...) {
    out <- readBin(con, what, n = 1L, endian = "little", ...)
    
    stopifnot(length(out) == 1L)

    if (bit64::is.integer64(what)) {
        oldClass(out) <- "integer64"
    }
    out
}

.write_nbt_unit_payload <- function(con, val, ...) {
    stopifnot(length(val) == 1L)
    writeBin(val, con, endian = "little", ...)
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

.write_nbt_array_payload <- function(con, val, ...) {
    len <- length(val)
    writeBin(len, con, size = 4L, endian = "little")
    writeBin(val, con, endian = "little", ...)
}

.read_nbt_list_payload <- function (con) {
    ntype <- .read_nbt_type(con)
    len <- readBin(con, integer(), size = 4L, endian = "little")
    stopifnot((length(len) > 0) && (ntype > 0) == (len > 0) )
    out <- list()
    for(i in seq_len(len)) {
        out[[i]] <- .read_nbt_payload(con,ntype)
    }
    out
}

.write_nbt_list_payload <- function(con, val) {
    ntype <- sapply(val, attr, "nbt_type")
    stopifnot(all(ntype == ntype[1]))
    ntype <- ntype[1]
    len <- length(val)
    .write_nbt_type(con, ntype)
    writeBin(len, con, size = 4L, endian = "little")
    for(v in val) {
        .write_nbt_payload(con, v, ntype)
    }
}

.read_nbt_payload <- function(con, type) {
    val <- switch(type,
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
    attr(val, "nbt_type") <- type
    val
}

.write_nbt_payload <- function(con, val, type) {
    attr(val, "nbt_type") <- NULL
    class(val) <- NULL
    switch(type,
        # BYTE
        .write_nbt_unit_payload(con, val, size = 1L),
        # SHORT
        .write_nbt_unit_payload(con, val, size = 2L),
        # INT
        .write_nbt_unit_payload(con, val, size = 4L),
        # LONG
        .write_nbt_unit_payload(con, val, size = 8L),
        # FLOAT
        .write_nbt_unit_payload(con, val, size = 4L),
        # DOUBLE
        .write_nbt_unit_payload(con, val, size = 8L),
        # BYTEARRAY
        .write_nbt_array_payload(con, val, size = 1L),
        # STRING
        .write_nbt_name(con, val),
        # LIST
        .write_nbt_list_payload(con, val),
        # COMPOUND
        .write_nbt_compound_payload(con, val),
        # INTARRAY
        .write_nbt_array_payload(con, val, size = 4L),
        # LONGARRAY
        .write_nbt_array_payload(con, val, size = 8L)
    )
}
