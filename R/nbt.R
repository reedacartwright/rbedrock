##' @export
read_nbt <- function (val) {
    stopifnot(is.raw(val))
    # return empty list if val is empty
    if (length(val) == 0) {
        return(list())
    }
    con <- rawConnection(val)
    on.exit(close(con))

    out <- .read_nbt_compound_payload(con)

    return(out)
}

# nbt_type = list(
#   END = 0,
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

.read_nbt_name <- function (con) {
    len <- readBin(con, integer(), size = 2, endian="little")
    stopifnot(length(len) == 1L)
    name <- readChar(con, len, useBytes = TRUE)
    stopifnot(nchar(name, type="bytes") == len)
    return(name)
}

.read_nbt_compound_payload <- function(con) {
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
        k <- k+1
    }
    nbt
}

.read_nbt_unit_payload <- function(con, what, ...) {
    out <- readBin(con, what, n = 1L, endian = "little", ...)
    
    stopifnot(length(out) == 1L)

    if(bit64::is.integer64(what)) {
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

.read_nbt_list_payload <- function (con, type) {
    ntype <- .read_nbt_type(con)
    len <- readBin(con, integer(), size = 4L, endian = "little")
    stopifnot((length(len) > 0) && (ntype > 0) == (len > 0) )
    out <- list()
    for(i in seq_len(len)) {
        out[[i]] <- .read_nbt_payload(con,ntype)
    }
    out
}

.read_nbt_payload <- function(con, type) {
    switch(type,
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
        .read_nbt_array_payload(con, integer(), size=1L),
        # STRING
        .read_nbt_name(con),
        # LIST
        .read_nbt_list_payload(con),
        # COMPOUND
        .read_nbt_compound_payload(con),
        # INTARRAY
        .read_nbt_array_payload(con, integer(), size=4L),
        # LONGARRAY
        .read_nbt_array_payload(con, bit64::integer64(), size=8L)
    )
}
