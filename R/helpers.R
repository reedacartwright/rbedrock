read_file_raw <- function(file, ...) {
    readBin(file, "raw", file.info(file)$size, ...)
}

write_file_raw <- function(x, file, ...) {
    writeBin(x, file, ...)
}

file_path <- function(...) {
    file.path(..., fsep = "/")
}

normalize_path <- function(...) {
    normalizePath(file_path(...), winslash = "/", mustWork = FALSE)
}


# ---- Test Helpers ------------------------------------------------------------

as_raw <- function(...) {
    as.raw(c(...))
}

as_raw_le <- function(...) {
    lst <- list(...)
    nn <- names(lst) %||% rep("", length(lst))
    r <- lapply(seq_along(lst), function(i) {
        n <- nn[[i]]
        v <- lst[[i]]
        if (n == "b") {
            writeBin(as.integer(v), raw(), size = 1, endian = "little")
        } else if (n == "s") {
            writeBin(as.integer(v), raw(), size = 2, endian = "little")
        } else if (n == "i") {
            writeBin(as.integer(v), raw(), size = 4, endian = "little")
        } else if (n == "f") {
            writeBin(as.double(v), raw(), size = 4, endian = "little")
        } else if (n == "d") {
            writeBin(as.double(v), raw(), size = 8, endian = "little")
        } else if (n == "l") {
            writeBin(unclass(bit64::as.integer64(v)), raw(), size = 8, endian = "little")
        } else if (is.character(v)) {
            k <- nchar(v, type = "bytes")
            k <- writeBin(k, raw(), size = 2, endian = "little")
            c(k, charToRaw(v))        
        } else {
            writeBin(as.integer(v), raw(), size = 4, endian = "little")
        }
    })
    unlist(r)
}

as_raw_be <- function(...) {
    lst <- list(...)
    nn <- names(lst) %||% rep("", length(lst))
    r <- lapply(seq_along(lst), function(i) {
        n <- nn[[i]]
        v <- lst[[i]]

        if (n == "b") {
            writeBin(as.integer(v), raw(), size = 1, endian = "big")
        } else if (n == "s") {
            writeBin(as.integer(v), raw(), size = 2, endian = "big")
        } else if (n == "i") {
            writeBin(as.integer(v), raw(), size = 4, endian = "big")
        } else if (n == "f") {
            writeBin(as.double(v), raw(), size = 4, endian = "big")
        } else if (n == "d") {
            writeBin(as.double(v), raw(), size = 8, endian = "big")
        } else if (n == "l") {
            writeBin(unclass(bit64::as.integer64(v)), raw(), size = 8, endian = "big")
        } else if (is.character(v)) {
            k <- nchar(v, type = "bytes")
            k <- writeBin(k, raw(), size = 2, endian = "big")
            c(k, charToRaw(v))
        } else {
            writeBin(as.integer(v), raw(), size = 4, endian = "big")
        }
    })
    unlist(r)
}

as_raw_varint_s <- function(x) {
    x <- bit64::as.integer64(x)
    cls <- class(x)
    x <- ifelse(x < 0, -2 * x - 1, 2 * x)
    class(x) <- cls
    unlist(lapply(x, as_raw_varint_1))
}

as_raw_varint_1 <- function(x) {
    x <- bit64::as.integer64(x)
    r <- c()
    repeat {
        b <- as.integer(x %% 128L)
        x <- x %/% 128L
        if(x > 0) {
            b <- b + 128L
        }
        r <- c(r, b)
        if(x == 0) {
            break
        }
    }
    as.raw(r)
}

as_raw_lv <- function(...) {
    lst <- list(...)
    nn <- names(lst) %||% rep("", length(lst))
    r <- lapply(seq_along(lst), function(i) {
        n <- nn[[i]]
        v <- lst[[i]]
        if (n == "b") {
            writeBin(as.integer(v), raw(), size = 1, endian = "little")
        } else if (n %in% c("s", "i", "l")) {
            as_raw_varint_s(v)
        } else if (n == "f") {
            writeBin(as.double(v), raw(), size = 4, endian = "little")
        } else if (n == "d") {
            writeBin(as.double(v), raw(), size = 8, endian = "little")
        } else if (is.character(v)) {
            k <- nchar(v, type = "bytes")
            k <- as_raw_varint_1(k)
            c(k, charToRaw(v))        
        } else {
            as_raw_varint_s(v)
        }
    })
    unlist(r)
}
