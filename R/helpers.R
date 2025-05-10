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

dir_exists <- function(...) {
    utils::file_test("-d", file_path(...))
}

file_exists <- function(...) {
    utils::file_test("-f", file_path(...))
}

path_exists <- function(...) {
    file.exists(file_path(...))
}

is_abs_path <- function(...) {
    path <- file_path(...)
    grepl("^([~/\\]|[A-Za-z]:)", path)
}

# ---- Test Helpers ------------------------------------------------------------

as_raw <- function(...) {
    as.raw(c(...))
}

as_raw_le <- function(...) {
    lst <- list(...)
    if (is.null(names(lst)) && length(lst) == 1 && is.list(lst[[1]])) {
        lst <- lst[[1]]
    }
    nn <- names(lst) %||% rep("", length(lst))
    r <- lapply(seq_along(lst), function(i) {
        n <- nn[[i]]
        v <- lst[[i]]
        if (n == "b") {
            writeBin(as.integer(v), raw(), size = 1, endian = "little")
        } else if (n == "s" || n == "us") {
            writeBin(as.integer(v), raw(), size = 2, endian = "little")
        } else if (n == "i" || n == "ui") {
            writeBin(as.integer(v), raw(), size = 4, endian = "little")
        } else if (n == "l" || n == "ul") {
            writeBin(unclass(bit64::as.integer64(v)), raw(), size = 8,
                     endian = "little")
        } else if (n == "f") {
            writeBin(as.double(v), raw(), size = 4, endian = "little")
        } else if (n == "d") {
            writeBin(as.double(v), raw(), size = 8, endian = "little")
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
    if (is.null(names(lst)) && length(lst) == 1 && is.list(lst[[1]])) {
        lst <- lst[[1]]
    }
    nn <- names(lst) %||% rep("", length(lst))
    r <- lapply(seq_along(lst), function(i) {
        n <- nn[[i]]
        v <- lst[[i]]

        if (n == "b") {
            writeBin(as.integer(v), raw(), size = 1, endian = "big")
        } else if (n == "s" || n == "us") {
            writeBin(as.integer(v), raw(), size = 2, endian = "big")
        } else if (n == "i" || n == "ui") {
            writeBin(as.integer(v), raw(), size = 4, endian = "big")
        } else if (n == "l" || n == "ul") {
            writeBin(unclass(bit64::as.integer64(v)), raw(), size = 8,
                     endian = "big")
        } else if (n == "f") {
            writeBin(as.double(v), raw(), size = 4, endian = "big")
        } else if (n == "d") {
            writeBin(as.double(v), raw(), size = 8, endian = "big")
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

as_raw_varint_u <- function(x) {
    x <- bit64::as.integer64(x)
    unlist(lapply(x, as_raw_varint_1))
}

as_raw_varint_1 <- function(x) {
    x <- bit64::as.integer64(x)
    r <- c()
    repeat {
        b <- as.integer(x %% 128L)
        x <- x %/% 128L
        if (x > 0) {
            b <- b + 128L
        }
        r <- c(r, b)
        if (x == 0) {
            break
        }
    }
    as.raw(r)
}

as_raw_lv <- function(...) {
    lst <- list(...)
    if (is.null(names(lst)) && length(lst) == 1 && is.list(lst[[1]])) {
        lst <- lst[[1]]
    }
    nn <- names(lst) %||% rep("", length(lst))
    r <- lapply(seq_along(lst), function(i) {
        n <- nn[[i]]
        v <- lst[[i]]
        if (n == "b") {
            writeBin(as.integer(v), raw(), size = 1, endian = "little")
        } else if (n %in% c("s", "i", "l")) {
            as_raw_varint_s(v)
        } else if (n %in% c("us", "ui", "ul")) {
            as_raw_varint_u(v)
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

# ---- nbt helpers -------------------------------------------------------------

as_double <- function(x) {
    suppressWarnings(as.double(x))
}

is_int64 <- function(x) {
    # TODO: test the actual value as well
    grepl("^[-+]?[1-9][0-9]*$", x)
}

trunc_int64 <- function(x) {
    sub("\\..*$", "", x)
}

## modified from rlang/standalone-vctrs.R

rac_recycle <- function(x, size) {
    if (is.null(x) || is.null(size)) {
        return(NULL)
    }
    n_x <- length(x)
    if (n_x == size) {
        x
    } else if (size == 0L) {
        rac_slice(x, 0L)
    } else if (n_x == 1L) {
        rac_slice(x, rep(1L, size))
    } else {
        stop("Incompatible lengths: ", n_x, ", ", size, call. = FALSE)
    }
}

rac_recycle_common <- function(xs, size = NULL) {
    sizes <- vapply(xs, length, 0L)
    n <- unique(sizes)
    if (length(n) == 1 && is.null(size)) {
        return(xs)
    }
    n <- setdiff(n, 1L)
    ns <- length(n)
    if (ns == 0) {
        if (is.null(size)) {
            return(xs)
        }
    } else if (ns == 1) {
        size <- size %||% n
        if (n != size) {
            stop("Inputs can't be recycled to `size`.", call. = FALSE)
        }
    } else {
        stop("Inputs can't be recycled to a common size.", call. = FALSE)
    }
    to_recycle <- sizes == 1L
    xs[to_recycle] <- lapply(xs[to_recycle], rac_slice, i = rep(1L, size))
    xs
}

rac_index <- function(x, i, ...) {
    i <- if (missing(i)) TRUE else i
    rac_slice(x, i)
}

rac_slice <- function(x, i) {
    if (is.logical(i)) {
        i <- which(rep(i, length.out = length(x)))
    }
    stopifnot(is.numeric(i) || is.character(i))
    if (is.null(x)) {
        return(NULL)
    }
    out <- rac_data(x)
    out <- out[i, drop = FALSE]
    class(out) <- oldClass(x)
    out
}

rac_assign <- function(x, i, value) {
    if (is.null(x)) {
        return(NULL)
    }
    if (is.logical(i)) {
        i <- which(i)
    }
    stopifnot(is.numeric(i) || is.character(i))
    value <- rac_recycle(value, length(i))
    to <- rac_slice(x, 0L)
    value <- rac_cast(value, to = to)

    x[i] <- value
    x
}


#' Cast a value to a specific type
#'
#' This is a helper function used (mostly) for modifying NBT values.
#'
#' @param x Value to cast
#' @param to Type to cast to
#' @param ... Currently unused.
#' @param x_arg Argument name for `x`, used in error messages.
#' @keywords internal
#' @export
rac_cast <- function(x, to, ..., x_arg = "x") {
    UseMethod("rac_cast", to)
}

#' @export
rac_cast.default <- function(x, to, ..., x_arg = "x") {
    msg <- sprintf("Casting `%s` <%s> to <%s> not implemented.",
                   x_arg, class(x)[1], class(to)[1])
    stop(msg, call. = FALSE)
}

rac_data <- function(x) {
    unclass(x)
}

#' Object type as a string
#'
#' `rac_type_full()` displays the fill type of the object.
#' `rac_type_abbr()` provides an abbreviated description.
#'
#' @param x An object.
#'
#' @keywords internal
#' @export
rac_type_full <- function(x) {
    UseMethod("rac_type_full", x)
}

#' @export
rac_type_full.default <- function(x) {
    class(x)[1]
}

#' @keywords internal
#' @rdname rac_type_full
#' @export
rac_type_abbr <- function(x) {
    UseMethod("rac_type_abbr", x)
}

#' @export
rac_type_abbr.default <- function(x) {
    if (is.recursive(x)) "obj" else "val"
}
