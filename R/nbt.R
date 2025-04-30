#' Create an NBT value
#'
#' The Named Binary Tag (NBT) format is used by Minecraft for various data
#' types. An NBT value holds a 'payload' of data and a 'tag' indicating the type
#' of data held.
#'
#' - `nbt_*()` family of functions create nbt data types.
#' - `unnbt()` recursively strips NBT metadata from an NBT value.
#'
#' @param x An nbt payload.
#'
#' @name nbt
NULL

# ---- nbt ---------------------------------------------------------------------

# Use something like lobstr::tree to display NBT data

#' @export
print.rbedrock_nbt <- function(x, ...) {
    cat("NBT formatted data...\n")
    x
}

#' @rdname nbt
#' @export
nbt <- function(...) {
    lst <- list(...)
    validate_nbt(new_nbt(lst))
}

new_nbt <- function(x) {
    stopifnot(is.list(x))
    structure(x, class = "rbedrock_nbt")
}

validate_nbt <- function(x) {
    x
}

#' @export
format.rbedrock_nbt_numeric <- function(x, suffix = "", ...) {
    out <- formatC(unclass(x), ...)
    out[is.na(x)] <- NA
    out[!is.na(x)] <- paste0(out[!is.na(x)], suffix)
    out
}

# ---- nbt_byte [1] ------------------------------------------------------------

#' @rdname nbt
#' @export
nbt_byte <- function(x) {
    validate_nbt_byte(new_nbt_byte(trunc(as.double(x))))
}

validate_nbt_byte <- function(x) {
    p <- unclass(x)
    if (any(is.na(p) | p > 127 | p < -128 )) {
        stop("`x` cannot be coerced into an 8-bit signed integer",
             call. = FALSE)
    }
    x
}

new_nbt_byte <- function(x) {
    stopifnot(is.double(x) && length(x) == 1)
    structure(x, class = c("rbedrock_nbt_byte", "rbedrock_nbt_numeric",
        "rbedrock_nbt_value"))
}

#' @export
format.rbedrock_nbt_byte <- function(x, ...) {
    NextMethod(suffix = "b")
}

# ---- nbt_short [2] -----------------------------------------------------------

#' @rdname nbt
#' @export
nbt_short <- function(x) {
    validate_nbt_short(new_nbt_short(trunc(as.double(x))))
}

validate_nbt_short <- function(x) {
    p <- unclass(x)
    if (any(is.na(p) | p > 32767 | p < -32768 )) {
        stop("`x` cannot be coerced into a 16-bit signed integer",
             call. = FALSE)
    }
    x
}

new_nbt_short <- function(x) {
    stopifnot(is.double(x) && length(x) == 1)
    structure(x, class = c("rbedrock_nbt_short", "rbedrock_nbt_numeric",
        "rbedrock_nbt_value"))
}

#' @export
format.rbedrock_nbt_short <- function(x, ...) {
    NextMethod(suffix = "s")
}


# ---- nbt_int [3] -------------------------------------------------------------

#' @rdname nbt
#' @export
nbt_int <- function(x) {
    validate_nbt_int(new_nbt_int(trunc(as.double(x))))
}

validate_nbt_int <- function(x) {
    p <- unclass(x)
    if (any(is.na(p) | p > 2147483647 | p < -2147483648 )) {
        stop("`x` cannot be coerced into a 32-bit signed integer",
             call. = FALSE)
    }
    x
}

new_nbt_int <- function(x) {
    stopifnot(is.double(x) && length(x) == 1)
    structure(x, class = c("rbedrock_nbt_int", "rbedrock_nbt_numeric",
        "rbedrock_nbt_value"))
}

#' @export
format.rbedrock_nbt_int <- function(x, ...) {
    NextMethod(suffix = "")
}

# ---- nbt_float [5] -----------------------------------------------------------

#' @rdname nbt
#' @export
nbt_float <- function(x) {
    validate_nbt_float(new_nbt_float(as.double(x)))
}

validate_nbt_float <- function(x) {
    p <- unclass(x)
    if (any(is.na(p) | (is.finite(p) && (p > 3.4028234663852886e+38 |
            p < -3.4028234663852886e+38 )))) {
        stop("`x` cannot be coerced into a float", call. = FALSE)
    }
    x
}

new_nbt_float <- function(x) {
    stopifnot(is.double(x) && length(x) == 1)
    structure(x, class = c("rbedrock_nbt_float", "rbedrock_nbt_numeric",
        "rbedrock_nbt_value"))
}

#' @export
format.rbedrock_nbt_float <- function(x, ...) {
    out <- NextMethod(suffix = "0f", flag = "#")
    sub("(\\.[0-9]*[1-9])0+f$", "\\1f", out) # fix trailing zeros
}

# ---- nbt_double [6] ----------------------------------------------------------

#' @rdname nbt
#' @export
nbt_double <- function(x) {
    validate_nbt_double(new_nbt_double(as.double(x)))
}

validate_nbt_double <- function(x) {
    p <- unclass(x)
    if (any(is.na(p))) {
        stop("`x` cannot be coerced into a double", call. = FALSE)
    }
    x
}

new_nbt_double <- function(x) {
    stopifnot(is.double(x) && length(x) == 1)
    structure(x, class = c("rbedrock_nbt_double", "rbedrock_nbt_numeric",
        "rbedrock_nbt_value"))
}

#' @export
format.rbedrock_nbt_double <- function(x, ...) {
    out <- NextMethod(suffix = "0", flag = "#")
    sub("(\\.[0-9]*[1-9])0+$", "\\1", out) # fix trailing zeros
}

# ---- nbt_byte_array [7] ------------------------------------------------------

#' @rdname nbt
#' @export
nbt_byte_array <- function(x) {
    validate_nbt_byte_array(new_nbt_byte_array(trunc(as.double(x))))
}

validate_nbt_byte_array <- validate_nbt_byte

new_nbt_byte_array <- function(x) {
    stopifnot(is.double(x))
    structure(x, class = c("rbedrock_nbt_byte_array", "rbedrock_nbt_numeric",
        "rbedrock_nbt_value"))
}

#' @export
format.rbedrock_nbt_byte_array <- format.rbedrock_nbt_byte

# ---- nbt_int_array [11] ------------------------------------------------------

#' @rdname nbt
#' @export
nbt_int_array <- function(x) {
    validate_nbt_int_array(new_nbt_int_array(trunc(as.double(x))))
}

validate_nbt_int_array <- validate_nbt_int

new_nbt_int_array <- function(x) {
    stopifnot(is.double(x))
    structure(x, class = c("rbedrock_nbt_int_array", "rbedrock_nbt_numeric",
        "rbedrock_nbt_value"))
}

#' @export
format.rbedrock_nbt_int_array <- format.rbedrock_nbt_int

# ---- nbt_long [4] ------------------------------------------------------------

# ---- nbt_long_array [12] -----------------------------------------------------

# ---- nbt_string [8] ----------------------------------------------------------

