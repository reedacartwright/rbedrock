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

# ---- nbt generics ------------------------------------------------------------

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

#' @rdname nbt
#' @export
unnbt <- function(x) {
    if (is.recursive(x)) {
        rapply(x, rac_data, how = "list")
    } else {
        rac_data(x)
    }
}

new_nbt <- function(x) {
    stopifnot(is.list(x))
    structure(x, class = c("rbedrock_nbt", "list"))
}

validate_nbt <- function(x) {
    if (!all(vapply(x, is_nbt_value, FALSE))) {
        stop("`x` contains non-NBT elements.", call. = FALSE)
    }
    x
}

#' @export
`[<-.rbedrock_nbt` <- function(x, i, value) {
    if (is.logical(i)) {
        i <- which(i)
    }
    stopifnot(is.numeric(i) || is.character(i))
    if (is_nbt_value(value)) {
        value <- list(value)
    }
    value <- rac_recycle(value, length(i))
    force(x)
    value <- lapply(seq_along(value), function(j) {
        nbt_value_or_cast(rac_slice(value, j), x[[i[j], exact = TRUE]])
    })
    NextMethod()
}

#' @export
`[[<-.rbedrock_nbt` <- function(x, i, value) {
    if (is.null(value)) {
        x[i] <- list(value)
        return(x)
    }
    value <- nbt_value_or_cast(value, x[[i, exact = TRUE]])
    NextMethod()
}

#' @export
`$<-.rbedrock_nbt` <- function(x, i, value) {
    value <- nbt_value_or_cast(value, x[[i, exact = TRUE]])
    NextMethod()
}

# ---- nbt_compound [10] -------------------------------------------------------

#' @rdname nbt
#' @export
nbt_compound <- function(...) {
    lst <- list(...)
    validate_nbt_compound(new_nbt_compound(lst))
}

new_nbt_compound <- function(x) {
    stopifnot(is.list(x))
    structure(x, class = c("rbedrock_nbt_compound", "rbedrock_nbt_value",
                           "list"))
}

validate_nbt_compound <- function(x) {
    if (!all(vapply(x, is_nbt_value, FALSE))) {
        stop("`x` contains non-NBT elements.", call. = FALSE)
    }
    x
}

#' @export
format.rbedrock_nbt_compound <- function(x, ...) {
    NextMethod()
}

#' @export
rac_cast.rbedrock_nbt_compound <- function(x, to, ...) {
    nbt_compound(x)
}

#' @export
`[<-.rbedrock_nbt_compound` <- `[<-.rbedrock_nbt`

#' @export
`[[<-.rbedrock_nbt_compound` <- `[[<-.rbedrock_nbt`

#' @export
`$<-.rbedrock_nbt_compound` <- `$<-.rbedrock_nbt`

# ---- nbt_value ---------------------------------------------------------------

is_nbt_value <- function(x) {
    inherits(x, "rbedrock_nbt_value")
}

nbt_value_or_cast <- function(x, to) {
    if (is_nbt_value(x)) x else rac_cast(x, to)
}

is_nbt_list_value <- function(x) {
    inherits(x, "rbedrock_nbt_list_value")
}

is_nbt_compound <- function(x) {
    inherits(x, "rbedrock_nbt_compound")
}

# ---- nbt_byte [1] ------------------------------------------------------------

#' @rdname nbt
#' @export
nbt_byte <- function(x) {
    validate_nbt_byte(new_nbt_byte(trunc(as_double(x))))
}

validate_nbt_byte <- function(x) {
    p <- unclass(x)
    if (any(is.na(p) | p > 127 | p < -128)) {
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

#' @export
rac_cast.rbedrock_nbt_byte <- function(x, to, ...) {
    nbt_byte(x)
}

# ---- nbt_byte_array [7] ------------------------------------------------------

#' @rdname nbt
#' @export
nbt_byte_array <- function(x) {
    validate_nbt_byte_array(new_nbt_byte_array(trunc(as_double(x))))
}

validate_nbt_byte_array <- validate_nbt_byte

new_nbt_byte_array <- function(x) {
    stopifnot(is.double(x))
    structure(x, class = c("rbedrock_nbt_byte_array", "rbedrock_nbt_numeric",
                           "rbedrock_nbt_value"))
}

#' @export
format.rbedrock_nbt_byte_array <- format.rbedrock_nbt_byte

#' @export
rac_cast.rbedrock_nbt_byte_array <- function(x, to, ...) {
    nbt_byte_array(x)
}

# ---- nbt_byte_list [101] -----------------------------------------------------

#' @rdname nbt
#' @export
nbt_byte_list <- function(x) {
    validate_nbt_byte_list(new_nbt_byte_list(trunc(as_double(x))))
}

validate_nbt_byte_list <- validate_nbt_byte

new_nbt_byte_list <- function(x) {
    stopifnot(is.double(x))
    structure(x, class = c("rbedrock_nbt_byte_list", "rbedrock_nbt_numeric",
                           "rbedrock_nbt_list_value", "rbedrock_nbt_value"))
}

#' @export
format.rbedrock_nbt_byte_list <- format.rbedrock_nbt_byte

#' @export
rac_cast.rbedrock_nbt_byte_list <- function(x, to, ...) {
    nbt_byte_list(x)
}

# ---- nbt_byte_array_list [107] -----------------------------------------------

#' @rdname nbt
#' @export
nbt_byte_array_list <- function(x) {
    x <- lapply(x, nbt_byte_array)
    validate_nbt_byte_array_list(new_nbt_byte_array_list(x))
}

validate_nbt_byte_array_list <- function(x) {
    lapply(x, validate_nbt_byte_array)
}

new_nbt_byte_array_list <- function(x) {
    stopifnot(is.list(x))
    structure(x, class = c("rbedrock_nbt_byte_array_list",
                           "rbedrock_nbt_list_value", "rbedrock_nbt_value",
                           "list"))
}

#' @export
format.rbedrock_nbt_byte_array_list <- function(x, ...) {
    NextMethod()
}

#' @export
rac_cast.rbedrock_nbt_byte_array_list <- function(x, to, ...) {
    nbt_byte_array_list(x)
}

# ---- nbt_short [2] -----------------------------------------------------------

#' @rdname nbt
#' @export
nbt_short <- function(x) {
    validate_nbt_short(new_nbt_short(trunc(as_double(x))))
}

validate_nbt_short <- function(x) {
    p <- unclass(x)
    if (any(is.na(p) | p > 32767 | p < -32768)) {
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

#' @export
rac_cast.rbedrock_nbt_short <- function(x, to, ...) {
    nbt_short(x)
}

# ---- nbt_short_list [102] ----------------------------------------------------

#' @rdname nbt
#' @export
nbt_short_list <- function(x) {
    validate_nbt_short_list(new_nbt_short_list(trunc(as_double(x))))
}

validate_nbt_short_list <- validate_nbt_short

new_nbt_short_list <- function(x) {
    stopifnot(is.double(x))
    structure(x, class = c("rbedrock_nbt_short_list", "rbedrock_nbt_numeric",
                           "rbedrock_nbt_list_value", "rbedrock_nbt_value"))
}

#' @export
format.rbedrock_nbt_short_list <- format.rbedrock_nbt_short

#' @export
rac_cast.rbedrock_nbt_short_list <- function(x, to, ...) {
    nbt_short_list(x)
}

# ---- nbt_int [3] -------------------------------------------------------------

#' @rdname nbt
#' @export
nbt_int <- function(x) {
    validate_nbt_int(new_nbt_int(trunc(as_double(x))))
}

validate_nbt_int <- function(x) {
    p <- unclass(x)
    if (any(is.na(p) | p > 2147483647 | p < -2147483648)) {
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


#' @export
rac_cast.rbedrock_nbt_short <- function(x, to, ...) {
    nbt_short(x)
}

# ---- nbt_int_array [11] ------------------------------------------------------

#' @rdname nbt
#' @export
nbt_int_array <- function(x) {
    validate_nbt_int_array(new_nbt_int_array(trunc(as_double(x))))
}

validate_nbt_int_array <- validate_nbt_int

new_nbt_int_array <- function(x) {
    stopifnot(is.double(x))
    structure(x, class = c("rbedrock_nbt_int_array", "rbedrock_nbt_numeric",
                           "rbedrock_nbt_value"))
}

#' @export
format.rbedrock_nbt_int_array <- format.rbedrock_nbt_int

#' @export
rac_cast.rbedrock_nbt_int_array <- function(x, to, ...) {
    nbt_int_array(x)
}

# ---- nbt_int_list [103] ------------------------------------------------------

#' @rdname nbt
#' @export
nbt_int_list <- function(x) {
    validate_nbt_int_list(new_nbt_int_list(trunc(as_double(x))))
}

validate_nbt_int_list <- validate_nbt_int

new_nbt_int_list <- function(x) {
    stopifnot(is.double(x))
    structure(x, class = c("rbedrock_nbt_int_list", "rbedrock_nbt_numeric",
                           "rbedrock_nbt_list_value", "rbedrock_nbt_value"))
}

#' @export
format.rbedrock_nbt_int_list <- format.rbedrock_nbt_int

#' @export
rac_cast.rbedrock_nbt_int_list <- function(x, to, ...) {
    nbt_int_list(x)
}

# ---- nbt_int_array_list [111] ------------------------------------------------

#' @rdname nbt
#' @export
nbt_int_array_list <- function(x) {
    x <- lapply(x, nbt_int_array)
    validate_nbt_int_array_list(new_nbt_int_array_list(x))
}

validate_nbt_int_array_list <- function(x) {
    lapply(x, validate_nbt_int_array)
}

new_nbt_int_array_list <- function(x) {
    stopifnot(is.list(x))
    structure(x, class = c("rbedrock_nbt_int_array_list",
                           "rbedrock_nbt_list_value", "rbedrock_nbt_value",
                           "list"))
}

#' @export
format.rbedrock_nbt_int_array_list <- function(x, ...) {
    NextMethod()
}

#' @export
rac_cast.rbedrock_nbt_int_array_list <- function(x, to, ...) {
    nbt_int_array_list(x)
}

# ---- nbt_float [5] -----------------------------------------------------------

#' @rdname nbt
#' @export
nbt_float <- function(x) {
    validate_nbt_float(new_nbt_float(as_double(x)))
}

validate_nbt_float <- function(x) {
    p <- unclass(x)
    if (any(is.na(p) | (is.finite(p) && (p > 3.4028234663852886e+38 |
                                             p < -3.4028234663852886e+38)))) {
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
    sub("(\\.[0-9]*[1-9])0+f$", "\\1f", out) # fixes trailing zeros
}

#' @export
rac_cast.rbedrock_nbt_float <- function(x, to, ...) {
    nbt_float(x)
}

# ---- nbt_float_list [105] ----------------------------------------------------

#' @rdname nbt
#' @export
nbt_float_list <- function(x) {
    validate_nbt_float_list(new_nbt_float_list(as_double(x)))
}

validate_nbt_float_list <- validate_nbt_float

new_nbt_float_list <- function(x) {
    stopifnot(is.double(x))
    structure(x, class = c("rbedrock_nbt_float_list", "rbedrock_nbt_numeric",
                           "rbedrock_nbt_list_value", "rbedrock_nbt_value"))
}

#' @export
format.rbedrock_nbt_float_list <- format.rbedrock_nbt_float

#' @export
rac_cast.rbedrock_nbt_float_list <- function(x, to, ...) {
    nbt_float_list(x)
}

# ---- nbt_double [6] ----------------------------------------------------------

#' @rdname nbt
#' @export
nbt_double <- function(x) {
    validate_nbt_double(new_nbt_double(as_double(x)))
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

#' @export
rac_cast.rbedrock_nbt_double <- function(x, to, ...) {
    nbt_double(x)
}

# ---- nbt_double_list [106] ---------------------------------------------------

#' @rdname nbt
#' @export
nbt_double_list <- function(x) {
    validate_nbt_double_list(new_nbt_double_list(as_double(x)))
}

validate_nbt_double_list <- validate_nbt_double

new_nbt_double_list <- function(x) {
    stopifnot(is.double(x))
    structure(x, class = c("rbedrock_nbt_double_list", "rbedrock_nbt_numeric",
                           "rbedrock_nbt_list_value", "rbedrock_nbt_value"))
}

#' @export
format.rbedrock_nbt_double_list <- format.rbedrock_nbt_double

#' @export
rac_cast.rbedrock_nbt_double_list <- function(x, to, ...) {
    nbt_double_list(x)
}

# ---- nbt_long [4] ------------------------------------------------------------

#' @rdname nbt
#' @export
nbt_long <- function(x) {
    validate_nbt_long(new_nbt_long(trunc_int64(as.character(x))))
}

validate_nbt_long <- function(x) {
    p <- unclass(x)
    if (any(!is_int64(p))) {
        stop("`x` cannot be coerced into a 64-bit signed integer",
             call. = FALSE)
    }
    x
}

new_nbt_long <- function(x) {
    stopifnot(is.character(x) && length(x) == 1)
    structure(x, class = c("rbedrock_nbt_long", "rbedrock_nbt_int64",
                           "rbedrock_nbt_value"))
}

#' @export
format.rbedrock_nbt_long <- function(x, ...) {
    NextMethod(suffix = "")
}

#' @export
rac_cast.rbedrock_nbt_long <- function(x, to, ...) {
    nbt_long(x)
}

# ---- nbt_long_array [12] -----------------------------------------------------

#' @rdname nbt
#' @export
nbt_long_array <- function(x) {
    validate_nbt_long_array(new_nbt_long_array(trunc_int64(as.character(x))))
}

validate_nbt_long_array <- validate_nbt_long

new_nbt_long_array <- function(x) {
    stopifnot(is.character(x))
    structure(x, class = c("rbedrock_nbt_long_array", "rbedrock_nbt_int64",
                           "rbedrock_nbt_value"))
}

#' @export
format.rbedrock_nbt_long_array <- format.rbedrock_nbt_long

#' @export
rac_cast.rbedrock_nbt_long_array <- function(x, to, ...) {
    nbt_long_array(x)
}

# ---- nbt_long_list [104] -----------------------------------------------------

#' @rdname nbt
#' @export
nbt_long_list <- function(x) {
    validate_nbt_long_list(new_nbt_long_list(trunc_int64(as.character(x))))
}

validate_nbt_long_list <- validate_nbt_long

new_nbt_long_list <- function(x) {
    stopifnot(is.character(x))
    structure(x, class = c("rbedrock_nbt_long_list", "rbedrock_nbt_int64",
                           "rbedrock_nbt_list_value", "rbedrock_nbt_value"))
}

#' @export
format.rbedrock_nbt_long_list <- format.rbedrock_nbt_long

#' @export
rac_cast.rbedrock_nbt_long_list <- function(x, to, ...) {
    nbt_long_list(x)
}

# ---- nbt_long_array_list [112] -----------------------------------------------

#' @rdname nbt
#' @export
nbt_long_array_list <- function(x) {
    x <- lapply(x, nbt_long_array)
    validate_nbt_long_array_list(new_nbt_long_array_list(x))
}

validate_nbt_long_array_list <- function(x) {
    lapply(x, validate_nbt_long_array)
}

new_nbt_long_array_list <- function(x) {
    stopifnot(is.list(x))
    structure(x, class = c("rbedrock_nbt_long_array_list",
                           "rbedrock_nbt_list_value", "rbedrock_nbt_value",
                           "list"))
}

#' @export
format.rbedrock_nbt_long_array_list <- function(x, ...) {
    NextMethod()
}

#' @export
rac_cast.rbedrock_nbt_long_array_list <- function(x, to, ...) {
    nbt_long_array_list(x)
}

# ---- nbt_string [8] ----------------------------------------------------------

#' @rdname nbt
#' @export
nbt_string <- function(x) {
    validate_nbt_string(new_nbt_string(as.character(x)))
}

validate_nbt_string <- function(x) {
    p <- unclass(x)
    if (any(is.na(p))) {
        stop("`x` cannot be coerced into a string", call. = FALSE)
    }
    x
}

new_nbt_string <- function(x) {
    stopifnot(is.character(x) && length(x) == 1)
    structure(x, class = c("rbedrock_nbt_string", "rbedrock_nbt_value"))
}

#' @export
format.rbedrock_nbt_string <- function(x, ...) {
    NextMethod()
}

#' @export
rac_cast.rbedrock_nbt_string <- function(x, to, ...) {
    nbt_string(x)
}

# ---- nbt_raw_string [58] -----------------------------------------------------

#' @rdname nbt
#' @export
nbt_raw_string <- function(x) {
    validate_nbt_string(new_nbt_string_raw(as.raw(x)))
}

validate_nbt_raw_string <- function(x) {
    x
}

new_nbt_raw_string <- function(x) {
    stopifnot(is.raw(x))
    structure(x, class = c("rbedrock_nbt_string_raw", "rbedrock_nbt_value"))
}

#' @export
format.rbedrock_nbt_raw_string <- function(x, ...) {
    NextMethod()
}

#' @export
rac_cast.rbedrock_nbt_raw_string <- function(x, to, ...) {
    nbt_raw_string(x)
}

# ---- nbt_string_list [108] ---------------------------------------------------

#' @rdname nbt
#' @export
nbt_string_list <- function(x) {
    validate_nbt_string_list(new_nbt_string_list(as.character(x)))
}

validate_nbt_string_list <- validate_nbt_string

new_nbt_string_list <- function(x) {
    stopifnot(is.character(x))
    structure(x, class = c("rbedrock_nbt_string_list",
                           "rbedrock_nbt_list_value",
                           "rbedrock_nbt_value"))
}

#' @export
format.rbedrock_nbt_string_list <- function(x, ...) {
    NextMethod()
}

#' @export
rac_cast.rbedrock_nbt_string_list <- function(x, to, ...) {
    nbt_string_list(x)
}

# 

# ---- nbt_raw_string_list [158] -----------------------------------------------

#' @rdname nbt
#' @export
nbt_raw_string_list <- function(x) {
    x <- lapply(x, nbt_raw_string)
    validate_nbt_raw_string_list(new_nbt_raw_string_list(x))
}

validate_nbt_raw_string_list <- function(x) {
    x
}

new_nbt_raw_string_list <- function(x) {
    stopifnot(is.list(x))
    structure(x, class = c("rbedrock_nbt_raw_string_list",
                           "rbedrock_nbt_list_value",
                           "rbedrock_nbt_value",
                           "list"))
}

#' @export
format.rbedrock_nbt_raw_string_list <- function(x, ...) {
    NextMethod()
}

#' @export
rac_cast.rbedrock_nbt_raw_string_list <- function(x, to, ...) {
    nbt_string_list(x)
}

# ---- nbt_numeric -------------------------------------------------------------

#' @export
format.rbedrock_nbt_numeric <- function(x, suffix = "", ...) {
    out <- formatC(unclass(x), ...)
    out[is.na(x)] <- NA
    out[!is.na(x)] <- paste0(out[!is.na(x)], suffix)
    out
}

# ---- nbt_empty_list [100] ----------------------------------------------------

#' @rdname nbt
#' @export
nbt_empty_list <- function(x = list()) {
    validate_nbt_empty_list(new_nbt_empty_list(as.list(x)))
}

validate_nbt_empty_list <- function(x) {
    x
}

new_nbt_empty_list <- function(x) {
    stopifnot(is.list(x) && length(x) == 0)
    structure(x, class = c("rbedrock_nbt_empty_list", "rbedrock_nbt_list_value",
                           "rbedrock_nbt_value"))
}

#' @export
format.rbedrock_empty_list <- function(x, ...) {
    NextMethod()
}

#' @export
rac_cast.rbedrock_empty_list <- function(x, to, ...) {
    nbt_empty_list(x)
}


# ---- nbt_long_compound_list [110] --------------------------------------------

#' @rdname nbt
#' @export
nbt_compound_list <- function(x) {
    x <- lapply(x, nbt_compound)
    validate_nbt_compound_list(new_nbt_compound_list(x))
}

validate_nbt_compound_list <- function(x) {
    lapply(x, validate_nbt_compound)
}

new_nbt_compound_list <- function(x) {
    stopifnot(is.list(x))
    structure(x, class = c("rbedrock_nbt_compound_list",
                           "rbedrock_nbt_list_value", "rbedrock_nbt_value",
                           "list"))
}

#' @export
format.rbedrock_nbt_compound_list <- function(x, ...) {
    NextMethod()
}

#' @export
rac_cast.rbedrock_nbt_compound_list <- function(x, to, ...) {
    nbt_compound_list(x)
}

# ---- nbt_nested_list [109] ---------------------------------------------------

#' @rdname nbt
#' @export
nbt_nested_list <- function(x) {
    validate_nbt_nested_list(new_nbt_nested_list(x))
}

validate_nbt_nested_list <- function(x) {
    if (!all(vapply(x, is_nbt_list_value, FALSE))) {
        stop("`x` contains non NBT lists elements.", call. = FALSE)
    }
    x
}

new_nbt_nested_list <- function(x) {
    stopifnot(is.list(x))
    structure(x, class = c("rbedrock_nbt_nested_list",
                           "rbedrock_nbt_value", "list"))
}

#' @export
format.rbedrock_nbt_nested_list <- function(x, ...) {
    NextMethod()
}

#' @export
rac_cast.rbedrock_nbt_nested_list <- function(x, to, ...) {
    nbt_nested_list(x)
}
