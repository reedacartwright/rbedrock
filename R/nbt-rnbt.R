# raw nbt ----------------------------------------------------------------------

#' Raw Named Binary Tag Format
#'
#' `rnbt` is a recursive, intermediate data structure that closely
#' resembles how NBT data is encoded.
#'
#' - `read_rnbt()` converts a `raw` vector to `rnbt` data.
#' - `write_rnbt()` converts `rnbt` data to a `raw` vector
#' - `from_rnbt()` converts `rnbt` data to `nbt` data.
#' - `to_rnbt()` converts `nbt` data to `rnbt` data.
#'
#' @param x An object
#' @param rawvalue A `raw` vector
#' @param format A character string specifying which binary NBT format to use.
#'
#' @name rnbt
#' @keywords internal
NULL

#' @rdname rnbt
#' @keywords internal
#' @export
#' @useDynLib rbedrock R_read_nbt
read_rnbt <- function(rawvalue,
                      format = c("little", "big", "network", "network_big")) {
    format <- match.arg(format)
    format_int <- switch(format, "little" = 0L, "big" = 1L,
                         "network" = 2L, "network_big" = 3L)
    .Call(R_read_nbt, rawvalue, format_int)
}

#' @rdname rnbt
#' @keywords internal
#' @useDynLib rbedrock R_write_nbt
#' @export
write_rnbt <- function(x,
                       format = c("little", "big", "network", "network_big")) {
    format <- match.arg(format)
    format_int <- switch(format, "little" = 0L, "big" = 1L,
                         "network" = 2L, "network_big" = 3L)
    .Call(R_write_nbt, x, format_int)
}

#' @rdname rnbt
#' @export
from_rnbt <- function(x) {
    from_rnbt_impl(x)
}

from_rnbt_impl <- function(x) {
    if (is.null(x)) {
        return(x)
    }
    # extract names
    n <- sapply(x, rnbt_name)
    # extract values
    v <- lapply(x, rnbt_value)
    # Set names if any exist
    if (any(n != "")) {
        names(v) <- n
    }
    v
}

rnbt_value <- function(x) {
    type <- x[["type", exact = TRUE]]
    value <- x[["value", exact = TRUE]]

    if (type == 109L) {
        value <- lapply(value, rnbt_value)
        new_nbt_nested_list(value)
    } else if(type == 110L) {
        value <- lapply(value, from_rnbt_impl)
        new_nbt_compound_list(value)
    } else if (type == 10L) {
        new_nbt_compound(from_rnbt_impl(value))
    } else {
        new_nbt_value_impl(value, type)
    }
}

new_nbt_value_impl <- function(x, type) {
    ret <- switch(as.character(type),
        "0" = NULL,
        "1" = new_nbt_byte(x),
        "2" = new_nbt_short(x),
        "3" = new_nbt_int(x),
        "4" = new_nbt_long(x),
        "5" = new_nbt_float(x),
        "6" = new_nbt_double(x),
        "7" = new_nbt_byte_array(x),
        "8" = new_nbt_string(x),
        "9" = NULL,
        "10" = new_nbt_compound(x),
        "11" = new_nbt_int_array(x),
        "12" = new_nbt_long_array(x),
        "58" = new_nbt_raw_string(x),
        "100" = new_nbt_empty_list(x),
        "101" = new_nbt_byte_list(x),
        "102" = new_nbt_short_list(x),
        "103" = new_nbt_int_list(x),
        "104" = new_nbt_long_list(x),
        "105" = new_nbt_float_list(x),
        "106" = new_nbt_double_list(x),
        "107" = new_nbt_byte_array_list(x),
        "108" = new_nbt_string_list(x),
        "109" = new_nbt_nested_list(x),
        "110" = new_nbt_compound_list(x),
        "111" = new_nbt_int_array_list(x),
        "112" = new_nbt_long_array_list(x),
        "158" = new_nbt_raw_string_list(x),
        NULL
    )
    stopifnot(!is.null(ret))
    ret
}

rnbt_name <- function(x) {
    if (!is.list(x)) {
        abort("Malformed rnbt data.")
    }
    n <- x$name %||% ""
    if (is_raw(n)) {
        # truncate at the first null
        idx <- n == 0L
        if (any(idx)) {
            idx <- which(idx) - 1
            n <- n[seq_len(idx)]
        }
        n <- rawToChar(n)
    }
    n
}

#' @rdname rnbt
#' @keywords internal
#' @export
to_rnbt <- function(x) {
    n <- names(x) %||% rep("", length(x))
    names(x) <- NULL
    ret <- vector("list", length(x))
    for (i in seq_along(ret)) {
        ret[[i]] <- c(list(name = n[[i]]), to_rnbt_payload(x[[i]]))
    }
    ret
}

#' @rdname rnbt
#' @keywords internal
#' @export
to_rnbt_payload <- function(x) {
    UseMethod("to_rnbt_payload")
}

#' @export
to_rnbt_payload.rbedrock_nbt <- function(x) {
    list(tag = get_nbt_tag(x), value = payload(x))
}

#' @export
to_rnbt_payload.rbedrock_nbt_compound <- function(x) {
    list(tag = 10L, value = to_rnbt(x))
}

to_rnbt_list <- function(x, tag) {
    if (tag == 9L) {
        lapply(x, function(y) {
            value <- payload(y)
            type <- attr(x, "type", exact = TRUE)
            if (is.recursive(value)) {
                value <- to_rnbt_list(value, type)
            }
            list(value = value, type = type)
        })

    } else if (tag == 10L) {
        lapply(x, to_rnbt)
    } else {
        stop("Malformed rnbt data.")
    }
}

#' @export
to_rnbt_payload.rbedrock_nbt_list <- function(x) {
    type <- attr(x, "type", exact = TRUE)
    value <- payload(x)
    if (type != 0L && is.recursive(value)) {
        value <- to_rnbt_list(x, type)
    }
    list(tag = 9L, value = value, type = type)
}
