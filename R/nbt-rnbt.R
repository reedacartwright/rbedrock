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
write_rnbt <- function(x) {
    .Call(R_write_nbt, x)
}

#' @rdname rnbt
#' @export
from_rnbt <- function(x) {
    if (is.null(x)) {
        return(x)
    }
    # extract names
    n <- sapply(x, .extract_rnbt_name)
    # extract values
    v <- lapply(x, from_rnbt_payload)
    # Set names if any exist
    if (all(n == "")) {
        v
    } else {
        set_names(v, n)
    }
}

.extract_rnbt_name <- function(x) {
    if (!is_list(x)) {
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

from_rnbt_recursive_list <- function(x, type) {
    if (type == 9L) {
        payload <- lapply(x, function(y) {
            value <- y[["value", exact = TRUE]]
            type <- y[["type", exact = TRUE]]
            type <- type %% 100L
            if (type != 0L && is.recursive(value)) {
                value <- from_rnbt_recursive_list(value, type)
            }
            new_nbt_list(value, type)
        })
        new_nbt_list(payload, type)
    } else if (type == 10L) {
        payload <- lapply(x, from_rnbt)
        new_nbt_list(payload, type)
    } else {
        stop("Malformed rnbt data.")
    }
}

#' @rdname rnbt
#' @keywords internal
#' @export
from_rnbt_payload <- function(x) {
    type <- x[["type", exact = TRUE]]
    payload <- x[["value", exact = TRUE]]

    is_nbt_list <- type >= 100
    type <- type %% 100

    if(is_nbt_list) {
        if (type != 0L && is.recursive(payload)) {
            payload <- from_rnbt_recursive_list(payload, type)
        }
        new_nbt_list(payload, type)
    } else if (type == 10L) {
        ret <- from_rnbt(payload)
        new_nbt_compound(ret)
    } else if (type == 8L && is.raw(payload)) {
        new_nbt_raw_string(payload)
    } else {
        new_nbt(payload, type)
    }
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


