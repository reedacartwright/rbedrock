#' Create an NBT value
#'
#' @description
#' The Named Binary Tag (NBT) format is used by Minecraft for various data
#' types. An NBT value holds a 'payload' of data and a 'tag' indicating the type
#' of data held.
#'
#' @description
#' `nbt_*()` family of functions create nbt data types.
#' `unnbt()` recursively strips NBT metadata from an NBT value.
#'
#' @param x An nbt payload.
#' @param .type The type that a list holds.
#' @param ... Arguments to collect into an NBT compound or NBT list value.
#'     Supports dynamic dots via `rlang::list2()`.
#'
#'
#' @rdname nbt
#' @export
nbt_byte <- function(x) {
    new_nbt_byte(trunc(vec_cast(x, double())))
}

#' @rdname nbt
#' @export
nbt_short <- function(x) {
    new_nbt_short(trunc(vec_cast(x, double())))
}

#' @rdname nbt
#' @export
nbt_int <- function(x) {
    new_nbt_int(trunc(vec_cast(x, double())))
}

#' @rdname nbt
#' @export
nbt_long <- function(x) {
    if (is.numeric(x)) {
        x <- as.character(x)
    }
    new_nbt_long(vec_cast(x, character()))
}

#' @rdname nbt
#' @export
nbt_float <- function(x) {
    new_nbt_float(vec_cast(x, double()))
}

#' @rdname nbt
#' @export
nbt_double <- function(x) {
    new_nbt_double(vec_cast(x, double()))
}

#' @rdname nbt
#' @export
nbt_byte_array <- function(x) {
    new_nbt_byte_array(trunc(vec_cast(x, double())))
}

#' @rdname nbt
#' @export
nbt_string <- function(x) {
    new_nbt_string(vec_cast(x, character()))
}

#' @rdname nbt
#' @export
nbt_raw_string <- function(x) {
    new_nbt_raw_string(vec_cast(x, raw()))
}

#' @rdname nbt
#' @export
nbt_int_array <- function(x) {
    new_nbt_int_array(trunc(vec_cast(x, double())))
}

#' @rdname nbt
#' @export
nbt_long_array <- function(x) {
    if (is.numeric(x)) {
        x <- as.character(x)
    }
    new_nbt_long_array(vec_cast(x, character()))
}

#' @rdname nbt
#' @export
nbt_compound <- function(...) {
    new_nbt_compound(list2(...))
}

#' @rdname nbt
#' @export
nbt_list <- function(x, .type) {
    if (missing(x)) {
        new_nbt_list(list(), .type = 0L)
    } else {
        new_nbt_list(x, .type = .type)
    }
}

#' @rdname nbt
#' @export
is_nbt <- function(x) {
    inherits(x, "rbedrock_nbt")
}

all_nbt <- function(x) {
    all(vapply(x, is_nbt, logical(1L)))
}

#' @description
#' `payload()` reads an nbt value's payload.
#'
#' @param x An nbt value
#'
#' @rdname nbt
#' @export
payload <- function(x) {
    UseMethod("payload")
}

#' @export
payload.default <- function(x) {
    vec_data(x)
}

#' @export
payload.rbedrock_nbt_long <- function(x) {
    vec_data(x)
}

#' @export
payload.rbedrock_nbt_long_array <- function(x) {
    vec_data(x)
}

#' @rdname nbt
#' @export
unnbt <- function(x) {
    if (is_list(x)) {
        rapply(x, payload, how = "list")
    } else {
        payload(x)
    }
}

is_nbt_list <- function(x) {
    inherits(x, "rbedrock_nbt_list")
}

is_nbt_compound <- function(x) {
    inherits(x, "rbedrock_nbt_compound")
}

#' @export
`$.rbedrock_nbt_container` <- function(x, i, ...) {
    NextMethod()
}

#' @export
`[[.rbedrock_nbt_container` <- function(x, i, ...) {
    NextMethod()
}

#' @export
`$<-.rbedrock_nbt_container` <- function(x, i, value) {
    if (!is_nbt(value)) {
        value <- vec_cast(value, vec_ptype(x[[i]]), x_arg = "value")
        if (!is_nbt(value)) {
            abort("conversion of value to nbt failed.")
        }
    }
    NextMethod()
}

#' @export
`[[<-.rbedrock_nbt_container` <- function(x, i, value) {
    if (!is_nbt(value)) {
        value <- vec_cast(value, vec_ptype(x[[i]]), x_arg = "value")
        if (!is_nbt(value)) {
            abort("conversion of value to nbt failed.")
        }
    }
    NextMethod()
}

.tag_assert <- function(tag, allowed) {
    vec_assert(tag, ptype = integer(), size = 1L)
    if (!tag %in% allowed) {
        msg <- paste0("invalid tag `", tag, "`")
        abort(msg)
    }
    invisible()
}

#' Create an NBT value
#'
#' @param x An nbt payload.
#' @param tag An integer specifying the tag of the data.
#' @param type An integer specifying the type for nbt lists
#' @keywords internal
#' @rdname new_nbt
#' @export
new_nbt <- function(x, tag, type) {
    tag <- vec_recycle(vec_cast(tag, integer()), 1L, x_arg = "tag")
    switch(tag,
        new_nbt_byte(as.double(x)),
        new_nbt_short(as.double(x)),
        new_nbt_int(as.double(x)),
        new_nbt_long(as.character(x)),
        new_nbt_float(as.double(x)),
        new_nbt_double(as.double(x)),
        new_nbt_byte_array(as.double(x)),
        new_nbt_string(x),
        new_nbt_list(x, .type = type),
        new_nbt_compound(x),
        new_nbt_int_array(as.double(x)),
        new_nbt_long_array(as.character(x))
    )
}

new_rbedrock_nbt_scalar <- function(x, class, ptype = NULL, size = NULL) {
    vec_assert(x, ptype = ptype, size = size)
    new_vctr(x, class = c(paste0("rbedrock_nbt_", class), "rbedrock_nbt"))
}

#' @keywords internal
#' @rdname new_nbt
#' @export
new_nbt_byte <- function(x) {
    new_rbedrock_nbt_scalar(x, "byte", double(), 1)
}
#' @keywords internal
#' @rdname new_nbt
#' @export
new_nbt_short <- function(x) {
    new_rbedrock_nbt_scalar(x, "short", double(), 1)
}
#' @keywords internal
#' @rdname new_nbt
#' @export
new_nbt_int <- function(x) {
    new_rbedrock_nbt_scalar(x, "int", double(), 1)
}
#' @keywords internal
#' @rdname new_nbt
#' @export
new_nbt_long <- function(x) {
    new_rbedrock_nbt_scalar(x, "long", character(), 1)
}
#' @keywords internal
#' @rdname new_nbt
#' @export
new_nbt_float <- function(x) {
    new_rbedrock_nbt_scalar(x, "float", double(), 1)
}
#' @keywords internal
#' @rdname new_nbt
#' @export
new_nbt_double <- function(x) {
    new_rbedrock_nbt_scalar(x, "double", double(), 1)
}
#' @keywords internal
#' @rdname new_nbt
#' @export
new_nbt_byte_array <- function(x) {
    new_rbedrock_nbt_scalar(x, "byte_array", double())
}
#' @keywords internal
#' @rdname new_nbt
#' @export
new_nbt_string <- function(x) {
    new_rbedrock_nbt_scalar(x, "string", character(), 1)
}
#' @keywords internal
#' @rdname new_nbt
#' @export
new_nbt_raw_string <- function(x) {
    new_rbedrock_nbt_scalar(x, "raw_string", raw())
}

#' @keywords internal
#' @rdname new_nbt
#' @export
new_nbt_int_array <- function(x) {
    new_rbedrock_nbt_scalar(x, "int_array", double())
}
#' @keywords internal
#' @rdname new_nbt
#' @export
new_nbt_long_array <- function(x) {
    new_rbedrock_nbt_scalar(x, "long_array", character())
}
#' @keywords internal
#' @rdname new_nbt
#' @export
new_nbt_compound <- function(x) {
    vec_assert(x, ptype = list())
    if (!all(sapply(x, is_nbt))) {
        abort("an nbt_compound can only hold nbt data")
    }
    new_vctr(x, class = c("rbedrock_nbt_compound",
                          "rbedrock_nbt",
                          "rbedrock_nbt_container"))
}
#' @keywords internal
#' @rdname new_nbt
#' @export
new_nbt_list <- function(x, .type) {
    cls <- class(x)
    structure(x, type = .type,
              class = c("rbedrock_nbt_list", "rbedrock_nbt", cls))
}

# nolint start : commented_code_linter
# new_nbt_byte_list <- function(x) {
#     new_rbedrock_nbt_scalar(x, "byte_list", integer())
# }

# new_nbt_short_list <- function(x) {
#     new_rbedrock_nbt_scalar(x, "short_list", integer())
# }

# new_nbt_int_list <- function(x) {
#     new_rbedrock_nbt_scalar(x, "int_list", integer())
# }

# new_nbt_long_list <- function(x) {
#     x <- new_rbedrock_nbt_scalar(x, "long_list", bit64::integer64())
#     .fixup_long(x)
# }

# new_nbt_float_list <- function(x) {
#     new_rbedrock_nbt_scalar(x, "float_list", integer())
# }

# new_nbt_double_list <- function(x) {
#     new_rbedrock_nbt_scalar(x, "double_list", integer())
# }

# new_nbt_string_list <- function(x) {
#     new_rbedrock_nbt_scalar(x, "string_list", character())
# }
# nolint end

######

#' @description
#' `get_nbt_tag()` returns the NBT tag corresponding to and NBT value.
#'
#' @rdname nbt
#' @export
get_nbt_tag <- function(x) {
    UseMethod("get_nbt_tag")
}

#' @export
get_nbt_tag.rbedrock_nbt_byte       <- function(x) 1L
#' @export
get_nbt_tag.rbedrock_nbt_short      <- function(x) 2L
#' @export
get_nbt_tag.rbedrock_nbt_int        <- function(x) 3L
#' @export
get_nbt_tag.rbedrock_nbt_long       <- function(x) 4L
#' @export
get_nbt_tag.rbedrock_nbt_float      <- function(x) 5L
#' @export
get_nbt_tag.rbedrock_nbt_double     <- function(x) 6L
#' @export
get_nbt_tag.rbedrock_nbt_byte_array <- function(x) 7L
#' @export
get_nbt_tag.rbedrock_nbt_string     <- function(x) 8L
#' @export
get_nbt_tag.rbedrock_nbt_raw_string <- function(x) 8L
#' @export
get_nbt_tag.rbedrock_nbt_list       <- function(x) 9L
#' @export
get_nbt_tag.rbedrock_nbt_compound   <- function(x) 10L
#' @export
get_nbt_tag.rbedrock_nbt_int_array  <- function(x) 11L
#' @export
get_nbt_tag.rbedrock_nbt_long_array <- function(x) 12L

# work around integer64 methods not stripping all class variables
#' @export
is.na.rbedrock_nbt_long <- function(x, ...) vec_data(NextMethod())
#' @export
is.na.rbedrock_nbt_long_array <- function(x, ...) vec_data(NextMethod())
#' @export
`==.rbedrock_nbt_long` <- function(e1, e2) vec_data(NextMethod())
#' @export
`==.rbedrock_nbt_long_array` <- function(e1, e2) vec_data(NextMethod())

# raw nbt ----------------------------------------------------------------------

#' Raw Named Binary Tag Format
#'
#' `rnbt` is a recursive, intermediate data structure that closely
#' resembles how NBT data is encoded.
#'
#' @description
#' `from_rnbt()` converts `rnbt` data to `nbt` data.
#' `to_rnbt()` converts `nbt` data to `rnbt` data.
#' `read_rnbt()` converts a `raw` vector to `rnbt` data.
#' `write_rnbt()` converts `rnbt` data to a `raw` vector
#'
#' @param x an object
#' @param rawdata A `raw` vector
#' @param object An nbt object or a list of nbt objects
#'
#' @rdname rnbt
#' @keywords internal
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

from_rnbt_recursive_list <- function(x, tag) {
    if (tag == 9L) {
        payload <- lapply(x, function(y) {
            value <- y[["value", exact = TRUE]]
            type <- y[["type", exact = TRUE]]
            if (is.recursive(value)) {
                value <- from_rnbt_recursive_list(value, type)
            }
            new_nbt_list(value, type)
        })
        new_nbt_list(payload, tag)
    } else if (tag == 10L) {
        payload <- lapply(x, from_rnbt)
        new_nbt_list(payload, tag)
    } else {
        stop("Malformed rnbt data.")
    }
}

#' @rdname rnbt
#' @keywords internal
#' @export
from_rnbt_payload <- function(x) {
    tag <- x[["tag", exact = TRUE]]
    payload <- x[["value", exact = TRUE]]
    if (tag == 9L) {
        type <- x[["type", exact = TRUE]]
        if (type != 0L && is.recursive(payload)) {
            payload <- from_rnbt_recursive_list(payload, type)
        }
        new_nbt_list(payload, type)
    } else if (tag == 10L) {
        ret <- from_rnbt(payload)
        new_nbt_compound(ret)
    } else if (tag == 8L && is.raw(payload)) {
        new_nbt_raw_string(payload)
    } else {
        new_nbt(payload, tag)
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

#' @rdname rnbt
#' @keywords internal
#' @export
read_rnbt <- function(rawdata, format = "little") {
    .Call(Cread_nbt, rawdata, 0L)
}

#' @rdname rnbt
#' @keywords internal
#' @export
write_rnbt <- function(object) {
    .Call(Cwrite_nbt, object)
}

# printing ---------------------------------------------------------------------

#' @export
print.rbedrock_nbt <- function(x, ...) {
    obj_print(x, ...)
    invisible(x)
}
#' @export
str.rbedrock_nbt <- function(object, ...) {
    obj_str(object, ...)
}
#' @export
format.rbedrock_nbt <- function(x, ...) {
    tag <- get_nbt_tag(x)
    suffix <- c("B", "S", "", "L", "F", "", "B", "", "", "", "", "L")
    suffix <- suffix[tag]
    paste0(format(payload(x), nsmall = 1), suffix)
}
#' @export
format.rbedrock_nbt_string <- function(x, ...) {
    paste0("\"", format(payload(x)), "\"")
}
#' @export
format.rbedrock_nbt_raw_string <- function(x, ...) {
    format(payload(x))
}
#' @export
format.rbedrock_nbt_list <- function(x, ...) {
    lapply(payload(x), format)
}
#' @export
format.rbedrock_nbt_compound <- function(x, ...) {
    lapply(payload(x), format)
}

# ptypes -----------------------------------------------------------------------

#' @export
vec_ptype_full.rbedrock_nbt_list <- function(x, ...) {
    # override vec_ptype_full.vctrs_list_of
    "rbedrock_nbt_list"
}
#' @export
vec_ptype_full.rbedrock_nbt_long <- function(x, ...) {
    "rbedrock_nbt_long"
}
#' @export
vec_ptype_full.rbedrock_nbt_long_array <- function(x, ...) {
    "rbedrock_nbt_long_array"
}
#' @export
vec_ptype_abbr.rbedrock_nbt <- function(x, ...) {
    "nbt"
}

# coercion and casting ---------------------------------------------------------

# nolint start: object_length_linter

#' @export
vec_ptype2.rbedrock_nbt_byte.rbedrock_nbt_byte <- function(x, y, ...) x

#' @export
vec_ptype2.rbedrock_nbt_short.rbedrock_nbt_short <- function(x, y, ...) x

#' @export
vec_ptype2.rbedrock_nbt_int.rbedrock_nbt_int <- function(x, y, ...) x

#' @export
vec_ptype2.rbedrock_nbt_long.rbedrock_nbt_long <- function(x, y, ...) x

#' @export
vec_ptype2.rbedrock_nbt_float.rbedrock_nbt_float <- function(x, y, ...) x

#' @export
vec_ptype2.rbedrock_nbt_double.rbedrock_nbt_double <- function(x, y, ...) x

#' @export
vec_ptype2.rbedrock_nbt_byte_array.rbedrock_nbt_byte_array <-
    function(x, y, ...) x

#' @export
vec_ptype2.rbedrock_nbt_string.rbedrock_nbt_string <- function(x, y, ...) x

#' @export
vec_ptype2.rbedrock_nbt_raw_string.rbedrock_nbt_raw_string <-
    function(x, y, ...) x

#' @export
vec_ptype2.rbedrock_nbt_compound.rbedrock_nbt_compound <-
    function(x, y, ...) x

#' @export
vec_ptype2.rbedrock_nbt_int_array.rbedrock_nbt_int_array <-
    function(x, y, ...) x

#' @export
vec_ptype2.rbedrock_nbt_long_array.rbedrock_nbt_long_array <-
    function(x, y, ...) x

#' @export
vec_ptype2.rbedrock_nbt_list.rbedrock_nbt_list <- function(x, y, ...) {
    x <- vec_ptype2.vctrs_list_of(x, y, ...)
    structure(x, class = c("rbedrock_nbt_list", "rbedrock_nbt", class(x)))
}

#' @export
vec_ptype2.rbedrock_nbt_byte.integer <- function(x, y, ...) integer()
#' @export
vec_ptype2.integer.rbedrock_nbt_byte <- function(x, y, ...) integer()
#' @export
vec_cast.rbedrock_nbt_byte.integer <- function(x, to, ...) {
    nbt_byte(x)
}
#' @export
vec_cast.integer.rbedrock_nbt_byte <- function(x, to, ...) {
    vec_data(x)
}
#' @export
vec_ptype2.rbedrock_nbt_byte.logical <- function(x, y, ...) integer()
#' @export
vec_ptype2.logical.rbedrock_nbt_byte <- function(x, y, ...) integer()
#' @export
vec_cast.rbedrock_nbt_byte.logical <- function(x, to, ...) {
    nbt_byte(x)
}
#' @export
vec_cast.logical.rbedrock_nbt_byte <- function(x, to, ...) {
    vec_cast(vec_data(x), to)
}

#' @export
vec_ptype2.rbedrock_nbt_short.integer <- function(x, y, ...) integer()
#' @export
vec_ptype2.integer.rbedrock_nbt_short <- function(x, y, ...) integer()
#' @export
vec_cast.rbedrock_nbt_short.logical <- function(x, to, ...) {
    nbt_short(x)
}
#' @export
vec_cast.rbedrock_nbt_short.integer <- function(x, to, ...) {
    nbt_short(x)
}
#' @export
vec_cast.rbedrock_nbt_short.double <- function(x, to, ...) {
    nbt_short(x)
}
#' @export
vec_cast.logical.rbedrock_nbt_short <- function(x, to, ...) {
    vec_cast(vec_data(x), to)
}
#' @export
vec_cast.integer.rbedrock_nbt_short <- function(x, to, ...) {
    vec_cast(vec_data(x), to)
}
#' @export
vec_cast.double.rbedrock_nbt_short <- function(x, to, ...) {
    vec_cast(vec_data(x), to)
}

#' @export
vec_ptype2.rbedrock_nbt_int.integer <- function(x, y, ...) integer()
#' @export
vec_ptype2.integer.rbedrock_nbt_int <- function(x, y, ...) integer()
#' @export
vec_cast.rbedrock_nbt_int.logical <- function(x, to, ...) {
    nbt_int(x)
}
#' @export
vec_cast.rbedrock_nbt_int.integer <- function(x, to, ...) {
    nbt_int(x)
}
#' @export
vec_cast.rbedrock_nbt_int.double <- function(x, to, ...) {
    nbt_int(x)
}
#' @export
vec_cast.logical.rbedrock_nbt_int <- function(x, to, ...) {
    vec_cast(vec_data(x), to)
}
#' @export
vec_cast.integer.rbedrock_nbt_int <- function(x, to, ...) {
    vec_cast(vec_data(x), to)
}
#' @export
vec_cast.double.rbedrock_nbt_int <- function(x, to, ...) {
    vec_cast(vec_data(x), to)
}

#' @export
vec_ptype2.rbedrock_nbt_long.integer64 <- function(x, y, ...) character()
#' @export
vec_ptype2.integer64.rbedrock_nbt_long <- function(x, y, ...) character()
#' @export
vec_cast.rbedrock_nbt_long.logical <- function(x, to, ...) {
    nbt_long(x)
}
#' @export
vec_cast.rbedrock_nbt_long.integer <- function(x, to, ...) {
    nbt_long(x)
}
#' @export
vec_cast.rbedrock_nbt_long.double <- function(x, to, ...) {
    nbt_long(x)
}
#' @export
vec_cast.rbedrock_nbt_long.integer64 <- function(x, to, ...) {
    nbt_long(x)
}
#' @export
vec_cast.logical.rbedrock_nbt_long <- function(x, to, ...) {
    vec_cast(vec_data(x), to)
}
#' @export
vec_cast.integer.rbedrock_nbt_long <- function(x, to, ...) {
    vec_cast(vec_data(x), to)
}
#' @export
vec_cast.double.rbedrock_nbt_long <- function(x, to, ...) {
    vec_cast(vec_data(x), to)
}
#' @export
vec_cast.integer64.rbedrock_nbt_long <- function(x, to, ...) {
    vec_cast(vec_data(x), to)
}

#' @export
vec_ptype2.rbedrock_nbt_float.double <- function(x, y, ...) double()
#' @export
vec_ptype2.double.rbedrock_nbt_float <- function(x, y, ...) double()
#' @export
vec_cast.rbedrock_nbt_float.integer <- function(x, to, ...) {
    nbt_float(x)
}
#' @export
vec_cast.rbedrock_nbt_float.double <- function(x, to, ...) {
    nbt_float(x)
}
#' @export
vec_cast.integer.rbedrock_nbt_float <- function(x, to, ...) {
    vec_cast(vec_data(x), to)
}
#' @export
vec_cast.double.rbedrock_nbt_float <- function(x, to, ...) {
    vec_cast(vec_data(x), to)
}

#' @export
vec_ptype2.rbedrock_nbt_double.double <- function(x, y, ...) double()
#' @export
vec_ptype2.double.rbedrock_nbt_double <- function(x, y, ...) double()
#' @export
vec_cast.rbedrock_nbt_double.integer <- function(x, to, ...) {
    nbt_double(x)
}
#' @export
vec_cast.rbedrock_nbt_double.double <- function(x, to, ...) {
    nbt_double(x)
}
#' @export
vec_cast.integer.rbedrock_nbt_double <- function(x, to, ...) {
    vec_cast(vec_data(x), to)
}
#' @export
vec_cast.double.rbedrock_nbt_double <- function(x, to, ...) {
    vec_cast(vec_data(x), to)
}

#' @export
vec_ptype2.rbedrock_nbt_byte_array.integer <- function(x, y, ...) integer()
#' @export
vec_ptype2.integer.rbedrock_nbt_byte_array <- function(x, y, ...) integer()
#' @export
vec_cast.rbedrock_nbt_byte_array.logical <- function(x, to, ...) {
    nbt_byte_array(x)
}
#' @export
vec_cast.rbedrock_nbt_byte_array.integer <- function(x, to, ...) {
    nbt_byte_array(x)
}
#' @export
vec_cast.rbedrock_nbt_byte_array.double <- function(x, to, ...) {
    nbt_byte_array(x)
}
#' @export
vec_cast.logical.rbedrock_nbt_byte_array <- function(x, to, ...) {
    vec_cast(vec_data(x), to)
}
#' @export
vec_cast.integer.rbedrock_nbt_byte_array <- function(x, to, ...) {
    vec_cast(vec_data(x), to)
}
#' @export
vec_cast.double.rbedrock_nbt_byte_array <- function(x, to, ...) {
    vec_cast(vec_data(x), to)
}

#' @export
vec_ptype2.rbedrock_nbt_int_array.integer <- function(x, y, ...) integer()
#' @export
vec_ptype2.integer.rbedrock_nbt_int_array <- function(x, y, ...) integer()
#' @export
vec_cast.rbedrock_nbt_int_array.logical <- function(x, to, ...) {
    nbt_int_array(x)
}
#' @export
vec_cast.rbedrock_nbt_int_array.integer <- function(x, to, ...) {
    nbt_int_array(x)
}
#' @export
vec_cast.rbedrock_nbt_int_array.double <- function(x, to, ...) {
    nbt_int_array(x)
}
#' @export
vec_cast.logical.rbedrock_nbt_int_array <- function(x, to, ...) {
    vec_cast(vec_data(x), to)
}
#' @export
vec_cast.integer.rbedrock_nbt_int_array <- function(x, to, ...) {
    vec_cast(vec_data(x), to)
}
#' @export
vec_cast.double.rbedrock_nbt_int_array <- function(x, to, ...) {
    vec_cast(vec_data(x), to)
}

#' @export
vec_ptype2.rbedrock_nbt_long_array.integer <-
    function(x, y, ...) character()
#' @export
vec_ptype2.integer.rbedrock_nbt_long_array <-
    function(x, y, ...) character()
#' @export
vec_cast.rbedrock_nbt_long_array.logical <- function(x, to, ...) {
    nbt_long_array(x)
}
#' @export
vec_cast.rbedrock_nbt_long_array.integer <- function(x, to, ...) {
    nbt_long_array(x)
}
#' @export
vec_cast.rbedrock_nbt_long_array.double <- function(x, to, ...) {
    nbt_long_array(x)
}
#' @export
vec_cast.rbedrock_nbt_long_array.integer64 <- function(x, to, ...) {
    nbt_long_array(x)
}
#' @export
vec_cast.logical.rbedrock_nbt_long_array <- function(x, to, ...) {
    vec_cast(vec_data(x), to)
}
#' @export
vec_cast.integer.rbedrock_nbt_long_array <- function(x, to, ...) {
    vec_cast(vec_data(x), to)
}
#' @export
vec_cast.double.rbedrock_nbt_long_array <- function(x, to, ...) {
    vec_cast(vec_data(x), to)
}
#' @export
vec_cast.integer64.rbedrock_nbt_long_array <- function(x, to, ...) {
    vec_cast(vec_data(x), to)
}

#' @export
vec_ptype2.rbedrock_nbt_string.character <- function(x, y, ...) character()
#' @export
vec_ptype2.character.rbedrock_nbt_string <- function(x, y, ...) character()
#' @export
vec_cast.rbedrock_nbt_string.character <- function(x, to, ...) {
    nbt_string(x)
}
#' @export
vec_cast.character.rbedrock_nbt_string <- function(x, to, ...) {
    vec_cast(vec_data(x), to)
}

#' @export
vec_ptype2.rbedrock_nbt_raw_string.raw <- function(x, y, ...) raw()
#' @export
vec_ptype2.raw.rbedrock_nbt_raw_string <- function(x, y, ...) raw()
#' @export
vec_cast.rbedrock_nbt_raw_string.character <- function(x, to, ...) {
    nbt_raw_string(x)
}
#' @export
vec_cast.character.rbedrock_nbt_raw_string <- function(x, to, ...) {
    vec_cast(vec_data(x), to)
}


#' @export
vec_cast.rbedrock_nbt_list.rbedrock_nbt_list <- function(x, to, ...) {
    x <- as_list_of(x, .ptype = attr(to, "ptype"))
    structure(x, class = class(to))
}
#' @export
vec_cast.rbedrock_nbt_list.vctrs_list_of <- function(x, to, ...) {
    x <- as_list_of(x, .ptype = attr(to, "ptype"))
    structure(x, class = class(to))
}
#' @export
vec_cast.rbedrock_nbt_list.double <- function(x, to, ...) {
    x <- as_list_of(as.list(x), .ptype = attr(to, "ptype"))
    structure(x, class = class(to))
}
#' @export
vec_cast.rbedrock_nbt_list.integer <- function(x, to, ...) {
    x <- as_list_of(as.list(x), .ptype = attr(to, "ptype"))
    structure(x, class = class(to))
}
#' @export
vec_cast.rbedrock_nbt_list.character <- function(x, to, ...) {
    x <- as_list_of(as.list(x), .ptype = attr(to, "ptype"))
    structure(x, class = class(to))
}
#' @export
vec_cast.rbedrock_nbt_list.logical <- function(x, to, ...) {
    x <- as_list_of(as.list(x), .ptype = attr(to, "ptype"))
    structure(x, class = class(to))
}
#' @export
vec_cast.rbedrock_nbt_list.integer64 <- function(x, to, ...) {
    x <- as_list_of(as.list(x), .ptype = attr(to, "ptype"))
    structure(x, class = class(to))
}
# nolint end
