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
#' @param ... Arguments to collect into an NBT compound or NBT list value.
#'     Supports dynamic dots via `rlang::list2()`.
#'
#'
#' @rdname nbt
#' @export
nbt_byte <- function(x) {
    new_nbt_byte(vec_cast(x, integer()))
}

#' @rdname nbt
#' @export
nbt_short <- function(x) {
    new_nbt_short(vec_cast(x, integer()))
}

#' @rdname nbt
#' @export
nbt_int <- function(x) {
    new_nbt_int(vec_cast(x, integer()))
}

#' @rdname nbt
#' @export
nbt_long <- function(x) {
    new_nbt_long(vec_cast(x, bit64::integer64()))
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
    new_nbt_byte_array(vec_cast(x, integer()))
}

#' @rdname nbt
#' @export
nbt_string <- function(x) {
    new_nbt_string(vec_cast(x, character()))
}

#' @rdname nbt
#' @export
nbt_int_array <- function(x) {
    new_nbt_int_array(vec_cast(x, integer()))
}

#' @rdname nbt
#' @export
nbt_long_array <- function(x) {
    new_nbt_long_array(vec_cast(x, bit64::integer64()))
}

#' @rdname nbt
#' @export
nbt_compound <- function(...) {
    new_nbt_compound(list2(...))
}

#' @rdname nbt
#' @export
nbt_list <- function(...) {
    new_nbt_list(list2(...))
}

#' @rdname nbt
#' @export
is_nbt <- function(x) {
    inherits(x, "rbedrock_nbt")
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
    structure(vec_data(x), class="integer64")
}

#' @export
payload.rbedrock_nbt_long_array <- function(x) {
    structure(vec_data(x), class="integer64")
}

#' @rdname nbt
#' @export
unnbt <- function(x) {
    if(is_list(x)) {
        rapply(x, payload, how="list")
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
    if(!is_nbt(value)) {
        value <- vec_cast(value, vec_ptype(x[[i]]), x_arg="value")
        if(!is_nbt(value)) {
            abort("conversion of value to nbt failed.")
        }
    }
    NextMethod()
}

#' @export
`[[<-.rbedrock_nbt_container` <- function(x, i, value) {
    if(!is_nbt(value)) {
        value <- vec_cast(value, vec_ptype(x[[i]]), x_arg="value")
        if(!is_nbt(value)) {
            abort("conversion of value to nbt failed.")
        }
    }
    NextMethod()
}

.tag_assert <- function(tag, allowed) {
    vec_assert(tag, ptype=integer(), size=1L)
    if(!tag %in% allowed) {
        msg <- paste0("invalid tag `", tag, "`")
        abort(msg)
    }
    return()
}

#' Create an NBT value
#'
#' @param x An nbt payload.
#' @param tag An integer specifying the tag of the data.
#' @keywords internal
#' @rdname new_nbt
#' @export
new_nbt <- function(x, tag) {
    tag <- vec_recycle(vec_cast(tag, integer()), 1L, x_arg = "tag")
    switch(tag,
        new_nbt_byte(x),
        new_nbt_short(x),
        new_nbt_int(x),
        new_nbt_long(x),
        new_nbt_float(x),
        new_nbt_double(x),
        new_nbt_byte_array(x),
        new_nbt_string(x),
        new_nbt_list(x),
        new_nbt_compound(x),
        new_nbt_int_array(x),
        new_nbt_long_array(x)
    )
}

new_rbedrock_nbt_scalar <- function(x, class, ptype = NULL, size = NULL) {
    vec_assert(x, ptype = ptype, size = size)
    new_vctr(x, class=c(paste0("rbedrock_nbt_", class), "rbedrock_nbt"))
}

.fixup_long <- function(x) {
    cls <- setdiff(class(x), "integer64")
    pos <- which(cls == "vctrs_vctr")
    class(x) <- append(cls, "integer64", pos-1)
    x
}

#' @keywords internal
#' @rdname new_nbt
#' @export
new_nbt_byte <- function(x) {
    new_rbedrock_nbt_scalar(x, "byte", integer(), 1)
}
#' @keywords internal
#' @rdname new_nbt
#' @export
new_nbt_short <- function(x) {
    new_rbedrock_nbt_scalar(x, "short", integer(), 1)
}
#' @keywords internal
#' @rdname new_nbt
#' @export
new_nbt_int <- function(x) {
    new_rbedrock_nbt_scalar(x, "int", integer(), 1)
}
#' @keywords internal
#' @rdname new_nbt
#' @export
new_nbt_long <- function(x) {
    x <- new_rbedrock_nbt_scalar(x, "long", bit64::integer64(), 1)
    .fixup_long(x)
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
    new_rbedrock_nbt_scalar(x, "byte_array", integer())
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
new_nbt_int_array <- function(x) {
    new_rbedrock_nbt_scalar(x, "int_array", integer())
}
#' @keywords internal
#' @rdname new_nbt
#' @export
new_nbt_long_array <- function(x) {
    x <- new_rbedrock_nbt_scalar(x, "long_array", bit64::integer64())
    .fixup_long(x)   
}
#' @keywords internal
#' @rdname new_nbt
#' @export
new_nbt_compound <- function(x) {
    vec_assert(x, ptype=list())
    if(!all(purrr::map_lgl(x, is_nbt))) {
        abort("an nbt_compound can only hold nbt data")
    }
    new_vctr(x, class=c("rbedrock_nbt_compound",
        "rbedrock_nbt", "rbedrock_nbt_container"))
}
#' @keywords internal
#' @rdname new_nbt
#' @export
new_nbt_list <- function(x) {
    ptype <- NULL
    if(length(x) == 0) {
        # use a ptype of an empty list for an empty nbt_list
        ptype=list()
    }
    y <- list_of(!!!x, .ptype = ptype)
    cls <- class(y)
    structure(y, class=c("rbedrock_nbt_list", "rbedrock_nbt", cls))
}

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

#' Read and Write NBT Data
#'
#' The Named Binary Tag (NBT) format is used by Minecraft for various data types.
#'
#' @description
#' `get_nbt_data()` and `get_nbt_value()` load nbt-formatted data from `db` and parses it.
#' `get_nbt_values()` is a synonym for `get_nbt_data()`.
#'
#' @param db A `bedrockdb` object
#' @param keys A character vector of keys.
#' @param readoptions A `bedrock_leveldb_readoptions` object
#' @param simplify If TRUE, simplifies a list containing a single unnamed `nbtnode`.
#' @export
get_nbt_data <- function(db, keys, readoptions = NULL, simplify = TRUE) {
    dat <- get_values(db, keys, readoptions = readoptions)
    read_nbt_data(dat, simplify = simplify)
}

#' @param key  A single key.
#' @rdname get_nbt_data
#' @export
get_nbt_value <- function(db, key, readoptions = NULL, simplify = TRUE) {
    dat <- get_value(db, key, readoptions = readoptions)
    read_nbt(dat, simplify = simplify)
}

#' @rdname get_nbt_data
#' @export
get_nbt_values <- get_nbt_data

#' @description
#' `put_nbt_values`, `put_nbt_value`, and `put_nbt_data` stores nbt data into `db` in binary form.
#'
#' @param values A list of nbt objects
#' @param writeoptions A `bedrock_leveldb_writeoptions` object
#' @rdname get_nbt_data
#' @export
put_nbt_values <- function(db, keys, values, writeoptions = NULL) {
    dat <- write_nbt_data(values)
    put_values(db, keys, dat, writeoptions = writeoptions)
}

#' @param value An nbt object.
#' @rdname get_nbt_data
#' @export
put_nbt_value <- function(db, key, value, writeoptions = NULL) {
    dat <- write_nbt(value)
    put_value(db, key, dat, writeoptions = writeoptions)
}

#' @rdname get_nbt_data
#' @param data A named-list specifying key-value pairs.
#' @export
put_nbt_data <- function(db, data, writeoptions = NULL) {
    dat <- write_nbt_data(data)
    put_data(db, dat, writeoptions = writeoptions)
}

#' @description
#' `read_nbt` reads NBT data from a `raw` vector.
#'
#' @param rawdata A `raw` vector
#' @rdname get_nbt_data
#' @export
read_nbt <- function(rawdata, simplify = TRUE) {
    res <- read_rnbt(rawdata)
    res <- from_rnbt(res)
    if(isTRUE(simplify) && length(res) == 1L && is.null(attributes(res))) {
        res <- res[[1]]
    }
    res
}

#' @description
#' `read_nbt_data` calls `read_nbt` on each element of a list.
#'
#' @rdname get_nbt_data
#' @export
read_nbt_data <- function(data, simplify = TRUE) {
    purrr::map(data, read_nbt, simplify = simplify)
}

#' @description
#' `write_nbt` encodes NBT data into a `raw` vector.
#'
#' @param object An nbt object or a list of nbt objects
#' @rdname get_nbt_data
#' @export
write_nbt <- function(object) {
    if(is_nbt(object)) {
        object <- list(object)
    }
    object <- to_rnbt(object)
    .Call(Cwrite_nbt, object)
}

#' @description
#' `write_nbt_data` calls `write_nbt` on each element of a list.
#'
#' @rdname get_nbt_data
#' @export
write_nbt_data <- function(data) {
    purrr::map(data, write_nbt)
}

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
    # extract names
    n <- purrr::map_chr(x, "name", .default="")
    # extract values
    v <- purrr::map(x, function(y) {
        from_rnbt_payload(y[["payload"]], y[["tag"]])
    })
    # Set names if any exist
    if(all(n == "")) {
        v
    } else {
        set_names(v, n)
    }
}

#' @rdname rnbt
#' @keywords internal
#' @export
to_rnbt <- function(x) {
    n <- names(x) %||% rep("", length(x))
    names(x) <- NULL
    purrr::map2(x, n, function(y, z) {
        vec_c(list(name = z), to_rnbt_payload(y), .ptype=list())
    })
}

#' @rdname rnbt
#' @keywords internal
#' @export
from_rnbt_payload <- function(x, tag) {
    if(tag == 9L) {
        v <- purrr::map(x, function(y) {
            from_rnbt_payload(y[["payload"]], y[["tag"]])
        })
        new_nbt_list(v)
    } else if(tag == 10L) {
        ret <- from_rnbt(x)
        new_nbt_compound(ret)
    } else {
        new_nbt(x, tag)
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
    list(tag = get_nbt_tag(x), payload = payload(x))
}

#' @export
to_rnbt_payload.rbedrock_nbt_compound <- function(x) {
    list(tag = 10L, payload = to_rnbt(x))
}

#' @export
to_rnbt_payload.rbedrock_nbt_list <- function(x) {
    list(tag = 9L, payload = purrr::map(x, to_rnbt_payload))
}

#' @rdname rnbt
#' @keywords internal
#' @export
read_rnbt <- function(rawdata) {
    .Call(Cread_nbt, rawdata)
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
    paste0(format(payload(x), nsmall=1), suffix)
}
#' @export
format.rbedrock_nbt_string <- function(x, ...) {
    paste0("\"", format(payload(x)), "\"")
}
#' @export
format.rbedrock_nbt_list <- function(x, ...) {
    lapply(payload(x),format)
}
#' @export
format.rbedrock_nbt_compound <- function(x, ...) {
    lapply(payload(x),format)
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
vec_ptype2.rbedrock_nbt_byte_array.rbedrock_nbt_byte_array <- function(x, y, ...) x

#' @export
vec_ptype2.rbedrock_nbt_string.rbedrock_nbt_string <- function(x, y, ...) x

#' @export
vec_ptype2.rbedrock_nbt_compound.rbedrock_nbt_compound <- function(x, y, ...) x

#' @export
vec_ptype2.rbedrock_nbt_int_array.rbedrock_nbt_int_array <- function(x, y, ...) x

#' @export
vec_ptype2.rbedrock_nbt_long_array.rbedrock_nbt_long_array <- function(x, y, ...) x

#' @export
vec_ptype2.rbedrock_nbt_list.rbedrock_nbt_list <- function(x, y, ...) {
    x <- vec_ptype2.vctrs_list_of(x, y, ...)
    structure(x, class=c("rbedrock_nbt_list", "rbedrock_nbt", class(x)))
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
vec_cast.rbedrock_nbt_short.logical<- function(x, to, ...) {
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
vec_cast.rbedrock_nbt_int.logical<- function(x, to, ...) {
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
vec_ptype2.rbedrock_nbt_long.integer64 <- function(x, y, ...) bit64::integer64()
#' @export
vec_ptype2.integer64.rbedrock_nbt_long <- function(x, y, ...) bit64::integer64()
#' @export
vec_cast.rbedrock_nbt_long.logical<- function(x, to, ...) {
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
vec_cast.rbedrock_nbt_byte_array.logical<- function(x, to, ...) {
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
vec_cast.rbedrock_nbt_int_array.logical<- function(x, to, ...) {
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
vec_ptype2.rbedrock_nbt_long_array.integer <- function(x, y, ...) bit64::integer64()
#' @export
vec_ptype2.integer.rbedrock_nbt_long_array <- function(x, y, ...) bit64::integer64()
#' @export
vec_cast.rbedrock_nbt_long_array.logical<- function(x, to, ...) {
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

