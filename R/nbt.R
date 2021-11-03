# #' Create an NBT value
# #'
# #' @description
# #' The Named Binary Tag (NBT) format is used by Minecraft for various data
# #' types. An NBT value holds a 'payload' of data and a 'tag' indicating the type
# #' of data held.
# #'
# #' @description
# #' `nbt()` creates an nbt value. `nbt_*()` family of functions are wrappers
# #' around `nbt()` to create specific tags. `unnbt()` recursively strips NBT
# #' metadata from an NBT value.
# #'
# #' @param x An nbt payload.
# #' @param tag The tag of the data.
# #' @param ... Arguments to collect into an NBT compound or NBT list value.
# #'     Supports dynamic dots via `rlang::list2()`.
# #'
# #' @export
# nbt <- function(x = list(), tag = 0L) {
#     tag <- vec_recycle(vec_cast(tag, integer()), 1L, x_arg = "tag")
#     if(tag == 0L || tag == 9L || tag == 10L) {
#         x <- vec_cast(x, list())
#     } else if(tag == 1L || tag == 2L || tag == 3L ||
#         tag == 7L || tag == 11L) {
#         x <- vec_cast(x, integer())
#     } else if(tag == 4L || tag == 12L) {
#         x <- vec_cast(x, bit64::integer64())
#     } else if(tag == 5L || tag == 6L) {
#         x <- vec_cast(x, double())
#     } else if(tag == 8L) {
#         x <- vec_cast(x, character())
#     }
#     new_nbt(x, tag = tag)
# }

tag_str <- function(x) {
    chr <- c("END", "BYTE", "SHORT", "INT", "LONG", "FLOAT",
        "DOUBLE", "BYTE_ARRAY", "STRING", "LIST", "COMPOUND",
        "INT_ARRAY", "LONG_ARRAY")
    chr[1L+tag(x)]
}

# #' @export
# print.rbedrock_nbt <- function(x, ...) {
#   obj_print(x, ...)
#   invisible(x)
# }

# #' @export
# str.rbedrock_nbt <- function(object, ...) {
#   obj_str(object, ...)
# }

# #' @export
# format.rbedrock_nbt <- function(x, ...) {
#     tag <- tag(x)

#     suffix <- c("B","S","", "L", "F", "", "B", "", "", "", "", "L")

#     if(tag == 0L) {
#         NA_character_
#     } else if(1L <= tag && tag <= 6L) {
#         paste0(format(payload(x), nsmall=1), suffix[tag])
#     } else if(tag == 7L || tag == 11L || tag == 12L) {
#         paste0(format(payload(x), nsmall=1), suffix[tag])
#     } else if(tag == 8L) {
#         paste0("\"", payload(x), "\"")
#     } else if(tag == 9L) {
#         lapply(payload(x), format)
#     } else if(tag == 10L) {
#         lapply(payload(x), format)
#     } else {
#         NA_character_
#     }
# }

# #' @rdname nbt
# #' @export
# unnbt <- function(x) {
#     if(is.list(x)) {
#         rapply(x, payload, how="list")
#     } else {
#         payload(x)
#     }
# }

# #' @export
# `$<-.rbedrock_nbt` <- function(x, i, value) {
#     if(tag(x) == 10L) {
#         value <- vec_cast(value, vec_ptype(x[[i]]))
#     }
#     NextMethod()
# }

# #' @export
# `[[<-.rbedrock_nbt` <- function(x, i, value) {
#     if(tag(x) == 10L) {
#         value <- vec_cast(value, vec_ptype(x[[i]]))
#     }
#     NextMethod()    
# }

# #' @export
# vec_ptype_abbr.rbedrock_nbt <- function(x, ...) {
#     "nbt"
# }

# #' @export
# vec_ptype_full.rbedrock_nbt <- function(x, ...) {
#     paste0("rbedrock_nbt<", tag_str(x), ">")
# }

# #' @description
# #' `payload()` and `payload<-()` read and write an nbt value's payload.
# #'
# #' @param object An nbt value
# #' @param value A new payload
# #'
# #' @rdname nbt
# #' @export
# `payload` <- function(object) {
#     UseMethod('payload', object)
# }

# #' @export
# `payload.rbedrock_nbt` <- function(object) {
#     if(bit64::is.integer64(object)) {
#         structure(vec_data(object), class="integer64")
#     } else {
#         vec_data(object)
#     }
# }

# #' @rdname nbt
# #' @export
# `payload<-` <- function(object, value) {
#     UseMethod('payload<-', object)
# }

# #' @export
# `payload<-.rbedrock_nbt` <- function(object, value) {
#     object[] <- value
#     object
# }

# #' @export
# vec_cast.rbedrock_nbt.rbedrock_nbt <- function(x, to, ...) {
#     if (tag(x) != tag(to) || (tag(x) == 9L && list_tag(x) != list_tag(to))) {
#         # try to convert payloads if different nbt types
#         nbt(payload(x), tag(to))
#     } else {
#         # return x unchanged if it matches to. This allows casts with
#         # matching zero-length vectors to bypass asserts in nbt().
#         x
#     }
# }

# #' @export
# vec_cast.rbedrock_nbt.logical <- function(x, to, ...) nbt(x, tag(to))

# #' @export
# vec_cast.rbedrock_nbt.character <- function(x, to, ...) nbt(x, tag(to))

# #' @export
# vec_cast.rbedrock_nbt.integer64 <- function(x, to, ...) nbt(x, tag(to))

# #' @export
# vec_cast.rbedrock_nbt.integer <- function(x, to, ...) nbt(x, tag(to))

# #' @export
# vec_cast.rbedrock_nbt.double <- function(x, to, ...) nbt(x, tag(to))

# #' @export
# vec_cast.rbedrock_nbt.list <- function(x, to, ...) nbt(x, tag(to))

# #' @export
# vec_cast.logical.rbedrock_nbt <- function(x, to, ...) {
#     vec_cast(payload(x), logical())
# }

# #' @export
# vec_cast.character.rbedrock_nbt <- function(x, to, ...) {
#     vec_cast(payload(x), character())
# }

# #' @export
# vec_cast.integer.rbedrock_nbt <- function(x, to, ...) {
#     vec_cast(payload(x), integer())
# }

# #' @export
# vec_cast.double.rbedrock_nbt <- function(x, to, ...) {
#     vec_cast(payload(x), double())
# }

# #' @export
# vec_cast.integer64.rbedrock_nbt <- function(x, to, ...) {
#     vec_cast(payload(x), bit64::integer64())
# }

# #' @export
# vec_cast.list.rbedrock_nbt <- function(x, to, ...) {
#     vec_cast(payload(x), list())
# }

# #' @export
# vec_ptype2.rbedrock_nbt.rbedrock_nbt <- function(x, y, ..., x_arg = "", y_arg = "") {
#     if (tag(x) != tag(y) || (tag(x) == 9L && list_tag(x) != list_tag(y))) {
#         stop_incompatible_type(x, y, x_arg = x_arg, y_arg = y_arg)
#     }
#     x
# }

# #' @export
# vec_ptype2.rbedrock_nbt.character <- function(x, y, ...) character()

# #' @export
# vec_ptype2.character.rbedrock_nbt <- function(x, y, ...) character()

# #' @export
# vec_ptype2.rbedrock_nbt.logical <- function(x, y, ...) logical()

# #' @export
# vec_ptype2.logical.rbedrock_nbt <- function(x, y, ...) logical()

# #' @export
# vec_ptype2.rbedrock_nbt.integer <- function(x, y, ...) integer()

# #' @export
# vec_ptype2.integer.rbedrock_nbt <- function(x, y, ...) integer()

# #' @export
# vec_ptype2.rbedrock_nbt.double <- function(x, y, ...) double()

# #' @export
# vec_ptype2.double.rbedrock_nbt <- function(x, y, ...) double()

# #' @export
# vec_ptype2.integer64.rbedrock_nbt <- function(x, y, ...) bit64::integer64()

# #' @export
# vec_ptype2.rbedrock_nbt.integer64 <- function(x, y, ...) bit64::integer64()

# # work around integer64 methods not stripping class variables
# #' @export
# is.na.rbedrock_nbt <- function(x, ...) vec_data(NextMethod())

# #' Read and Write NBT Data
# #'
# #' The Named Binary Tag (NBT) format is used by Minecraft for various data types.
# #'
# #' @description
# #' `get_nbt_data()` and `get_nbt_value()` load nbt-formatted data from `db` and parses it.
# #' `get_nbt_values()` is a synonym for `get_nbt_data()`.
# #'
# #' @param db A `bedrockdb` object
# #' @param keys A character vector of keys.
# #' @param readoptions A `bedrock_leveldb_readoptions` object
# #' @param max_elements Maximum number of elements to parse.
# #' @param simplify If TRUE, simplifies a list containing a single unnamed `nbtnode`.
# #' @export
# get_nbt_data <- function(db, keys, readoptions = NULL, max_elements = NULL, simplify=TRUE) {
#     dat <- get_values(db, keys, readoptions = readoptions)
#     read_nbt_data(dat, max_elements = max_elements, simplify = simplify)
# }

# #' @param key  A single key.
# #' @rdname get_nbt_data
# #' @export
# get_nbt_value <- function(db, key, readoptions = NULL, max_elements = NULL, simplify=TRUE) {
#     dat <- get_value(db, key, readoptions = readoptions)
#     read_nbt(dat, max_elements = max_elements, simplify = simplify)
# }

# #' @rdname get_nbt_data
# #' @export
# get_nbt_values <- get_nbt_data

# #' @description
# #' `put_nbt_values`, `put_nbt_value`, and `put_nbt_data` stores nbt data into `db` in binary form.
# #'
# #' @param values A list of nbt objects
# #' @param writeoptions A `bedrock_leveldb_writeoptions` object
# #' @rdname get_nbt_data
# #' @export
# put_nbt_values <- function(db, keys, values, writeoptions = NULL) {
#     dat <- write_nbt_data(values)
#     put_values(db, keys, dat, writeoptions = writeoptions)
# }

# #' @param value An nbt object.
# #' @rdname get_nbt_data
# #' @export
# put_nbt_value <- function(db, key, value, writeoptions = NULL) {
#     dat <- write_nbt(value)
#     put_value(db, key, dat, writeoptions = writeoptions)
# }

# #' @rdname get_nbt_data
# #' @param data A named-list specifying key-value pairs.
# #' @export
# put_nbt_data <- function(db, data, writeoptions = NULL) {
#     dat <- write_nbt_data(data)
#     put_data(db, dat, writeoptions = writeoptions)
# }

#' @rdname nbt
#' @export
is_nbt <- function(x) {
    inherits(x, "rbedrock_nbt")
}

is_nbt_list <- function(x) {
    inherits(x, "rbedrock_nbt_list")
}

is_nbt_compound <- function(x) {
    inherits(x, "rbedrock_nbt_compound")
}

#' @export
`$.rbedrock_nbt_container` <- function(x,i) {
    NextMethod()
}

#' @export
`$<-.rbedrock_nbt_container` <- function(x,i,value) {
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

# #' Create an NBT value
# #'
# #' @param x An nbt payload.
# #' @param tag An integer specifying the tag of the data.
# #' @keywords internal
# #' @export
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

new_nbt_byte <- function(x) {
    new_rbedrock_nbt_scalar(x, "byte", integer(), 1)
}

new_nbt_short <- function(x) {
    new_rbedrock_nbt_scalar(x, "short", integer(), 1)
}

new_nbt_int <- function(x) {
    new_rbedrock_nbt_scalar(x, "int", integer(), 1)
}

new_nbt_long <- function(x) {
    x <- new_rbedrock_nbt_scalar(x, "long", bit64::integer64(), 1)
    .fixup_long(x)
}

new_nbt_float <- function(x) {
    new_rbedrock_nbt_scalar(x, "float", double(), 1)
}

new_nbt_double <- function(x) {
    new_rbedrock_nbt_scalar(x, "double", double(), 1)
}

new_nbt_byte_array <- function(x) {
    new_rbedrock_nbt_scalar(x, "byte_array", integer())
}

new_nbt_string <- function(x) {
    new_rbedrock_nbt_scalar(x, "character", character(), 1)
}

new_nbt_int_array <- function(x) {
    new_rbedrock_nbt_scalar(x, "int_array", integer())
}

new_nbt_long_array <- function(x) {
    x <- new_rbedrock_nbt_scalar(x, "long_array", bit64::integer64())
    .fixup_long(x)   
}

new_nbt_compound <- function(x) {
    vec_assert(x, ptype=list())
    if(!all(purrr::map_lgl(x, is_nbt))) {
        abort("an nbt_compound can only hold nbt data")
    }
    structure(x, class=c("rbedrock_nbt_compound",
        "rbedrock_nbt", "rbedrock_nbt_container"))
}

new_nbt_list <- function(x) {
    if(length(x) == 0) {
        # use a ptype of an empty list for an empty nbt_list
        x <- list(list())
    }
    y <- list_of(!!!x)
    cls <- class(y)
    structure(y, class=c("rbedrock_nbt_list", "rbedrock_nbt", cls))
}

new_nbt_byte_list <- function(x) {
    new_rbedrock_nbt_scalar(x, "byte_list", integer())
}

new_nbt_short_list <- function(x) {
    new_rbedrock_nbt_scalar(x, "short_list", integer())
}

new_nbt_int_list <- function(x) {
    new_rbedrock_nbt_scalar(x, "int_list", integer())
}

new_nbt_long_list <- function(x) {
    x <- new_rbedrock_nbt_scalar(x, "long_list", bit64::integer64())
    .fixup_long(x) 
}

new_nbt_float_list <- function(x) {
    new_rbedrock_nbt_scalar(x, "float_list", integer())
}

new_nbt_double_list <- function(x) {
    new_rbedrock_nbt_scalar(x, "double_list", integer())
}

new_nbt_string_list <- function(x) {
    new_rbedrock_nbt_scalar(x, "string_list", character())
}

######

nbt_byte <- function(x) {
    new_nbt_byte(vec_cast(x, integer()))
}

nbt_short <- function(x) {
    new_nbt_short(vec_cast(x, integer()))
}

nbt_int <- function(x) {
    new_nbt_int(vec_cast(x, integer()))
}

nbt_long <- function(x) {
    new_nbt_long(vec_cast(x, bit64::integer64()))
}

nbt_float <- function(x) {
    new_nbt_float(vec_cast(x, double()))
}

nbt_double <- function(x) {
    new_nbt_double(vec_cast(x, double()))
}

nbt_byte_array <- function(x) {
    new_nbt_byte_array(vec_cast(x, integer()))
}

nbt_string <- function(x) {
    new_nbt_string(vec_cast(x, character()))
}

nbt_int_array <- function(x) {
    new_nbt_int_array(vec_cast(x, integer()))
}

nbt_long_array <- function(x) {
    new_nbt_long_array(vec_cast(x, bit64::integer64()))
}

nbt_compound <- function(...) {
    new_nbt_compound(list2(...))
}

nbt_list <- function(...) {
    new_nbt_list(list2(...))
}

######

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

######

#' @export
get_nbt_tag <- function(x) {
    UseMethod("get_nbt_tag", x)
}

get_nbt_tag.rbedrock_nbt_byte       <- function(x) 1L
get_nbt_tag.rbedrock_nbt_short      <- function(x) 2L
get_nbt_tag.rbedrock_nbt_int        <- function(x) 3L
get_nbt_tag.rbedrock_nbt_long       <- function(x) 4L
get_nbt_tag.rbedrock_nbt_float      <- function(x) 5L
get_nbt_tag.rbedrock_nbt_double     <- function(x) 6L
get_nbt_tag.rbedrock_nbt_byte_array <- function(x) 7L
get_nbt_tag.rbedrock_nbt_string     <- function(x) 8L
get_nbt_tag.rbedrock_nbt_list       <- function(x) 9L
get_nbt_tag.rbedrock_nbt_compound   <- function(x) 10L
get_nbt_tag.rbedrock_nbt_int_array  <- function(x) 11L
get_nbt_tag.rbedrock_nbt_long_array <- function(x) 12L

######
#' @export
vec_ptype_full.rbedrock_nbt_list <- function(x, ...) {
    "rbedrock_nbt_list"
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

# work around integer64 methods not stripping all class variables
#' @export
is.na.rbedrock_nbt_long <- function(x, ...) vec_data(NextMethod())

#' @export
is.na.rbedrock_nbt_long_array <- function(x, ...) vec_data(NextMethod())

#' @export
`==.rbedrock_nbt_long` <- function(e1, e2) vec_data(NextMethod())

#' @export
`==.rbedrock_nbt_long_array` <- function(e1, e2) vec_data(NextMethod())


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

#' @rdname get_nbt_data
#' @export
read_rnbt <- function(rawdata) {
    .Call(Cread_nbt, rawdata)
}


#' @description
#' `read_nbt_data` calls `read_nbt` on each element of a list.
#'
#' @rdname get_nbt_data
#' @export
read_nbt_data <- function(data, simplify = TRUE) {
    purrr::map(data, read_nbt, simplify = simplify)
}


# #' @description
# #' `write_nbt` encodes NBT data into a `raw` vector.
# #'
# #' @param object An nbt object or a list of nbt objects
# #' @rdname get_nbt_data
# #' @export
# write_nbt <- function(object) {
#     if(is_nbt(object)) {
#         object <- list(object)
#     }
#     .Call(Cwrite_nbt, object)
# }


# #' @description
# #' `write_nbt_data` calls `write_nbt` on each element of a list.
# #'

# #' @rdname get_nbt_data
# #' @export
# write_nbt_data <- function(data) {
#     purrr::map(data, write_nbt)
# }