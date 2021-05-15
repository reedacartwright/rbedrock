#' Create an NBT value
#'
#' @description
#' The Named Binary Tag (NBT) format is used by Minecraft for various data
#' types. An NBT value holds a 'payload' of data and a 'tag' indicating the type
#' of data held.
#'
#' @description
#' `nbt()` creates an nbt value. `nbt_*()` family of functions are wrappers
#' around `nbt()` to create specific tags. `unnbt()` recursively strips NBT
#' metadata from an NBT value.
#'
#' @param x An nbt payload.
#' @param tag The tag of the data.
#' @param ... Arguments to collect into an NBT compound or NBT list value.
#'     Supports dynamic dots via `rlang::list2()`.
#'
#' @export
nbt <- function(x = list(), tag = 0L) {
    tag <- vec_recycle(vec_cast(tag, integer()), 1L, x_arg = "tag")
    if(tag == 0L || tag == 9L || tag == 10L) {
        x <- vec_cast(x, list())
    } else if(tag == 1L || tag == 2L || tag == 3L ||
        tag == 7L || tag == 11L) {
        x <- vec_cast(x, integer())
    } else if(tag == 4L || tag == 12L) {
        x <- vec_cast(x, bit64::integer64())
    } else if(tag == 5L || tag == 6L) {
        x <- vec_cast(x, double())
    } else if(tag == 8L) {
        x <- vec_cast(x, character())
    }
    new_nbt(x, tag = tag)
}

#' Create an NBT value
#'
#' @param x An nbt payload.
#' @param tag An integer specifying the tag of the data.
#' @keywords internal
#' @export
new_nbt <- function(x = list(), tag = 0L) {
    vec_assert(tag, ptype = integer(), size = 1L)

    cls = "rbedrock_nbt"

    if(tag == 0L) {
        vec_assert(x, ptype = list(), size = 0L)
    } else if(tag == 1L || tag == 2L || tag == 3L) {
        vec_assert(x, ptype = integer(), size = 1L)
    } else if(tag == 4L) {
        vec_assert(x, ptype = bit64::integer64(), size = 1L)
        cls <- c(cls, "integer64")
    } else if(tag == 5L || tag == 6L) {
        vec_assert(x, ptype = double(), size = 1L)
    } else if(tag == 7L || tag == 11L ) {
        vec_assert(x, ptype = integer())
    } else if(tag == 8L) {
        vec_assert(x, ptype = character(), size = 1L)
    } else if(tag == 12L) {
        vec_assert(x, ptype = bit64::integer64())
        cls <- c(cls, class(x))
    } else if(tag == 10L) {
        vec_assert(x, ptype = list())
        stopifnot(all(sapply(x, is_nbt)))
    } else {
        vec_assert(x, ptype = list())
        stopifnot(all(sapply(x, is_nbt)))
        stopifnot(is.null(names(x)))
        if(length(x) == 0) {
            ptype <- nbt_end()
        } else {
            # Strip ptype attributes when doing list of lists
            y <- purrr::map(x, `attr<-`, "ptype", NULL )
            ptype <- vec_ptype_common(!!!y)
            if(is.null(ptype)) {
                abort("Could not find common type for elements of `x`.")
            }
        }
        ret <- new_list_of(x, tag = tag, ptype = ptype, class = cls)
        return(ret)
    }
    new_vctr(x, tag = tag, class = cls)
}

#' @rdname nbt
#' @export
nbt_end <- function() new_nbt(list(), tag = 0L)

#' @rdname nbt
#' @export
nbt_byte <- function(x = 0L) new_nbt(vec_cast(x, integer()), tag = 1L)

#' @rdname nbt
#' @export
nbt_short <- function(x = 0L) new_nbt(vec_cast(x, integer()), tag = 2L)

#' @rdname nbt
#' @export
nbt_int <- function(x = 0L) new_nbt(vec_cast(x, integer()), tag = 3L)

#' @rdname nbt
#' @export
nbt_long <- function(x = 0L) new_nbt(vec_cast(x, bit64::integer64()), tag = 4L)

#' @rdname nbt
#' @export
nbt_float <- function(x = 0) new_nbt(vec_cast(x, double()), tag = 5L)

#' @rdname nbt
#' @export
nbt_double <- function(x = 0) new_nbt(vec_cast(x, double()), tag = 6L)

#' @rdname nbt
#' @export
nbt_string <- function(x = "") new_nbt(vec_cast(x, character()), tag = 8L)

#' @rdname nbt
#' @export
nbt_byte_array <- function(x = integer()) new_nbt(vec_cast(x, integer()), tag = 7L)

#' @rdname nbt
#' @export
nbt_int_array <- function(x = integer()) new_nbt(vec_cast(x, integer()), tag = 11L)

#' @rdname nbt
#' @export
nbt_long_array <- function(x = bit64::integer64()) new_nbt(vec_cast(x, bit64::integer64()), tag = 12L)

#' @rdname nbt
#' @export
nbt_compound <- function(...) new_nbt(list2(...), tag = 10L)

#' @rdname nbt
#' @export
nbt_list <- function(...) new_nbt(list2(...), tag = 9L)

tag <- function(x) attr(x, "tag")

list_tag <- function(x) tag(attr(x, "ptype"))

tag_str <- function(x) {
    chr <- c("END", "BYTE", "SHORT", "INT", "LONG", "FLOAT",
        "DOUBLE", "BYTE_ARRAY", "STRING", "LIST", "COMPOUND",
        "INT_ARRAY", "LONG_ARRAY")
    chr[1L+tag(x)]
}

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
    tag <- tag(x)

    suffix <- c("B","S","", "L", "F", "", "B", "", "", "", "", "L")

    if(tag == 0L) {
        NA_character_
    } else if(1L <= tag && tag <= 6L) {
        paste0(format(payload(x), nsmall=1), suffix[tag])
    } else if(tag == 7L || tag == 11L || tag == 12L) {
        paste0(format(payload(x), nsmall=1), suffix[tag])
    } else if(tag == 8L) {
        paste0("\"", payload(x), "\"")
    } else if(tag == 9L) {
        lapply(payload(x), format)
    } else if(tag == 10L) {
        lapply(payload(x), format)
    } else {
        NA_character_
    }
}

#' @rdname nbt
#' @export
is_nbt <- function(x) {
    inherits(x, "rbedrock_nbt")
}

#' @rdname nbt
#' @export
unnbt <- function(x) {
    if(is.list(x)) {
        rapply(x, payload, how="list")
    } else {
        payload(x)
    }
}

#' @export
`$<-.rbedrock_nbt` <- function(x, i, value) {
    if(tag(x) == 10L) {
        value <- vec_cast(value, vec_ptype(x[[i]]))
    }
    NextMethod()
}

#' @export
`[[<-.rbedrock_nbt` <- function(x, i, value) {
    if(tag(x) == 10L) {
        value <- vec_cast(value, vec_ptype(x[[i]]))
    }
    NextMethod()    
}

#' @export
vec_ptype_abbr.rbedrock_nbt <- function(x, ...) {
    "nbt"
}

#' @export
vec_ptype_full.rbedrock_nbt <- function(x, ...) {
    paste0("rbedrock_nbt<", tag_str(x), ">")
}

#' @description
#' `payload()` and `payload<-()` read and write an nbt value's payload.
#'
#' @param object An nbt value
#' @param value A new payload
#'
#' @rdname nbt
#' @export
`payload` <- function(object) {
    UseMethod('payload', object)
}

#' @export
`payload.rbedrock_nbt` <- function(object) {
    if(bit64::is.integer64(object)) {
        structure(vec_data(object), class="integer64")
    } else {
        vec_data(object)
    }
}

#' @rdname nbt
#' @export
`payload<-` <- function(object, value) {
    UseMethod('payload<-', object)
}

#' @export
`payload<-.rbedrock_nbt` <- function(object, value) {
    object[] <- value
    object
}

#' @export
vec_cast.rbedrock_nbt.rbedrock_nbt <- function(x, to, ...) {
    if (tag(x) != tag(to) || (tag(x) == 9L && list_tag(x) != list_tag(to))) {
        # try to convert payloads if different nbt types
        nbt(payload(x), tag(to))
    } else {
        # return x unchanged if it matches to. This allows casts with
        # matching zero-length vectors to bypass asserts in nbt().
        x
    }
}

#' @export
vec_cast.rbedrock_nbt.logical <- function(x, to, ...) nbt(x, tag(to))

#' @export
vec_cast.rbedrock_nbt.character <- function(x, to, ...) nbt(x, tag(to))

#' @export
vec_cast.rbedrock_nbt.integer64 <- function(x, to, ...) nbt(x, tag(to))

#' @export
vec_cast.rbedrock_nbt.integer <- function(x, to, ...) nbt(x, tag(to))

#' @export
vec_cast.rbedrock_nbt.double <- function(x, to, ...) nbt(x, tag(to))

#' @export
vec_cast.rbedrock_nbt.list <- function(x, to, ...) nbt(x, tag(to))

#' @export
vec_cast.logical.rbedrock_nbt <- function(x, to, ...) {
    vec_cast(payload(x), logical())
}

#' @export
vec_cast.character.rbedrock_nbt <- function(x, to, ...) {
    vec_cast(payload(x), character())
}

#' @export
vec_cast.integer.rbedrock_nbt <- function(x, to, ...) {
    vec_cast(payload(x), integer())
}

#' @export
vec_cast.double.rbedrock_nbt <- function(x, to, ...) {
    vec_cast(payload(x), double())
}

#' @export
vec_cast.integer64.rbedrock_nbt <- function(x, to, ...) {
    vec_cast(payload(x), bit64::integer64())
}

#' @export
vec_cast.list.rbedrock_nbt <- function(x, to, ...) {
    vec_cast(payload(x), list())
}

#' @export
vec_ptype2.rbedrock_nbt.rbedrock_nbt <- function(x, y, ..., x_arg = "", y_arg = "") {
    if (tag(x) != tag(y) || (tag(x) == 9L && list_tag(x) != list_tag(y))) {
        stop_incompatible_type(x, y, x_arg = x_arg, y_arg = y_arg)
    }
    x
}

#' @export
vec_ptype2.rbedrock_nbt.character <- function(x, y, ...) character()

#' @export
vec_ptype2.character.rbedrock_nbt <- function(x, y, ...) character()

#' @export
vec_ptype2.rbedrock_nbt.logical <- function(x, y, ...) logical()

#' @export
vec_ptype2.logical.rbedrock_nbt <- function(x, y, ...) logical()

#' @export
vec_ptype2.rbedrock_nbt.integer <- function(x, y, ...) integer()

#' @export
vec_ptype2.integer.rbedrock_nbt <- function(x, y, ...) integer()

#' @export
vec_ptype2.rbedrock_nbt.double <- function(x, y, ...) double()

#' @export
vec_ptype2.double.rbedrock_nbt <- function(x, y, ...) double()

#' @export
vec_ptype2.integer64.rbedrock_nbt <- function(x, y, ...) bit64::integer64()

#' @export
vec_ptype2.rbedrock_nbt.integer64 <- function(x, y, ...) bit64::integer64()

# work around integer64 methods not stripping class variables
#' @export
is.na.rbedrock_nbt <- function(x, ...) vec_data(NextMethod())

#' @export
`==.rbedrock_nbt` <- function(e1, e2) vec_data(NextMethod())

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
#' @param max_elements Maximum number of elements to parse.
#' @param simplify If TRUE, simplifies a list containing a single unnamed `nbtnode`.
#' @export
get_nbt_data <- function(db, keys, readoptions = NULL, max_elements = NULL, simplify=TRUE) {
    dat <- get_values(db, keys, readoptions = readoptions)
    read_nbt_data(dat, max_elements = max_elements, simplify = simplify)
}

#' @param key  A single key.
#' @rdname get_nbt_data
#' @export
get_nbt_value <- function(db, key, readoptions = NULL, max_elements = NULL, simplify=TRUE) {
    dat <- get_value(db, key, readoptions = readoptions)
    read_nbt(dat, max_elements = max_elements, simplify = simplify)
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
read_nbt <- function(rawdata, max_elements = NULL, simplify = TRUE) {
    if(!is.null(max_elements)) {
        stopifnot(length(max_elements) == 1L && !is.na(max_elements))
    }
    res <- .Call(Cread_nbt, rawdata, max_elements)
    if(isTRUE(simplify) && length(res) == 1L && is.null(attributes(res))) {
        res <- res[[1]]
    }
    res
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
    .Call(Cwrite_nbt, object)
}

#' @description
#' `read_nbt_data` calls `read_nbt` on each element of a list.
#'
#' @rdname get_nbt_data
#' @export
read_nbt_data <- function(data, max_elements = NULL, simplify=TRUE) {
    purrr::map(data, read_nbt, max_elements = max_elements, simplify = simplify)
}

#' @description
#' `write_nbt_data` calls `write_nbt` on each element of a list.
#'

#' @rdname get_nbt_data
#' @export
write_nbt_data <- function(data) {
    purrr::map(data, write_nbt)
}
