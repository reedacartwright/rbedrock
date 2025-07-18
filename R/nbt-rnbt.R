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
read_rnbt <- function(
  rawvalue,
  format = c("little", "big", "network", "network_big")
) {
  format <- match.arg(format)
  format_int <- switch(
    format,
    "little" = 0L,
    "big" = 1L,
    "network" = 2L,
    "network_big" = 3L
  )
  .Call(R_read_nbt, rawvalue, format_int)
}

#' @rdname rnbt
#' @keywords internal
#' @export
#' @useDynLib rbedrock R_read_nbt_once
read_rnbt_once <- function(
  rawvalue,
  format = c("little", "big", "network", "network_big")
) {
  format <- match.arg(format)
  format_int <- switch(
    format,
    "little" = 0L,
    "big" = 1L,
    "network" = 2L,
    "network_big" = 3L
  )
  .Call(R_read_nbt_once, rawvalue, format_int)
}

#' @rdname rnbt
#' @keywords internal
#' @useDynLib rbedrock R_write_nbt
#' @export
write_rnbt <- function(
  x,
  format = c("little", "big", "network", "network_big")
) {
  format <- match.arg(format)
  format_int <- switch(
    format,
    "little" = 0L,
    "big" = 1L,
    "network" = 2L,
    "network_big" = 3L
  )
  .Call(R_write_nbt, x, format_int)
}

#' @rdname rnbt
#' @export
from_rnbt <- function(x) {
  x <- from_rnbt_impl(x)
  if (is.null(x)) x else new_nbt_list_of(x)
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
  value <- x[["value", exact = TRUE]]
  type <- x[["type", exact = TRUE]]

  new_nbt_value_impl(value, type)
}

new_nbt_value_impl <- function(x, type) {
  do_new_nbt_compound <- function(x) {
    new_nbt_compound(from_rnbt_impl(x))
  }
  do_new_nbt_nested_list <- function(x) {
    new_nbt_nested_list(lapply(x, rnbt_value))
  }
  do_new_nbt_compound_list <- function(x) {
    x <- lapply(x, from_rnbt_impl)
    new_nbt_compound_list(lapply(x, new_nbt_compound))
  }
  do_new_nbt_byte_array_list <- function(x) {
    new_nbt_byte_array_list(lapply(x, new_nbt_byte_array))
  }
  do_new_nbt_int_array_list <- function(x) {
    new_nbt_int_array_list(lapply(x, new_nbt_int_array))
  }
  do_new_nbt_long_array_list <- function(x) {
    new_nbt_long_array_list(lapply(x, new_nbt_long_array))
  }
  do_new_nbt_raw_string_list <- function(x) {
    new_nbt_raw_string_list(lapply(x, new_nbt_raw_string))
  }

  ret <- switch(
    as.character(type),
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
    "10" = do_new_nbt_compound(x),
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
    "107" = do_new_nbt_byte_array_list(x),
    "108" = new_nbt_string_list(x),
    "109" = do_new_nbt_nested_list(x),
    "110" = do_new_nbt_compound_list(x),
    "111" = do_new_nbt_int_array_list(x),
    "112" = do_new_nbt_long_array_list(x),
    "158" = do_new_nbt_raw_string_list(x),
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
  to_rnbt_impl(x)
}

to_rnbt_impl <- function(x) {
  n <- names(x) %||% rep("", length(x))
  ret <- vector("list", length(x))
  for (i in seq_along(ret)) {
    ret[[i]] <- list(
      name = n[[i]],
      type = to_rnbt_type(x[[i]]),
      value = to_rnbt_value(x[[i]])
    )
  }
  ret
}

to_rnbt_type <- function(x) {
  for (cls in class(x)) {
    type <- to_rnbt_type_impl(cls)
    if (!is.null(type)) {
      return(type)
    }
  }
  msg <- sprintf(
    "Unable to identify rnbt type for object of class <%s>.",
    class(x)[1]
  )
  stop(msg, call. = FALSE)
}

to_rnbt_type_impl <- function(x) {
  switch(
    as.character(x),
    "rbedrock_nbt_byte" = 1L,
    "rbedrock_nbt_byte_array" = 7L,
    "rbedrock_nbt_byte_array_list" = 107L,
    "rbedrock_nbt_byte_list" = 101L,
    "rbedrock_nbt_compound" = 10L,
    "rbedrock_nbt_compound_list" = 110L,
    "rbedrock_nbt_double" = 6L,
    "rbedrock_nbt_double_list" = 106L,
    "rbedrock_nbt_empty_list" = 100L,
    "rbedrock_nbt_float" = 5L,
    "rbedrock_nbt_float_list" = 105L,
    "rbedrock_nbt_int" = 3L,
    "rbedrock_nbt_int_array" = 11L,
    "rbedrock_nbt_int_array_list" = 111L,
    "rbedrock_nbt_int_list" = 103L,
    "rbedrock_nbt_long" = 4L,
    "rbedrock_nbt_long_array" = 12L,
    "rbedrock_nbt_long_array_list" = 112L,
    "rbedrock_nbt_long_list" = 104L,
    "rbedrock_nbt_nested_list" = 109L,
    "rbedrock_nbt_raw_string" = 58L,
    "rbedrock_nbt_raw_string_list" = 158L,
    "rbedrock_nbt_short" = 2L,
    "rbedrock_nbt_short_list" = 102L,
    "rbedrock_nbt_string" = 8L,
    "rbedrock_nbt_string_list" = 108L,
    NULL
  )
}

#' Convert an nbt value to an rnbt value
#'
#' This is a helper function used to convert from nbt values to rnbt data.
#'
#' @param x Value to cast
#' @param ... Currently unused.
#' @keywords internal
#' @export
to_rnbt_value <- function(x, ...) {
  UseMethod("to_rnbt_value")
}

#' @export
to_rnbt_value.default <- function(x, ...) {
  msg <- sprintf(
    "Unable to identify rnbt value for object of class <%s>.",
    class(x)[1]
  )
  stop(msg)
}

#' @export
to_rnbt_value.rbedrock_nbt_value <- function(x, ...) {
  # NOTE: We should not get here on well-formed data
  msg <- sprintf(
    "Rnbt value for `nbt_value` of class <%s> not implemented.",
    class(x)[1]
  )
  stop(msg)
}

#' @export
to_rnbt_value.rbedrock_nbt_numeric <- function(x, ...) {
  as.double(rac_data(x))
}

#' @export
to_rnbt_value.rbedrock_nbt_int64 <- function(x, ...) {
  as.character(rac_data(x))
}

#' @export
to_rnbt_value.rbedrock_nbt_string <- function(x, ...) {
  as.character(rac_data(x))
}

#' @export
to_rnbt_value.rbedrock_nbt_raw_string <- function(x, ...) {
  as.raw(rac_data(x))
}

#' @export
to_rnbt_value.rbedrock_nbt_raw_string <- function(x, ...) {
  as.raw(rac_data(x))
}

#' @export
to_rnbt_value.rbedrock_nbt_raw_string_list <- function(x, ...) {
  lapply(rac_data(x), as.raw)
}

#' @export
to_rnbt_value.rbedrock_nbt_empty_list <- function(x, ...) {
  list()
}

#' @export
to_rnbt_value.rbedrock_nbt_compound <- function(x, ...) {
  to_rnbt_impl(rac_data(x))
}

#' @export
to_rnbt_value.rbedrock_nbt_compound_list <- function(x, ...) {
  lapply(rac_data(x), to_rnbt_impl)
}

#' @export
to_rnbt_value.rbedrock_nbt_numeric_list <- function(x, ...) {
  lapply(rac_data(x), as.double)
}

#' @export
to_rnbt_value.rbedrock_nbt_long_array_list <- function(x, ...) {
  lapply(rac_data(x), as.character)
}

#' @export
to_rnbt_value.rbedrock_nbt_nested_list <- function(x, ...) {
  lapply(rac_data(x), function(y) {
    list(type = to_rnbt_type(y), value = to_rnbt_value(y))
  })
}
