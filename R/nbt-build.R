#' Convert a basic R object into an NBT object
#'
#' The Named Binary Tag (NBT) format is used by Minecraft for various data
#' types. An NBT value holds a 'payload' of data and a 'tag' indicating the type
#' of data held. The "build" functions are useful for automatically converting
#' basic R objects into the most commonly used NBT objects.
#'
#' The following conversion rules are used:
#'
#' - nbt_byte: logical or raw scalars
#' - nbt_short: complex scalars (via `Im(x)`)
#' - nbt_int: integer scalars
#' - nbt_long: scalars of class integer64
#' - nbt_float: double scalars
#' - nbt_string: character scalars
#' - nbt_byte_list: logical vectors
#' - nbt_raw_string: raw vectors
#' - nbt_short_list: complex vectors (via `Im(x)`)
#' - nbt_int_list: integer vectors
#' - nbt_long_list: vectors of class integer64
#' - nbt_float_list: double vectors
#' - nbt_string_list: character vectors
#' - nbt_empty_list: NULL
#' - nbt_compound: named lists
#' - nbt_compound_list: unnamed lists
#'
#' In addition, `I()` can be used to treat scalars as vectors of length 1, and
#' to convert unnamed lists to nbt_compounds.
#'
#' @param x an R object
#' @param ... R objects, possibly named.
#'
#' @return An NBT object with type inferred from x. `nbt_build_compound()`
#' always returns an nbt_compound constructed from `...`.
#'
#' @seealso [nbt] [base::AsIs] [base::complex]
#' @name nbt_build
#' @examples
#' LL <- bit64::as.integer64(1L)
#' nbt_build_compound(
#'   byte = TRUE,
#'   short = 1i,
#'   int = 1L,
#'   long = 1 * LL,
#'   float = 1.0,
#'   string = "1"
#' )
NULL

#' @rdname nbt_build
#' @export
nbt_build <- function(x) {
  if (is_nbt_value(x)) {
    return(x)
  }
  asis <- inherits(x, "AsIs")

  type <- if (inherits(x, "integer64")) {
    "integer64"
  } else {
    typeof(x)
  }
  n <- length(x)

  if (n != 1L || asis) {
    switch(type,
      logical = nbt_byte_list(x),
      raw = nbt_raw_string(x),
      complex = nbt_short_list(Im(x)),
      integer = nbt_int_list(x),
      integer64 = nbt_long_list(x),
      double = nbt_float_list(x),
      character = nbt_string_list(x),
      NULL = nbt_empty_list(x),
      list = nbt_build_list(x),
      stop(sprintf("type '%s' is not supported", type))
    )
  } else {
    switch(type,
      logical = nbt_byte(x),
      raw = nbt_byte(x),
      complex = nbt_short(Im(x)),
      integer = nbt_int(x),
      integer64 = nbt_long(x),
      double = nbt_float(x),
      character = nbt_string(x),
      list = nbt_build_list(x),
      stop(sprintf("type '%s' is not supported", type))
    )
  }
}

nbt_build_list <- function(x) {
  asis <- inherits(x, "AsIs")
  x <- lapply(x, nbt_build)
  if (!is.null(names(x)) || asis) {
    nbt_compound0(x)
  } else {
    nbt_compound_list0(x)
  }
}

#' @rdname nbt_build
#' @export
nbt_build_compound <- function(...) {
  x <- lapply(list(...), nbt_build)
  nbt_compound0(x)
}

#' @rdname nbt
#' @export
nbt_compound2 <- nbt_build_compound
