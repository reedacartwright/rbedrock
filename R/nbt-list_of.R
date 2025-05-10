#' Create a list of nbt objects.
#'
#' @param ... NBT objects, possibly named.
#'
#' @export
nbt_list_of <- function(...) {
    lst <- list(...)
    validate_nbt_list_of(new_nbt_list_of(lst))
}

new_nbt_list_of <- function(x) {
    stopifnot(is.list(x))
    structure(x, class = c("rbedrock_nbt_list_of", "list"))
}

validate_nbt_list_of <- function(x) {
    if (!all_nbt_values(x)) {
        stop("`x` contains non-NBT elements.", call. = FALSE)
    }
    x
}

#' @export
`[.rbedrock_nbt_list_of` <- function(x, i, ...) {
    rac_index(x, i, ...)
}

#' @export
`[<-.rbedrock_nbt_list_of` <- function(x, i, value) {
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
`[[<-.rbedrock_nbt_list_of` <- function(x, i, value) {
    if (is.null(value)) {
        x[i] <- list(value)
        return(x)
    }
    value <- nbt_value_or_cast(value, x[[i, exact = TRUE]])
    NextMethod()
}

#' @export
`$<-.rbedrock_nbt_list_of` <- function(x, i, value) {
    value <- nbt_value_or_cast(value, x[[i, exact = TRUE]])
    NextMethod()
}

# Implement tree-drawing code inspired by lobstr, cli, and rlang

#' @export
print.rbedrock_nbt_list_of <- function(x, style = NULL, ...) {
    # collect options
    opts <- list(style = style)

    cat("NBT DATA\n")
    nbt_tree(x, opts = opts)
    x
}

make_type_str <- function(x, x_id = NULL, val_lab = NULL) {
    s <- if (is.null(x_id) && is.null(val_lab)) {
        rac_type_full(x)
    } else {
        rac_type_abbr(x)
    }
    paste0("<", s, ">")
}

nbt_tree <- function(x, x_id = NULL, branch_hist = character(0L), opts) {
    style <- opts$style %||% box_chars()

    depth <- length(branch_hist)
    branch_chars <- rep_len("  ", depth)
    branch_chars[branch_hist == "child"] <- paste0(style$v, " ")

    if (depth > 0L) {
        step <- if (branch_hist[depth] == "last") style$l else style$j
        branch_chars[depth] <- paste0(step, style$h)
    }

    val_lab <- NULL
    if (is.atomic(x)) {
        val_lab <- format(x)
        if (length(val_lab) > 1) {
            val_lab <- paste(val_lab, collapse = ", ")
            val_lab <- paste0("[ ", val_lab, " ]")
        }
    }

    label <- paste0(x_id,
                    make_type_str(x, x_id, val_lab),
                    if (!is.null(val_lab)) ": ",
                    val_lab)
    cat(
        paste(branch_chars, collapse = ""),
        label,
        "\n",
        sep = ""
    )
    if (is.list(x)) {
        children <- rac_data(x)
        n_children <- length(children)
        child_names <- names(children)
        for (i in seq_along(children)) {
            id <- child_names[i]
            child_type <- if (i < n_children) "child" else "last"
            Recall(x = children[[i]],
                x_id = id,
                branch_hist = c(branch_hist, child_type),
                opts = opts
            )
        }
    }
}

is_utf8_output <- function() {
    opt <- getOption("cli.unicode", NULL)
    opt <- opt %||% l10n_info()[["UTF-8", exact = TRUE]]
    isTRUE(opt)
}

box_chars <- function() {
    if (is_utf8_output()) {
        list(
            "h" = "\u2500", # horizontal
            "v" = "\u2502", # vertical
            "l" = "\u2514", # leaf
            "j" = "\u251C" # junction
        )
    } else {
        list(
            "h" = "-", # horizontal
            "v" = "|", # vertical
            "l" = "\\", # leaf
            "j" = "+" # junction
        )
    }
}

#---- Formatting nbt values ----------------------------------------------------

#### nbt numeric values ####

#' @export
format.rbedrock_nbt_numeric <- function(x, suffix = "", ...) {
    out <- formatC(unclass(x), ...)
    out[is.na(x)] <- NA
    out[!is.na(x)] <- paste0(out[!is.na(x)], suffix)
    out
}

#' @export
format.rbedrock_nbt_byte <- function(x, ...) {
    NextMethod(suffix = "b")
}

#' @export
format.rbedrock_nbt_short <- function(x, ...) {
    NextMethod(suffix = "s")
}

#' @export
format.rbedrock_nbt_int <- function(x, ...) {
    NextMethod(suffix = "")
}

#' @export
format.rbedrock_nbt_float <- function(x, ...) {
    out <- NextMethod(suffix = "0f", flag = "#")
    sub("(\\.[0-9]*[1-9])0+f$", "\\1f", out) # fixes trailing zeros
}

#' @export
format.rbedrock_nbt_double <- function(x, ...) {
    out <- NextMethod(suffix = "0", flag = "#")
    sub("(\\.[0-9]*[1-9])0+$", "\\1", out) # fix trailing zeros
}

#### nbt string values ####

#' @export
format.rbedrock_nbt_long <- function(x, ...) {
    NextMethod(suffix = "")
}

#' @export
format.rbedrock_nbt_string <- function(x, ...) {
    NextMethod()
}

#' @export
format.rbedrock_nbt_raw_string <- function(x, ...) {
    NextMethod()
}

#### nbt special lists ####

#' @export
format.rbedrock_empty_list <- function(x, ...) {
    NextMethod()
}

#' @export
format.rbedrock_nbt_compound <- function(x, ...) {
    NextMethod()
}

#' @export
format.rbedrock_nbt_nested_list <- function(x, ...) {
    NextMethod()
}

#' @export
format.rbedrock_nbt_byte_array_list <- function(x, ...) {
    NextMethod()
}

#' @export
format.rbedrock_nbt_int_array_list <- function(x, ...) {
    NextMethod()
}

#' @export
format.rbedrock_nbt_long_array_list <- function(x, ...) {
    NextMethod()
}

#' @export
format.rbedrock_nbt_raw_string_list <- function(x, ...) {
    NextMethod()
}

#' @export
format.rbedrock_nbt_compound_list <- function(x, ...) {
    NextMethod()
}

#### nbt aliases ####

#' @export
format.rbedrock_nbt_byte_array <- format.rbedrock_nbt_byte

#' @export
format.rbedrock_nbt_byte_list <- format.rbedrock_nbt_byte

#' @export
format.rbedrock_nbt_short_list <- format.rbedrock_nbt_short

#' @export
format.rbedrock_nbt_int_array <- format.rbedrock_nbt_int

#' @export
format.rbedrock_nbt_int_list <- format.rbedrock_nbt_int

#' @export
format.rbedrock_nbt_float_list <- format.rbedrock_nbt_float

#' @export
format.rbedrock_nbt_double_list <- format.rbedrock_nbt_double

#' @export
format.rbedrock_nbt_long_array <- format.rbedrock_nbt_long

#' @export
format.rbedrock_nbt_long_list <- format.rbedrock_nbt_long

#' @export
format.rbedrock_nbt_string_list <- format.rbedrock_nbt_string

#---- Pretty type information --------------------------------------------------

#' @export
rac_type_full.rbedrock_nbt_byte <- function(x) "byte"
#' @export
rac_type_full.rbedrock_nbt_short <- function(x) "short"
#' @export
rac_type_full.rbedrock_nbt_int <- function(x) "int"
#' @export
rac_type_full.rbedrock_nbt_long <- function(x) "long"
#' @export
rac_type_full.rbedrock_nbt_float <- function(x) "float"
#' @export
rac_type_full.rbedrock_nbt_double <- function(x) "double"
#' @export
rac_type_full.rbedrock_nbt_byte_array <- function(x) "byte_array"
#' @export
rac_type_full.rbedrock_nbt_string <- function(x) "string"
#' @export
rac_type_full.rbedrock_nbt_compound <- function(x) "compound"
#' @export
rac_type_full.rbedrock_nbt_int_array <- function(x) "int_array"
#' @export
rac_type_full.rbedrock_nbt_long_array <- function(x) "long_array"
#' @export
rac_type_full.rbedrock_nbt_raw_string <- function(x) "raw_string"
#' @export
rac_type_full.rbedrock_nbt_empty_list <- function(x) "list-empty"
#' @export
rac_type_full.rbedrock_nbt_byte_list <- function(x) "list-bytes"
#' @export
rac_type_full.rbedrock_nbt_short_list <- function(x) "list-short"
#' @export
rac_type_full.rbedrock_nbt_int_list <- function(x) "list-int"
#' @export
rac_type_full.rbedrock_nbt_long_list <- function(x) "list-long"
#' @export
rac_type_full.rbedrock_nbt_float_list <- function(x) "list-float"
#' @export
rac_type_full.rbedrock_nbt_double_list <- function(x) "list-double"
#' @export
rac_type_full.rbedrock_nbt_byte_array_list <- function(x) "list-byte_array"
#' @export
rac_type_full.rbedrock_nbt_string_list <- function(x) "list-string"
#' @export
rac_type_full.rbedrock_nbt_compound_list <- function(x) "list-compound"
#' @export
rac_type_full.rbedrock_nbt_int_array_list <- function(x) "list-int_array"
#' @export
rac_type_full.rbedrock_nbt_long_array_list <- function(x) "list-long_array"
#' @export
rac_type_full.rbedrock_nbt_raw_string_list <- function(x) "list-raw_string"
#' @export
rac_type_full.rbedrock_nbt_nested_list <- function(x) "list-list"
#' @export
rac_type_full.rbedrock_nbt_list_of <- function(x) "list"

#' @export
rac_type_abbr.rbedrock_nbt_byte <- function(x) "byte"
#' @export
rac_type_abbr.rbedrock_nbt_short <- function(x) "shrt"
#' @export
rac_type_abbr.rbedrock_nbt_int <- function(x) "int"
#' @export
rac_type_abbr.rbedrock_nbt_long <- function(x) "long"
#' @export
rac_type_abbr.rbedrock_nbt_float <- function(x) "flt"
#' @export
rac_type_abbr.rbedrock_nbt_double <- function(x) "dbl"
#' @export
rac_type_abbr.rbedrock_nbt_byte_array <- function(x) "barr"
#' @export
rac_type_abbr.rbedrock_nbt_string <- function(x) "str"
#' @export
rac_type_abbr.rbedrock_nbt_compound <- function(x) "cmpd"
#' @export
rac_type_abbr.rbedrock_nbt_int_array <- function(x) "iarr"
#' @export
rac_type_abbr.rbedrock_nbt_long_array <- function(x) "larr"
#' @export
rac_type_abbr.rbedrock_nbt_raw_string <- function(x) "raw"
#' @export
rac_type_abbr.rbedrock_nbt_empty_list <- function(x) "l-emp"
#' @export
rac_type_abbr.rbedrock_nbt_byte_list <- function(x) "l-byte"
#' @export
rac_type_abbr.rbedrock_nbt_short_list <- function(x) "l-shrt"
#' @export
rac_type_abbr.rbedrock_nbt_int_list <- function(x) "l-int"
#' @export
rac_type_abbr.rbedrock_nbt_long_list <- function(x) "l-long"
#' @export
rac_type_abbr.rbedrock_nbt_float_list <- function(x) "l-flt"
#' @export
rac_type_abbr.rbedrock_nbt_double_list <- function(x) "l-dbl"
#' @export
rac_type_abbr.rbedrock_nbt_byte_array_list <- function(x) "l-barr"
#' @export
rac_type_abbr.rbedrock_nbt_string_list <- function(x) "l-str"
#' @export
rac_type_abbr.rbedrock_nbt_compound_list <- function(x) "l-cmpd"
#' @export
rac_type_abbr.rbedrock_nbt_int_array_list <- function(x) "l-iarr"
#' @export
rac_type_abbr.rbedrock_nbt_long_array_list <- function(x) "l-larr"
#' @export
rac_type_abbr.rbedrock_nbt_raw_string_list <- function(x) "l-raw"
#' @export
rac_type_abbr.rbedrock_nbt_nested_list <- function(x) "l-lst"
#' @export
rac_type_abbr.rbedrock_nbt_list_of <- function(x) "list"
