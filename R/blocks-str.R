#' Convert block data from nbt format to a string
#'
#' Block data is stored in nbt format, which includes a block's name and
#' properties. `blocks_str()` converts the nbt data into strings with the
#' following format: `blockname@@prop1=val1@@prop2=val2`. Blocks can have
#' zero or more properties. `blocks_nbt()` does the inverse operation.
#'
#' @param x block data, either as a list of nbt values or a vector of strings.
#' @param names_only A logical scalar. Return only the names of the blocks,
#' ignoring block properties.
#'
#' @export
blocks_str <- function(x, names_only = FALSE) {
    vapply(x, blocks_str_impl, character(1L), names_only = names_only)
}

blocks_str_impl <- function(x, names_only = FALSE) {
    block_name <- payload(x$name)
    states <- payload(x$states)
    if (length(states) == 0L || isTRUE(names_only)) {
        return(block_name)
    }
    states_str <- character(length(states))
    for (i in seq_along(states)) {
        x <- states[[i]]
        xtag <- get_nbt_tag(x)
        if (!(xtag %in% c(1, 3, 8))) {
            msg <- sprintf(
                "block State '%s' has NBT tag '%s'. Possible loss of information when converting to a string.", # nolint
                names(states)[i], xtag)
            warning(msg, call. = FALSE)
        }
        states_str[[i]] <- as.character(payload(x))
    }
    states <- paste(names(states), states_str,
                    sep = "=", collapse = "@")
    paste(block_name, states, sep = "@")
}

#' @export
#' @rdname blocks_str
blocks_nbt <- function(x) {
    lapply(x, blocks_nbt_impl)
}

globalVariables(c("vanilla_block_states_df",
                  "vanilla_block_property_type_list"))

blocks_nbt_impl <- function(x) {
    # convert block string back into palette information
    s <- strsplit(x, "@", fixed = TRUE)[[1]]
    name <- s[1]
    if (length(s) > 1) {
        # properties
        s <- s[-1]
        # extract property name
        names(s) <- regmatches(s, regexpr("^[^=]+", s))
        # convert to nbt
        v <- lapply(s, block_state_nbt)
        states <- nbt_compound(!!!v)
    } else {
        states <- nbt_compound()
    }

    data_version <- attr(vanilla_block_states_df, "data_version", exact = TRUE)

    nbt_compound(name = nbt_string(name),
                 states = states,
                 version = nbt_int(data_version))
}

block_state_nbt <- function(prop) {
    s <- strsplit(prop, "=", fixed = TRUE)[[1]]
    n <- s[1]
    v <- s[2]
    type <- vanilla_block_property_type_list[n]
    if (is.na(type)) {
        msg <- sprintf("unknown block state '%s' converted to a string. Possible loss of information.", prop) # nolint
        warning(msg, call. = FALSE)
        nbt_string(v)
    } else if (type == "str") {
        nbt_string(v)
    } else {
        # both bits and nums are expected to be integers
        v <- suppressWarnings(as.integer(v))
        if (is.na(v)) {
            msg <- sprintf("block state '%s' could not be converted to an integer. Possible loss of information.", prop) # nolint
            warning(msg, call. = FALSE)
        }
        if (type == "bit") {
            # bit is false if it is equal 0 and true otherwise.
            nbt_byte(is.na(v) || v != 0)
        } else {
            nbt_int(v)
        }
    }
}


#' Bedrock block data
#'
#' @description
#' Information about blocks used in Bedrock edition. Generated from the
#' PyMCTranslate project.
#'
#' @format
#' ## vanilla_block_states_df
#' A data.frame in long-format with `r nrow(vanilla_block_states_df)` rows and
#' `r ncol(vanilla_block_states_df)` columns. Block data version is
#' `r attr(vanilla_block_states_df, "data_version")`.
#'
#' \describe{
#'   \item{name}{Block name.}
#'   \item{property}{Property name.}
#'   \item{type}{Property type.}
#'   \item{default}{Default value.}
#'   \item{allowed}{Allowed values.}
#' }
#'
#' ## vanilla_block_list
#'
#' List of blocks names. Includes blocks without properties, which don't show
#' up in vanilla_block_states_df.
#'
#' ## vanilla_block_property_type_list
#'
#' List of properties (names) and their types (values).
#'
#' @source
#' * <https://github.com/gentlegiantJGC/PyMCTranslate/>
"vanilla_block_states_df"

#' @rdname vanilla_block_states_df
#' @format NULL
"vanilla_block_list"

#' @rdname vanilla_block_states_df
#' @format NULL
"vanilla_block_property_type_list"
