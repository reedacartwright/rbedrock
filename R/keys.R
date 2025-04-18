#' Convert between key types.
#'
#' rbedrock represents database keys two different ways. `chrkeys` are a
#' human-readable format understood by most functions. `rawkeys` are used
#' internally, by the methods of `bedrockdb` objects and `bedrock_leveldb_*`
#' functions.
#'
#' @param keys a character vector of chrkeys or a list or rawkeys
#'
#' @return `chrkeys_to_rawkeys()` returns a list of raw vectors.
#'
#'         `rawkeys_to_chrkeys()` returns a character vector.
#'
#'         `chrkeys_to_rawkeys_1()` returns a raw vector.
#'
#' @keywords internal
#' @export
chrkeys_to_rawkeys <- function(keys) {
    keys <- vec_cast(keys, character())
    .Call(Cchrkeys_to_rawkeys, keys)
}

#' @rdname chrkeys_to_rawkeys
#' @export
chrkeys_to_rawkeys_1 <- function(keys) {
    chrkeys_to_rawkeys(keys)[[1]]
}

#' @rdname chrkeys_to_rawkeys
#' @export
rawkeys_to_chrkeys <- function(keys) {
    if (is.raw(keys)) {
        keys <- list(keys)
    }
    .Call(Crawkeys_to_chrkeys, keys)
}


.process_key_args <- function(x, z, d, tag, subtag,
                              stop_if_filtered = FALSE) {
    # is z is missing then x should contain keys as strings
    if (missing(z) && is.character(x)) {
        # if tag exists, we are going to filter on data type
        if (!missing(tag)) {
            b <- check_chunk_key_tag(x, tag, subtag,
                                     silent = !stop_if_filtered)
            x <- x[b]
        }
        return(x)
    }
    create_chunk_keys(x, z, d, tag, subtag)
}

.process_key_args_prefix <- function(x, z, d, stop_if_filtered = FALSE) {
    # is z is missing then x should contain keys as strings
    if (missing(z) && is.character(x)) {
        x <- get_stem_from_chunk_key(x)
        b <- !is.na(x)
        if (stop_if_filtered && any(!b)) {
            stop(str_c("Some keys passed to .process_key_arg_prefix ",
                       "are not chunk keys."))
        }
        return(x[b])
    }
    args <- vec_recycle_common(x, z, d)

    str_c("chunk", args[[1]], args[[2]], args[[3]], sep = ":")
}

.create_rawkey_prefix <- function(starts_with) {
    if (is.null(starts_with)) {
        return(NULL)
    }
    vec_assert(starts_with, character(), 1L)

    if (is_chunk_key(starts_with)) {
        # Chunk-key prefixes must refer to a chunk
        v <- str_count(starts_with, fixed(":"))
        if (v < 3) {
            abort("Argument 'starts_with' does not identify a chunk")
        }
        if (v == 3) {
            #append a dummy tag
            starts_with <- paste0(starts_with, ":44")
            res <- chrkeys_to_rawkeys(starts_with)[[1]]
            # strip last byte
            return(head(res, -1))
        }
    }
    chrkeys_to_rawkeys(starts_with)[[1]]
}


# TODO: retire .create_rawkey_prefix
create_rawkey_prefix <- .create_rawkey_prefix
