#' Convert between key types.
#'
#' rbedrock represents database keys two different ways. `chrkeys` are a human-readable
#' format understood by most functions. `rawkeys` are used internally, by the methods
#' of `bedrockdb` objects and `bedrock_leveldb_*` functions.
#'
#' @param keys a character vector of chrkeys or a list or rawkeys
#'  
#' @return `chrkeys_to_rawkeys()` returns a list of raw vectors.
#'
#'         `rawkeys_to_chrkeys()` returns a character vector.
#'
#' @keywords internal
#' @export
chrkeys_to_rawkeys <- function(keys) {
    keys <- vec_cast(keys, character())
    .Call(Cchrkeys_to_rawkeys, keys)
}

#' @rdname chrkeys_to_rawkeys
#' @export
rawkeys_to_chrkeys <- function(keys) {
    if(is.raw(keys)) {
        keys <- list(keys)
    }
    .Call(Crawkeys_to_chrkeys, keys)
}

#' @importFrom utils head
.create_rawkey_prefix <- function(starts_with) {
    if(is.null(starts_with)) {
        return(NULL)
    }
    vec_assert(starts_with, character(), 1L)

    if(.is_chunk_key(starts_with)) {
        # Chunk-key prefixes must refer to a chunk
        v <- str_count(starts_with, fixed(":"))
        if(v < 3) {
            abort("Argument 'starts_with' does not identify a chunk")
        }
        if(v == 3) {
            #append a dummy tag
            starts_with <- paste0(starts_with, ":44")
            res <- chrkeys_to_rawkeys(starts_with)[[1]]
            # strip last byte
            return(head(res,-1))
        }
    }
    chrkeys_to_rawkeys(starts_with)[[1]]
}
