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
#' @useDynLib rbedrock R_chrkeys_to_rawkeys
#' @export
chrkeys_to_rawkeys <- function(keys) {
  .Call(R_chrkeys_to_rawkeys, keys)
}

#' @rdname chrkeys_to_rawkeys
#' @export
chrkeys_to_rawkeys_1 <- function(keys) {
  chrkeys_to_rawkeys(keys)[[1]]
}

#' @rdname chrkeys_to_rawkeys
#' @useDynLib rbedrock R_rawkeys_to_chrkeys
#' @export
rawkeys_to_chrkeys <- function(keys) {
  if (is.raw(keys)) {
    keys <- list(keys)
  }
  .Call(R_rawkeys_to_chrkeys, keys)
}

create_rawkey_prefix <- function(prefix) {
  if (is.null(prefix)) {
    return(NULL)
  }
  stopifnot(is.character(prefix) && length(prefix) == 1L)

  if (is_chunk_key(prefix)) {
    # Chunk-key prefixes must be either a valid key or a valid key prefix
    if (!is_valid_chunk_key(prefix)) {
      if (is_valid_chunk_key_prefix(prefix)) {
        # append a dummy tag
        prefix <- paste0(prefix, ":44")
        res <- chrkeys_to_rawkeys_1(prefix)
        # strip last byte
        return(res[-length(res)])
      } else {
        stop("Argument `prefix` does not identify a chunk")
      }
    }
  }
  chrkeys_to_rawkeys_1(prefix)
}
