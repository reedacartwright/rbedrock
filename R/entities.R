#' Load and store Entities NBT data
#'
#' @description
#' `get_entities_data` loads Entities NBT data from a `bedrockdb`.
#'
#' @param db A bedrockdb object.
#' @param x,z,dimension Chunk coordinates to extract data from.
#'    `x` can also be a character vector of db keys and any keys not
#'    representing Entities data will be silently dropped.
#' @param data A named-list of key-value pairs for Entities NBT data.
#'
#' @export
get_entities_data <- function(db, x=get_keys(db), z, dimension) {
    keys <- .process_key_args(x,z,dimension, tag=50L)
    get_nbt_values(db, keys, simplify = FALSE)
}

#' @description
#' `put_entities_data` stores Entities NBT data into a `bedrockdb`.
#'
#' @export
#' @rdname get_entities_data
put_entities_data <- function(db, data) {
    stopifnot(check_chunk_key_tag(names(data), 50L))
    put_nbt_data(db, data)
}
