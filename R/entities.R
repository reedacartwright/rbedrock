#' Load and store Entities NBT data
#'
#' Entities data (tag 50) holds a list of NBT values for
#' mobs and other entities in the game.
#'
#' @name Entities
NULL

#'
#' @description
#' `get_entities_data()` loads Entities data from a `bedrockdb`.
#' It will silently drop and keys not representing Entities data.
#'
#' @param db A bedrockdb object.
#' @param x,z,dimension Chunk coordinates to extract data from.
#'    `x` can also be a character vector of db keys.
#'
#' @rdname Entities
#' @export
get_entities_data <- function(db, x=get_keys(db), z, dimension) {
    keys <- .process_key_args(x,z,dimension, tag=50L)
    get_nbt_values(db, keys, simplify = FALSE)
}

#' @description
#' `put_entities_data()` stores Entities data into a `bedrockdb`.
#'
#' @param data A named-list of key-value pairs for Entities data.
#'
#' @rdname Entities
#' @export
put_entities_data <- function(db, data) {
    .check_chunk_key_tag(names(data), 50L)
    put_nbt_data(db, data)
}
