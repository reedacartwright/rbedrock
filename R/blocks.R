#' Load and store block data from one or more chunks
#'
#' These functions return block data as strings containing the
#' block name and block states. The strings' format is
#' `blockname@@state1=value1@@state2=value2` etc.
#' Blocks may have 0 or more states.
#'
#' `get_chunk_blokcs_data()` and `get_chunk_blocks_value()` load block data from `db`.
#' `get_chunk_blocks_value()` only supports loading a single value.
#'
#' `put_chunk_blocks_data()` and
#' `put_chunk_blocks_value()` store block data into a `bedrockdb`.
#'
#'
#' @param db A bedrockdb object.
#' @param x,z,dimension Chunk coordinates to extract data from.
#'    `x` can also be a character vector of db keys.
#'
#' @param names_only  A logical scalar. Return only the names of the blocks,
#' ignoring block states.
#' @param extra_block A logical scalar. Append the extra block layer to the
#' output (separated by ";"). This is mostly useful if you have waterlogged
#' blocks. If the extra block is air, it will not be appended.
#'
#' @param values A list of 16xNx16 character() arrays.
#' If `x` is missing, the names of `values` will be taken as the keys.
#' @param value A 16xNx16 character array
#' @param version Which format of subchunk data to use.
#'
#' @return `get_chunk_blocks_value()`
#' return a 16xNx16 character array. The axes represent the `x`, `y`, and `z`
#' dimensions in that order. The size of the y-axis is based on the highest
#' subchunk in the coordinate. Missing subchunks are considered air.
#' `get_chunk_blocks_data()` returns a list of the of the values
#' returned by `read_chunk_blocks_value()`.
#'
#' @export
get_chunk_blocks_data <- function(x, z, dimension, db,
        names_only = FALSE, extra_block = FALSE) {
    keys <- .process_chunk_key_args_prefix(x, z, dimension, tag = 47L,
        assert_validity = TRUE)
    dat <- purrr::map(keys, .get_chunk_blocks_value_impl,
        db = db, names_only = names_only, extra_block = extra_block)
    set_names(dat, keys)
}

#' @rdname get_chunk_blocks_data
#' @export
get_chunk_blocks_value <- function(x, z, dimension, db,
        names_only = FALSE, extra_block = FALSE) {
    key <- .process_chunk_key_args_prefix(x, z, dimension, tag = 47L,
        assert_validity = TRUE, assert_scalar = TRUE)

    .get_chunk_blocks_value_impl(key,
        db = db,
        names_only = names_only,
        extra_block = extra_block
    )
}

#' @rdname get_chunk_blocks_data
#' @export
put_chunk_blocks_data <- function(values, x, z, dimension, db, version = 9L) {
    keys <- .process_chunk_key_args_prefix(x, z, dimension, tag = 47L, values = values,
        assert_validity = TRUE)
    values <- vec_recycle(values, length(keys), x_arg = "values")
    purrr::walk2(values, keys, .put_chunk_blocks_value_impl, db = db, version = version)
}

#' @rdname get_chunk_blocks_data
#' @export
put_chunk_blocks_value <- function(value, x, z, dimension, db, version = 9L) {
    key <- .process_chunk_key_args_prefix(x, z, dimension, tag = 47L,
        assert_validity = TRUE, assert_scalar = TRUE)

    .put_chunk_blocks_value_impl(value, key, db = db, version = version)
}

#' Get or set the coordinates of the origin of a chunk
#'
#' @param x an array of block data
#' @param value an integer vector
#' @export
chunk_origin <- function(x) {
    attr(x, "origin", exact = TRUE)
}

#' @export
#' @rdname chunk_origin
`chunk_origin<-` <- function (x, value) {
    attr(x, "origin") <- value
    return(x)
}

.get_chunk_blocks_value_impl <- function(prefix, db, ...) {
    dat <- .get_subchunk_blocks_data_impl(key_prefix(prefix), db = db, ...)
    if(length(dat) == 0L) {
        return(NULL)
    }

    pos <- purrr::map_int(dat, attr, "offset")
    # calculate the lowest subchunk and adjust as necessary
    # ideally calculation should be based on dimension
    # and chunk version
    bottom <- min(pos)
    if(bottom < 0) {
       bottom <- min(bottom, -4)
    } else if(bottom != 0) {
        bottom <- 0
    }
    max_y <- 16*(max(pos)-bottom)+16
    mat <- array("minecraft:air", c(16, max_y, 16))
    for(i in seq_along(pos)) {
        mat[,((pos[i]-bottom)*16)+1:16,] <- dat[[i]]
    }
    o <- as.integer(.split_chunk_stems(prefix)[1:2])
    attr(mat,"origin") <- c(o[1], bottom, o[2])*16L
    mat
}

.put_chunk_blocks_value_impl <- function(value, prefix, db, version = 9L) {
    d <- dim(value)
    if(!is.character(value) || is.null(d) || length(d) != 3L || 
        d[1] != 16L || d[3] != 16L || (d[2] %% 16L) != 0L ) {
        abort("`value` must be a 16 x 16*N x 16 character array.")
    }

    origin <- chunk_origin(value)
    if(is.null(origin)) {
        abort("`value` must have an origin.")
    }
    
    
    # schedule deletion of all old keys
    old_keys <- str_c(prefix, ":", -4:19)
    data <- rep_named(old_keys, list(NULL))

    # construct new chunk data
    bottom <- origin[2] %/% 16L
    subtags <- seq.int(bottom, length.out=(d[2] %/% 16L))
    new_keys <- str_c(prefix, ":", subtags)
    for(s in seq_along(subtags)) {
        subchunk <- value[,(s-1L)*16L+(1:16),]
        # skip empty subchunks
        if(all(subchunk == "minecraft:air")) {
            next
        }
        # extract and encode subchunk data
        data[[new_keys[s]]] <- write_subchunk_blocks_value(subchunk,
            version=version, missing_offset=subtags[s])
    }
    write_data(data, db = db)

    invisible()
}

#' Locate the coordinates of blocks in a chunk
#'
#' @param blocks A character array containing block data.
#' @param pattern The pattern to look for. Passed to `stringr::str_detect`.
#' @param negate  If `TRUE`, return non-matching elements.
#'
#' @examples
#' dbpath <- rbedrock_example_world("example1.mcworld")
#' db <- bedrockdb(dbpath)
#' blocks <- get_chunk_blocks_value(x=37, z=10, dimension=0, db = db)
#' locate_blocks(blocks, "ore")
#' close(db)
#'
#' @export
locate_blocks <- function(blocks, pattern, negate = FALSE) {
    ind <- which(str_detect(blocks, pattern, negate))

    aind <- arrayInd(ind, dim(blocks))
    coords <- chunk_origin(blocks) + t(aind) - 1
    if(length(coords) == 0) {
        dim(coords) <- c(3,0)
    }
    ret <- tibble::tibble(x = coords[1,],
        y = coords[2,], z = coords[3,],
        block = as.character(blocks[ind]))
    dplyr::arrange(ret, .data$y, .data$x, .data$z)
}

#' Load and store SubchunkBlocks data
#'
#' SubchunkBlocks data (tag 47) holds information about the blocks in a
#' subchunks. Each chunk is divided into multiple 16x16x16 subchunks, and each
#' subchunk is stored separately and indicated by the use of the subtag.
#' Blocks are stored in a palette-based format. Subchunks can have two layers
#' of blocks, and the extra layer is most-often used to store water for
#' water-logged blocks.
#'
#' These functions return block data as strings containing the
#' block name and block states. The strings' format is
#' `blockname@@state1=value1@@state2=value2` etc.
#' Blocks may have 0 or more states. 
#'
#' `get_subchunk_blocks_data()` and `get_subchunk_blocks_value()` load SubchunkBlocks data from `db`.
#' `get_subchunk_blocks_value()` only supports loading a single value.
#'
#' `put_subchunk_blocks_data()`, `put_subchunk_blocks_values()`, and
#' `put_subchunk_blocks_value()` store SubchunkBlocks data into `db`.
#'
#' `read_subchunk_blocks_value()` decodes binary SubchunkBlock data.
#'
#' @param db A bedrockdb object.
#' @param x,z,dimension Chunk coordinates to extract data from.
#'    `x` can also be a character vector of db keys.
#' @param subchunk Subchunk indexes to extract data from.
#' @param values A list of 16x16x16 character() arrays. If `x` is missing, the names of `values` will be taken as the keys.
#' ignoring block states.
#' @param value A 16x16x16 character array.
#' @param names_only  A logical scalar. Return only the names of the blocks.
#' @param extra_block A logical scalar. Append the extra block layer to the
#' output (separated by ";"). This is mostly useful if you have waterlogged
#' blocks. If the extra block is air, it will not be appended.
#' @param version Which format of subchunk data to use
#' @param rawdata a raw vector holding binary SubchunkBlock data
#' @param missing_offset subchunk offset to use if one is not found in `rawdata`
#' @param object A 16x16x16 character array.
#'
#' @return `get_subchunk_blocks_value()` and `read_subchunk_blocks_value()`
#' return a 16x16x16 character array. The axes represent the `x`, `y`, and `z`
#' dimensions in that order.
#' `get_subchunk_blocks_data()`
#' return a list of the of the values returned by `read_subchunk_blocks_value()`.
#' `read_subchunk_blocks_value()` returns a 16x16x16 character array.
#' The axes represent the `x`, `y`, and `z` dimensions in that order.
#'
#' If a subchunk contains only air it will not be stored in the database, and
#' missing subchunks are considered air.
#
#' @name SubchunkBlocks
NULL

#' @rdname SubchunkBlocks
#' @export
get_subchunk_blocks_data <- function(x, z, dimension, subchunk, db,
        names_only = FALSE, extra_block = FALSE) {
    keys <- .process_chunk_key_args(x, z, dimension, tag = 47L, subtag = subchunk,
        assert_validity = TRUE)
    .get_subchunk_blocks_data_impl(keys, db, names_only = names_only, extra_block = extra_block)
}

.get_subchunk_blocks_data_impl <- function(keys, db, readoptions = NULL,
        names_only = FALSE, extra_block = FALSE) {
    dat <- get_data(keys, db = db, readoptions = readoptions)
    offsets <- .get_subtag_from_chunk_key(names(dat))
    ret <- purrr::map2(dat, offsets, read_subchunk_blocks_value, names_only = names_only,
        extra_block = extra_block)
    ret
}

#' @rdname SubchunkBlocks
#' @export
get_subchunk_blocks_value <- function(x, z, dimension, subchunk, db,
        names_only = FALSE, extra_block = FALSE) {
    key <- .process_chunk_key_args(x, z, dimension, tag = 47L, subtag = subchunk,
        assert_validity = TRUE, assert_scalar = TRUE)
    dat <- get_value(key, db)
    offset <- .get_subtag_from_chunk_key(key)
    read_subchunk_blocks_value(dat, offset, names_only = names_only,
        extra_block = extra_block)
}

#' @rdname SubchunkBlocks
#' @export
put_subchunk_blocks_data <- function(values, x, z, dimension, subchunk, db, version = 9L) {
    keys <- .process_chunk_key_args(x, z, dimension, tag = 47L, subtag = subchunk,
        values = values, assert_validity = TRUE)
    values <- vec_recycle(values, length(keys), x_arg="values")
    offsets <- .get_subtag_from_chunk_key(keys)
    values <- purrr::map2(values, offsets, ~write_subchunk_blocks_value(.x,version=version,missing_offset=.y))
    put_data(values, keys, db = db)
}

#' @rdname SubchunkBlocks
#' @export
put_subchunk_blocks_value <- function(value, x, z, dimension, subchunk, db, version = 9L) {
    key <- .process_chunk_key_args(x, z, dimension, tag = 47L, subtag = subchunk,
        assert_validity = TRUE, assert_scalar = TRUE)
    offset <- .get_subtag_from_chunk_key(key)
    value <- write_subchunk_blocks_value(value, version = version, missing_offset=offset)
    put_value(value, key, db = db)
}

#' @rdname SubchunkBlocks
#' @export
read_subchunk_blocks_value <- function(rawdata, missing_offset = NA, names_only = FALSE,
        extra_block = FALSE) {
    if(is.null(rawdata)) {
        return(NULL)
    }
    subchunk <- read_subchunk_layers_value(rawdata)
    offset <- attr(subchunk,"offset") %|% missing_offset
    blocks <- purrr::map(subchunk, function(x) {
        pal <- purrr::map_chr(x[["palette"]], .block_string, names_only = names_only)
        array(pal[ x[["values"]] ], dim = dim(x[["values"]]))
    })
    if(isTRUE(extra_block) && length(blocks) >= 2) {
        ret <- str_c(blocks[[1]], blocks[[2]], sep=";") %>%
            str_replace(";minecraft:air$", "")
        dim(ret) <- dim(blocks[[1]])
    } else {
        ret <- blocks[[1]]
    }
    structure(ret,offset=offset)
}

#' @rdname SubchunkBlocks
#' @export
write_subchunk_blocks_value <- function(object, version = 9L, missing_offset = NA_integer_) {
    if(!is_character(object, n=16*16*16)) {
        abort("`object` is not a character vector of length 4096")
    }
    # check to see if we have extra blocks and split as needed
    s <- str_split(object, fixed(";"))
    n <- max(purrr::map_int(s,length))
    # construct palettes and maps
    block_layers <- purrr::map(seq_len(n), function(i) {
        layer <- purrr::map_chr(s, i, .default = "minecraft:air")
        u <- vec_unique(layer)
        list(values = match(layer,u), palette = purrr::map(u, .block_nbt))
    })
    # copy attribute
    attr(block_layers, "offset") <- attr(object, "offset")
    write_subchunk_layers_value(block_layers, version=version, missing_offset=missing_offset)
}

#' Load and store SubchunkBlocks layers
#'
#' `get_subchunk_layers_data()` and `get_subchunk_layers_value()`
#' load SubchunkBlocks data from `db`.
#' `get_subchunk_layers_value()` only supports loading a single value.
#'
#' `get_subchunk_layers_value()` loads SubchunkBlocks data from a `bedrockdb`.
#' It supports efficiently loading subchunk block data from a single chunk.
#'
#' `put_subchunk_layers_data()` and
#' `put_subchunk_layers_value()` store SubchunkBlocks data into a `bedrockdb`.
#'
#' `write_subchunk_layers_value()` encode SubchunkBlock data
#' into binary form.
#' `read_subchunk_layers_value()` decodes binary SubchunkBlock data
#' into index-mapped arrays and associated block palettes.
#'
#' @param db A bedrockdb object.
#' @param x,z,dimension Chunk coordinates to extract data from.
#'    `x` can also be a character vector of db keys.
#' @param subchunk Subchunk indexes to extract data from.
#' @param values A list of lists of 16x16x16 integer indexes with associated block_palettes.
#' If `x` is missing, the names of `values` will be taken as the keys.
#' @param value A list of 16x16x16 integer indexes with associated block_palettes.
#' @param missing_offset subchunk offset to use if one is not found in `rawdata`
#'
#' @return `get_subchunk_layers_value()` and `read_subchunk_layers_value()`
#' return a list of block layers. Each block layer is a 16x16x16 array of
#' integers associated with a block palette. The block palette is stored in the
#' "palette" attribute of the array.
#' `get_subchunk_layers_data()` returns a list of the of the values
#'          returned by `read_subchunk_layers_value()`.
#'
#' @keywords internal
#' @export
get_subchunk_layers_data <- function(x, z, dimension, subchunk, db) {
    dat <- .get_chunk_data(x, z, dimension, tag = 47L, subtag = subchunk, db = db)
    purrr::map(dat, read_subchunk_layers_value)
}

#' @rdname get_subchunk_layers_data
#' @export
get_subchunk_layers_value <- function(x, z, dimension, subchunk, db) {
    dat <- .get_chunk_value(x, z, dimension, tag = 47L, subtag = subchunk, db = db)
    read_subchunk_layers_value(dat)
}

#' @rdname get_subchunk_layers_data
#' @export
put_subchunk_layers_data <- function(values, x, z, dimension, subchunk, db, ...) {
    keys <- .process_chunk_key_args(x, z, dimension, tag = 47L, values = values, assert_validity = TRUE)
    values <- vec_recycle(values, length(keys), x_arg="values")
    values <- purrr::map(values, write_subchunk_layers_value, ...)
    put_data(values, keys, db = db)
}

#' @rdname get_subchunk_layers_data
#' @export
put_subchunk_layers_value <- function(value, x, z, dimension, subchunk, db, ...) {
    key <- .process_chunk_key_args(x, z, dimension, tag = 47L,
        assert_validity = TRUE, assert_scalar = TRUE)
    value <- write_subchunk_layers_value(value, ...)
    put_value(value, key, db)
}

#' @rdname get_subchunk_layers_data
#' @export
read_subchunk_layers_value <- function(rawdata) {
    if(is.null(rawdata)) {
        return(NULL)
    }
    vec_assert(rawdata, raw())
    x <- .Call(rbedrock_chunk_read_subchunk, rawdata)
    x <- purrr::modify(x, function(y) {
        y$palette <- from_rnbt(y$palette)
        y
    })
    x
}

#' @rdname get_subchunk_layers_data
#' @export
write_subchunk_layers_value <- function(object, version=9L, missing_offset=NA_integer_) {
    if(is_null(object)) {
        return(NULL)
    }
    if(!is_list(object)) {
        abort("`object` must be a list.")
    }
    if(version != 8L && version != 9L) {
        abort(str_glue("`version` {version} not supported."))
    }
    if(version >= 9L) {
        # identify offset
        offset <- attr(object, "offset") %||% missing_offset
        offset <- vec_cast(offset, integer())
        if(is.na(offset)) {
            abort("subchunk format 9 requires a valid subchunk offset.")
        }
    }
    values <- purrr::map(object, function(x) {
        if(length(x[["values"]]) != 16*16*16) {
            abort("an element of `object` is malformed")
        }
        as.integer(x[["values"]])
    })
    palette <- purrr::map(object, function(x) {
        if(!has_name(x, "palette") || 
            any(purrr::map_lgl(x[["palette"]],is_nbt) == FALSE)) {
            abort("an element of `object` is malformed")
        }
        to_rnbt(x[["palette"]])
    })

    .Call(rbedrock_chunk_write_subchunk, values, palette, version, offset)
}

.block_string <- function(x, names_only = FALSE) {
    # convert block information in a palette entry into a string
    block_name <- payload(x$name)
    states <- payload(x$states)
    if(length(states) == 0L || isTRUE(names_only)) {
        return(block_name)
    }
    states <- purrr::imap(states, function(x,y) {
        xtag <- get_nbt_tag(x)
        p <- payload(x)
        if(xtag == 1) {
            tolower(as.character(as.logical(p)))
        } else if(xtag == 3 || xtag == 8) {
            as.character(p)
        } else {
            msg <- str_glue(
                "Block State '{y}' has NBT tag '{xtag}'. ",
                "Possible loss of information when converting to a string."
            )
            rlang::warn(msg)
            as.character(p)
        }
    })
    states <- str_c(names(states), states,
        sep="=", collapse="@")
    str_c(block_name, states, sep="@")
}

.BIT_STATES <- c(
    "age_bit", "allow_underwater_bit", "attached_bit",
    "brewing_stand_slot_a_bit", "brewing_stand_slot_b_bit",
    "brewing_stand_slot_c_bit", "button_pressed_bit", "color_bit",
    "conditional_bit", "coral_hang_type_bit", "covered_bit", "dead_bit",
    "deprecated", "disarmed_bit", "door_hinge_bit", "drag_down",
    "end_portal_eye_bit", "explode_bit", "extinguished", "hanging",
    "head_piece_bit", "in_wall_bit", "infiniburn_bit", "item_frame_map_bit",
    "no_drop_bit", "occupied_bit", "open_bit", "output_lit_bit",
    "output_subtract_bit", "persistent_bit", "powered_bit", "rail_data_bit",
    "stability_check", "stripped_bit", "suspended_bit", "toggle_bit",
    "top_slot_bit", "triggered_bit", "update_bit", "upper_block_bit",
    "upside_down_bit"
)

.INTEGER_STATES <- c(
    "age", "bite_counter", "cluster_count", "composter_fill_level",
    "coral_direction", "direction", "facing_direction", "fill_level",
    "ground_sign_direction", "growth", "height", "honey_level",
    "huge_mushroom_bits", "liquid_depth", "moisturized_amount",
    "rail_direction", "redstone_signal", "repeater_delay", "stability",
    "vine_direction_bits", "weirdo_direction"
)

.STRING_STATES <- c(
    "attachment", "bamboo_leaf_size", "bamboo_stalk_thickness",
    "cauldron_liquid", "chemistry_table_type", "chisel_type", "color",
    "coral_color", "cracked_state", "damage", "dirt_type", "double_plant_type",
    "flower_type", "monster_egg_stone_type", "new_leaf_type", "new_log_type",
    "old_leaf_type", "old_log_type", "pillar_axis", "portal_axis",
    "prismarine_block_type", "sand_stone_type", "sand_type", "sapling_type",
    "sea_grass_type", "sponge_type", "stone_brick_type", "stone_slab_type_2",
    "stone_slab_type_3", "stone_slab_type_4", "stone_slab_type", "stone_type",
    "structure_block_type", "structure_void_type", "tall_grass_type",
    "torch_facing_direction", "turtle_egg_count", "wall_block_type", "wood_type"
)

.as_bit <- function(x, strict=FALSE) {
    true_values <- c("true", "TRUE", "1", "T", "t")
    false_values <- c("false", "FALSE", "0", "F", "f")
    if(isTRUE(strict)) {
        true_values <- true_values[1:2]
        false_values <- false_values[1:2]
    }
    if(x %in% true_values) {
        return(1L)
    }
    if(x %in% false_values) {
        return(0L)
    }
    NA_integer_
}

.block_state_nbt <- function(state, name) {
    vec_assert(state, character(), size=1L)
    if(name %in% .STRING_STATES) {
        return(nbt_string(state))
    } else if(name %in% .INTEGER_STATES) {
        p <- as.integer(state)
        if(is.na(p)) {
            msg <- str_glue("Block State '{name}={state}' could not ",
                "be converted to an integer.")
            rlang::warn(msg)
        }
        return(nbt_int(p))
    } else if(name %in% .BIT_STATES) {
        p <- .as_bit(state)
        if(is.na(p)) {
            msg <- str_glue("Block State '{name}={state}' could not ",
                "be converted to a boolean bit.")
            rlang::warn(msg)
        }
        return(nbt_byte(p))
    }
    msg <- str_glue("Unknown Block State '{name}={state}' ",
        "converted to an ")

    p <- suppressWarnings(as.integer(state))
    if(!is.na(p)) {
        msg <- str_glue(msg, "nbt_int.")
        ret <- nbt_int(p)
    } else {
        p <- .as_bit(state, strict=TRUE)
        if(!is.na(p)) {
            msg <- str_glue(msg, "nbt_byte.")
            ret <- nbt_byte(as.integer(p))
        } else {
            msg <- str_glue(msg, "nbt_string.")
            ret <- nbt_string(state)
        }        
    }
    rlang::warn(msg)
    ret
}

.block_nbt <- function(x) {
    # convert block string back into palette information
    s <- str_split(x, fixed('@'))[[1]]
    name <- s[1]
    if(length(s) > 1) {
        s <- str_split(s[-1], fixed("="))
        s <- purrr::transpose(s)
        s[[1]] <- tolower(s[[1]])
        states <- rlang::set_names(s[[2]], s[[1]])
        states <- nbt_compound(!!!purrr::imap(states, .block_state_nbt))
    } else {
        states <- nbt_compound()
    }

    nbt_compound(name = nbt_string(name),
        states = states,
        version = nbt_int(0x1100010)) # 1.16.0.16 ??
}

#' @description
#' `subchunk_origins()` returns a matrix containing the block coordinate of the
#' lower NW corner of subchunk keys
#' @param keys A character vector of database keys.
#' @export
#' @rdname SubchunkBlocks
subchunk_origins <- function(keys) {
    pos <- .extract_chunk_key_components(keys, which=c(1,5,2))
    pos*16L
}

#' @description
#' `subchunk_coords()` determines the block coordinates of blocks based on their
#' array indexes and their subchunk origins.
#'
#' @param ind Numeric vector or a named list of numeric vectors containing
#'            indexes for blocks in a subchunk.
#' @param origins A matrix of subchunk origins.
#'
#' @return `subchunk_coords()` returns a 3-column matrix of block coordinates.
#' @export
#' @rdname SubchunkBlocks
subchunk_coords <- function(ind, origins=subchunk_origins(names(ind))) {
    if(is.character(origins)) {
        origins <- subchunk_origins(origins)
    }
    
    f <- function(x,y) t(y - 1L + t(arrayInd(x,c(16,16,16))))
    if(is.list(ind)) {
        o <- purrr::array_tree(origins,1)
        purrr::map2(ind, o, f)
    } else {
        f(ind, as.vector(origins))
    }
}
