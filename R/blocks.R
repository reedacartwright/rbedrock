#' Load block data from one or more chunks
#'
#' @description
#' These functions return block data as strings containing the
#' block name and block states. The strings' format is
#' `blockname@@state1=value1@@state2=value2` etc.
#' Blocks may have 0 or more states.
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
#' @return `get_chunk_blocks_data()` returns a list of the of the values
#' returned by `read_chunk_blocks_value()`.
#' @export
get_chunk_blocks_data <- function(db, x, z, dimension,
        names_only = FALSE, extra_block = FALSE) {
    keys <- .process_key_args_prefix(x, z, dimension)
    keys <- vec_unique(keys)
    keys <- str_c(keys, ":47")

    dat <- purrr::map(keys, ~.get_chunk_blocks_value_impl(db,
        prefix = .,
        names_only = names_only,
        extra_block = extra_block
    ))

    set_names(dat, keys)
}

#' @description
#' `get_chunk_blocks_value()` is an alias for `get_chunk_blocks_data()`
#' @export
#' @rdname get_chunk_blocks_data
get_chunk_blocks_values <- get_chunk_blocks_data

#' @description
#' `get_chunk_blocks_value()` loads block data from a `bedrockdb`.
#' It only supports loading a single value.
#'
#' @return `get_chunk_blocks_value()`
#' return a 16xNx16 character array. The axes represent the `x`, `y`, and `z`
#' dimensions in that order. The size of the y-axis is based on the highest
#' subchunk in the coordinate. Missing subchunks are considered air.
#'
#' @rdname get_chunk_blocks_data
#' @export
get_chunk_blocks_value <- function(db, x, z, dimension,
        names_only = FALSE, extra_block = FALSE) {
    keys <- .process_key_args_prefix(x, z, dimension)
    keys <- vec_unique(keys)
    vec_assert(keys, character(), 1L)

    .get_chunk_blocks_value_impl(db, keys,
        names_only = names_only,
        extra_block = extra_block
    )
}

#' @description
#' `put_chunk_blocks_data()`, `put_chunk_blocks_values()`, and
#' `put_chunk_blocks_value()` stores block data into a `bedrockdb`.
#'
#' @param data A named list of 16xNx16 character() arrays
#'
#' @rdname get_chunk_blocks_data
#' @export
put_chunk_blocks_data <- function(db, data) {
    .check_chunk_key_tag(names(data), tag = 47L)

    purrr::iwalk(data, ~.put_chunk_blocks_value_impl(db, .y, .x))
}

#' @param values A list of 16xNx16 character() arrays
#'
#' @rdname get_chunk_blocks_data
#' @export
put_chunk_blocks_values <- function(db, x, z, dimension, values) {
    keys <- .process_key_args(x, z, dimension, tag = 47L, stop_if_filtered = TRUE)
    values <- vec_recycle(values, length(keys), x_arg = "values")

    purrr::walk2(keys, values, ~.put_chunk_blocks_value_impl(db, .x, .y))
}

#' @param value A 16xNx16 character array
#'
#' @rdname get_chunk_blocks_data
#' @export
put_chunk_blocks_value <- function(db, x, z, dimension, value) {
    key <- .process_key_args(x, z, dimension, tag=47L)
    vec_assert(key, character(), 1L)

    .put_chunk_blocks_value_impl(db, key, value)
}


.get_chunk_blocks_value_impl <- function(db, prefix, ...) {
    keys <- get_keys(db, starts_with = prefix)
    if(length(keys) == 0L) {
        return(NULL)
    }
    dat <- get_subchunk_blocks_data(db, keys, ...)
    pos <- .get_subtag_from_chunk_key(names(dat))
    max_y <- 16*max(pos)+16
    mat <- array("minecraft:air", c(16, max_y, 16))
    for(i in seq_along(pos)) {
        mat[,(pos[i]*16)+1:16,] <- dat[[i]]
    }
    mat
}

.put_chunk_blocks_value_impl <- function(db, prefix, value) {
    d <- dim(value)
    if(!is.character(value) || is.null(d) || length(d) != 3L || 
        d[1] != 16L || d[3] != 16L) {
        abort("`value` must be a 16xNx16 character array.")
    }

    max_y <- d[2]
    if((max_y %% 16L) != 0L) {
        # increase the vector
        max_y <- ((d[2]-1L) %/% 16L)*16L+16L
        old_value <- value
        value <- array("minecraft:air", c(16L,max_y,16L))
        value[,1:d[2],] <- old_value
    }
    
    # identify existing chunk data.
    old_keys <- get_keys(db, starts_with = prefix)
    
    # construct new chunk data
    data <- list()
    subtags <- seq.int(length.out=(max_y %/% 16L))
    new_keys <- str_c(prefix, "-", subtags-1L)
    for(s in subtags) {
        subchunk <- value[,(s-1L)*16L+(1:16),]
        # skip empty chunks
        if(all(subchunk == "minecraft:air")) {
            next
        }
        # extract and encode subchunk data
        data[[new_keys[s]]] <- write_subchunk_blocks_value(subchunk)
    }
    put_keys <- names(data)
    del_keys <- old_keys[!(old_keys %in% put_keys)]
    # write a batch
    batch <- db$writebatch()
    batch$mdelete(chrkeys_to_rawkeys(del_keys))
    batch$mput(chrkeys_to_rawkeys(put_keys), data)
    batch$write()
    # clean up
    batch$destroy()

    invisible()
}

#' Load and store SubchunkBlocks data
#'
#' @description
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
#' @details
#' If a subchunk contains only air it will not be stored in the database, and
#' missing subchunks are considered air.
#
#' @name SubchunkBlocks
NULL

#' @description
#' `get_subchunk_blocks_data()` loads SubchunkBlocks data from a `bedrockdb`.
#'  It will silently drop and keys not representing SubchunkBlocks data.
#'
#' @param db A bedrockdb object.
#' @param x,z,dimension Chunk coordinates to extract data from.
#'    `x` can also be a character vector of db keys.
#' @param subchunk Subchunk indexes to extract data from.
#'
#' @param names_only  A logical scalar. Return only the names of the blocks,
#' ignoring block states.
#' @param extra_block A logical scalar. Append the extra block layer to the
#' output (separated by ";"). This is mostly useful if you have waterlogged
#' blocks. If the extra block is air, it will not be appended.
#'
#' @return `get_subchunk_blocks_data()` returns a list of the of the values
#' returned by `read_subchunk_blocks_value()`.
#'
#' @rdname SubchunkBlocks
#' @export
get_subchunk_blocks_data <- function(db, x, z, dimension, subchunk,
        names_only = FALSE, extra_block = FALSE) {
    keys <- .process_key_args(x,z,dimension, tag=47L, subtag = subchunk)
    dat <- get_values(db, keys)
    ret <- purrr::map(dat, read_subchunk_blocks_value, names_only = names_only,
        extra_block = extra_block)
    ret
}

#' @description
#' `get_subchunk_blocks_value()` loads SubchunkBlocks data from a `bedrockdb`.
#' It only supports loading a single value.
#'
#' @return `get_subchunk_blocks_value()` and `read_subchunk_blocks_value()`
#' return a 16x16x16 character array. The axes represent the `x`, `y`, and `z`
#' dimensions in that order.
#'
#' @rdname SubchunkBlocks
#' @export
get_subchunk_blocks_value <- function(db, x, z, dimension, subchunk,
        names_only = FALSE, extra_block = FALSE) {
    key <- .process_key_args(x,z,dimension, tag=47L, subtag = subchunk)
    vec_assert(key, character(), 1L)

    dat <- get_value(db, key)
    
    read_subchunk_blocks_value(dat, names_only = names_only,
        extra_block = extra_block)
}

#' @description
#' `put_subchunk_blocks_data()`, `put_subchunk_blocks_values()`, and
#' `put_subchunk_blocks_value()` store SubchunkBlocks data into a `bedrockdb`.
#'
#' @param data A named list of 16x16x16 character() arrays
#'
#' @rdname SubchunkBlocks
#' @export
put_subchunk_blocks_data <- function(db, data) {
    .check_chunk_key_tag(names(data), 47L)
    dat <- purrr::map(data, write_subchunk_blocks_value)
    put_data(db, dat)
}

#' @param values A list of 16x16x16 character() arrays
#'
#' @rdname SubchunkBlocks
#' @export
put_subchunk_blocks_values <- function(db, x, z, dimension, subchunk, values) {
    keys <- .process_key_args(x, z, dimension, tag=47L, subtag = subchunk, stop_if_filtered = TRUE)
    values <- vec_recycle(values, length(keys), x_arg="values")
    values <- purrr::map(values, write_subchunk_blocks_value)
    put_values(db, keys, values)
}

#' @param value A 16x16x16 character array
#'
#' @rdname SubchunkBlocks
#' @export
put_subchunk_blocks_value <- function(db, x, z, dimension, subchunk, value) {
    key <- .process_key_args(x, z, dimension, tag=47L, subtag = subchunk)
    vec_assert(key, character(), 1L)
    value <- write_subchunk_blocks_value(value)
    put_value(db, key, value)
}

#' @description
#' `read_subchunk_blocks_value()` decodes binary SubchunkBlock data.
#'
#' @param rawdata a raw vector holding binary SubchunkBlock data
#'
#' @return `read_subchunk_blocks_value()` returns a 16x16x16 character array.
#' The axes represent the `x`, `y`, and `z` dimensions in that order.
#' 
#' @rdname SubchunkBlocks
#' @export
read_subchunk_blocks_value <- function(rawdata, names_only = FALSE,
        extra_block = FALSE) {
    if(is.null(rawdata)) {
        return(NULL)
    }
    blocks <- .read_subchunk(rawdata)
    pal <- block_palette(blocks[[1]]) %>%
        purrr::map_chr(.block_string, names_only = names_only)
    b <- array(pal[blocks[[1]]], dim = dim(blocks[[1]]))
    if(isTRUE(extra_block) && length(blocks) >= 2) {
        pal2 <- block_palette(blocks[[2]]) %>%
            purrr::map_chr(.block_string, names_only = names_only)
        pal2[pal2 == 'minecraft:air'] <- NA_character_
        b2 <- array(pal2[blocks[[2]]], dim = dim(blocks[[2]]))
        b[] <- str_c(b, b2, sep=";") %|% b
    }
    b
}

#' @param object A 16x16x16 character array.
#' @rdname SubchunkBlocks
#' @export
write_subchunk_blocks_value <- function(object) {
    vec_assert(object, array(character(), c(0,16,16)), 16)
    # check to see if we have extra blocks and split as needed
    n <- max(str_count(object, fixed(";")))+1
    if(n == 1) {
        block_layers <- list(c(object))
    } else {
        s <- str_split(object, fixed(";"))
        block_layers <- list()
        for(i in 1:n) {
            block_layers[[i]] <- purrr::map_chr(s, i, .default = "minecraft:air")
        }
    }

    # construct palettes and maps
    block_layers <- purrr::map(block_layers, function(x) {
        u <- vec_unique(x)
        structure(match(x,u), "palette" = purrr::map(u, .block_nbt))
    })
    #block_layers
    .write_subchunk(block_layers)
}

#' Load and store SubchunkBlocks layers
#'
#' @description
#' `get_subchunk_layers_data()` loads SubchunkBlocks data from a `bedrockdb`.
#'  It will silently drop and keys not representing SubchunkBlocks data.
#'
#' @param db A bedrockdb object.
#' @param x,z,dimension Chunk coordinates to extract data from.
#'    `x` can also be a character vector of db keys.
#' @param subchunk Subchunk indexes to extract data from.
#'
#' @param layer an integer vector specifying which layers of storage to return
#'              or `NULL` to return all layers
#'
#' @param simplify a logical scalar indicating whether to simplify single-layer
#'                 results to an array instead of a list containing one array
#'
#' @return `get_subchunk_layers_data()` returns a list of the of the values
#'          returned by `read_subchunk_layers_value()`.
#'
#' @keywords internal
#' @export
get_subchunk_layers_data <- function(db, x, z, dimension, subchunk, layer = 1L,
        simplify = TRUE) {
    keys <- .process_key_args(x,z,dimension, tag=47L, subtag = subchunk)
    dat <- get_values(db, keys)
    ret <- purrr::map(dat, read_subchunk_layers_value, layer = layer,
        simplify = simplify)
    ret
}

#' @description
#' `get_subchunk_layers_value()` loads SubchunkBlocks data from a `bedrockdb`.
#' It only supports loading a single value.
#'
#' @return `get_subchunk_layers_value()` and `read_subchunk_layers_value()`
#' return a list of block layers. Each block layer is a 16x16x16 array of
#' integers associated with a block palette. The block palette is stored in the
#' "palette" attribute of the array.
#'
#' @rdname get_subchunk_layers_data
#' @export
get_subchunk_layers_value <- function(db, x, z, dimension, subchunk, layer = 1L,
        simplify = TRUE) {
    key <- .process_key_args(x, z, dimension, tag=47L, subtag = subchunk)
    vec_assert(key, character(), 1L)

    dat <- get_value(db, key)
    read_subchunk_layers_value(dat, layer = layer, simplify = simplify)
}

#' @description
#' `put_subchunk_layers_data()`, `put_subchunk_layers_values()`, and
#' `put_subchunk_layers_value()` store SubchunkBlocks data into a `bedrockdb`.
#'
#' @param data A named-vector of key-value pairs for SubchunkBlocks data.
#'
#' @rdname get_subchunk_layers_data
#' @export
put_subchunk_layers_data <- function(db, data) {
    .check_chunk_key_tag(names(data), 47L)
    dat <- purrr::map(data, write_subchunk_layers_value)
    put_data(db, dat)
}

#' @param values A list of lists of 16x16x16 integer indexes with associated block_palettes.
#'
#' @rdname get_subchunk_layers_data
#' @export
put_subchunk_layers_values <- function(db, x, z, dimension, subchunk, values) {
    keys <- .process_key_args(x, z, dimension, tag=47L, subtag = subchunk, stop_if_filtered = TRUE)
    values <- vec_recycle(values, length(keys), x_arg="values")
    values <- purrr::map(values, write_subchunk_layers_value)
    put_values(db, keys, values)
}

#' @param value A list of 16x16x16 integer indexes with associated block_palettes.
#'
#' @rdname get_subchunk_layers_data
#' @export
put_subchunk_layers_value <- function(db, x, z, dimension, subchunk, value) {
    key <- .process_key_args(x, z, dimension, tag=47L, subtag = subchunk)
    vec_assert(key, character(), 1L)
    value <- write_subchunk_layers_value(value)
    put_value(db, key, value)
}

#' @description
#' `read_subchunk_layers_value()` decodes binary SubchunkBlock data
#' into index-mapped arrays and associated block palettes.
#'
#' @rdname get_subchunk_layers_data
#' @export
read_subchunk_layers_value <- function(rawdata, layer = 1L, simplify = TRUE) {
    if(is.null(rawdata)) {
        return(NULL)
    }
    blocks <- .read_subchunk(rawdata)
    # subset the storage layers
    if (!is.null(layer)) {
        blocks <- blocks[layer]
    }
    # Simplify if requested
    if (isTRUE(simplify) && length(blocks) == 1) {
        blocks <- blocks[[1]]
    }
    blocks
}

#' @description
#' `write_subchunk_layers_value()` encode SubchunkBlock data
#' into binary form.
#'
#' @rdname get_subchunk_layers_data
#' @export
write_subchunk_layers_value <- function(object) {
    if(is.null(object)) {
        return(NULL)
    }
    if(!is.list(object)) {
        object <- list(object)
    }
    .write_subchunk(object)
}

#' @description
#' `block_palette()` returns the NBT palette associated with a block storage layer.
#'
#' @param object A storage layer returned from `read_subchunk` or `get_subchunk`.
#' @rdname get_subchunk_layers_data
#' @export
block_palette <- function(object) {
    attr(object, 'palette')
}

.block_string <- function(x, names_only = FALSE) {
    # convert block information in a palette entry into a string
    block_name <- payload(x$name)
    states <- payload(x$states)
    if(length(states) == 0L || isTRUE(names_only)) {
        return(block_name)
    }
    states <- purrr::imap(states, function(x,y) {
        xtag <- tag(x)
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

.read_subchunk <- function(rawdata) {
    vec_assert(rawdata, raw())
    .Call(Cread_subchunk, rawdata)
}

.write_subchunk <- function(object) {
    vec_assert(object, list())
    .Call(Cwrite_subchunk, object)
}

#' @description
#' `subchunk_origins()` returns a matrix containing the block coordinate of the
#' lower NW corner of subchunk keys
#' @param keys A character vector of database keys.
#' @export
#' @rdname SubchunkBlocks
subchunk_origins <- function(keys) {
    pos <- .split_chunk_keys(keys)[,c(2,6,3), drop = FALSE]
    mode(pos) <- "integer"
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
