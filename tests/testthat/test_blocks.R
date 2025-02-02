dbpath <- rbedrock_example_world("example1.mcworld")
db <- bedrockdb(dbpath)

test_that("SubChunkBlocks is chunk tag 47.", {
    expect_equal(chunk_tag_int("SubChunkBlocks"), 47L)
    expect_equal(chunk_tag_str(47L), "SubChunkBlocks")
})

test_that("write_subchunk_layers_value() encodes subchunk data.", {
    raw1_orig <- get_value(db, "chunk:31:2:0:47:0")
    layer1 <- read_subchunk_layers_value(raw1_orig)
    raw1_test <- write_subchunk_layers_value(layer1, version = 8L)

    expect_equal(raw1_test, raw1_orig)
})

test_that(".block_nbt() and .block_string() are inverses.", {
    dat <- get_subchunk_layers_value(db, "chunk:31:2:0:47:4")
    pal_orig <- dat[[1]]$palette
    text <- purrr::map_chr(pal_orig, .block_string)
    pal_test <- purrr::map(text, .block_nbt)

    expect_equal(pal_orig, pal_test)
})

test_that("put_subchunk_blocks_value() writes subchunk data.", {
    val <- array("minecraft:air", c(16, 16, 16))
    val[, 1, ] <- "minecraft:bedrock@infiniburn_bit=false"
    val[, 2, ] <- "minecraft:stone@stone_type=stone"
    val[, 3, ] <- "minecraft:iron_ore"
    val[, 4, ] <- "minecraft:snow_layer@height=5@covered_bit=true"
    attr(val, "offset") <- 0L

    put_subchunk_blocks_value(db, 0, 0, 0, 0, val, version = 8L)
    dat <- get_subchunk_blocks_value(db, 0, 0, 0, 0)
    expect_equal(dat, val)

    put_subchunk_blocks_value(db, 0, 0, 0, 0, val)
    dat <- get_subchunk_blocks_value(db, 0, 0, 0, 0)
    expect_equal(dat, val)
})

test_that("put_subchunk_blocks_values() writes subchunk data.", {
    val <- array("minecraft:air", c(16, 16, 16))
    val[, 1, ] <- "minecraft:bedrock@infiniburn_bit=false"
    val[, 2, ] <- "minecraft:stone@stone_type=granite"
    val[, 3, ] <- "minecraft:iron_ore"
    val[, 4, ] <- "minecraft:snow_layer@height=5@covered_bit=true"
    attr(val, "offset") <- NA_integer_

    put_subchunk_blocks_values(db, 0, 0, 0, 1:4, list(val), version = 8L)
    dat <- get_subchunk_blocks_data(db, 0, 0, 0, 1:4)

    val <- rlang::set_names(
        rep(list(val), 4),
        stringr::str_glue("chunk:0:0:0:47:{1:4}")
    )
    for (i in seq_along(val)) {
        attr(val[[i]], "offset") <- i
    }

    expect_equal(dat, val)
})

test_that("put_subchunk_blocks_data() writes subchunk data.", {
    val <- array("minecraft:air", c(16, 16, 16))
    val[, 1, ] <- "minecraft:bedrock@infiniburn_bit=false"
    val[, 2, ] <- "minecraft:stone@stone_type=andesite"
    val[, 3, ] <- "minecraft:iron_ore"
    val[, 4, ] <- "minecraft:snow_layer@height=5@covered_bit=true"
    attr(val, "offset") <- NA_integer_

    val <- rlang::set_names(
        rep(list(val), 4),
        stringr::str_glue("chunk:1:1:0:47:{1:4}")
    )
    for (i in seq_along(val)) {
        attr(val[[i]], "offset") <- i
    }

    put_subchunk_blocks_data(db, val, version = 8L)
    dat <- get_subchunk_blocks_data(db, names(val))

    expect_equal(dat, val)

    names(val) <- stringr::str_glue("chunk:1:2:0:47:{-4:-1}")
    for (i in seq_along(val)) {
        attr(val[[i]], "offset") <- i - 5
    }
    put_subchunk_blocks_data(db, val)
    dat <- get_subchunk_blocks_data(db, names(val))

    expect_equal(dat, val)
})

test_that("put_chunk_blocks_value() writes chunk data.", {
    val <- array("minecraft:air", c(16, 32, 16))
    val[, 21, ] <- "minecraft:bedrock@infiniburn_bit=false"
    val[, 22, ] <- "minecraft:stone@stone_type=andesite"
    val[, 23, ] <- "minecraft:iron_ore"
    val[, 24, ] <- "minecraft:snow_layer@height=5@covered_bit=true"
    chunk_origin(val) <- c(10, -4, 12) * 16L

    # Standard Behavior
    expected_dat <- array("minecraft:air", c(16, 384, 16))
    expected_dat[, 1:32, ] <- val
    chunk_origin(expected_dat) <- c(10, -4, 12) * 16L

    put_chunk_blocks_value(db, "chunk:10:12:0:47", value = val)

    dat <- get_chunk_blocks_value(db, "chunk:10:12:0")
    expect_equal(dat, expected_dat)
    dat <- get_keys(db, starts_with = "chunk:10:12:0:47")
    expect_equal(dat, "chunk:10:12:0:47:-3")

    # Alternative chunk_origin
    chunk_origin(val) <- c(10, 0, 13) * 16L
    put_chunk_blocks_value(db, "chunk:10:13:0:47", value = val)
    dat <- get_chunk_blocks_value(db, "chunk:10:13:0:47")

    expected_dat <- array("minecraft:air", c(16, 384, 16))
    expected_dat[, 65:96, ] <- val
    chunk_origin(expected_dat) <- c(10, -4, 13) * 16L

    expect_equal(dat, expected_dat)

    dat <- get_keys(db, starts_with = "chunk:10:13:0:47")
    expect_equal(dat, "chunk:10:13:0:47:1")
})

test_that("put_chunk_blocks_value() overwrites chunk data.", {
    dat <- get_chunk_blocks_value(db, 31, 4, 0)
    val <- dat[, 1:32 + 5 + 65, ]
    chunk_origin(val) <- c(31, -4, 4) * 16L

    # Standard Behavior
    expected_dat <- array("minecraft:air", c(16, 384, 16))
    expected_dat[, 1:32, ] <- val
    chunk_origin(expected_dat) <- c(31, -4, 4) * 16L

    put_chunk_blocks_value(db, 31, 4, 0, val)
    dat <- get_chunk_blocks_value(db, 31, 4, 0)
    expect_equal(dat, expected_dat)

    dat <- get_keys(db, starts_with = "chunk:31:4:0:47")
    expect_equal(dat, c("chunk:31:4:0:47:-4", "chunk:31:4:0:47:-3"))

    # Using empty chunks just deletes block data
    val <- array("minecraft:air", c(16, 16, 16))
    chunk_origin(val) <- c(31, 0, 4) * 16L
    put_chunk_blocks_value(db, 31, 4, 0, val, version = 9L)
    dat <- get_keys(db, starts_with = "chunk:31:4:0:47")
    expect_length(dat, 0)
})

test_that("put_chunk_blocks_values() writes chunk data.", {
    val <- array("minecraft:air", c(16, 32, 16))
    val[, 21, ] <- "minecraft:bedrock@infiniburn_bit=false"
    val[, 22, ] <- "minecraft:stone@stone_type=andesite"
    val[, 23, ] <- "minecraft:iron_ore"
    val[, 24, ] <- "minecraft:snow_layer@height=5@covered_bit=true"
    chunk_origin(val) <- c(11, -4, 12) * 16L

    # Standard Behavior
    expected_dat <- array("minecraft:air", c(16, 384, 16))
    expected_dat[, 1:32, ] <- val
    chunk_origin(expected_dat) <- c(11, -4, 12) * 16L

    put_chunk_blocks_values(db, 11:12, 12, 0, value = list(val))
    dat <- get_chunk_blocks_values(db, 11:12, 12, 0)

    expect_equal(dat[[1]], expected_dat)

    chunk_origin(expected_dat) <- c(12, -4, 12) * 16L
    expect_equal(dat[[2]], expected_dat)

    dat <- get_keys(db, starts_with = "chunk:11:12:0:47")
    expect_equal(dat, "chunk:11:12:0:47:-3")
    dat <- get_keys(db, starts_with = "chunk:12:12:0:47")
    expect_equal(dat, "chunk:12:12:0:47:-3")
})

test_that("put_chunk_blocks_data() writes chunk data.", {
    val <- list()
    val[["chunk:13:12:0:47"]] <- array("minecraft:air", c(16, 32, 16))
    val[["chunk:14:12:0:47"]] <- array("minecraft:air", c(16, 32, 16))
    chunk_origin(val[[1]]) <- c(13, -4, 12) * 16L
    chunk_origin(val[[2]]) <- c(14, -4, 12) * 16L

    val[["chunk:13:12:0:47"]][, 21, ] <-
        "minecraft:bedrock@infiniburn_bit=false"
    val[["chunk:14:12:0:47"]][, 1, ] <-
        "minecraft:bedrock@infiniburn_bit=false"

    put_chunk_blocks_data(db, val)

    expected_dat <- array("minecraft:air", c(16, 384, 16))
    expected_dat <- list(
        "chunk:13:12:0" = expected_dat,
        "chunk:14:12:0" = expected_dat
    )
    expected_dat[[1]][, 21, ] <- "minecraft:bedrock@infiniburn_bit=false"
    expected_dat[[2]][, 1, ] <- "minecraft:bedrock@infiniburn_bit=false"

    chunk_origin(expected_dat[[1]]) <- c(13, -4, 12) * 16L
    chunk_origin(expected_dat[[2]]) <- c(14, -4, 12) * 16L

    dat <- get_chunk_blocks_values(db, c(
        "chunk:13:12:0",
        "chunk:14:12:0"
    ))

    expect_equal(dat, expected_dat)

    dat <- get_keys(db, starts_with = "chunk:13:12:0:47")
    expect_equal(dat, "chunk:13:12:0:47:-3")
    dat <- get_keys(db, starts_with = "chunk:14:12:0:47")
    expect_equal(dat, "chunk:14:12:0:47:-4")
})

test_that("read_subchunk_layers_value() decodes subchunk data", {
    dat <- c(
        9, 2, 0, 8, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 51, 51, 65, 2, 0, 19, 17, 17, 51, 51, 68, 51, 51, 19, 17,
        17, 102, 51, 68, 52, 51, 17, 17, 17, 19, 17, 68, 68, 17, 17, 17, 17,
        17, 17, 65, 20, 17, 17, 17, 49, 17, 17, 72, 52, 51, 17, 17, 51, 17, 17,
        136, 51, 51, 17, 17, 51, 17, 51, 51, 51, 51, 17, 17, 51, 49, 51, 51,
        51, 51, 17, 17, 51, 49, 51, 51, 53, 68, 20, 17, 51, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 51, 51, 68, 52, 48, 17, 17, 17, 102, 51, 68, 68, 19, 17, 17, 17,
        19, 51, 68, 68, 17, 17, 17, 17, 19, 129, 68, 68, 17, 17, 17, 17, 17,
        129, 68, 68, 19, 17, 17, 51, 17, 129, 68, 68, 51, 17, 49, 51, 17, 49,
        67, 52, 51, 17, 49, 51, 49, 51, 51, 51, 51, 17, 49, 51, 49, 51, 51, 51,
        67, 17, 17, 51, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 51, 0, 0, 0, 0, 0, 0, 0, 102,
        51, 68, 68, 16, 17, 17, 17, 51, 51, 68, 68, 17, 17, 17, 17, 19, 67, 68,
        68, 17, 17, 17, 17, 17, 65, 68, 68, 17, 17, 49, 51, 17, 129, 68, 68,
        51, 17, 49, 51, 17, 51, 68, 68, 51, 17, 51, 51, 68, 51, 115, 52, 51,
        17, 49, 51, 68, 51, 51, 51, 51, 17, 49, 51, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 51, 51, 0, 0, 0, 0, 0, 0, 51, 67, 68, 4, 0, 0, 16,
        49, 51, 68, 68, 68, 19, 17, 17, 49, 68, 65, 68, 68, 19, 17, 49, 51, 68,
        65, 68, 68, 19, 17, 51, 51, 68, 53, 68, 68, 51, 17, 51, 51, 68, 52, 67,
        52, 51, 17, 51, 51, 68, 51, 51, 51, 51, 17, 49, 51, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 3, 0, 0, 0, 0, 0, 0, 0, 51, 3, 0, 0, 0,
        0, 0, 0, 68, 68, 68, 0, 0, 0, 0, 0, 68, 68, 68, 52, 51, 17, 17, 17, 68,
        68, 68, 20, 17, 17, 49, 51, 68, 84, 69, 52, 19, 17, 51, 51, 68, 52, 83,
        51, 51, 17, 51, 51, 68, 51, 51, 51, 51, 17, 51, 51, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 51, 0, 0, 0, 0,
        0, 0, 0, 153, 153, 0, 0, 0, 0, 0, 0, 68, 68, 68, 0, 0, 0, 0, 0, 68, 68,
        68, 49, 17, 17, 49, 51, 68, 84, 85, 17, 17, 17, 49, 51, 68, 53, 83, 51,
        19, 17, 51, 51, 19, 53, 51, 51, 51, 17, 51, 51, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 9, 0, 0, 0, 0, 0, 0, 0,
        153, 0, 0, 0, 0, 0, 0, 0, 153, 153, 0, 0, 0, 0, 0, 0, 68, 68, 20, 0, 0,
        0, 0, 0, 68, 84, 21, 51, 0, 0, 0, 0, 68, 85, 53, 51, 51, 17, 51, 51,
        131, 88, 53, 51, 51, 17, 51, 51, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 9, 0, 0, 0, 0,
        0, 0, 0, 153, 9, 0, 0, 0, 0, 0, 0, 153, 153, 0, 0, 0, 0, 0, 0, 148,
        153, 9, 0, 0, 0, 0, 0, 51, 24, 17, 3, 0, 0, 0, 0, 131, 24, 49, 51, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 153, 0, 0, 0,
        0, 0, 0, 0, 153, 9, 0, 0, 0, 0, 0, 0, 153, 153, 0, 0, 0, 0, 0, 0, 51,
        153, 9, 0, 0, 0, 0, 0, 131, 24, 17, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 9, 0, 0, 0, 0, 0, 0, 0, 153, 0, 0, 0, 0, 0,
        0, 0, 153, 9, 0, 0, 0, 0, 0, 0, 153, 153, 0, 0, 0, 0, 0, 0, 152, 153,
        9, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 9, 0,
        0, 0, 0, 0, 0, 0, 153, 0, 0, 0, 0, 0, 0, 0, 169, 0, 0, 0, 0, 0, 0, 0,
        153, 9, 0, 0, 0, 0, 0, 0, 153, 153, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 9, 0, 0, 0, 0, 0,
        0, 0, 153, 0, 0, 0, 0, 0, 0, 0, 169, 0, 0, 0, 0, 0, 0, 0, 153, 9, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 9, 0, 0, 0, 0, 0, 0, 0, 10, 0, 0, 0, 0, 0, 0, 0, 153, 0, 0,
        0, 0, 0, 0, 0, 169, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 12, 0, 0, 0, 0, 0, 0, 0, 9,
        0, 0, 0, 0, 0, 0, 0, 153, 0, 0, 0, 0, 0, 0, 0, 169, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 9, 0, 0, 0, 0, 0, 0, 0, 10, 0, 0, 0, 0, 0,
        0, 0, 153, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 9, 0, 0, 0, 0,
        0, 0, 0, 10, 0, 0, 0, 0, 0, 0, 0, 153, 0, 0, 0, 0, 0, 0, 0, 14, 0, 0,
        0, 10, 0, 0, 8, 4, 0, 110, 97, 109, 101, 13, 0, 109, 105, 110, 101, 99,
        114, 97, 102, 116, 58, 97, 105, 114, 10, 6, 0, 115, 116, 97, 116, 101,
        115, 0, 3, 7, 0, 118, 101, 114, 115, 105, 111, 110, 8, 40, 17, 1, 0,
        10, 0, 0, 8, 4, 0, 110, 97, 109, 101, 15, 0, 109, 105, 110, 101, 99,
        114, 97, 102, 116, 58, 115, 116, 111, 110, 101, 10, 6, 0, 115, 116, 97,
        116, 101, 115, 8, 10, 0, 115, 116, 111, 110, 101, 95, 116, 121, 112,
        101, 5, 0, 115, 116, 111, 110, 101, 0, 3, 7, 0, 118, 101, 114, 115,
        105, 111, 110, 8, 40, 17, 1, 0, 10, 0, 0, 8, 4, 0, 110, 97, 109, 101,
        22, 0, 109, 105, 110, 101, 99, 114, 97, 102, 116, 58, 114, 101, 100,
        115, 116, 111, 110, 101, 95, 111, 114, 101, 10, 6, 0, 115, 116, 97,
        116, 101, 115, 0, 3, 7, 0, 118, 101, 114, 115, 105, 111, 110, 8, 40,
        17, 1, 0, 10, 0, 0, 8, 4, 0, 110, 97, 109, 101, 16, 0, 109, 105, 110,
        101, 99, 114, 97, 102, 116, 58, 103, 114, 97, 118, 101, 108, 10, 6, 0,
        115, 116, 97, 116, 101, 115, 0, 3, 7, 0, 118, 101, 114, 115, 105, 111,
        110, 8, 40, 17, 1, 0, 10, 0, 0, 8, 4, 0, 110, 97, 109, 101, 19, 0, 109,
        105, 110, 101, 99, 114, 97, 102, 116, 58, 100, 101, 101, 112, 115, 108,
        97, 116, 101, 10, 6, 0, 115, 116, 97, 116, 101, 115, 8, 11, 0, 112,
        105, 108, 108, 97, 114, 95, 97, 120, 105, 115, 1, 0, 121, 0, 3, 7, 0,
        118, 101, 114, 115, 105, 111, 110, 8, 40, 17, 1, 0, 10, 0, 0, 8, 4, 0,
        110, 97, 109, 101, 15, 0, 109, 105, 110, 101, 99, 114, 97, 102, 116,
        58, 115, 116, 111, 110, 101, 10, 6, 0, 115, 116, 97, 116, 101, 115, 8,
        10, 0, 115, 116, 111, 110, 101, 95, 116, 121, 112, 101, 7, 0, 100, 105,
        111, 114, 105, 116, 101, 0, 3, 7, 0, 118, 101, 114, 115, 105, 111, 110,
        8, 40, 17, 1, 0, 10, 0, 0, 8, 4, 0, 110, 97, 109, 101, 20, 0, 109, 105,
        110, 101, 99, 114, 97, 102, 116, 58, 99, 111, 112, 112, 101, 114, 95,
        111, 114, 101, 10, 6, 0, 115, 116, 97, 116, 101, 115, 0, 3, 7, 0, 118,
        101, 114, 115, 105, 111, 110, 8, 40, 17, 1, 0, 10, 0, 0, 8, 4, 0, 110,
        97, 109, 101, 32, 0, 109, 105, 110, 101, 99, 114, 97, 102, 116, 58,
        100, 101, 101, 112, 115, 108, 97, 116, 101, 95, 114, 101, 100, 115,
        116, 111, 110, 101, 95, 111, 114, 101, 10, 6, 0, 115, 116, 97, 116,
        101, 115, 0, 3, 7, 0, 118, 101, 114, 115, 105, 111, 110, 8, 40, 17, 1,
        0, 10, 0, 0, 8, 4, 0, 110, 97, 109, 101, 14, 0, 109, 105, 110, 101, 99,
        114, 97, 102, 116, 58, 116, 117, 102, 102, 10, 6, 0, 115, 116, 97, 116,
        101, 115, 0, 3, 7, 0, 118, 101, 114, 115, 105, 111, 110, 8, 40, 17, 1,
        0, 10, 0, 0, 8, 4, 0, 110, 97, 109, 101, 14, 0, 109, 105, 110, 101, 99,
        114, 97, 102, 116, 58, 99, 108, 97, 121, 10, 6, 0, 115, 116, 97, 116,
        101, 115, 0, 3, 7, 0, 118, 101, 114, 115, 105, 111, 110, 8, 40, 17, 1,
        0, 10, 0, 0, 8, 4, 0, 110, 97, 109, 101, 15, 0, 109, 105, 110, 101, 99,
        114, 97, 102, 116, 58, 119, 97, 116, 101, 114, 10, 6, 0, 115, 116, 97,
        116, 101, 115, 3, 12, 0, 108, 105, 113, 117, 105, 100, 95, 100, 101,
        112, 116, 104, 0, 0, 0, 0, 0, 3, 7, 0, 118, 101, 114, 115, 105, 111,
        110, 8, 40, 17, 1, 0, 10, 0, 0, 8, 4, 0, 110, 97, 109, 101, 30, 0, 109,
        105, 110, 101, 99, 114, 97, 102, 116, 58, 115, 109, 97, 108, 108, 95,
        100, 114, 105, 112, 108, 101, 97, 102, 95, 98, 108, 111, 99, 107, 10,
        6, 0, 115, 116, 97, 116, 101, 115, 3, 9, 0, 100, 105, 114, 101, 99,
        116, 105, 111, 110, 2, 0, 0, 0, 1, 15, 0, 117, 112, 112, 101, 114, 95,
        98, 108, 111, 99, 107, 95, 98, 105, 116, 0, 0, 3, 7, 0, 118, 101, 114,
        115, 105, 111, 110, 8, 40, 17, 1, 0, 10, 0, 0, 8, 4, 0, 110, 97, 109,
        101, 18, 0, 109, 105, 110, 101, 99, 114, 97, 102, 116, 58, 105, 114,
        111, 110, 95, 111, 114, 101, 10, 6, 0, 115, 116, 97, 116, 101, 115, 0,
        3, 7, 0, 118, 101, 114, 115, 105, 111, 110, 8, 40, 17, 1, 0, 10, 0, 0,
        8, 4, 0, 110, 97, 109, 101, 23, 0, 109, 105, 110, 101, 99, 114, 97,
        102, 116, 58, 102, 108, 111, 119, 105, 110, 103, 95, 119, 97, 116, 101,
        114, 10, 6, 0, 115, 116, 97, 116, 101, 115, 3, 12, 0, 108, 105, 113,
        117, 105, 100, 95, 100, 101, 112, 116, 104, 0, 0, 0, 0, 0, 3, 7, 0,
        118, 101, 114, 115, 105, 111, 110, 8, 40, 17, 1, 0, 0, 10, 0, 0, 8, 4,
        0, 110, 97, 109, 101, 13, 0, 109, 105, 110, 101, 99, 114, 97, 102, 116,
        58, 97, 105, 114, 10, 6, 0, 115, 116, 97, 116, 101, 115, 0, 3, 7, 0,
        118, 101, 114, 115, 105, 111, 110, 8, 40, 17, 1, 0
    )
    val <- read_subchunk_layers_value(as.raw(dat))
    expect_length(val, 2L)
    expect_length(val[[1]]$palette, 14L)
    expect_length(val[[2]]$palette, 1L)
})

test_that("get_subchunk_blocks_from_chunk returns all block data in a chunk", {
    keys <- get_keys(db)
    dat <- get_subchunk_blocks_from_chunk(db, 37, 6, 0)
    expect_equal(names(dat), grep("^chunk:37:6:0:47", keys, value = TRUE))
    for (elt in dat) {
        expect_type(elt, "character")
    }
})

test_that("get_subchunk_layers_from_chunk returns all block data in a chunk", {
    keys <- get_keys(db)
    dat <- get_subchunk_layers_from_chunk(db, 37, 6, 0)
    expect_equal(names(dat), grep("^chunk:37:6:0:47", keys, value = TRUE))
    for (elt in dat) {
        expect_type(elt, "list")
        for (layer in elt) {
            expect_named(layer, c("values", "palette"))
        }
    }
})

test_that("chunk_blocks() subsets blocks", {
    blocks <- array("minecraft:air", c(16, 16, 16))
    blocks[4, 4, 4] <- "minecraft:stone"
    blocks[5, 4, 4] <- "minecraft:dirt"
    chunk_origin(blocks) <- c(32, 0, -32)
    bool_index <- rep(FALSE, length = length(blocks))
    bool_index[820] <- TRUE
    mat_index <- matrix(c(35, 3, -29), 1, 3)

    expect_equal(chunk_blocks(blocks), blocks)

    expect_equal(chunk_blocks(blocks, 35, 3, -29), "minecraft:stone")
    expect_equal(chunk_blocks(blocks, 820), "minecraft:stone")
    expect_equal(chunk_blocks(blocks, bool_index), "minecraft:stone")
    expect_equal(chunk_blocks(blocks, mat_index), "minecraft:stone")

    expect_equal(chunk_blocks(blocks, 35, 3, -29, drop = FALSE),
                 array("minecraft:stone", c(1, 1, 1)))

    expect_equal(chunk_blocks(blocks, 35:37, 3, -29),
                 c("minecraft:stone", "minecraft:dirt", "minecraft:air"))

})

test_that("chunk_blocks()<- replaces blocks", {
    blocks <- array("minecraft:air", c(16, 16, 16))
    chunk_origin(blocks) <- c(32, 0, -32)

    expected_blocks <- blocks
    expected_blocks[] <- "minecraft:stone"
    chunk_blocks(blocks) <- "minecraft:stone"
    expect_equal(blocks, expected_blocks)

    chunk_blocks(blocks, 32, 0, -32) <- "minecraft:air"
    expected_blocks[1, 1, 1] <- "minecraft:air"
    expect_equal(blocks, expected_blocks)

    chunk_blocks(blocks, 32, 0, -32 + 0:15) <- "minecraft:air"
    expected_blocks[1, 1, 1:16] <- "minecraft:air"
    expect_equal(blocks, expected_blocks)

    chunk_blocks(blocks, 32, TRUE, ) <- "minecraft:dirt"
    expected_blocks[1, TRUE, ] <- "minecraft:dirt"
    expect_equal(blocks, expected_blocks)

    mat_index <- matrix(c(35, 3, -29), 1, 3)
    chunk_blocks(blocks, mat_index) <- "miencraft:smooth_stone"
    expected_blocks[4, 4, 4] <- "miencraft:smooth_stone"
    expect_equal(blocks, expected_blocks)
})

# clean up
close(db)
fs::dir_delete(dbpath)
