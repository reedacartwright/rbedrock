dbpath <- rbedrock_example_world("example1.mcworld")
db <- bedrockdb(dbpath)

# @31:2:0:47-0

test_that("SubchunkBlocks is chunk tag 47.", {
    expect_equal(chunk_tag_int("SubchunkBlocks"), 47L)
    expect_equal(chunk_tag_str(47L), "SubchunkBlocks")
})

test_that("write_subchunk_layers_value() encodes subchunk data.", {
    raw1_orig <- get_value(db, "@31:2:0:47-0")
    layer1 <- read_subchunk_layers_value(raw1_orig, layer = NULL, simplify = FALSE)
    raw1_test <- write_subchunk_layers_value(layer1)

    expect_equal(raw1_test, raw1_orig)
})

test_that(".block_nbt() and .block_string() are inverses.", {
    dat <- get_subchunk_layers_value(db, "@31:2:0:47-4")
    pal_orig <- block_palette(dat)
    text <- purrr::map_chr(pal_orig, .block_string)
    pal_test <- purrr::map(text, .block_nbt)

    expect_equal(pal_orig, pal_test)
})

test_that("put_subchunk_blocks_value() writes subchunk data.", {
    val <- array("minecraft:air", c(16,16,16))
    val[,1,] <- "minecraft:bedrock@infiniburn_bit=false"
    val[,2,] <- "minecraft:stone@stone_type=stone"
    val[,3,] <- "minecraft:iron_ore"
    val[,4,] <- "minecraft:snow_layer@height=5@covered_bit=true"

    put_subchunk_blocks_value(db, 0, 0, 0, 0, val)
    dat <- get_subchunk_blocks_value(db, 0, 0, 0, 0)
    expect_equal(dat, val)
})

test_that("put_subchunk_blocks_values() writes subchunk data.", {
    val <- array("minecraft:air", c(16,16,16))
    val[,1,] <- "minecraft:bedrock@infiniburn_bit=false"
    val[,2,] <- "minecraft:stone@stone_type=granite"
    val[,3,] <- "minecraft:iron_ore"
    val[,4,] <- "minecraft:snow_layer@height=5@covered_bit=true"

    put_subchunk_blocks_values(db, 0, 0, 0, 1:4, list(val))
    dat <- get_subchunk_blocks_data(db, 0, 0, 0, 1:4)

    val <- rlang::set_names(rep(list(val), 4),
        stringr::str_glue("@0:0:0:47-{1:4}"))

    expect_equal(dat, val)
})

test_that("put_subchunk_blocks_data() writes subchunk data.", {
    val <- array("minecraft:air", c(16,16,16))
    val[,1,] <- "minecraft:bedrock@infiniburn_bit=false"
    val[,2,] <- "minecraft:stone@stone_type=andesite"
    val[,3,] <- "minecraft:iron_ore"
    val[,4,] <- "minecraft:snow_layer@height=5@covered_bit=true"

    val <- rlang::set_names(rep(list(val), 4),
        stringr::str_glue("@1:1:0:47-{1:4}"))

    put_subchunk_blocks_data(db, val)
    dat <- get_subchunk_blocks_data(db, names(val))

    expect_equal(dat, val)
})

test_that("put_chunk_blocks_value() writes chunk data.",{
    val <- array("minecraft:air", c(16,32,16))
    val[,21,] <- "minecraft:bedrock@infiniburn_bit=false"
    val[,22,] <- "minecraft:stone@stone_type=andesite"
    val[,23,] <- "minecraft:iron_ore"
    val[,24,] <- "minecraft:snow_layer@height=5@covered_bit=true"

    put_chunk_blocks_value(db, "@10:12:0:47", value=val)
    dat <- get_chunk_blocks_value(db, "@10:12:0:47")

    expect_equal(dat, val)

    dat <- get_keys(db, starts_with="@10:12:0:47")
    expect_equal(dat, "@10:12:0:47-1")
})

test_that("put_chunk_blocks_value() overwrites chunk data.", {
    dat <- get_chunk_blocks_value(db, 31, 4, 0)
    put_chunk_blocks_value(db, 31, 4, 0, dat[,1:20,])

    val <- array("minecraft:air", c(16,32,16))
    val[,1:20,] <- dat[,1:20,]
    dat <- get_chunk_blocks_value(db, 31, 4, 0)

    expect_equal(dat, val)

    dat <- get_keys(db, starts_with="@31:4:0:47")
    expect_equal(dat, c("@31:4:0:47-0","@31:4:0:47-1"))
})

test_that("put_chunk_blocks_values() writes chunk data.",{
    val <- array("minecraft:air", c(16,32,16))
    val[,21,] <- "minecraft:bedrock@infiniburn_bit=false"
    val[,22,] <- "minecraft:stone@stone_type=andesite"
    val[,23,] <- "minecraft:iron_ore"
    val[,24,] <- "minecraft:snow_layer@height=5@covered_bit=true"

    put_chunk_blocks_values(db, 11:12, 12, 0, value=list(val))
    dat <- get_chunk_blocks_values(db, 11:12, 12, 0)

    expect_equal(dat[[1]], val)
    expect_equal(dat[[2]], val)

    dat <- get_keys(db, starts_with="@11:12:0:47")
    expect_equal(dat, "@11:12:0:47-1")
    dat <- get_keys(db, starts_with="@12:12:0:47")
    expect_equal(dat, "@12:12:0:47-1")
})

test_that("put_chunk_blocks_data() writes chunk data.",{
    val <- list()
    val[["@13:12:0:47"]] <- array("minecraft:air", c(16,32,16))
    val[["@14:12:0:47"]] <- array("minecraft:air", c(16,32,16))

    val[["@13:12:0:47"]][,21,] <- "minecraft:bedrock@infiniburn_bit=false"
    val[["@14:12:0:47"]][,1,] <- "minecraft:bedrock@infiniburn_bit=false"

    put_chunk_blocks_data(db, val)

    val[["@14:12:0:47"]] <- val[["@14:12:0:47"]][,1:16,]
    
    dat <- get_chunk_blocks_values(db, c("@13:12:0:47","@14:12:0:47"))

    expect_equal(dat, val)

    dat <- get_keys(db, starts_with="@13:12:0:47")
    expect_equal(dat, "@13:12:0:47-1")
    dat <- get_keys(db, starts_with="@14:12:0:47")
    expect_equal(dat, "@14:12:0:47-0")
})

# clean up
close(db)
fs::dir_delete(dbpath)
