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

# clean up
close(db)
unlink(dirname(dbpath), recursive=TRUE, expand = FALSE)
