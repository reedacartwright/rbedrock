dbpath <- rbedrock_example_world("example1.mcworld")
db <- bedrockdb(dbpath)

# @31:2:0:47-0

test_that("SubchunkBlocks is chunk tag 47", {
    expect_equal(chunk_tag_int("SubchunkBlocks"), 47L)
    expect_equal(chunk_tag_str(47L), "SubchunkBlocks")
})


test_that("write_subchunk_layers_value() encodes subchunk data", {
    raw1_orig <- get_value(db, "@31:2:0:47-0")
    layer1 <- read_subchunk_layers_value(raw1_orig, layer = NULL, simplify = FALSE)
    raw1_test <- write_subchunk_layers_value(layer1)

    expect_equal(raw1_test, raw1_orig)

    # pal <- list()
    # pal[[1]] <- nbt_compound(
    #     name = nbt_string("minecraft:air"),
    #     states = nbt_compound(),
    #     version =  nbt_int(17825808)
    # )
    # blocks1 <- list()
    # blocks1[[1]] <- array(1L, c(16,16,16))
    # attr(blocks1[[1]], 'palette') <- pal

})

# clean up
close(db)
unlink(dirname(dbpath), recursive=TRUE, expand = FALSE)
