dbpath <- rbedrock_example_world("example2.mcworld")
db <- bedrockdb(dbpath)

test_that("blocks_str() and block_nbt() are inverses.", {
  # Note: there are two layers in this subchunk.
  val <- get_subchunk_blocks_value("chunk:0:0:0:47:6", db = db)

  block_string <- blocks_str(val[[1]]$palette)
  expect_vector(block_string, character(), 16L)

  expect_true(all(grepl("minecraft:\\w+(@\\w+=\\w+)*", block_string)))

  block_pal <- blocks_nbt(block_string)
  block_string2 <- blocks_str(block_pal)
  expect_equal(block_string2, block_string)
})

# clean up
close(db)
unlink(dbpath, recursive = TRUE)
