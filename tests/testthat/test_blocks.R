dbpath <- rbedrock_example_world("example2.mcworld")
db <- bedrockdb(dbpath)

test_that("get_blocks_value() loads one value", {
  dat <- get_blocks_value(0, 0, 0, db = db)

  expect_type(dat, "character")
  expect_equal(dim(dat), c(16, 384, 16))
  expect_equal(chunk_origin(dat), c(0, -64, 0))
  expect_true(all(dat[, 1, ] == "minecraft:bedrock@infiniburn_bit=0"))
  expect_true(all(dat[, 188:384, ] == "minecraft:air"))
  expect_equal(grep("observer", dat), c(45814, 45815))
  expect_equal(grep(";", dat), c(27285, 27301, 27317))
})

test_that("get_blocks_data() loads multiple value", {
  dat <- get_blocks_data(0, c(0, 4), 0, db = db)

  expect_type(dat, "list")
  expect_length(dat, 2L)
  expect_named(dat, c("chunk:0:0:0:47", "chunk:0:4:0:47"))
  expect_type(dat[[1]], "character")
  expect_type(dat[[2]], "character")
  expect_equal(dim(dat[[1]]), c(16, 384, 16))
  expect_equal(dim(dat[[2]]), c(16, 384, 16))
  expect_equal(chunk_origin(dat[[1]]), c(0, -64, 0))
  expect_equal(chunk_origin(dat[[2]]), c(0, -64, 64))
})

test_that("put_blocks_value() saves one value", {
  expected_val <- array("minecraft:air", c(16L, 384L, 16L))
  expected_val[10, , ] <- "minecraft:white_wool"
  expected_val[, , 11] <- "minecraft:red_wool"
  expected_val[, 128, ] <- "minecraft:blue_wool"

  # Overworld
  put_blocks_value(expected_val, "chunk:100:0:0:47", db = db)
  chunk_origin(expected_val) <- c(1600, -64, 0)
  val <- get_blocks_value("chunk:100:0:0:47", db = db)
  expect_equal(val, expected_val)

  # Nether
  chunk_origin(expected_val) <- c(1600, -64, 0)
  put_blocks_value(expected_val, 100, 0, 1, db = db)
  expected_val_nether <- array("minecraft:air", c(16, 384, 16))
  expected_val_nether[, 1:128, ] <- expected_val[, 65:192, ]
  chunk_origin(expected_val_nether) <- c(1600, 0, 0)
  val <- get_blocks_value("chunk:100:0:1:47", db = db)
  expect_equal(val, expected_val_nether)

  # The End
  chunk_origin(expected_val) <- c(1600, -64, 0)
  put_blocks_value(expected_val, 100, 0, 2, db = db)
  expected_val_end <- array("minecraft:air", c(16, 384, 16))
  expected_val_end[, 1:256, ] <- expected_val[, 65:320, ]
  chunk_origin(expected_val_end) <- c(1600, 0, 0)
  val <- get_blocks_value("chunk:100:0:2:47", db = db)
  expect_equal(val, expected_val_end)

  # Air subchunks are deleted
  expected_val[, 128:384, ] <- "minecraft:air"
  put_blocks_value(expected_val, 100, 0, 0, db = db)
  keys <- get_keys(key_prefix("chunk:100:0:0:47"), db = db)
  expect_equal(keys, sprintf("chunk:100:0:0:47:%d", c(0:3, -4:-1)))

  # Saving an empty chunk deletes everything
  expected_val <- array("minecraft:air", c(16L, 384L, 16L))
  put_blocks_value(expected_val, 100, 0, 0, db = db)
  keys <- get_keys(key_prefix("chunk:100:0:0:47"), db = db)
  expect_equal(keys, character(0L))
})

test_that("put_blocks_data() saves multiple values", {
  red <- array("minecraft:red_wool", c(16L, 384L, 16L))
  green <- array("minecraft:green_wool", c(16L, 64L, 16L))
  blue <- array("minecraft:blue_wool", c(16L, 16L, 16L))

  chunk_origin(green) <- c(0, -32, 0)
  chunk_origin(blue) <- c(0, 32, 0)

  rgb_dat <- list(red, green, blue)
  names(rgb_dat) <- c(
    "chunk:200:0:1:47",
    "chunk:200:0:0:47",
    "chunk:200:0:2:47"
  )

  put_blocks_data(rgb_dat, db = db)

  red_ <- array("minecraft:air", c(16L, 384L, 16L))
  green_ <- array("minecraft:air", c(16L, 384L, 16L))
  blue_ <- array("minecraft:air", c(16L, 384L, 16L))
  red_[, 1:128, ] <- "minecraft:red_wool"
  green_[, 33:96, ] <- "minecraft:green_wool"
  blue_[, 33:48, ] <- "minecraft:blue_wool"

  chunk_origin(red_) <- c(3200, 0, 0)
  chunk_origin(green_) <- c(3200, -64, 0)
  chunk_origin(blue_) <- c(3200, 0, 0)

  expected_val <- list(red_, green_, blue_)
  names(expected_val) <- names(rgb_dat)

  val <- get_blocks_data(names(rgb_dat), db = db)

  expect_equal(val, expected_val)
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

  expect_equal(
    chunk_blocks(blocks, 35, 3, -29, drop = FALSE),
    array("minecraft:stone", c(1, 1, 1))
  )

  expect_equal(
    chunk_blocks(blocks, 35:37, 3, -29),
    c("minecraft:stone", "minecraft:dirt", "minecraft:air")
  )
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
unlink(dbpath, recursive = TRUE)
