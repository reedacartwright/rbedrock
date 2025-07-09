dbpath <- rbedrock_example_world("example2.mcworld")
db <- bedrockdb(dbpath)

test_that("SubChunkBlocks is chunk tag 47.", {
  expect_equal(chunk_tag_int("SubChunkBlocks"), 47L)
  expect_equal(chunk_tag_str(47L), "SubChunkBlocks")
})

test_that("Reading and Writing SubChunksBlocks Data", {
  raw_expected <- get_value("chunk:0:0:0:47:-4", db = db)
  val <- read_subchunk_blocks_value(raw_expected)

  # writing is inverse to reading
  raw_val <- write_subchunk_blocks_value(val, -4)
  expect_equal(raw_val, raw_expected)

  # Note: there is one layer in this subchunk.
  expect_type(val, "list")
  expect_length(val, 1L)
  expect_equal(attributes(val), list(subchunk_position = -4))
  expect_named(val[[1]], c("values", "palette"))
  expect_length(val[[1]]$palette, 16)
  expect_s3_class(val[[1]]$palette, "rbedrock_nbt_list_of")
  ptype <- array(integer(), dim = c(0, 16, 16))
  expect_vector(val[[1]]$values, ptype, 16L)

  # Note: This is a bottom chunk
  expect_equal(unnbt(val[[1]]$palette[[1]]$name), "minecraft:bedrock")
  expect_true(all(val[[1]]$values[, 1, ] == 1L))
})

test_that("get_subchunk_blocks_value() loads one value", {
  # Note: there are two layers in this subchunk.
  val <- get_subchunk_blocks_value("chunk:0:0:0:47:6", db = db)
  expect_type(val, "list")
  expect_length(val, 2L)
  expect_equal(attributes(val), list(subchunk_position = 6))
  expect_named(val[[1]], c("values", "palette"))
  expect_named(val[[2]], c("values", "palette"))
  expect_length(val[[1]]$palette, 16)
  expect_length(val[[2]]$palette, 2)
  expect_s3_class(val[[1]]$palette, "rbedrock_nbt_list_of")
  expect_s3_class(val[[2]]$palette, "rbedrock_nbt_list_of")
  ptype <- array(integer(), dim = c(0, 16, 16))
  expect_vector(val[[1]]$values, ptype, 16L)
  expect_vector(val[[2]]$values, ptype, 16L)

  # works with individual components
  val <- get_subchunk_blocks_value(0, 0, 0, 6, db = db)
  expect_type(val, "list")
  expect_length(val, 2L)
  expect_equal(attributes(val), list(subchunk_position = 6))
})

test_that("get_subchunk_blocks_data() loads multiple value", {
  keys <- c("chunk:0:0:0:47:-4", "chunk:0:0:0:47:6")
  dat <- get_subchunk_blocks_data(keys, db = db)
  expect_type(dat, "list")
  expect_named(dat, keys)
  expect_length(dat[[1]], 1L)
  expect_length(dat[[2]], 2L)
  expect_equal(attributes(dat[[1]]), list(subchunk_position = -4))
  expect_equal(attributes(dat[[2]]), list(subchunk_position = 6))

  dat <- get_subchunk_blocks_data(0, 0, 0, c(6, -4), db = db)
  expect_named(dat, keys[2:1])
  expect_length(dat[[1]], 2L)
  expect_length(dat[[2]], 1L)
})

test_that("put_subchunk_blocks_value() saves one value", {
  val <- array("minecraft:air", c(16, 16, 16))
  val[, 1, ] <- "minecraft:bedrock@infiniburn_bit=0"
  val[, 2, ] <- "minecraft:stone"
  val[, 3, ] <- "minecraft:iron_ore"
  val[, 4, ] <- "minecraft:snow_layer@height=5@covered_bit=1"
  attr(val, "subchunk_position") <- 0L

  val_ <- subchunk_blocks_array_as_value(val)

  put_subchunk_blocks_value(val_, 0, 0, 1, 0, db = db, version = 8L)
  dat <- get_subchunk_blocks_value(0, 0, 1, 0, db = db)
  expect_equal(subchunk_blocks_value_as_array(dat), val)

  put_subchunk_blocks_value(val_, 0, 0, 1, 0, db = db)
  dat <- get_subchunk_blocks_value(0, 0, 1, 0, db = db)
  expect_equal(subchunk_blocks_value_as_array(dat), val)
})

test_that("put_subchunk_blocks_data() saves multiple values.", {
  val <- array("minecraft:air", c(16, 16, 16))
  val[, 1, ] <- "minecraft:bedrock@infiniburn_bit=0"
  val[, 2, ] <- "minecraft:andesite"
  val[, 3, ] <- "minecraft:iron_ore"
  val[, 4, ] <- "minecraft:snow_layer@height=5@covered_bit=1"
  attr(val, "subchunk_position") <- NA_integer_

  val <- rep(list(val), 4)
  names(val) <- paste0("chunk:0:0:2:47:", 1:4)

  val_ <- lapply(val, subchunk_blocks_array_as_value)

  put_subchunk_blocks_data(val_, db = db)
  dat <- get_subchunk_blocks_data(names(val), db = db)

  dat <- lapply(dat, subchunk_blocks_value_as_array)

  for (i in seq_along(val)) {
    attr(val[[i]], "subchunk_position") <- i
  }
  expect_equal(dat, val)

  put_subchunk_blocks_data(val_, 0, 1, 2, 1:4, db = db)
  dat <- get_subchunk_blocks_data(0, 1, 2, 1:4, db = db)
  dat <- lapply(dat, subchunk_blocks_value_as_array)

  names(val) <- paste0("chunk:0:1:2:47:", 1:4)

  expect_equal(dat, val)
})

# clean up
close(db)
unlink(dbpath, recursive = TRUE)
