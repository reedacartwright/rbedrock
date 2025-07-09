dbpath <- rbedrock_example_world("example1.mcworld")
db <- bedrockdb(dbpath)

test_that("Entity is chunk tag 50", {
  expect_equal(chunk_tag_int("Entity"), 50L)
  expect_equal(chunk_tag_str(50L), "Entity")
})

test_that("get_entity_data() returns specific Entity data", {
  keys <- c("chunk:32:3:0:50", "plain:fake_data", "chunk:34:4:0:50")
  dat <- get_entity_data(keys, db = db)
  expect_vector(dat, list(), 3L)
  expect_named(dat, keys)

  expect_true(all(sapply(dat[[1]], is_nbt_value)))
  expect_null(dat[[2]])
  expect_true(all(sapply(dat[[3]], is_nbt_value)))
})

test_that("put_entity_data() writes Entity data", {
  dat <- get_entity_data("chunk:34:4:0:50", db = db)
  dat2 <- dat
  names(dat2) <- "chunk:10:0:0:50"
  put_entity_data(dat2, db = db)
  dat <- get_entity_data("chunk:10:0:0:50", db = db)
  expect_equal(dat, dat2)
})

test_that("put_entity_data() returns false for non Entity keys", {
  dat <- get_entity_data("chunk:34:4:0:50", db = db)
  dat2 <- dat
  names(dat2) <- "chunk:10:0:0:44"
  expect_equal(put_entity_data(dat2, db = db), FALSE)
})

# clean up
close(db)
unlink(dbpath, recursive = TRUE)
