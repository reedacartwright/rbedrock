dbpath <- rbedrock_example_world("example1.mcworld")
db <- bedrockdb(dbpath)

test_that("FinalizedState is chunk tag 54", {
  expect_equal(chunk_tag_int("FinalizedState"), 54L)
  expect_equal(chunk_tag_str(54L), "FinalizedState")
})

test_that("get_finalized_state_data() returns specific Finalization data", {
  keys <- c("chunk:36:16:0:54", "plain:fake_data", "chunk:37:15:0:54")
  dat <- get_finalized_state_data(keys, db = db)
  expected_dat <- setNames(c(2L, NA_integer_, 2L), keys)
  expect_equal(dat, expected_dat)
})

test_that("get_finalized_state_value() returns a single value", {
  dat <- get_finalized_state_value("chunk:36:16:0:54", db = db)
  expect_equal(dat, 2L)
})

test_that("put_finalized_state_data() updates database", {
  keys <- c("chunk:0:0:0:54", "chunk:100:100:1:54")
  dat <- setNames(c(1L, 2L), keys)
  put_finalized_state_data(dat, db = db)

  res <- get_finalized_state_data(keys, db = db)
  expect_equal(res, dat)
})

test_that("put_finalized_state_value() updates database", {
  put_finalized_state_value(2, 100, 20, 2, db = db)
  res <- get_finalized_state_value(100, 20, 2, db = db)
  expect_equal(res, 2L)
})

# clean up
close(db)
unlink(dbpath, recursive = TRUE)
