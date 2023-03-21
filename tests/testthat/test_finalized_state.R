dbpath <- rbedrock_example_world("example1.mcworld")
db <- bedrockdb(dbpath)

as_raw <- function(...) {
    as.raw(c(...))
}

test_that("FinalizedState is chunk tag 54", {
    expect_equal(chunk_tag_int("FinalizedState"), 54L)
    expect_equal(chunk_tag_str(54L), "FinalizedState")
})

test_that("get_finalized_state_data() returns specific Finalization data", {
    keys <- c("chunk:36:16:0:54", "chunk:37:15:0:54")
    dat <- get_finalized_state_data(keys, db = db)
    expect_equal(dat, !!setNames(c(2L, 2L), keys))

    expect_error(get_finalized_state_data(db, c("chunk:36:16:0:54",
        "chunk:fake", "chunk:37:15:0:54")))
})

test_that("get_finalized_state_value() returns a single value", {
    dat <- get_finalized_state_value("chunk:36:16:0:54", db = db)
    expect_equal(dat, 2L)
    expect_error(get_finalized_state_value(db, c(
        "chunk:36:16:0:54",
        "chunk:37:15:0:54"
    )))
})

test_that("put_finalized_state_data() updates database", {
    keys <- c("chunk:0:0:0:54", "chunk:100:100:1:54")
    dat <- setNames(c(1L, 2L), keys)

    put_finalized_state_data(dat, db = db)

    res <- get_finalized_state_data(keys, db = db)
    expect_equal(res, !!dat)

    keys <- c("chunk:0:1:0:54", "chunk:10:2:1:54")
    dat <- c(1L, 2L)

    put_finalized_state_data(dat, keys, db = db)

    res <- get_finalized_state_data(keys, db = db)
    expect_equal(res, !!setNames(dat, keys))

    put_finalized_state_data(c(2L, 1L), x = 1:2, z = 1:2, dimension = 1,
        db = db)

    res <- get_finalized_state_data(1:2, 1:2, 1, db = db)
    expect_equal(res, !!setNames(c(2L, 1L), c("chunk:1:1:1:54",
        "chunk:2:2:1:54")))

    keys <- c("chunk:0:2:0:54", "chunk:10:3:1:54")
    put_finalized_state_data(2L, keys, db = db)

    res <- get_finalized_state_data(keys, db = db)
    expect_equal(res, !!setNames(c(2L, 2L), keys))
})

test_that("put_finalized_state_data() stops on bad keys", {
    keys <- c("chunk:0:0:0:50", "plain:fake_name")
    dat <- setNames(c(1L, 2L), keys)
    expect_error(put_finalized_state_data(dat, db = db))
})

test_that("put_finalized_state_data() stops on bad input", {
    expect_error(put_finalized_state_data(2L, c("chunk:0:0:0:55",
        "plain:fake_data"), db = db))
    expect_error(put_finalized_state_data(c(2L, 2L, 2L), c("chunk:0:0:0:54",
        "chunk:0:0:1:54"), db = db))
})

test_that("put_finalized_state_value() updates database", {
    put_finalized_state_value(2L, x = 100, z = 20, dimension = 2, db = db)
    res <- get_finalized_state_value(x = 100, z = 20, dimension = 2, db = db)
    expect_equal(res, 2L)

    expect_error(put_finalized_state_value(2L, c("chunk:0:0:0:54",
        "chunk:0:0:1:54"), db = db))
})

test_that("write_finalized_state_value requires() integerish values", {
    expect_equal(write_finalized_state_value(2L), as_raw(2, 0, 0, 0))
    expect_equal(write_finalized_state_value(2.0), as_raw(2, 0, 0, 0))

    expect_error(write_finalized_state_value("2"))
    expect_error(write_finalized_state_value(2.1))
    expect_error(write_finalized_state_value(c(2L, 3L)))
})

# clean up
close(db)
fs::dir_delete(dbpath)
