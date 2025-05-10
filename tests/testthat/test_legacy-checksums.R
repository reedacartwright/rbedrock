dbpath <- rbedrock_example_world("example1.mcworld")
db <- bedrockdb(dbpath)

test_that("Checksums is chunk tag 59", {
    expect_equal(chunk_tag_int("Checksums"), 59L)
    expect_equal(chunk_tag_str(59L), "Checksums")
})

test_that("get_checksums_data() returns specific Checksums data", {
    keys <- c("chunk:36:16:0:59", "fake_data", "chunk:37:15:0:59")
    dat <- get_checksums_data(keys, db = db)
    expect_vector(dat, list(), 3L)
    expect_named(dat, keys)
    expect_named(dat[[1]])
    expect_null(dat[[2]])
    expect_named(dat[[3]])
})

test_that("get_checksums_value() returns a single record", {
    dat <- get_checksums_value("chunk:36:16:0:59", db = db)
    expect_vector(dat, character(), 7L)
    expect_named(dat)

    expect_error(get_checksums_value(db, c(
        "chunk:36:16:0:59",
        "chunk:37:15:0:59"
    )))
})

test_that("update_checksums_data() works", {
    keys <- c("chunk:36:16:0:59", "chunk:37:15:0:59")
    original_dat <- get_checksums_data(keys, db = db)
    delete_values(keys, db = db)
    expect_equal(has_values(keys, db = db), setNames(c(FALSE, FALSE), keys))

    update_checksums_data(keys, db = db)
    dat <- get_checksums_data(keys, db = db)
    expect_named(dat, keys)
    for (k in keys) {
        expect_mapequal(dat[[!!k]], original_dat[[!!k]])
    }
})

test_that("update_checksums_data() handles missing data", {
    dat <- get_checksums_value(-10, -10, 0, db = db)
    expect_equal(dat, NULL)
    update_checksums_data(-10, -10, 0, db = db)
    dat <- get_checksums_value(-10, -10, 0, db = db)
    expect_equal(dat, character())
})

test_that("update_checksums_data() throws an error if filtering", {
    expect_error(update_checksums_data(db, c(
        "chunk:36:16:0:47:0",
        "chunk:37:15:0:59"
    )))
})

helper_checksum_impl <- function(x) {
    digest::digest(x, algo = "xxhash64", serialize = FALSE, raw = TRUE)
}

helper_checksum_as_raw <- function(x) {
    y <- strsplit(x, character(0L))[[1]]
    y <- paste0(y[c(TRUE, FALSE)], y[c(FALSE, TRUE)])
    y <- strtoi(y, base = 16L)
    as.raw(rev(y))
}

test_that("read_checksums_value decodes correctly", {
    correct_val <- c(
        "chunk:0:0:0:45" = helper_checksum_impl(as.raw(0:10)),
        "chunk:0:0:0:47:1" = helper_checksum_impl(as.raw(0:255))
    )
    raw_val <- as_raw(
        2, 0, 0, 0,
        45, 0, 0, helper_checksum_as_raw(correct_val[[1]]),
        47, 0, 1, helper_checksum_as_raw(correct_val[[2]])
    )
    res <- read_checksums_value(raw_val, "chunk:0:0:0:59")
    expect_equal(res, correct_val)
})

test_that("write_checksums_value encodes correctly", {
    correct_val <- c(
        "chunk:0:0:0:45" = helper_checksum_impl(as.raw(0:10)),
        "chunk:0:0:0:47:1" = helper_checksum_impl(as.raw(0:255))
    )
    correct_raw <- as_raw(
        2, 0, 0, 0,
        45, 0, 0, helper_checksum_as_raw(correct_val[[1]]),
        47, 0, 1, helper_checksum_as_raw(correct_val[[2]])
    )
    res <- write_checksums_value(correct_val)
    expect_equal(res, correct_raw)
})

# 36:16:0

# clean up
close(db)
fs::dir_delete(dbpath)
