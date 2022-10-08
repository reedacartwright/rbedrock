dbpath <- rbedrock_example_world("example1.mcworld")
db <- bedrockdb(dbpath)

as_raw <- function(...) {
    as.raw(c(...))
}

test_that("Checksums is chunk tag 59", {
    expect_equal(chunk_tag_int("Checksums"), 59L)
    expect_equal(chunk_tag_str(59L), "Checksums")
})

test_that("get_checksums_data returns specific Checksums data", {
    keys <- c("chunk:36:16:0:59", "chunk:37:15:0:59")
    dat <- get_checksums_data(keys, db = db)
    expect_vector(dat, list(), 2L)
    expect_named(dat, keys)
    for(i in seq_along(dat)) {
        expect_named(dat[[!!i]])
    }

    expect_error(get_checksums_value(c("chunk:36:16:0:59", "plain:fake", "chunk:37:15:0:59"), db=db))
})

test_that("get_checksums_value returns a single record", {
    dat <- get_checksums_value("chunk:36:16:0:59", db = db)
    expect_vector(dat, character(), 7L)
    expect_named(dat)

    expect_error(get_checksums_value(db, c("chunk:36:16:0:59","chunk:37:15:0:59")))
})

test_that("update_checksums_data works", {
    keys <- c("chunk:36:16:0:59", "chunk:37:15:0:59")
    original_dat <- get_checksums_data(keys, db = db)
    delete_values(keys, db = db)
    expect_equal(has_values(keys, db = db), setNames(c(FALSE,FALSE),keys))

    update_checksums_data(keys, db = db)
    dat <- get_checksums_data(keys, db = db)
    expect_named(dat, keys)
    for(k in keys) {
        expect_mapequal(dat[[!!k]], original_dat[[!!k]])
    }
})

test_that("update_checksums_data() and get_checksums_value() handle missing data", {
    dat <- get_checksums_value(-10, -10, 0, db = db)
    expect_equal(dat, NULL)
    update_checksums_data(-10, -10, 0, db = db)
    dat <- get_checksums_value(-10, -10, 0, db = db)
    expect_equal(dat, character())
})

test_that("update_checksums_data throws an error if filtering", {
    expect_error(update_checksums_data(db,c("chunk:36:16:0:47:0", "chunk:37:15:0:59")))
})

helper_checksum_impl <- function(x) {
    digest::digest(x, algo="xxhash64", serialize=FALSE, raw=TRUE)
}

helper_checksum_as_raw <- function(x) {
    h <- strsplit(x, character(0L))[[1]]
    h <- paste(h[c(TRUE,FALSE)], h[c(FALSE,TRUE)], sep="")
    as.raw(as.hexmode(rev(h)))
}

test_that("read_checksums_value decodes correctly", {
    correct_val <- c(
        '45' = helper_checksum_impl(as.raw(0:10)),
        '47:1' = helper_checksum_impl(as.raw(0:255))
    )
    raw_val <- as_raw(2, 0, 0, 0,
        45, 0, 0, helper_checksum_as_raw(correct_val[[1]]),
        47, 0, 1, helper_checksum_as_raw(correct_val[[2]])
    )
    res <- read_checksums_value(raw_val)
    expect_equal(res, correct_val)
})

test_that("write_checksums_value encodes correctly",{
    correct_val <- c(
        '45' = helper_checksum_impl(as.raw(0:10)),
        '47:1' = helper_checksum_impl(as.raw(0:255))
    )
    correct_raw <- as_raw(2, 0, 0, 0,
        45, 0, 0, helper_checksum_as_raw(correct_val[[1]]),
        47, 0, 1, helper_checksum_as_raw(correct_val[[2]])
    )
    res <- write_checksums_value(correct_val)
    expect_equal(res, correct_raw)
})

#36:16:0

# clean up
close(db)
fs::dir_delete(dbpath)