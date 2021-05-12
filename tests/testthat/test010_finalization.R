dbpath <- rbedrock_example_world("example1.mcworld")
db <- bedrockdb(dbpath)

as_raw <- function(...) {
    as.raw(c(...))
}

test_that("Finalization is chunk tag 54", {
    expect_equal(chunk_tag_int("Finalization"), 54L)
    expect_equal(chunk_tag_str(54L), "Finalization")
})

test_that("get_finalization_data returns all Finalization data", {
    dat <- get_finalization_data(db, get_keys(db))
    expect_vector(dat, integer(), 105L)
    expect_named(dat)
    expect_true(all(grepl(":54$", names(dat))))
})

test_that("get_finalization_data returns specific Finalization data", {
    keys <- c("@36:16:0:54", "fake_data", "@37:15:0:54")
    dat <- get_finalization_data(db, keys)
    expect_equal(dat, !!setNames(c(2L,2L), keys[-2]))
})

test_that("get_finalization_value returns a single value", {
    dat <- get_finalization_value(db, "@36:16:0:54")
    expect_equal(dat, 2L)
    expect_error(get_finalization_value(db, c("@36:16:0:54","@37:15:0:54")))
})

test_that("put_finalization_data updates database", {
    keys <- c("@0:0:0:54", "@100:100:1:54")
    dat <- setNames(c(1L,2L), keys)

    put_finalization_data(db, dat)

    res <- get_finalization_data(db, keys)
    expect_equal(res, !!dat)
})

test_that("put_finalization_data stops on bad keys",{
    keys <- c("@0:0:0:50", "fake_name")
    dat <- setNames(c(1L,2L), keys)
    expect_error(put_finalization_data(db, dat))
})

test_that("put_finalization_values updates database", {
    keys <- c("@0:1:0:54", "@10:2:1:54")
    dat <- c(1L,2L)

    put_finalization_values(db, keys, values=dat)

    res <- get_finalization_data(db, keys)
    expect_equal(res, !!setNames(dat, keys))

    put_finalization_values(db, x=1:2, z=1:2, dimension=1, values=c(2L,1L))

    res <- get_finalization_data(db, 1:2, 1:2, 1)
    expect_equal(res, !!setNames(c(2L,1L), c("@1:1:1:54", "@2:2:1:54")))

    keys <- c("@0:2:0:54", "@10:3:1:54")
    put_finalization_values(db, keys, values=2L)

    res <- get_finalization_data(db, keys)
    expect_equal(res, !!setNames(c(2L,2L), keys))
})

test_that("put_finalization_values stops on bad input", {
    expect_error(put_finalization_values(db, c("@0:0:0:55","fake_data"), values=2L))
    expect_error(put_finalization_values(db, c("@0:0:0:54","@0:0:1:54"), values=c(2L,2L,2L)))
})

test_that("put_finalization_value updates database", {
    put_finalization_value(db, x=100, z=20, dimension=2, value=2L)
    res <- get_finalization_value(db, x=100, z=20, dimension=2)
    expect_equal(res, 2L)

    expect_error(put_finalization_value(db, c("@0:0:0:54","@0:0:1:54"), value=2L))
})

test_that("write_finalizaton_value requires integerish values", {
    expect_equal(write_finalization_value(2L), as_raw(2,0,0,0))
    expect_equal(write_finalization_value(2.0), as_raw(2,0,0,0))

    expect_error(write_finalization_value("2"))
    expect_error(write_finalization_value(2.1))
    expect_error(write_finalization_value(c(2L,3L)))
})

# clean up
close(db)
fs::dir_delete(dbpath)
