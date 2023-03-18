dbpath <- rbedrock_example_world("example1.mcworld")
db <- bedrockdb(dbpath)

as_raw <- function(...) {
    as.raw(c(...))
}

test_that("put_legacy_biomes_value() accepts strings", {
    set.seed(102031)
    put_legacy_biomes_value(db, 32, 3, 0, "plains")
    dat <- get_legacy_biomes_value(db, 32, 3, 0)
    expect_equal(dat, matrix("plains", 16L, 16L))

    new_dat <- sample(c("plains", "desert", "ocean"), 256, replace = TRUE)
    put_legacy_biomes_value(db, 32, 3, 0, new_dat)
    dat <- get_legacy_biomes_value(db, 32, 3, 0)
    expect_equal(dat, matrix(new_dat, 16L, 16L))

    expect_error(put_legacy_biomes_value(db, 32, 3, 0, "plain:fake_biome"))
})

test_that("put_legacy_biomes_value() accepts integers", {
    set.seed(102032)
    put_legacy_biomes_value(db, 32, 3, 0, 1L)
    dat <- get_legacy_biomes_value(db, 32, 3, 0, return_names = FALSE)
    expect_equal(dat, matrix(1L, 16L, 16L))

    new_dat <- sample(c(1, 10, 20), 256, replace = TRUE)
    put_legacy_biomes_value(db, 32, 3, 0, new_dat)
    dat <- get_legacy_biomes_value(db, 32, 3, 0, return_names = FALSE)
    expect_equal(dat, matrix(new_dat, 16L, 16L))
})

test_that("put_legacy_biomes_values() accepts strings", {
    keys <- c("chunk:37:5:0:45", "chunk:37:6:0:45")
    put_legacy_biomes_values(db, keys, values = "plains")

    new_dat <- list(
        `chunk:37:5:0:45` = matrix("plains", 16, 16),
        `chunk:37:6:0:45` = matrix("plains", 16, 16)
    )

    dat <- get_legacy_biomes_data(db, keys)
    expect_equal(dat, new_dat)
})

test_that("put_legacy_biomes_values() accepts integers", {
    keys <- c("chunk:37:5:0:45", "chunk:37:6:0:45")
    put_legacy_biomes_values(db, keys, values = 2L)

    new_dat <- list(
        `chunk:37:5:0:45` = matrix(2L, 16, 16),
        `chunk:37:6:0:45` = matrix(2L, 16, 16)
    )

    dat <- get_legacy_biomes_data(db, keys, return_names = FALSE)
    expect_equal(dat, new_dat)
})


close(db)
fs::dir_delete(dbpath)
