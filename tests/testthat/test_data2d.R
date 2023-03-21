dbpath <- rbedrock_example_world("example1.mcworld")
db <- bedrockdb(dbpath)

as_raw <- function(...) {
    as.raw(c(...))
}

test_that("Data2D is chunk tag 45", {
    expect_equal(chunk_tag_int("Data2D"), 45L)
    expect_equal(chunk_tag_str(45L), "Data2D")
})

test_that("write_data2d_value() accepts 1 parameter.", {
    res <- as_raw(rep(c(63, 0), 256), rep(10, 256))

    expect_equal(
        write_data2d_value(
            list(
                height_map = rep(63, 256),
                biome_map = rep(10, 256)
            )
        ),
        res
    )

    expect_equal(write_data2d_value(NULL), NULL)

    expect_error(write_data2d_value(list()))
})

test_that("write_data2d_value() accepts 2 parameters.", {
    res <- as_raw(rep(c(63, 0), 256), rep(10, 256))

    expect_equal(
        write_data2d_value(
            rep(63, 256), rep(10, 256)
        ),
        res
    )

    expect_equal(write_data2d_value(NULL, NULL), NULL)
})

test_that("write_data2d_value() recycles parameters.", {
    res <- as_raw(rep(c(63, 0), 256), rep(10, 256))

    expect_equal(
        write_data2d_value(
            list(height_map = 63, biome_map = 10)
        ),
        res
    )
    expect_equal(write_data2d_value(63, 10), res)

    expect_error(write_data2d_value(c(63, 4), 10))
    expect_error(write_data2d_value(63, c(9, 10)))
})

close(db)
fs::dir_delete(dbpath)
