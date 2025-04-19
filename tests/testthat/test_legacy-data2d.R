dbpath <- rbedrock_example_world("example1.mcworld")
db <- bedrockdb(dbpath)

as_raw <- function(...) {
    as.raw(c(...))
}

test_that("Data2D is chunk tag 45", {
    expect_equal(chunk_tag_int("Data2D"), 45L)
    expect_equal(chunk_tag_str(45L), "Data2D")
})

test_that("write_data2d_value() creates Data2d data.", {
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

    # Recycles values
    expect_equal(
        write_data2d_value(
            list(height_map = 63, biome_map = 10)
        ),
        res
    )

    # NULL to NULL
    expect_equal(write_data2d_value(NULL), NULL)
    # Error on empty
    expect_error(write_data2d_value(list()))
})

close(db)
fs::dir_delete(dbpath)
