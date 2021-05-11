test_that("rbedrock_example shows all example data", {
    dat <- rbedrock_example()
    expect_setequal(dat, c(
        "block_states.csv",
        "example1.mcworld"
    ))
})

test_that("opening an example world works", {
    dbpath <- expect_silent(rbedrock_example_world("example1.mcworld"))
    # clean up
    unlink(dirname(dbpath), recursive=TRUE, expand = FALSE)
})
