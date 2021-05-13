test_that("rbedrock_example shows all example data", {
    dat <- rbedrock_example()
    expect_setequal(dat, c(
        "block_states.csv",
        "example1.mcworld",
        "default_level.dat"
    ))
})

test_that("opening an example world works", {
    dbpath <- expect_silent(rbedrock_example_world("example1.mcworld"))
    # clean up
    fs::dir_delete(dbpath)
})
