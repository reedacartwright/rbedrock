test_that("rbedrock_example shows all example data", {
    dat <- rbedrock_example()
    expect_setequal(dat, c(
        "block_states.csv",
        "example1.mcworld",
        "example2.mcworld",
        "default_level.dat"
    ))
})

test_that("opening an example world works", {
    dbpath <- expect_silent(rbedrock_example_world("example1.mcworld"))
    db <- expect_silent(bedrockdb(dbpath))
    close(db, compact=FALSE)
    # clean up
    fs::dir_delete(dbpath)
})
