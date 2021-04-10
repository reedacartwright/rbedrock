test_that("opening an example world works", {
    dbpath <- expect_silent(rbedrock_example_world("example1.mcworld"))
    
    # clean up
    unlink(dirname(dbpath), recursive=TRUE, expand = FALSE)
})

test_that("bedrockdb can open and close a database", {
    dbpath <- rbedrock_example_world("example1.mcworld")
    db <- bedrockdb(dbpath)
    expect_true(db$is_open())
    close(db)
    expect_false(db$is_open())
    # clean up
    unlink(dirname(dbpath), recursive=TRUE, expand = FALSE)
})

