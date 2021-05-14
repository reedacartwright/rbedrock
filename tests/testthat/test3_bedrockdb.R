test_that("bedrockdb can open and close a database", {
    dbpath <- rbedrock_example_world("example1.mcworld")
    db <- bedrockdb(dbpath)
    expect_true(db$is_open())
    close(db)
    expect_false(db$is_open())
    # clean up
    fs::dir_delete(dbpath)
})
