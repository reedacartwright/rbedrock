context("Database Opening and Closing")
library(rbedrock)

dbpath <- rbedrock_example_world("example1.mcworld")

test_that("bedrockdb can open and close a database", {
    db <- bedrockdb(dbpath)
    expect_true(db$is_open())
    close(db)
    expect_false(db$is_open())
})

# clean up
unlink(dirname(dbpath), recursive=TRUE, expand = FALSE)
