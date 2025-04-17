dbpath1 <- rbedrock_example_world("example1.mcworld")
dbpath2 <- rbedrock_example_world("example1.mcworld")

test_that("default_db() returns the last opened db", {
    db1 <- bedrockdb(dbpath1)
    expect_identical(default_db(), db1)
    db2 <- bedrockdb(dbpath2)
    expect_identical(default_db(), db2)

    # closing db1 does not change the default
    close(db1)
    expect_identical(default_db(), db2)
    # closing db2 *does* change the default
    close(db2)
    expect_error(default_db())
    expect_null(default_db(check = FALSE))
})

test_that("default_db() gets/sets the default db", {
    db1 <- bedrockdb(dbpath1)
    db2 <- bedrockdb(dbpath2)
    expect_identical(default_db(), db2)
    # updates and returns NULL on first set
    expect_null(default_db(db1))
    expect_identical(default_db(), db1)

    # unset the default -> return the last opened db
    expect_identical(default_db(NULL), db1)
    expect_identical(default_db(), db2)

    # updates and returns previously set value
    default_db(db1)
    expect_identical(default_db(db2), db1)
    expect_identical(default_db(), db2)

    # closing db1 does not change the default
    close(db1)
    expect_identical(default_db(), db2)
    # closing db2 *does* change the default
    close(db2)
    expect_error(default_db())
    expect_null(default_db(check = FALSE))
})

# clean up
unlink(dbpath1, recursive = TRUE)
unlink(dbpath2, recursive = TRUE)
