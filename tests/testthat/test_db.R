dbpath1 <- rbedrock_example_world("example2.mcworld")
dbpath2 <- rbedrock_example_world("example2.mcworld")

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
    # tracks the last opened db
    db1 <- bedrockdb(dbpath1)
    expect_identical(default_db(), db1)
    db2 <- bedrockdb(dbpath2)
    expect_identical(default_db(), db2)
    # updates and returns last value
    expect_identical(default_db(db1), db2)
    expect_identical(default_db(), db1)

    # unset the default -> return the last opened db
    expect_identical(default_db(NULL), db1)
    expect_null(default_db(check = FALSE))

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

    # A manually set value takes precedence
    db1 <- bedrockdb(dbpath1)
    default_db(db1)
    expect_identical(default_db(), db1)
    db2 <- bedrockdb(dbpath2)
    expect_identical(default_db(), db1)
    close(db2)

    # unsetting default db allows last_opened to work again
    default_db(NULL)
    expect_null(default_db(check = FALSE))
    db2 <- bedrockdb(dbpath2)
    expect_identical(default_db(), db2)
    close(db2)
    expect_null(default_db(check = FALSE))
    close(db1)
})

test_that("with_db() sets the default db temporarily", {
    # Sanity Check
    expect_null(default_db(check = FALSE))

    # With a db connection
    db1 <- bedrockdb(dbpath1)
    default_db(NULL) # Forget db1
    expect_null(default_db(check = FALSE))
    db <- with_db(db1, default_db())
    expect_null(default_db(check = FALSE))
    expect_identical(db$db, db1$db)
    expect_true(db$is_open())
    close(db)

    # With a character string
    expect_null(default_db(check = FALSE))
    db <- with_db(dbpath1, default_db())
    expect_equal(db$path, file_path(dbpath1, "db"))
    expect_false(db$is_open())
    expect_null(default_db(check = FALSE))

    # With a db connection, restores previous value
    db2 <- bedrockdb(dbpath2)
    expect_identical(default_db()$db, db2$db)
    db <- with_db(bedrockdb(dbpath1), default_db())
    expect_identical(default_db()$db, db2$db)
    expect_equal(db$path, file_path(dbpath1, "db"))
    expect_true(db$is_open())
    close(db)
    close(db2)

    # With a db connection, closes if close = TRUE
    expect_null(default_db(check = FALSE))
    db <- with_db(bedrockdb(dbpath1), default_db(), close = TRUE)
    expect_null(default_db(check = FALSE))
    expect_equal(db$path, file_path(dbpath1, "db"))
    expect_false(db$is_open())

    # Acts just like default_db() was manually called
    db1 <- bedrockdb(dbpath1) # automatically set the$db to db1
    with_db(dbpath2, {}) # manually sets and resets db1
    expect_identical(default_db()$db, db1$db)
    db2 <- bedrockdb(dbpath2) # does not automatically set the$db
    expect_identical(default_db()$db, db1$db)
    close(db1) # the$db is unset
    close(db2)
    db2 <- bedrockdb(dbpath2) # automatically sets the$db
    expect_identical(default_db()$db, db2$db)
    close(db2)
})

test_that("local_db() sets the default db temporarily", {
    # Sanity Check
    expect_null(default_db(check = FALSE))

    db <- local({
        db <- local_db(dbpath1)
        expect_identical(default_db(), db)
        db
    })
    expect_null(default_db(check = FALSE))
    expect_false(db$is_open())

    db <- local({
        db <- local_db(bedrockdb(dbpath1))
        expect_identical(default_db(), db)
        db
    })
    expect_null(default_db(check = FALSE))
    expect_true(db$is_open())
    close(db)

    db2 <- bedrockdb(dbpath2)
    db <- local({
        db <- local_db(dbpath1)
        expect_identical(default_db(), db)
        db
    })
    expect_identical(default_db()$db, db2$db)
    expect_false(db$is_open())
})

# clean up
unlink(dbpath1, recursive = TRUE)
unlink(dbpath2, recursive = TRUE)
