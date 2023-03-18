test_that("bedrockdb can open and close a database", {
    dbpath <- rbedrock_example_world("example1.mcworld")
    db <- bedrockdb(dbpath)
    expect_true(db$is_open())
    close(db)
    expect_false(db$is_open())
    # clean up
    fs::dir_delete(dbpath)
})

test_that("create_unique_ids works", {
    epoch <- bit64::as.integer64(-2)

    dbpath <- rbedrock_example_world("example2.mcworld")
    db <- bedrockdb(dbpath)

    expect_equal(db$leveldat$worldStartCount, nbt_long(epoch + 2^32))

    u1 <- db$create_unique_ids(5)
    u2 <- db$create_unique_ids(5)
    close(db)

    expect_equal(c(u1, u2), (2 ^ 32 * epoch) + 1:10)

    db <- bedrockdb(dbpath)
    u1 <- db$create_unique_ids(10)
    u2 <- db$create_unique_ids(10)
    expect_equal(c(u1, u2), (2 ^ 32 * (epoch - 1)) + 1:20)

    close(db)

    # clean up
    fs::dir_delete(dbpath)
})
