test_that("bedrockdb can open and close a database", {
    dbpath <- rbedrock_example_world("example1.mcworld")
    db <- bedrockdb(dbpath)
    expect_true(db$is_open())
    close(db)
    expect_false(db$is_open())
    # clean up
    fs::dir_delete(dbpath)
})

test_that("bedrockdb clears open iterators when closed", {
    dbpath <- rbedrock_example_world("example1.mcworld")
    db <- bedrockdb(dbpath)
    it1 <- db$iterator()
    it2 <- db$iterator()
    it3 <- db$iterator()
    it4 <- db$iterator()
    expect_false(it1$isnil())
    expect_false(it2$isnil())
    expect_false(it3$isnil())
    expect_false(it4$isnil())

    it3$destroy()
    expect_true(it3$isnil())

    expect_silent(close(db))
    expect_silent(gc())

    expect_true(it1$isnil())
    expect_true(it2$isnil())
    expect_true(it3$isnil())
    expect_true(it4$isnil())

    # clean up
    fs::dir_delete(dbpath)
})

test_that("bedrockdb clears open snapshots when closed", {
    dbpath <- rbedrock_example_world("example1.mcworld")
    db <- bedrockdb(dbpath)
    snap1 <- db$snapshot()
    snap2 <- db$snapshot()
    snap3 <- db$snapshot()
    snap4 <- db$snapshot()
    expect_false(snapshot_isnil(snap1))
    expect_false(snapshot_isnil(snap2))
    expect_false(snapshot_isnil(snap3))
    expect_false(snapshot_isnil(snap4))

    db$snapshot_release(snap3)
    expect_true(snapshot_isnil(snap3))

    expect_silent(close(db))
    expect_silent(gc())

    expect_true(snapshot_isnil(snap1))
    expect_true(snapshot_isnil(snap2))
    expect_true(snapshot_isnil(snap3))
    expect_true(snapshot_isnil(snap4))

    # clean up
    fs::dir_delete(dbpath)
})

test_that("close_all_bedrockdb works", {
    dbpath1 <- rbedrock_example_world("example1.mcworld")
    dbpath2 <- rbedrock_example_world("example2.mcworld")
    db1 <- bedrockdb(dbpath1)
    db2 <- bedrockdb(dbpath2)
    expect_true(db1$is_open())
    expect_true(db2$is_open())
    close_all_bedrockdb()
    expect_false(db1$is_open())
    expect_false(db2$is_open())
    # clean up
    fs::dir_delete(dbpath1)
    fs::dir_delete(dbpath2)
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
