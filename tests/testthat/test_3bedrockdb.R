test_that("bedrockdb can open and close a database", {
    dbpath <- rbedrock_example_world("example1.mcworld")
    db <- bedrockdb(dbpath)
    expect_true(db$is_open())
    close(db)
    expect_false(db$is_open())
    # clean up
    fs::dir_delete(dbpath)
})

test_that("bedrockdb clears open iterators when closed",{
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
    
    it3$destroy();
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


test_that("bedrockdb clears open snapshots when closed",{
    dbpath <- rbedrock_example_world("example1.mcworld")
    db <- bedrockdb(dbpath)
    snap1 <- db$snapshot()
    snap2 <- db$snapshot()
    snap3 <- db$snapshot()
    snap4 <- db$snapshot()
    expect_false(db_snapshot_isnil(snap1))
    expect_false(db_snapshot_isnil(snap2))
    expect_false(db_snapshot_isnil(snap3))
    expect_false(db_snapshot_isnil(snap4))
    
    db$snapshot_release(snap3)
    expect_true(db_snapshot_isnil(snap3))
    
    expect_silent(close(db))
    expect_silent(gc())

    expect_true(db_snapshot_isnil(snap1))
    expect_true(db_snapshot_isnil(snap2))
    expect_true(db_snapshot_isnil(snap3))
    expect_true(db_snapshot_isnil(snap4))

    # clean up
    fs::dir_delete(dbpath)
})
