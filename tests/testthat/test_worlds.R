test_that("create_world creates a world that can be loaded", {
    worlds_dir <- tempdir(TRUE)

    dbpath <- expect_message(create_world(
        id = "test",
        LevelName = "test",
        RandomSeed = 1234,
        showcoordinates = TRUE,
        GameType = 1L,
        SpawnX = 0L,
        SpawnY = 65535,
        SpawnZ = 0.0,
        worldStartCount = 10L,
        worlds_dir = worlds_dir
    ), "Success")

    expect_equal(dbpath, fs::path(worlds_dir, "test"))

    # test that all the values have been properly set
    dat <- read_leveldat(dbpath, old = FALSE)
    expect_equal(dat$LevelName, nbt_string("test"))
    expect_equal(dat$RandomSeed, nbt_long(1234L))
    expect_equal(dat$showcoordinates, nbt_byte(TRUE))
    expect_equal(dat$GameType, nbt_int(1))
    expect_equal(dat$SpawnX, nbt_int(0))
    expect_equal(dat$SpawnY, nbt_int(65535))
    expect_equal(dat$SpawnZ, nbt_int(0))
    expect_equal(dat$worldStartCount, nbt_long(10))

    # can the DB be opened
    db <- expect_silent(bedrockdb(dbpath))
    close(db, compact = FALSE)
    fs::dir_delete(dbpath)
})
