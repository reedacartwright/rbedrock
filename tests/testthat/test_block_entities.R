dbpath <- rbedrock_example_world("example1.mcworld")
db <- bedrockdb(dbpath)

test_that("BlockEntities is chunk tag 49", {
    expect_equal(chunk_tag_int("BlockEntities"), 49L)
    expect_equal(chunk_tag_str(49L), "BlockEntities")
})

test_that("get_block_entities_data returns all BlockEnties data", {
    dat <- get_block_entities_data(db)
    expect_vector(dat, list(), 22L)
    expect_named(dat)
    expect_true(all(grepl(":49$", names(dat))))
    for(i in seq_along(dat)) {
        expect_named(dat[[!!i]], NULL)
    }
})

test_that("get_block_entities_data returns specific BlockEnties data", {
    keys <- c("@37:13:0:49", "fake_data", "@37:15:0:49")
    dat <- get_block_entities_data(db, keys)
    expect_vector(dat, list(), 2L)
    expect_named(dat)
    expect_true(all(grepl(":49$", names(dat))))
    for(i in seq_along(dat)) {
        expect_named(dat[[!!i]], NULL)
    }
})

test_that("MobSpawners can be identified and placed in a table", {
    dat <- purrr::flatten(get_block_entities_data(db))
    dat <- purrr::keep(dat, ~.$id == "MobSpawner")
    dat <- purrr::map_dfr(unnbt(dat), tibble::as_tibble)

    expect_named(dat, c("Delay", "DisplayEntityHeight", "DisplayEntityScale",
        "DisplayEntityWidth", "EntityIdentifier", "MaxNearbyEntities",
        "MaxSpawnDelay", "MinSpawnDelay", "RequiredPlayerRange", "SpawnCount",
        "SpawnRange", "id", "isMovable", "x", "y", "z"))

    expect_equal(nrow(dat), 9L)
})

test_that("put_block_entities_data writes BlockEnties data", {
    dat <- get_block_entities_data(db, "@37:13:0:49")
    dat2 <- dat
    names(dat2) <- "@10:0:0:49"
    put_block_entities_data(db, dat2)
    dat <- get_block_entities_data(db, "@10:0:0:49")
    expect_equal(dat, dat2)
})

test_that("put_block_entities_data throws error if asked to write non BlockEnties data", {
    dat <- get_block_entities_data(db, "@37:13:0:49")
    dat2 <- dat
    names(dat2) <- "@10:0:0:44"
    expect_error(put_block_entities_data(db, dat2))
})

# clean up
close(db)
fs::dir_delete(dbpath)