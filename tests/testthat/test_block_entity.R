dbpath <- rbedrock_example_world("example1.mcworld")
db <- bedrockdb(dbpath)

test_that("BlockEntity is chunk tag 49", {
    expect_equal(chunk_tag_int("BlockEntity"), 49L)
    expect_equal(chunk_tag_str(49L), "BlockEntity")
})

test_that("get_block_entity_data() returns specific BlockEntity data", {
    keys <- c("chunk:37:13:0:49", "chunk:37:15:0:49")
    dat <- get_block_entity_data(keys, db = db)
    expect_vector(dat, list(), 2L)
    expect_named(dat)
    expect_true(all(grepl(":49$", names(dat))))
    for (i in seq_along(dat)) {
        expect_named(dat[[!!i]], NULL)
    }

    keys <- c("chunk:37:13:0:49", "plain:fake_data", "chunk:37:15:0:49")
    expect_error(get_block_entity_data(keys, db = db))
})

test_that("get_block_entity_value() accepts returns one BlockEntity data", {
    key <- "chunk:37:13:0:49"
    dat <- get_block_entity_value(key, db = db)
    expect_vector(dat, list(), 3L)
    expect_named(dat, NULL)
    expect_true(is_nbt(dat[[1]]))
    expect_true(is_nbt(dat[[2]]))
    expect_true(is_nbt(dat[[3]]))

    expect_error(get_block_entity_value(c(key, "plain:fake_key"), db = db))
    expect_error(get_block_entity_value("", db = db))
    expect_error(get_block_entity_value("plain:fake_key", db = db))
    expect_error(get_block_entity_value(c("chunk:37:13:0:49",
                                          "chunk:37:15:0:49"), db = db))
})

test_that("MobSpawners can be identified and placed in a table", {
    keys <- filter_chunk_keys(get_keys(db = db), tag = 49L)
    dat <- get_block_entity_data(keys, db = db)
    dat <- purrr::flatten(dat)
    dat <- purrr::keep(dat, ~.$id == "MobSpawner")
    dat <- purrr::map_dfr(unnbt(dat), tibble::as_tibble)

    expect_named(dat, c(
        "Delay", "DisplayEntityHeight", "DisplayEntityScale",
        "DisplayEntityWidth", "EntityIdentifier", "MaxNearbyEntities",
        "MaxSpawnDelay", "MinSpawnDelay", "RequiredPlayerRange", "SpawnCount",
        "SpawnRange", "id", "isMovable", "x", "y", "z"
    ))

    expect_equal(nrow(dat), 9L)
})

test_that("put_block_entity_data() writes BlockEnties data", {
    dat <- get_block_entity_data(37, 13, 0, db = db)
    names(dat) <- NULL
    put_block_entity_data(dat, 10, 1, 0, db = db)
    dat2 <- dat
    names(dat2) <- "chunk:10:1:0:49"
    dat3 <- get_block_entity_data(10, 1, 0, db = db)
    expect_equal(dat2, dat3)

    dat <- get_block_entity_data("chunk:37:13:0:49", db = db)
    dat2 <- dat
    names(dat2) <- "chunk:10:0:0:49"
    put_block_entity_data(dat2, db = db)
    dat <- get_block_entity_data("chunk:10:0:0:49", db = db)
    expect_equal(dat, dat2)

    expect_error(put_block_entity_data(dat, "chunk:10:1:0:44", db = db))
    names(dat2) <- "chunk:10:0:0:44"
    expect_error(put_block_entitiy_data(dat2, db = db))
})

test_that("put_block_entity_value() writes BlockEnties data", {
    dat <- get_block_entity_value(37, 13, 0, db = db)
    put_block_entity_value(dat, 10, 2, 0, db = db)
    dat2 <- get_block_entity_value(10, 2, 0, db = db)
    expect_equal(dat, dat2)

    expect_error(put_block_entity_value(dat, c("chunk:10:2:0:44",
        "chunk:10:2:0:49"), db = db))
    expect_error(put_block_entity_value(dat, c("chunk:10:3:0:49",
        "chunk:10:2:0:49"), db = db))
})

# clean up
close(db)
fs::dir_delete(dbpath)
