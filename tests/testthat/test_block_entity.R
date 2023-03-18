dbpath <- rbedrock_example_world("example1.mcworld")
db <- bedrockdb(dbpath)

test_that("BlockEntity is chunk tag 49", {
    expect_equal(chunk_tag_int("BlockEntity"), 49L)
    expect_equal(chunk_tag_str(49L), "BlockEntity")
})

test_that("get_block_entity_data() returns specific BlockEntity data", {
    keys <- c("chunk:37:13:0:49", "plain:fake_data", "chunk:37:15:0:49")
    dat <- get_block_entity_data(db, keys)
    expect_vector(dat, list(), 2L)
    expect_named(dat)
    expect_true(all(grepl(":49$", names(dat))))
    for (i in seq_along(dat)) {
        expect_named(dat[[!!i]], NULL)
    }
})

test_that("get_block_entity_values() returns specific BlockEntity data", {
    keys <- c("chunk:37:13:0:49", "plain:fake_data", "chunk:37:15:0:49")
    dat <- get_block_entity_values(db, keys)
    expect_vector(dat, list(), 2L)
    expect_named(dat)
    expect_true(all(grepl(":49$", names(dat))))
    for (i in seq_along(dat)) {
        expect_named(dat[[!!i]], NULL)
    }
})

test_that("get_block_entity_value() accepts returns one BlockEntity data", {
    key <- "chunk:37:13:0:49"
    dat <- get_block_entity_value(db, key)
    expect_vector(dat, list(), 3L)
    expect_named(dat, NULL)
    expect_true(is_nbt(dat[[1]]))
    expect_true(is_nbt(dat[[2]]))
    expect_true(is_nbt(dat[[3]]))

    expect_silent(get_block_entity_value(db, c(key, "plain:fake_key")))

    expect_error(get_block_entity_value(db, ""))
    expect_error(get_block_entity_value(db, "plain:fake_key"))
    expect_error(get_block_entity_value(db, c(
        "chunk:37:13:0:49",
        "chunk:37:15:0:49"
    )))
})

test_that("MobSpawners can be identified and placed in a table", {
    dat <- purrr::flatten(get_block_entity_data(db, get_keys(db)))
    dat <- purrr::keep(dat, ~ .$id == "MobSpawner")
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
    dat <- get_block_entity_data(db, "chunk:37:13:0:49")
    dat2 <- dat
    names(dat2) <- "chunk:10:0:0:49"
    put_block_entity_data(db, dat2)
    dat <- get_block_entity_data(db, "chunk:10:0:0:49")
    expect_equal(dat, dat2)

    names(dat2) <- "chunk:10:0:0:44"
    expect_error(put_block_entitiy_data(db, dat2))
})

test_that("put_block_entity_values() writes BlockEnties data", {
    dat <- get_block_entity_data(db, 37, 13, 0)
    names(dat) <- NULL
    put_block_entity_values(db, 10, 1, 0, dat)
    dat2 <- dat
    names(dat2) <- "chunk:10:1:0:49"
    dat3 <- get_block_entity_data(db, 10, 1, 0)
    expect_equal(dat2, dat3)

    expect_error(put_block_entity_values(db, "chunk:10:1:0:44", dat))
})

test_that("put_block_entity_value() writes BlockEnties data", {
    dat <- get_block_entity_value(db, 37, 13, 0)
    put_block_entity_value(db, 10, 2, 0, dat)
    dat2 <- get_block_entity_value(db, 10, 2, 0)
    expect_equal(dat, dat2)

    expect_silent(put_block_entity_value(db, c(
        "chunk:10:2:0:44",
        "chunk:10:2:0:49"
    ), value = dat))
    expect_error(put_block_entity_value(db, c(
        "chunk:10:3:0:49",
        "chunk:10:2:0:49"
    ), value = dat))
})

# clean up
close(db)
fs::dir_delete(dbpath)
