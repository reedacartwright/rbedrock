dbpath <- rbedrock_example_world("example1.mcworld")
db <- bedrockdb(dbpath)

test_that("Entity is chunk tag 50", {
    expect_equal(chunk_tag_int("Entity"), 50L)
    expect_equal(chunk_tag_str(50L), "Entity")
})

test_that("get_entity_data returns specific Entity data", {
    keys <- c("chunk:32:3:0:50", "chunk:34:4:0:50")
    dat <- get_entity_data(keys, db = db)
    expect_vector(dat, list(), 2L)
    expect_named(dat)
    expect_true(all(grepl(":50$", names(dat))))
    for (i in seq_along(dat)) {
        expect_named(dat[[!!i]], NULL)
    }

    keys <- c("chunk:32:3:0:50", "chunk:34:4:0:50", "plain:fake")
    expect_error(get_entity_data(keys, db = db))
})

test_that("put_entity_data writes Entity data", {
    dat <- get_entity_data("chunk:34:4:0:50", db = db)
    dat2 <- dat
    names(dat2) <- "chunk:10:0:0:50"
    put_entity_data(dat2, db = db)
    dat <- get_entity_data("chunk:10:0:0:50", db = db)
    expect_equal(dat, dat2)
})

test_that("put_entity_data throws error if asked to write non Entity data", {
    dat <- get_entity_data("chunk:34:4:0:50", db = db)
    dat2 <- dat
    names(dat2) <- "chunk:10:0:0:44"
    expect_error(put_entity_data(dat2, db = db))
})

# clean up
close(db)
fs::dir_delete(dbpath)
