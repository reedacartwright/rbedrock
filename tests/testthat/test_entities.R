dbpath <- rbedrock_example_world("example1.mcworld")
db <- bedrockdb(dbpath)

test_that("Entities is chunk tag 50", {
    expect_equal(chunk_tag_int("Entities"), 50L)
    expect_equal(chunk_tag_str(50L), "Entities")
})

test_that("get_entities_data returns all Entities data", {
    dat <- get_entities_data(db)
    expect_vector(dat, list(), 11L)
    expect_named(dat)
    expect_true(all(grepl(":50$", names(dat))))
    for(i in seq_along(dat)) {
        expect_named(dat[[!!i]], NULL)
    }
})

test_that("get_entities_data returns specific Entities data", {
    keys <- c("@32:3:0:50", "fake_data", "@34:4:0:50")
    dat <- get_entities_data(db, keys)
    expect_vector(dat, list(), 2L)
    expect_named(dat)
    expect_true(all(grepl(":50$", names(dat))))
    for(i in seq_along(dat)) {
        expect_named(dat[[!!i]], NULL)
    }
})

test_that("put_entities_data writes Entities data", {
    dat <- get_entities_data(db, "@34:4:0:50")
    dat2 <- dat
    names(dat2) <- "@10:0:0:50"
    put_entities_data(db, dat2)
    dat <- get_entities_data(db, "@10:0:0:50")
    expect_equal(dat, dat2)
})

test_that("put_entities_data throws error if asked to write non Entities data", {
    dat <- get_entities_data(db, "@34:4:0:50")
    dat2 <- dat
    names(dat2) <- "@10:0:0:44"
    expect_error(put_entities_data(db, dat2))
})

# clean up
close(db)
fs::dir_delete(dbpath)