dbpath <- rbedrock_example_world("example3.mcworld")
db <- bedrockdb(dbpath)

as_raw <- function(...) {
    as.raw(c(...))
}

test_that("get_acdig_value() returns a vector of actor keys.", {
    dat <- get_acdig_value(0, 0, 0, db = db)
    expect_equal(dat, c("actor:000000010000000A", "actor:000000010000000B",
                        "actor:000000010000000C"))
    dat <- get_acdig_value("acdig:-7:-2:0", db = db)
    expect_equal(dat, c("actor:000000010000000F", "actor:0000000100000010"))
    dat <- get_acdig_value("acdig:1:0:0", db = db)
    expect_equal(dat, character(0L))
    dat <- get_acdig_value("acdig:1:0:1", db = db)
    expect_null(dat)

    expect_error(get_acdig_value(c("acdig:1:0:0", "acdig:0:0:0"), db = db))
    expect_null(get_acdig_value(c("acdig:0:0:0:0"), db = db))

    expect_error(read_acdig_value(as_raw(0, 1)))
})

test_that("get_acdig_data() returns a list of vectors of actor keys.", {
    dat <- get_acdig_data(c(0, -7, 1, 1), c(0, -2, 0, 0), c(0, 0, 0, 1),
                          db = db)
    expected <- list(
        "acdig:0:0:0" = c("actor:000000010000000A", "actor:000000010000000B",
                          "actor:000000010000000C"),
        "acdig:-7:-2:0" = c("actor:000000010000000F", "actor:0000000100000010"),
        "acdig:1:0:0" = character(0L),
        "acdig:1:0:1" = NULL
    )
    expect_equal(dat, expected)
    dat <- get_acdig_data(c("acdig:0:0:0", "acdig:-7:-2:0",
                            "acdig:1:0:0", "acdig:1:0:1"), db = db)
    expect_equal(dat, expected)
})

test_that("put_acdig_value() writes actor digest data to db.", {
    actors <- c("actor:000000020000000A", "actor:000000020000000B")
    put_acdig_value(actors, 0, 0, 1, db = db)
    dat <- get_value("acdig:0:0:1", db = db)
    expect_equal(dat, as_raw(0, 0, 0, 2, 0, 0, 0, 10, 0, 0, 0, 2, 0, 0, 0, 11))

    expect_null(write_acdig_value(NULL))

    expect_error(write_acdig_value(c("actor:000000020000000A",
                                     "actor:000000020000000")))
    expect_error(write_acdig_value("error"))
})

test_that("put_acdig_data() writes actor digest data to db.", {
    actors1 <- c("actor:000000020000000A", "actor:000000020000000B")
    actors2 <- c("actor:000000020000000C", "actor:000000020000000D")
    actors <- list(
        "acdig:0:1:1" = actors1,
        "acdig:0:2:1" = actors2
    )
    put_acdig_data(actors, db = db)
    dat <- get_data(names(actors), db = db)
    expected <- list(
        "acdig:0:1:1" = as_raw(0, 0, 0, 2, 0, 0, 0, 10,
                               0, 0, 0, 2, 0, 0, 0, 11),
        "acdig:0:2:1" = as_raw(0, 0, 0, 2, 0, 0, 0, 12,
                               0, 0, 0, 2, 0, 0, 0, 13)
    )
    expect_equal(dat, expected)

    new_names <- sub(":0:", ":1:", names(actors))
    put_acdig_data(setNames(actors, NULL), new_names, db = db)
    dat <- get_data(new_names, db = db)
    names(expected) <- new_names
    expect_equal(dat, expected)

    new_names <- sub(":0:", ":2:", names(actors))
    put_acdig_data(actors, new_names, db = db)
    dat <- get_data(new_names, db = db)
    names(expected) <- new_names
    expect_equal(dat, expected)

    new_names <- sub(":0:", ":3:", names(actors))
    put_acdig_data(actors, 3, 1:2, 1, db = db)
    dat <- get_data(new_names, db = db)
    names(expected) <- new_names
    expect_equal(dat, expected)
})

test_that("get_actors_value() returns a list of NBT data.", {
    dat <- get_actors_value(0, 0, 0, db = db)
    expect_type(dat, "list")
    expect_named(dat, c("actor:000000010000000A", "actor:000000010000000B",
                        "actor:000000010000000C"))
    expect_true(all(sapply(dat, is_nbt)))

    dat <- get_actors_value("acdig:0:0:0", db = db)
    expect_type(dat, "list")
    expect_named(dat, c("actor:000000010000000A", "actor:000000010000000B",
                        "actor:000000010000000C"))
    expect_true(all(sapply(dat, is_nbt)))

    expect_null(get_actors_value("acdig:1:0:1", db = db))
    expect_equal(get_actors_value("acdig:1:0:0", db = db),
                 setNames(list(), character(0L)))
})

test_that("get_actors_data() returns a list of lists of NBT data.", {
    dat <- get_actors_data(c(0, -7, 1, 1), c(0, -2, 0, 0), c(0, 0, 0, 1),
                           db = db)
    expect_named(dat, c(
        "acdig:0:0:0", "acdig:-7:-2:0", "acdig:1:0:0", "acdig:1:0:1"
    ))

    expect_type(dat[[1]], "list")
    expect_type(dat[[2]], "list")
    expect_type(dat[[3]], "list")
    expect_null(dat[[4]])

    expect_named(dat[[1]], c("actor:000000010000000A", "actor:000000010000000B",
                             "actor:000000010000000C"))
    expect_named(dat[[2]], c("actor:000000010000000F",
                             "actor:0000000100000010"))
    expect_length(dat[[3]], 0L)

    expect_true(all(sapply(dat[[1]], is_nbt)))
    expect_true(all(sapply(dat[[2]], is_nbt)))

    dat2 <- get_actors_data(c("acdig:0:0:0", "acdig:-7:-2:0",
                              "acdig:1:0:0", "acdig:1:0:1"), db = db)
    expect_equal(dat, dat2)
})

update_storage_key <- function(x, y) {
    y <- bit64::as.integer64(y)
    x$UniqueID <- y
    y <- writeBin(structure(y, class = NULL), raw(), endian = "big")
    x$internalComponents$EntityStorageKeyComponent$StorageKey <-
        nbt_raw_string(y)
    x
}

test_that("put_actors_value() writes actors data.", {
    actor_value <- get_nbt_value("actor:0000000100000011", db = db)
    actor_a <- update_storage_key(actor_value, 100L)
    actor_b <- update_storage_key(actor_value, 101L)

    actors_data <- list("actor:0000000000000064" = actor_a,
                        "actor:0000000000000065" = actor_b)
    put_actors_value(actors_data, "acdig:100:0:0", db = db)
    dat <- get_actors_value("acdig:100:0:0", db = db)
    expect_equal(dat, actors_data)

    put_actors_value(actors_data, 100, 1, 0, db = db)
    dat <- get_actors_value(100, 1, 0, db = db)
    expect_equal(dat, actors_data)
})

test_that("put_actors_value() writes actors data.", {
    actor_value <- get_nbt_value("actor:0000000100000011", db = db)
    actor_a <- update_storage_key(actor_value, 90L)
    actor_b <- update_storage_key(actor_value, 91L)
    actor_c <- update_storage_key(actor_value, 92L)

    actors_data <- list(
        "acdig:200:0:0" = list("actor:000000000000005A" = actor_a,
                               "actor:000000000000005B" = actor_b),
        "acdig:201:0:0" = list("actor:000000000000005C" = actor_c)
    )

    put_actors_data(actors_data, db = db)
    dat <- get_actors_data(names(actors_data), db = db)
    expect_equal(dat, actors_data)

    new_names <- sub(":0:", ":1:", names(actors_data))
    put_actors_data(actors_data, new_names, db = db)
    dat <- get_actors_data(new_names, db = db)
    expect_equal(dat, setNames(actors_data, new_names))

    new_names <- sub(":0:", ":2:", names(actors_data))
    put_actors_data(actors_data, 200:201, 2, 0, db = db)
    dat <- get_actors_data(new_names, db = db)
    expect_equal(dat, setNames(actors_data, new_names))
})

close(db)
fs::dir_delete(dbpath)
