dbpath <- rbedrock_example_world("example1.mcworld")
db <- bedrockdb(dbpath)

test_that("get_hsa_data() can read HSA data.", {
    keys <- c("chunk:36:15:0:57", "chunk:36:16:0:57", "chunk:37:15:0:57",
              "chunk:37:16:0:57")

    hsa <- get_hsa_data(keys, db = db)

    expected_hsa <- cbind(
        x1 = c(577L, 577L, 592L, 592L),
        y1 = c(65L, 65L, 65L, 65L),
        z1 = c(241L, 256L, 241L, 256L),
        x2 = c(591, 591, 592, 592),
        y2 = c(86L, 86L, 86L, 86L),
        z2 = c(255L, 256L, 255L, 256L),
        tag = c(5L, 5L, 5L, 5L)
    )
    expected_hsa <- lapply(1:4, function(x) expected_hsa[x, , drop = FALSE])
    names(expected_hsa) <- keys

    expect_equal(hsa, expected_hsa)
})

test_that("get_hsa_data() returns NULL if no HSA is found.", {
    hsa <- get_hsa_data("chunk:0:0:0:57", db = db)
    expect_equal(hsa, list("chunk:0:0:0:57" = NULL))
})

test_that("get_hsa_value() can read a single HSA data.", {
    hsa <- get_hsa_value(36, 15, 0, db = db)

    expected_hsa <- cbind(
        x1 = 577L, y1 = 65L, z1 = 241L,
        x2 = 591L, y2 = 86L, z2 = 255L,
        tag = 5L
    )
    expect_equal(hsa, expected_hsa)
})

test_that("get_hsa_value() returns an empty table if no HSA is found.", {
    hsa <- get_hsa_value("chunk:0:0:0:57", db = db)
    expect_null(hsa)
})

test_that("put_hsa_data() can write HSA data.", {
    keys <- c("chunk:36:15:1:57", "chunk:36:16:1:57", "chunk:37:15:1:57",
              "chunk:37:16:1:57")

    expected_hsa <- cbind(
        x1 = c(577L, 577L, 592L, 592L),
        y1 = c(65L, 65L, 65L, 65L),
        z1 = c(241L, 256L, 241L, 256L),
        x2 = c(591, 591, 592, 592),
        y2 = c(86L, 86L, 86L, 86L),
        z2 = c(255L, 256L, 255L, 256L),
        tag = c(5L, 5L, 5L, 5L)
    )
    expected_hsa <- lapply(1:4, function(x) expected_hsa[x, , drop = FALSE])
    names(expected_hsa) <- keys

    put_hsa_data(expected_hsa, db = db)

    new_hsa <- get_hsa_data(keys, db = db)

    expect_equal(new_hsa, expected_hsa)
})

test_that("put_hsa_value() can write HSA data.", {
    expected_hsa <- cbind(
        x1 = 577L, y1 = 65L, z1 = 241L,
        x2 = 591L, y2 = 86L, z2 = 255L,
        tag = 5L
    )
    put_hsa_value(expected_hsa, 36, 15, 2, db = db)

    hsa <- get_hsa_value(36, 15, 2, db = db)

    expect_equal(hsa, expected_hsa)
})

# clean up
close(db)
unlink(dbpath, recursive = TRUE)
