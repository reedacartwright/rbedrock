dbpath <- rbedrock_example_world("example1.mcworld")
db <- bedrockdb(dbpath)

test_that("get_keys returns all keys", {
    keys <- get_keys(db=db)
    expect_equal(length(keys), 1136L)

    keys2 <- get_keys(db)
    expect_equal(keys, keys2)
})

test_that("get_keys returns all keys with a prefix", {
    keys <- get_keys(db = db)
    pre_str <- get_keys("plain:VILLAGE", db = db)
    expect_equal(pre_str, grep("^plain:VILLAGE", keys, value=TRUE))
    pre_chunk <- get_keys(key_prefix("chunk:37:6:0"), db = db)
    expect_equal(pre_chunk, grep("^chunk:37:6:0", keys, value=TRUE))
})

test_that("get_value returns a single, raw value",{
    expect_equal(get_value("chunk:31:6:0:44", db=db), as.raw(0x15))
    expect_equal(get_value("plain:Overworld", db=db), as.raw(c(
        0x0a, 0x00, 0x00, 0x0a, 0x04, 0x00, 0x64, 0x61,
        0x74, 0x61, 0x09, 0x0d, 0x00, 0x4c, 0x69, 0x6d,
        0x62, 0x6f, 0x45, 0x6e, 0x74, 0x69, 0x74, 0x69,
        0x65, 0x73, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00
    )))
    expect_null(get_value("plain:MissingKey", db=db))
})

test_that("get_value requires a scalar character argument",{
    expect_error(get_value(NULL, db=db), class="vctrs_error_scalar_type")
    expect_error(get_value(1L, db=db), class="vctrs_error_assert_ptype")
    expect_error(get_value(as.raw(1L), db=db), class="vctrs_error_assert_ptype")
    expect_error(get_value(c("hello","world"), db=db), class="vctrs_error_assert_size")
})

test_that("get_data returns a list of raw values", {
    expect_equal(get_data(c("chunk:31:6:0:44","plain:Overworld","plain:NotFound"), db=db),
        list("chunk:31:6:0:44" = as.raw(0x15), "plain:Overworld" = as.raw(c(
        0x0a, 0x00, 0x00, 0x0a, 0x04, 0x00, 0x64, 0x61,
        0x74, 0x61, 0x09, 0x0d, 0x00, 0x4c, 0x69, 0x6d,
        0x62, 0x6f, 0x45, 0x6e, 0x74, 0x69, 0x74, 0x69,
        0x65, 0x73, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00
    )), "plain:NotFound" = NULL))
})

test_that("get_data returns all keys with a prefix", {
    keys <- get_keys(db = db)
    pre_str <- get_data(key_prefix("plain:VILLAGE"), db=db)
    expect_equal(names(pre_str), grep("^plain:VILLAGE", keys, value=TRUE))
    pre_chunk <- get_data(key_prefix("chunk:37:6:0"), db=db)
    expect_equal(names(pre_chunk), grep("^chunk:37:6:0", keys, value=TRUE))
    pre_chunk <- get_data(key_prefix("chunk:37:6:0:47"), db=db)
    expect_equal(names(pre_chunk), grep("^chunk:37:6:0:47", keys, value=TRUE))
})

test_that("has_values returns a logical vector", {
    expect_equal(has_values(c("chunk:31:6:0:44","plain:Overworld","plain:NotFound"), db=db),
        c("chunk:31:6:0:44" = TRUE, "plain:Overworld" = TRUE, "plain:NotFound" = FALSE))
})

test_that("put_value writes data into the db", {
    put_value(charToRaw("Hello World"), "plain:Test%01", db=db)
    put_result <- get_value("plain:Test%01", db=db)
    expect_equal(put_result, charToRaw("Hello World"), db=db)
    
    put_value(as.raw(15), "chunk:0:0:0:44", db=db)
    put_result <- get_value("chunk:0:0:0:44", db=db)
    expect_equal(put_result, as.raw(15))
    
    put_value(charToRaw("Hello New World"), "plain:Test%01", db=db)
    put_result <- get_value("plain:Test%01", db=db)
    expect_equal(put_result, charToRaw("Hello New World"))
})

test_that("put_value throws errors are incorrect arguments",{
    expect_error(put_value(raw(1L), NULL, db=db), class="vctrs_error_scalar_type")
    expect_error(put_value(raw(1L), 1L, db=db), class="vctrs_error_assert_ptype")
    expect_error(put_value(raw(1L), as.raw(1L), db=db), class="vctrs_error_assert_ptype")
    expect_error(put_value(raw(1L), c("plain:hello","plain:world"), db=db), class="vctrs_error_assert_size")
    expect_error(put_value("hello Error", "plain:Test%02", db=db), "expected raw")
})

test_that("put_data writes data into the db", {
    put_data(list("plain:Test%01" = as.raw(0x1), "plain:Test%02" = as.raw(0x2)), db = db)
    put_result <- get_data(c("plain:Test%01", "plain:Test%02"), db = db)
    expect_equal(put_result, list("plain:Test%01" = as.raw(0x1), "plain:Test%02" = as.raw(0x2)))

    put_data(list(as.raw(0x10), as.raw(0x20)), c("plain:Test%01", "plain:Test%02"), db = db)
    put_result <- get_data(c("plain:Test%01", "plain:Test%02"), db = db)
    expect_equal(put_result, list("plain:Test%01" = as.raw(0x10), "plain:Test%02" = as.raw(0x20)))
})

test_that("delete_values removes data from the db", {
    put_data(list("plain:Test%01" = as.raw(0x1), "plain:Test%02" = as.raw(0x2)), db = db)
    delete_ret <- delete_values(c("plain:Test%01", "plain:Test%02"), db = db)
    expect_null(delete_ret)
    delete_result <- get_data(c("plain:Test%01", "plain:Test%02"), db = db)
    expect_equal(delete_result, list("plain:Test%01" = NULL, "plain:Test%02" = NULL))
    put_data(list("plain:Test%01" = as.raw(0x1), "plain:Test%02" = as.raw(0x2)), db = db)
    delete_ret <- delete_values(c("plain:Test%01", "plain:Test%03","plain:Test%02"), db = db, report=TRUE)
    expect_equal(delete_ret, c(TRUE, FALSE, TRUE))
})

# clean up
close(db)
fs::dir_delete(dbpath)

