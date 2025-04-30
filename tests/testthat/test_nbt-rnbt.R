# tags <- list(
#     end = 0L, byte = 1L, short = 2L, int = 3L, long = 4L,
#     float = 5L, double = 6L, byte_array = 7L, string = 8L,
#     list = 9L, compound = 10L, int_array = 11L, long_array = 12L
# )

test_that("read_rnbt() can read bytes", {
    # little endian format
    rawval <- as_raw_le(b = 1L, "pos", b = 100,
                        b = 1L, "neg", b = 254)
    expect_equal(read_rnbt(rawval), list(
        list(name = "pos", type = 1L, value = 100L),
        list(name = "neg", type = 1L, value = -2L)
    ))

    # big endian format
    rawval <- as_raw_be(b = 1L, "pos", b = 100,
                        b = 1L, "neg", b = 254)
    expect_equal(read_rnbt(rawval, "big"), list(
        list(name = "pos", type = 1L, value = 100L),
        list(name = "neg", type = 1L, value = -2L)
    ))

    # network format
    rawval <- as_raw_lv(b = 1L, "pos", b = 100,
                        b = 1L, "neg", b = 254)
    expect_equal(read_rnbt(rawval, "network"), list(
        list(name = "pos", type = 1L, value = 100L),
        list(name = "neg", type = 1L, value = -2L)
    ))
})

test_that("read_rnbt() can read shorts", {
    # little endian format
    rawval <- as_raw_le(b = 2L, "pos", s = 100,
                        b = 2L, "neg", s = -2)
    expect_equal(read_rnbt(rawval), list(
        list(name = "pos", type = 2L, value = 100L),
        list(name = "neg", type = 2L, value = -2L)
    ))

    # big endian format
    rawval <- as_raw_be(b = 2L, "pos", s = 100,
                        b = 2L, "neg", s = -2)
    expect_equal(read_rnbt(rawval, "big"), list(
        list(name = "pos", type = 2L, value = 100L),
        list(name = "neg", type = 2L, value = -2L)
    ))

    # network format
    rawval <- as_raw_lv(b = 2L, "pos", s = 100,
                        b = 2L, "neg", s = -2)
    expect_equal(read_rnbt(rawval, "network"), list(
        list(name = "pos", type = 2L, value = 100L),
        list(name = "neg", type = 2L, value = -2L)
    ))
})

test_that("read_rnbt() can read ints", {
    # little endian format
    rawval <- as_raw_le(b = 3L, "pos", 100,
                        b = 3L, "neg", -2)
    expect_equal(read_rnbt(rawval), list(
        list(name = "pos", type = 3L, value = 100L),
        list(name = "neg", type = 3L, value = -2L)
    ))

    # big endian format
    rawval <- as_raw_be(b = 3L, "pos", 100,
                        b = 3L, "neg", -2)
    expect_equal(read_rnbt(rawval, "big"), list(
        list(name = "pos", type = 3L, value = 100L),
        list(name = "neg", type = 3L, value = -2L)
    ))

    # network format
    rawval <- as_raw_lv(b = 3L, "pos", 100,
                        b = 3L, "neg", -2)
    expect_equal(read_rnbt(rawval, "network"), list(
        list(name = "pos", type = 3L, value = 100L),
        list(name = "neg", type = 3L, value = -2L)
    ))

    # Check that INT_MIN isn't read as NA
    rawval_na <- as_raw_le(b = 3L, "na", b = c(0, 0, 0, 128))
    expect_equal(read_rnbt(rawval_na), list(list(
        name = "na", type = 3L,
        value = -2147483648
    )))
})

test_that("read_rnbt() can read longs", {
    # little endian format
    rawval <- as_raw_le(b = 4L, "pos", l = "8000000000000000001",
                        b = 4L, "neg", l = "-8000000000000000001")
    expect_equal(read_rnbt(rawval), list(
        list(name = "pos", type = 4L, value = "8000000000000000001"),
        list(name = "neg", type = 4L, value = "-8000000000000000001")
    ))

    # big endian format
    rawval <- as_raw_be(b = 4L, "pos", l = "8000000000000000001",
                        b = 4L, "neg", l = "-8000000000000000001")
    expect_equal(read_rnbt(rawval, "big"), list(
        list(name = "pos", type = 4L, value = "8000000000000000001"),
        list(name = "neg", type = 4L, value = "-8000000000000000001")
    ))

    # network format
    # Note: Using a smaller value here prevent integer overflow issues.
    rawval <- as_raw_lv(b = 4L, "pos", l = "4000000000000000001",
                        b = 4L, "neg", l = "-4000000000000000001")
    expect_equal(read_rnbt(rawval, "network"), list(
        list(name = "pos", type = 4L, value = "4000000000000000001"),
        list(name = "neg", type = 4L, value = "-4000000000000000001")
    ))
})

test_that("read_rnbt() can read floats", {
    # little endian format
    rawval <- as_raw_le(b = 5L, "pos", f = 36.125,
                        b = 5L, "neg", f = -1776.75)
    expect_equal(read_rnbt(rawval), list(
        list(name = "pos", type = 5L, value = 36.125),
        list(name = "neg", type = 5L, value = -1776.75)
    ))

    # big endian format
    rawval <- as_raw_be(b = 5L, "pos", f = 36.125,
                        b = 5L, "neg", f = -1776.75)
    expect_equal(read_rnbt(rawval, "big"), list(
        list(name = "pos", type = 5L, value = 36.125),
        list(name = "neg", type = 5L, value = -1776.75)
    ))

    # network format
    rawval <- as_raw_lv(b = 5L, "pos", f = 36.125,
                        b = 5L, "neg", f = -1776.75)
    expect_equal(read_rnbt(rawval, "network"), list(
        list(name = "pos", type = 5L, value = 36.125),
        list(name = "neg", type = 5L, value = -1776.75)
    ))
})

test_that("read_rnbt() can read doubles", {
    # little endian format
    rawval <- as_raw_le(b = 6L, "pos", d = 36.125,
                        b = 6L, "neg", d = -1776.75)
    expect_equal(read_rnbt(rawval), list(
        list(name = "pos", type = 6L, value = 36.125),
        list(name = "neg", type = 6L, value = -1776.75)
    ))

    # big endian format
    rawval <- as_raw_be(b = 6L, "pos", d = 36.125,
                        b = 6L, "neg", d = -1776.75)
    expect_equal(read_rnbt(rawval, "big"), list(
        list(name = "pos", type = 6L, value = 36.125),
        list(name = "neg", type = 6L, value = -1776.75)
    ))

    # network format
    rawval <- as_raw_lv(b = 6L, "pos", d = 36.125,
                        b = 6L, "neg", d = -1776.75)
    expect_equal(read_rnbt(rawval, "network"), list(
        list(name = "pos", type = 6L, value = 36.125),
        list(name = "neg", type = 6L, value = -1776.75)
    ))
})

test_that("read_rnbt() can read byte arrays", {
    # little endian format
    rawval <- as_raw_le(b = 7L, "8val", 8L, b = 10:17,
                        b = 7L, "1val", 1L, b = 20)
    expect_equal(read_rnbt(rawval), list(
        list(name = "8val", type = 7L, value = 10:17),
        list(name = "1val", type = 7L, value = 20)
    ))

    # big endian format
    rawval <- as_raw_be(b = 7L, "8val", 8L, b = 10:17,
                        b = 7L, "1val", 1L, b = 20)
    expect_equal(read_rnbt(rawval, "big"), list(
        list(name = "8val", type = 7L, value = 10:17),
        list(name = "1val", type = 7L, value = 20)
    ))

    # network format
    rawval <- as_raw_lv(b = 7L, "8val", 8L, b = 10:17,
                        b = 7L, "1val", 1L, b = 20)
    expect_equal(read_rnbt(rawval, "network"), list(
        list(name = "8val", type = 7L, value = 10:17),
        list(name = "1val", type = 7L, value = 20)
    ))
})

test_that("read_rnbt() can read strings", {
    # little endian format
    rawval <- as_raw_le(b = 8L, "test", "hello world",
                        b = 8L, "empty", "",
                        b = 8L, "greek", "\u03B1\u03B2\u03B3",
                        b = 8L, "raw", s = 3, b = c(97, 0, 98))
    expect_equal(read_rnbt(rawval), list(
        list(name = "test",  type = 8L, value = "hello world"),
        list(name = "empty", type = 8L, value = ""),
        list(name = "greek", type = 8L, value = "\u03B1\u03B2\u03B3"),
        list(name = "raw", type = 8L, value = as.raw(c(97, 0, 98)))
    ))

    # big endian format
    rawval <- as_raw_be(b = 8L, "test", "hello world",
                        b = 8L, "empty", "",
                        b = 8L, "greek", "\u03B1\u03B2\u03B3",
                        b = 8L, "raw", s = 3, b = c(97, 0, 98))
    expect_equal(read_rnbt(rawval, "big"), list(
        list(name = "test",  type = 8L, value = "hello world"),
        list(name = "empty", type = 8L, value = ""),
        list(name = "greek", type = 8L, value = "\u03B1\u03B2\u03B3"),
        list(name = "raw", type = 8L, value = as.raw(c(97, 0, 98)))
    ))

    # network format
    # NOTE: The "raw" string needs its width directly specified
    rawval <- as_raw_lv(b = 8L, "test", "hello world",
                        b = 8L, "empty", "",
                        b = 8L, "greek", "\u03B1\u03B2\u03B3",
                        b = 8L, "raw", b = c(3, 97, 0, 98))
    expect_equal(read_rnbt(rawval, "network"), list(
        list(name = "test",  type = 8L, value = "hello world"),
        list(name = "empty", type = 8L, value = ""),
        list(name = "greek", type = 8L, value = "\u03B1\u03B2\u03B3"),
        list(name = "raw", type = 8L, value = as.raw(c(97, 0, 98)))
    ))
})

test_that("read_rnbt() can read int arrays", {
    # little endian format
    rawval <- as_raw_le(b = 11L, "8val", 8L, i = 10:17,
                        b = 11L, "1val", 1L, i = 20)
    expect_equal(read_rnbt(rawval), list(
        list(name = "8val", type = 11L, value = 10:17),
        list(name = "1val", type = 11L, value = 20)
    ))

    # big endian format
    rawval <- as_raw_be(b = 11L, "8val", 8L, i = 10:17,
                        b = 11L, "1val", 1L, i = 20)
    expect_equal(read_rnbt(rawval, "big"), list(
        list(name = "8val", type = 11L, value = 10:17),
        list(name = "1val", type = 11L, value = 20)
    ))

    # network format
    rawval <- as_raw_lv(b = 11L, "8val", 8L, i = 10:17,
                        b = 11L, "1val", 1L, i = 20)
    expect_equal(read_rnbt(rawval, "network"), list(
        list(name = "8val", type = 11L, value = 10:17),
        list(name = "1val", type = 11L, value = 20)
    ))
})

test_that("read_rnbt() can read long arrays", {
    # little endian format
    rawval <- as_raw_le(b = 12L, "8val", 8L, l = 10:17,
                        b = 12L, "1val", 1L, l = 20)
    expect_equal(read_rnbt(rawval), list(
        list(name = "8val", type = 12L, value = as.character(10:17)),
        list(name = "1val", type = 12L, value = "20")
    ))

    # big endian format
    rawval <- as_raw_be(b = 12L, "8val", 8L, l = 10:17,
                        b = 12L, "1val", 1L, l = 20)
    expect_equal(read_rnbt(rawval, "big"), list(
        list(name = "8val", type = 12L, value = as.character(10:17)),
        list(name = "1val", type = 12L, value = "20")
    ))

    # network format
    rawval <- as_raw_lv(b = 12L, "8val", 8L, l = 10:17,
                        b = 12L, "1val", 1L, l = 20)
    expect_equal(read_rnbt(rawval, "network"), list(
        list(name = "8val", type = 12L, value = as.character(10:17)),
        list(name = "1val", type = 12L, value = "20")
    ))
})

test_that("read_rnbt() can read compound values", {
    # little endian format
    rawval <- as_raw_le(b = 10L, "test",
                        b =  1L, "a", b = 1,
                        b =  3L, "b", i = 2,
                        b =  0L)
    expect_equal(read_rnbt(rawval), list(
        list(name = "test", type = 10L, value = list(
            list(name = "a", type = 1L, value = 1),
            list(name = "b", type = 3L, value = 2)
        ))
    ))
    # big endian format
    rawval <- as_raw_be(b = 10L, "test",
                        b =  1L, "a", b = 1,
                        b =  3L, "b", i = 2,
                        b =  0L)
    expect_equal(read_rnbt(rawval, "big"), list(
        list(name = "test", type = 10L, value = list(
            list(name = "a", type = 1L, value = 1),
            list(name = "b", type = 3L, value = 2)
        ))
    ))

    # network format
    rawval <- as_raw_lv(b = 10L, "test",
                        b =  1L, "a", b = 1,
                        b =  3L, "b", i = 2,
                        b =  0L)
    expect_equal(read_rnbt(rawval, "network"), list(
        list(name = "test", type = 10L, value = list(
            list(name = "a", type = 1L, value = 1),
            list(name = "b", type = 3L, value = 2)
        ))
    ))

    # missing end byte
    rawval_noend <- as_raw_le(b = 10L, "test",
                        b =  1L, "a", b = 1,
                        b =  3L, "b", i = 2
                    )
    expect_error(read_rnbt(rawval))
})

test_that("read_rnbt() can read empty list values", {
    # little endian format
    rawval <- as_raw_le(b = 9L, "empty", b = 0L, 0L)
    expect_equal(read_rnbt(rawval), list(
        list(name = "empty", type = 100L, value = list())
    ))

    # big endian format
    rawval <- as_raw_be(b = 9L, "empty", b = 0L, 0L)
    expect_equal(read_rnbt(rawval, "big"), list(
        list(name = "empty", type = 100L, value = list())
    ))

    # network format
    rawval <- as_raw_lv(b = 9L, "empty", b = 0L, 0L)
    expect_equal(read_rnbt(rawval, "network"), list(
        list(name = "empty", type = 100L, value = list())
    ))

    # error if an empty list has length
    rawval <- as_raw_le(b = 9L, "empty", b = 0L, 1L)
    expect_error(read_rnbt(rawval))
})

test_that("read_rnbt() can read lists of bytes", {
    # little endian format
    rawval <- as_raw_le(b = 9L, "a", b = 1L, 2L, b = 1:2,
                        b = 9L, "b", b = 1L, 1L, b = -3)
    expect_equal(read_rnbt(rawval), list(
        list(name = "a", type = 101L, value = 1:2),
        list(name = "b", type = 101L, value = -3)
    ))

    # big endian format
    rawval <- as_raw_be(b = 9L, "a", b = 1L, 2L, b = 1:2,
                        b = 9L, "b", b = 1L, 1L, b = -3)
    expect_equal(read_rnbt(rawval, "big"), list(
        list(name = "a", type = 101L, value = 1:2),
        list(name = "b", type = 101L, value = -3)
    ))

    # network format
    rawval <- as_raw_lv(b = 9L, "a", b = 1L, 2L, b = 1:2,
                        b = 9L, "b", b = 1L, 1L, b = -3)
    expect_equal(read_rnbt(rawval, "network"), list(
        list(name = "a", type = 101L, value = 1:2),
        list(name = "b", type = 101L, value = -3)
    ))
})

test_that("read_rnbt() can read lists of shorts", {
    # little endian format
    rawval <- as_raw_le(b = 9L, "a", b = 2L, 2L, s = 1:2,
                        b = 9L, "b", b = 2L, 1L, s = -3)
    expect_equal(read_rnbt(rawval), list(
        list(name = "a", type = 102L, value = 1:2),
        list(name = "b", type = 102L, value = -3)
    ))

    # big endian format
    rawval <- as_raw_be(b = 9L, "a", b = 2L, 2L, s = 1:2,
                        b = 9L, "b", b = 2L, 1L, s = -3)
    expect_equal(read_rnbt(rawval, "big"), list(
        list(name = "a", type = 102L, value = 1:2),
        list(name = "b", type = 102L, value = -3)
    ))

    # network format
    rawval <- as_raw_lv(b = 9L, "a", b = 2L, 2L, s = 1:2,
                        b = 9L, "b", b = 2L, 1L, s = -3)
    expect_equal(read_rnbt(rawval, "network"), list(
        list(name = "a", type = 102L, value = 1:2),
        list(name = "b", type = 102L, value = -3)
    ))
})

test_that("read_rnbt() can read lists of ints", {
    # little endian format
    rawval <- as_raw_le(b = 9L, "a", b = 3L, 2L, i = 1:2,
                        b = 9L, "b", b = 3L, 1L, i = -3)
    expect_equal(read_rnbt(rawval), list(
        list(name = "a", type = 103L, value = 1:2),
        list(name = "b", type = 103L, value = -3)
    ))

    # big endian format
    rawval <- as_raw_be(b = 9L, "a", b = 3L, 2L, i = 1:2,
                        b = 9L, "b", b = 3L, 1L, i = -3)
    expect_equal(read_rnbt(rawval, "big"), list(
        list(name = "a", type = 103L, value = 1:2),
        list(name = "b", type = 103L, value = -3)
    ))

    # network format
    rawval <- as_raw_lv(b = 9L, "a", b = 3L, 2L, i = 1:2,
                        b = 9L, "b", b = 3L, 1L, i = -3)
    expect_equal(read_rnbt(rawval, "network"), list(
        list(name = "a", type = 103L, value = 1:2),
        list(name = "b", type = 103L, value = -3)
    ))
})

test_that("read_rnbt() can read lists of longs", {
    # little endian format
    rawval <- as_raw_le(b = 9L, "a", b = 4L, 2L, l = 1:2,
                        b = 9L, "b", b = 4L, 1L, l = -3)
    expect_equal(read_rnbt(rawval), list(
        list(name = "a", type = 104L, value = c("1", "2")),
        list(name = "b", type = 104L, value = "-3")
    ))

    # big endian format
    rawval <- as_raw_be(b = 9L, "a", b = 4L, 2L, l = 1:2,
                        b = 9L, "b", b = 4L, 1L, l = -3)
    expect_equal(read_rnbt(rawval, "big"), list(
        list(name = "a", type = 104L, value = c("1", "2")),
        list(name = "b", type = 104L, value = "-3")
    ))

    # network format
    rawval <- as_raw_lv(b = 9L, "a", b = 4L, 2L, l = 1:2,
                        b = 9L, "b", b = 4L, 1L, l = -3)
    expect_equal(read_rnbt(rawval, "network"), list(
        list(name = "a", type = 104L, value = c("1", "2")),
        list(name = "b", type = 104L, value = "-3")
    ))
})

test_that("read_rnbt() can read lists of floats", {
    # little endian format
    rawval <- as_raw_le(b = 9L, "a", b = 5L, 2L, f = 1:2,
                        b = 9L, "b", b = 5L, 1L, f = -3)
    expect_equal(read_rnbt(rawval), list(
        list(name = "a", type = 105L, value = 1:2),
        list(name = "b", type = 105L, value = -3)
    ))

    # big endian format
    rawval <- as_raw_be(b = 9L, "a", b = 5L, 2L, f = 1:2,
                        b = 9L, "b", b = 5L, 1L, f = -3)
    expect_equal(read_rnbt(rawval, "big"), list(
        list(name = "a", type = 105L, value = 1:2),
        list(name = "b", type = 105L, value = -3)
    ))

    # network format
    rawval <- as_raw_lv(b = 9L, "a", b = 5L, 2L, f = 1:2,
                        b = 9L, "b", b = 5L, 1L, f = -3)
    expect_equal(read_rnbt(rawval, "network"), list(
        list(name = "a", type = 105L, value = 1:2),
        list(name = "b", type = 105L, value = -3)
    ))
})

test_that("read_rnbt() can read lists of doubles", {
    # little endian format
    rawval <- as_raw_le(b = 9L, "a", b = 6L, 2L, d = 1:2,
                        b = 9L, "b", b = 6L, 1L, d = -3)
    expect_equal(read_rnbt(rawval), list(
        list(name = "a", type = 106L, value = 1:2),
        list(name = "b", type = 106L, value = -3)
    ))

    # big endian format
    rawval <- as_raw_be(b = 9L, "a", b = 6L, 2L, d = 1:2,
                        b = 9L, "b", b = 6L, 1L, d = -3)
    expect_equal(read_rnbt(rawval, "big"), list(
        list(name = "a", type = 106L, value = 1:2),
        list(name = "b", type = 106L, value = -3)
    ))

    # network format
    rawval <- as_raw_lv(b = 9L, "a", b = 6L, 2L, d = 1:2,
                        b = 9L, "b", b = 6L, 1L, d = -3)
    expect_equal(read_rnbt(rawval, "network"), list(
        list(name = "a", type = 106L, value = 1:2),
        list(name = "b", type = 106L, value = -3)
    ))
})

test_that("read_rnbt() can read lists of byte arrays", {
    # little endian format
    rawval <- as_raw_le(b = 9L, "a", b = 7L, 2L,
                        2, b = 1:2, 1, b = -3
                        )
    expect_equal(read_rnbt(rawval), list(
        list(name = "a", type = 107L, value = list(1:2, -3))
    ))

    # big endian format
    rawval <- as_raw_be(b = 9L, "a", b = 7L, 2L,
                        2, b = 1:2, 1, b = -3
                        )
    expect_equal(read_rnbt(rawval, "big"), list(
        list(name = "a", type = 107L, value = list(1:2, -3))
    ))

    # network format
    rawval <- as_raw_lv(b = 9L, "a", b = 7L, 2L,
                        2, b = 1:2, 1, b = -3
                        )
    expect_equal(read_rnbt(rawval, "network"), list(
        list(name = "a", type = 107L, value = list(1:2, -3))
    ))
})

test_that("read_rnbt() can read lists of strings", {
    # little endian format
    rawval <- as_raw_le(b = 9, "abc", b = 8, 3, "a", "b", "c",
                        b = 9, "raw", b = 8, 1, s = 3, b = c(97, 0, 98),
                        b = 9, "lst", b = 8, 2, "abc", s = 3, b = c(97, 0, 98)
        )
    expect_equal(read_rnbt(rawval), list(
        list(name = "abc", type = 108L, value = c("a", "b", "c")),
        list(name = "raw", type = 108L, value = as.raw(c(97, 0, 98))),
        list(name = "lst", type = 108L, value = list(
            charToRaw("abc"), as.raw(c(97, 0, 98))))

    ))

    # big endian format
    rawval <- as_raw_be(b = 9, "abc", b = 8, 3, "a", "b", "c",
                        b = 9, "raw", b = 8, 1, s = 3, b = c(97, 0, 98),
                        b = 9, "lst", b = 8, 2, "abc", s = 3, b = c(97, 0, 98)
        )
    expect_equal(read_rnbt(rawval, "big"), list(
        list(name = "abc", type = 108L, value = c("a", "b", "c")),
        list(name = "raw", type = 108L, value = as.raw(c(97, 0, 98))),
        list(name = "lst", type = 108L, value = list(
            charToRaw("abc"), as.raw(c(97, 0, 98))))

    ))

    # network format
    rawval <- as_raw_lv(b = 9, "abc", b = 8, 3, "a", "b", "c",
                        b = 9, "raw", b = 8, 1, b = c(3, 97, 0, 98),
                        b = 9, "lst", b = 8, 2, "abc", b = c(3, 97, 0, 98)
        )
    expect_equal(read_rnbt(rawval, "network"), list(
        list(name = "abc", type = 108L, value = c("a", "b", "c")),
        list(name = "raw", type = 108L, value = as.raw(c(97, 0, 98))),
        list(name = "lst", type = 108L, value = list(
            charToRaw("abc"), as.raw(c(97, 0, 98))))

    ))
})

test_that("read_rnbt() can read lists of int arrays", {
    # little endian format
    rawval <- as_raw_le(b = 9L, "a", b = 11L, 2L,
                        2, i = 1:2, 1, i = -3
                        )
    expect_equal(read_rnbt(rawval), list(
        list(name = "a", type = 111L, value = list(1:2, -3))
    ))

    # big endian format
    rawval <- as_raw_be(b = 9L, "a", b = 11L, 2L,
                        2, i = 1:2, 1, i = -3
                        )
    expect_equal(read_rnbt(rawval, "big"), list(
        list(name = "a", type = 111L, value = list(1:2, -3))
    ))

    # network format
    rawval <- as_raw_lv(b = 9L, "a", b = 11L, 2L,
                        2, i = 1:2, 1, i = -3
                        )
    expect_equal(read_rnbt(rawval, "network"), list(
        list(name = "a", type = 111L, value = list(1:2, -3))
    ))
})

test_that("read_rnbt() can read lists of long arrays", {
    # little endian format
    rawval <- as_raw_le(b = 9L, "a", b = 12L, 2L,
                        2, l = 1:2, 1, l = -3
                        )
    expect_equal(read_rnbt(rawval), list(
        list(name = "a", type = 112L, value = list(c("1", "2"), "-3"))
    ))

    # big endian format
    rawval <- as_raw_be(b = 9L, "a", b = 12L, 2L,
                        2, l = 1:2, 1, l = -3
                        )
    expect_equal(read_rnbt(rawval, "big"), list(
        list(name = "a", type = 112L, value = list(c("1", "2"), "-3"))
    ))

    # network format
    rawval <- as_raw_lv(b = 9L, "a", b = 12L, 2L,
                        2, l = 1:2, 1, l = -3
                        )
    expect_equal(read_rnbt(rawval, "network"), list(
        list(name = "a", type = 112L, value = list(c("1", "2"), "-3"))
    ))
})

test_that("read_rnbt() can read lists of compounds", {
    # little endian format
    rawval <- as_raw_le(b = 9, "cmplst", b = 10, 2,
                        b = 1, "a", b = 10, b = 0,
                        b = 1, "x", b = 20,
                        b = 1, "y", b = 21, b = 0
                        )
    expect_equal(read_rnbt(rawval), list(
        list(name = "cmplst", type = 110L, value = list(
            list(list(name = "a", type = 1, value = 10)),
            list(list(name = "x", type = 1, value = 20),
                 list(name = "y", type = 1, value = 21))
        ))
    ))

    # big endian format
    rawval <- as_raw_be(b = 9, "cmplst", b = 10, 2,
                        b = 1, "a", b = 10, b = 0,
                        b = 1, "x", b = 20,
                        b = 1, "y", b = 21, b = 0
                        )
    expect_equal(read_rnbt(rawval, "big"), list(
        list(name = "cmplst", type = 110L, value = list(
            list(list(name = "a", type = 1, value = 10)),
            list(list(name = "x", type = 1, value = 20),
                 list(name = "y", type = 1, value = 21))
        ))
    ))

    # network format
    rawval <- as_raw_lv(b = 9, "cmplst", b = 10, 2,
                        b = 1, "a", b = 10, b = 0,
                        b = 1, "x", b = 20,
                        b = 1, "y", b = 21, b = 0
                        )
    expect_equal(read_rnbt(rawval, "network"), list(
        list(name = "cmplst", type = 110L, value = list(
            list(list(name = "a", type = 1, value = 10)),
            list(list(name = "x", type = 1, value = 20),
                 list(name = "y", type = 1, value = 21))
        ))
    ))
})

test_that("read_rnbt() can read lists of lists", {
    # little endian format
    rawval <- as_raw_le(b = 9, "lstlst", b = 9, 2,
                        b = 1, 1, b = 1,
                        b = 2, 2, s = -1:0
                        )
    expect_equal(read_rnbt(rawval), list(
        list(name = "lstlst", type = 109L, value = list(
            list(type = 101, value = 1),
            list(type = 102, value = -1:0)
        ))
    ))

    # big endian format
    rawval <- as_raw_be(b = 9, "lstlst", b = 9, 2,
                        b = 1, 1, b = 1,
                        b = 2, 2, s = -1:0
                        )
    expect_equal(read_rnbt(rawval, "big"), list(
        list(name = "lstlst", type = 109L, value = list(
            list(type = 101, value = 1),
            list(type = 102, value = -1:0)
        ))
    ))

    # network format
    rawval <- as_raw_lv(b = 9, "lstlst", b = 9, 2,
                        b = 1, 1, b = 1,
                        b = 2, 2, s = -1:0
                        )
    expect_equal(read_rnbt(rawval, "network"), list(
        list(name = "lstlst", type = 109L, value = list(
            list(type = 101, value = 1),
            list(type = 102, value = -1:0)
        ))
    ))
})

test_that("read_rnbt() throws errors on malformed values", {
    rawval <- as_raw_le(b = 10, "",
                  b = 1, "A", b = 0,
                  b = 7, "BCD", 3, b = 1:3,
                  b = 1, "", b = 2,
                  b = 0)
    expect_silent(read_rnbt(rawval))
    for (i in seq.int(length(rawval) - 1)) {
        expect_error(read_rnbt(rawval[1:!!i]))
    }
})
