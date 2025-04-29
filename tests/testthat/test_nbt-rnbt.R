tags <- list(
    end = 0L, byte = 1L, short = 2L, int = 3L, long = 4L,
    float = 5L, double = 6L, byte_array = 7L, string = 8L,
    list = 9L, compound = 10L, int_array = 11L, long_array = 12L
)

rawstr <- function(x) {
    n <- nchar(x, type = "bytes")
    r <- writeBin(n, raw(), size = 2, endian = "little")
    as_raw(r, charToRaw(x))
}

test_that("read_rnbt can read bytes", {
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
    rawval <- as_raw(tags$byte, 3, charToRaw("pos"), 100,
        tags$byte, 3, charToRaw("neg"), 254)
    expect_equal(read_rnbt(rawval, "network"), list(
        list(name = "pos", type = 1L, value = 100L),
        list(name = "neg", type = 1L, value = -2L)
    ))
})

test_that("read_rnbt can read shorts", {
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
})

test_that("read_nbt can read ints", {
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

    # Check that INT_MIN isn't read as NA
    rawval_na <- as_raw_le(b = 3L, "na", b = c(0, 0, 0, 128))
    expect_equal(read_rnbt(rawval_na), list(list(
        name = "na", type = 3L,
        value = -2147483648
    )))
})

test_that("read_nbt can read longs", {
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
})

test_that("read_nbt can read floats", {
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
})

test_that("read_nbt can read doubles", {
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
})

test_that("read_nbt can read byte arrays", {
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
})

test_that("read_rnbt can read strings", {
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

    # list of strings
    v <- c("a", "b", "c")
    dat <- as_raw(
        tags$list, rawstr("test"), tags$string, 3, 0, 0, 0,
        rawstr(v[1]), rawstr(v[2]), rawstr(v[3])
    )
    expect_equal(read_rnbt(dat), list(list(
        name = "test", type = 108L, value = v
    )))

    # replace b with null
    dat[18] <- as.raw(0L)
    expect_equal(read_rnbt(dat), list(list(
        name = "test", type = 108L,
        value = list(charToRaw("a"), as.raw(0L), charToRaw("c"))
    )))
})

test_that("read_rnbt can read int arrays", {
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
})

test_that("read_nbt can read long arrays", {
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
})

test_that("read_nbt can read list values", {
    pos <- c(-25.25, 0, 100.5)
    dat <- as_raw(
        tags$list, rawstr("test"), tags$float, 3, 0, 0, 0,
        writeBin(pos, con = raw(), size = 4L, endian = "little")
    )
    expect_equal(read_rnbt(dat), list(list(
        name = "test", type = 9L,
        value = pos, type = 5L
    )))
    expect_equal(
        read_nbt(dat),
        list(test = nbt_list(pos, .type = 5L))
    )

    empty_dat <- as_raw(tags$list, rawstr("test"), tags$end, 0, 0, 0, 0)
    expect_equal(read_rnbt(empty_dat), list(list(
        name = "test", type = 9L,
        value = list(), type = 0L
    )))
    expect_equal(read_nbt(empty_dat), list(test = nbt_list()))

    # List of lists of lists
    dat <- as_raw(
        tags$list, rawstr("test"), tags$list, 2, 0, 0, 0,
        tags$byte, 3, 0, 0, 0, 0, 1, 3,
        tags$int, 1, 0, 0, 0, 10, 0, 0, 0
    )
    rnbt_dat <- list(list(
        name = "test", type = 9L,
        value = list(
            list(value = c(0, 1, 3), type = 1L),
            list(value = 10, type = 3L)
        ),
        type = 9L
    ))

    expect_equal(read_rnbt(dat), rnbt_dat)
    expect_equal(write_rnbt(rnbt_dat), dat)
})

test_that("read_nbt can read compound values", {
    dat <- as_raw(
        tags$compound, rawstr("test"),
        tags$byte, rawstr("A"), 0,
        tags$short, rawstr("B"), 1, 0,
        tags$end
    )
    expect_equal(read_rnbt(dat), list(list(
        name = "test", type = 10L,
        value = list(
            list(name = "A", type = 1L, value = 0L),
            list(name = "B", type = 2L, value = 1L)
        )
    )))
    expect_equal(read_nbt(dat), list(test = nbt_compound(
        A = nbt_byte(0),
        B = nbt_short(1)
    )))

    # list of compounds
    dat <- as_raw(
        tags$list, rawstr("test"), tags$compound, 1, 0, 0, 0,
        tags$byte, rawstr("A"), 0,
        tags$short, rawstr("B"), 1, 0,
        tags$end
    )
    expect_equal(read_rnbt(dat), list(list(
        name = "test", type = 9L,
        value = list(list(
            list(name = "A", type = 1L, value = 0L),
            list(name = "B", type = 2L, value = 1L)
        )),
        type = 10L
    )))

})

test_that("read_rnbt throws errors on malformed values", {
    dat <- as_raw(
        tags$compound, rawstr(""),
        tags$byte, rawstr("A"), 0,
        tags$byte_array, rawstr("BCD"), 3, 0, 0, 0, 1, 2, 3,
        tags$byte, rawstr(""), 2,
        tags$end
    )
    expect_silent(read_rnbt(dat))
    for (i in seq.int(length(dat) - 1)) {
        expect_error(read_rnbt(dat[1:!!i]))
    }
})
