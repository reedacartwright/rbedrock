as_raw <- function(...) {
    as.raw(c(...))
}

rawstr <- function(x) {
   n <- nchar(x, type = "bytes")
   r <- writeBin(n, raw(), size = 2, endian = "little")
   as_raw(r, charToRaw(x))
}

tags <- list(
    end = 0L, byte = 1L, short = 2L, int = 3L, long = 4L,
    float = 5L, double = 6L, byte_array = 7L, string = 8L,
    list = 9L, compound = 10L, int_array = 11L, long_array = 12L
)

test_that("read_nbt can read bytes", {
    dat <- as_raw(tags$byte, rawstr("test"), 100)
    expect_equal(read_nbt(dat), list(test = nbt_byte(100)))
    neg_dat <- as_raw(tags$byte, rawstr("test"), 254)
    expect_equal(read_nbt(neg_dat), list(test = nbt_byte(-2)))
})

test_that("read_nbt can read shorts", {
    dat <- as_raw(tags$short, rawstr("test"), 100, 2)
    expect_equal(read_nbt(dat), list(test = nbt_short(612)))
    neg_dat <- as_raw(tags$short, rawstr("test"), 156, 255)
    expect_equal(read_nbt(neg_dat), list(test = nbt_short(-100L)))
})

test_that("read_nbt can read ints", {
    dat <- as_raw(tags$int, rawstr("test"), 100, 2, 3, 4)
    expect_equal(read_nbt(dat), list(test = nbt_int(67306084L)))
    neg_dat <- as_raw(tags$int, rawstr("test"), 156, 255, 255, 255)
    expect_equal(read_nbt(neg_dat), list(test = nbt_int(-100L)))
})

test_that("read_nbt can read longs", {
    dat <- as_raw(tags$long, rawstr("test"), 1, 0, 0x20, 0x3b, 0x9d, 0xb5, 0x05, 0x6f)
    val <- bit64::as.integer64("8000000000000000001")
    expect_equal(read_nbt(dat), list(test = nbt_long(val)))
    dat <- as_raw(tags$long, rawstr("test"), 0xff, 0xff, 0xdf, 0xc4, 0x62, 0x4a, 0xfa, 0x90)
    val <- bit64::as.integer64("-8000000000000000001")
    expect_equal(read_nbt(dat), list(test = nbt_long(val)))
})

test_that("read_nbt can read floats", {
    dat <- as_raw(tags$float, rawstr("test"), 0, 0, 0x10, 0x42)
    expect_equal(read_nbt(dat), list(test = nbt_float(36.0)))
})

test_that("read_nbt can read doubles", {
    dat <- as_raw(tags$double, rawstr("test"), 0, 0, 0, 0, 0, 0, 0x42, 0x40)
    expect_equal(read_nbt(dat), list(test = nbt_double(36.0)))
})

test_that("read_nbt can read byte arrays", {
    dat <- as_raw(tags$byte_array, rawstr("test"), 8, 0, 0, 0, 10, 11, 12, 13, 14, 15, 16, 17)
    expect_equal(read_nbt(dat), list(test = nbt_byte_array(10:17)))
    single_dat <- as_raw(tags$byte_array, rawstr("test"), 1, 0, 0, 0, 20)
    expect_equal(read_nbt(single_dat), list(test = nbt_byte_array(20)))
})

test_that("read_nbt can read strings",{
    dat <- as_raw(tags$string, rawstr("test"), rawstr("hello world"))
    expect_equal(read_nbt(dat), list(test = nbt_string("hello world")))
    blank_dat <- as_raw(tags$string, rawstr("test"), rawstr(""))
    expect_equal(read_nbt(blank_dat), list(test = nbt_string("")))
    greek_dat <- as_raw(tags$string, rawstr("test"), rawstr("\u03B1\u03B2\u03B3"))
    expect_equal(read_nbt(greek_dat), list(test = nbt_string("\u03B1\u03B2\u03B3")))
})

test_that("read_nbt can read int arrays", {
    dat <- as_raw(tags$int_array, rawstr("test"), 2, 0, 0, 0, 10, 0, 0, 0, 11, 0, 0, 0)
    expect_equal(read_nbt(dat), list(test = nbt_int_array(10:11)))
    single_dat <- as_raw(tags$int_array, rawstr("test"), 1, 0, 0, 0, 20, 0, 0, 0)
    expect_equal(read_nbt(single_dat), list(test = nbt_int_array(20)))
})

test_that("read_nbt can read long arrays", {
    dat <- as_raw(tags$long_array, rawstr("test"), 2, 0, 0, 0,
        0x64, 0, 0, 0, 0, 0, 0, 0,
        0x9c, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff)
    val <- bit64::as.integer64(c("100","-100"))
    expect_equal(read_nbt(dat), list(test = nbt_long_array(val)))
})

test_that("read_nbt can read list values", {
    dat <- as_raw(tags$list, rawstr("test"), tags$short, 4, 0, 0, 0, 
        100, 0, 156, 255, 0, 1, 1, 0)
    expect_equal(read_nbt(dat), 
        list(test = nbt_list(nbt_short(100), nbt_short(-100), nbt_short(256), nbt_short(1))))

    empty_dat <- as_raw(tags$list, rawstr("test"), tags$end, 0, 0, 0, 0)
    expect_equal(read_nbt(empty_dat), list(test = nbt_list()))
})

test_that("read_nbt can read compound values", {
    dat <- as_raw(tags$compound, rawstr("test"),
            tags$byte, rawstr("A"), 0,
            tags$byte, rawstr("B"), 1,
            tags$end
            )
    expect_equal(read_nbt(dat), list(test = nbt_compound(A = nbt_byte(0), B = nbt_byte(1))))
})

test_that("read_nbt simplifies values without names", {
    dat <- as_raw(tags$compound, rawstr(""),
            tags$byte, rawstr("A"), 0,
            tags$byte, rawstr("B"), 1,
            tags$end
            )
    a <- nbt_byte(0)
    b <- nbt_byte(1)
    com <- nbt_compound(A = a, B = b)
    expect_equal(read_nbt(dat), com)
    expect_equal(read_nbt(dat, simplify = FALSE), list(com))

    unit_dat <- as_raw(tags$byte, rawstr(""), 0L)
    expect_equal(read_nbt(unit_dat), nbt_byte(0))
    expect_equal(read_nbt(unit_dat, simplify = FALSE), list(nbt_byte(0)))
})

test_that("read_nbt does not simplify multiple values", {
    dat <- as_raw(tags$compound, rawstr(""),
            tags$byte, rawstr("A"), 0,
            tags$byte, rawstr("B"), 1,
            tags$end,
            tags$byte, rawstr(""), 2
            )
    a <- nbt_byte(0)
    b <- nbt_byte(1)
    c <- nbt_byte(2)
    com <- nbt_compound(A = a, B = b)
    expect_equal(read_nbt(dat), list(com, c))
    expect_equal(read_nbt(dat, simplify = FALSE), list(com, c))
})

test_that("read_nbt reads a maximum number of values", {
    dat <- as_raw(
            tags$byte, rawstr("A"), 0,
            tags$byte, rawstr("B"), 1,
            tags$byte, rawstr("C"), 2
            )
    a <- nbt_byte(0)
    b <- nbt_byte(1)
    c <- nbt_byte(2)

    expect_equal(read_nbt(dat, max_elements = 0),
        structure(list(), bytes_read = 0L))
    expect_equal(read_nbt(dat, max_elements = 1),
        structure(list(A = a), bytes_read = 5L))
    expect_equal(read_nbt(dat, max_elements = 2),
        structure(list(A = a, B = b), bytes_read = 10L))
    expect_equal(read_nbt(dat, max_elements = 3),
        structure(list(A = a, B = b, C = c), bytes_read = 15L))
    expect_equal(read_nbt(dat, max_elements = 4),
        structure(list(A = a, B = b, C = c), bytes_read = 15L))
    
    expect_equal(read_nbt(dat, max_elements = 1, simplify = TRUE),
        structure(list(A = a), bytes_read = 5L))
})

test_that("read_nbt throws errors on malformed values", {
    dat <- as_raw(tags$compound, rawstr(""),
            tags$byte, rawstr("A"), 0,
            tags$byte_array, rawstr("BCD"), 3, 0, 0, 0, 1, 2, 3,
            tags$byte, rawstr(""), 2,
            tags$end
            )
    expect_silent(read_nbt(dat))
    for(i in seq.int(length(dat)-1)) {
        expect_error(read_nbt(dat[1:!!i]))
    }
})

test_that("unnbt() strips metadata from nbt data", {
    nbt_1 <- nbt_byte(10L)
    nbt_2 <- nbt_compound(A = nbt_int(10), B = nbt_compound(nbt_long(10), nbt_float(10)))
    nbt_3 <- nbt_compound(nbt_list(nbt_float(10), nbt_float(20), nbt_float(30)))

    expect_equal(unnbt(nbt_1), 10L)
    expect_equal(unnbt(nbt_2), list(A = 10L, B = list(bit64::as.integer64(10), 10)))
    expect_equal(unnbt(nbt_3), list(list(10,20,30)))
})

test_that("write_nbt() correctly encodes nbt data", {
    expect_equal(write_nbt(list(test = nbt_byte(10L))), as_raw(tags$byte, rawstr("test"), 10))

    expect_equal(write_nbt(nbt_byte(10)), as_raw(tags$byte, rawstr(""), 10))
    expect_equal(write_nbt(nbt_short(10)), as_raw(tags$short, rawstr(""), 10, 0))
    expect_equal(write_nbt(nbt_int(10)), as_raw(tags$int, rawstr(""), 10, 0, 0, 0))
    expect_equal(write_nbt(nbt_long(10)), as_raw(tags$long, rawstr(""), 10, 0, 0, 0, 0, 0, 0, 0))
    expect_equal(write_nbt(nbt_float(10)), as_raw(tags$float, rawstr(""), 0, 0, 32, 65))
    expect_equal(write_nbt(nbt_double(10)), as_raw(tags$double, rawstr(""), 0, 0, 0, 0, 0, 0, 36, 64))
    expect_equal(write_nbt(nbt_string("10")), as_raw(tags$string, rawstr(""), rawstr("10")))

    expect_equal(write_nbt(nbt_byte_array(c(10,20))),
        as_raw(tags$byte_array, rawstr(""), 2, 0, 0, 0, 10, 20))
    expect_equal(write_nbt(nbt_int_array(c(10,20))),
        as_raw(tags$int_array, rawstr(""), 2, 0, 0, 0, 10, 0, 0, 0, 20, 0, 0, 0))
    expect_equal(write_nbt(nbt_long_array(c(10,20))),
        as_raw(tags$long_array, rawstr(""), 2, 0, 0, 0, 10, 0, 0, 0, 0, 0, 0, 0,
            20, 0, 0, 0, 0, 0, 0, 0))

    expect_equal(write_nbt(nbt_compound(A = nbt_byte(1), B = nbt_byte(2))), 
        as_raw(tags$compound, rawstr(""),
               tags$byte, rawstr("A"), 1,
               tags$byte, rawstr("B"), 2,
               tags$end)
        )

    expect_equal(write_nbt(nbt_compound(nbt_byte(1), nbt_byte(2))), 
        as_raw(tags$compound, rawstr(""),
               tags$byte, rawstr(""), 1,
               tags$byte, rawstr(""), 2,
               tags$end)
        )

    expect_equal(write_nbt(nbt_list(nbt_byte(1), nbt_byte(2))),
        as_raw(tags$list, rawstr(""), tags$byte, 2, 0, 0, 0, 1, 2))
})
