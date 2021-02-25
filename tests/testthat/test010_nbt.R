library(rbedrock)

as_raw <- function(...) {
    as.raw(c(...))
}

rawstr <- function(x) {
   n <- nchar(x, type = "bytes")
   r <- writeBin(n, raw(), size = 2, endian = "little")
   as_raw(r, charToRaw(x))
}

tag <- list(
    end = 0L, byte = 1L, short = 2L, int = 3L, long = 4L,
    float = 5L, double = 6L, byte_array = 7L, string = 8L,
    list = 9L, compound = 10L, int_array = 11L, long_array = 12L
)

test_that("read_nbt can read bytes", {
    dat <- as_raw(tag$byte, rawstr("test"), 100)
    expect_equal(read_nbt(dat), list(test = structure(100L, class="nbtnode", tag=tag$byte)))
    neg_dat <- as_raw(tag$byte, rawstr("test"), 254)
    expect_equal(read_nbt(neg_dat), list(test  = structure(-2L, class="nbtnode", tag=tag$byte)))
})

test_that("read_nbt can read shorts", {
    dat <- as_raw(tag$short, rawstr("test"), 100, 2)
    expect_equal(read_nbt(dat), list(test = structure(612L, class="nbtnode", tag=tag$short)))
    neg_dat <- as_raw(tag$short, rawstr("test"), 156, 255)
    expect_equal(read_nbt(neg_dat), list(test = structure(-100L, class="nbtnode", tag=tag$short)))
})

test_that("read_nbt can read ints", {
    dat <- as_raw(tag$int, rawstr("test"), 100, 2, 3, 4)
    expect_equal(read_nbt(dat), list(test = structure(67306084L, class="nbtnode", tag=tag$int)))
    neg_dat <- as_raw(tag$int, rawstr("test"), 156, 255, 255, 255)
    expect_equal(read_nbt(neg_dat), list(test = structure(-100L, class="nbtnode", tag=tag$int)))
})

test_that("read_nbt can read longs", {
    dat <- as_raw(tag$long, rawstr("test"), 1, 0, 0x20, 0x3b, 0x9d, 0xb5, 0x05, 0x6f)
    val <- bit64::as.integer64("8000000000000000001")
    expect_equal(read_nbt(dat), list(test = structure(val, class=c("nbtnode","integer64"), tag=tag$long)))
    dat <- as_raw(tag$long, rawstr("test"), 0xff, 0xff, 0xdf, 0xc4, 0x62, 0x4a, 0xfa, 0x90)
    val <- bit64::as.integer64("-8000000000000000001")
    expect_equal(read_nbt(dat), list(test = structure(val, class=c("nbtnode","integer64"), tag=tag$long)))
})

test_that("read_nbt can read floats", {
    dat <- as_raw(tag$float, rawstr("test"), 0, 0, 0x10, 0x42)
    expect_equal(read_nbt(dat), list(test = structure(36.0, class="nbtnode", tag=tag$float)))
})

test_that("read_nbt can read doubles", {
    dat <- as_raw(tag$double, rawstr("test"), 0, 0, 0, 0, 0, 0, 0x42, 0x40)
    expect_equal(read_nbt(dat), list(test = structure(36.0, class="nbtnode", tag=tag$double)))
})

test_that("read_nbt can read byte arrays", {
    dat <- as_raw(tag$byte_array, rawstr("test"), 8, 0, 0, 0, 10, 11, 12, 13, 14, 15, 16, 17)
    expect_equal(read_nbt(dat), list(test = structure(10:17, class="nbtnode", tag=tag$byte_array)))
    single_dat <- as_raw(tag$byte_array, rawstr("test"), 1, 0, 0, 0, 20)
    expect_equal(read_nbt(single_dat), list(test = structure(20, class="nbtnode", tag=tag$byte_array)))
})

test_that("read_nbt can read strings",{
    dat <- as_raw(tag$string, rawstr("test"), rawstr("hello world"))
    expect_equal(read_nbt(dat), list(test = structure("hello world", class="nbtnode", tag=tag$string)))    
    blank_dat <- as_raw(tag$string, rawstr("test"), rawstr(""))
    expect_equal(read_nbt(blank_dat), list(test = structure("", class="nbtnode", tag=tag$string)))
    greek_dat <- as_raw(tag$string, rawstr("test"), rawstr("\u03B1\u03B2\u03B3"))
    expect_equal(read_nbt(greek_dat), list(test = structure("\u03B1\u03B2\u03B3", class="nbtnode", tag=tag$string)))
})

test_that("read_nbt can read int arrays", {
    dat <- as_raw(tag$int_array, rawstr("test"), 2, 0, 0, 0, 10, 0, 0, 0, 11, 0, 0, 0)
    expect_equal(read_nbt(dat), list(test = structure(10:11, class="nbtnode", tag=tag$int_array)))
    single_dat <- as_raw(tag$int_array, rawstr("test"), 1, 0, 0, 0, 20, 0, 0, 0)
    expect_equal(read_nbt(single_dat), list(test = structure(20, class="nbtnode", tag=tag$int_array)))
})

test_that("read_nbt can read long arrays", {
    dat <- as_raw(tag$long_array, rawstr("test"), 2, 0, 0, 0,
        0x64, 0, 0, 0, 0, 0, 0, 0,
        0x9c, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff)
    val <- bit64::as.integer64(c("100","-100"))
    expect_equal(read_nbt(dat), list(test = structure(val, class=c("nbtnode","integer64"), tag=tag$long_array)))
})

test_that("read_nbt can read list values", {
    dat <- as_raw(tag$list, rawstr("test"), tag$short, 4, 0, 0, 0, 
        100, 0, 156, 255, 0, 1, 1, 0)
    expect_equal(read_nbt(dat), list(test = structure(c(100, -100, 256, 1), class="nbtnode", tag=tag$list, list_tag=tag$short)))
    empty_dat <- as_raw(tag$list, rawstr("test"), tag$end, 0, 0, 0, 0)
    expect_equal(read_nbt(empty_dat), list(test = structure(list(), class="nbtnode", tag=tag$list, list_tag=tag$end)))
})

test_that("read_nbt can read compound values", {
    dat <- as_raw(tag$compound, rawstr("test"),
            tag$byte, rawstr("A"), 0,
            tag$byte, rawstr("B"), 1,
            tag$end
            )
    a <- structure(0L, class="nbtnode", tag=tag$byte)
    b <- structure(1L, class="nbtnode", tag=tag$byte)
    com <- structure(list(A = a, B = b), class="nbtnode", tag=tag$compound)
    expect_equal(read_nbt(dat), list(test = com))
})

test_that("read_nbt simplifies values without names", {
    dat <- as_raw(tag$compound, rawstr(""),
            tag$byte, rawstr("A"), 0,
            tag$byte, rawstr("B"), 1,
            tag$end
            )
    a <- structure(0L, class="nbtnode", tag=tag$byte)
    b <- structure(1L, class="nbtnode", tag=tag$byte)
    com <- structure(list(A = a, B = b), class="nbtnode", tag=tag$compound)
    expect_equal(read_nbt(dat), com)
    expect_equal(read_nbt(dat, simplify = FALSE), list(com))

    unit_dat <- as_raw(tag$byte, rawstr(""), 0L)
    expect_equal(read_nbt(unit_dat), structure(0L, class="nbtnode", tag=tag$byte))
    expect_equal(read_nbt(unit_dat, simplify = FALSE), list(structure(0L, class="nbtnode", tag=tag$byte)))
})

test_that("read_nbt does not simplify multiple values", {
    dat <- as_raw(tag$compound, rawstr(""),
            tag$byte, rawstr("A"), 0,
            tag$byte, rawstr("B"), 1,
            tag$end,
            tag$byte, rawstr(""), 2
            )
    a <- structure(0L, class="nbtnode", tag=tag$byte)
    b <- structure(1L, class="nbtnode", tag=tag$byte)
    d <- structure(2L, class="nbtnode", tag=tag$byte)
    com <- structure(list(A = a, B = b), class="nbtnode", tag=tag$compound)
    expect_equal(read_nbt(dat), list(com, d))
    expect_equal(read_nbt(dat, simplify = FALSE), list(com, d))
})

test_that("read_nbt matches new_nbtnode", {
    string_dat <- as_raw(tag$string, rawstr("strtest"), rawstr("hello world"))
    expect_equal(read_nbt(string_dat), list(strtest = new_nbtnode("hello world", tag$string)))
    
    long_dat <- as_raw(tag$long, rawstr("lngtest"), 1, 0, 0x20, 0x3b, 0x9d, 0xb5, 0x05, 0x6f)
    long_val <- bit64::as.integer64("8000000000000000001")
    expect_equal(read_nbt(long_dat), list(lngtest = new_nbtnode(long_val, tag$long)))
    
    list_dat <- as_raw(tag$list, rawstr("lsttest"), tag$short, 4, 0, 0, 0, 
        100, 0, 156, 255, 0, 1, 1, 0)
    expect_equal(read_nbt(list_dat), list(lsttest = new_nbtnode(c(100L, -100L, 256L, 1L), tag$list, tag$short)))
})