test_that("chrkeys_to_rawkeys supports strings", {
    expect_equal(chrkeys_to_rawkeys("test_string"),
        list(charToRaw("test_string")))
    expect_equal(chrkeys_to_rawkeys("test\u01_string"),
        list(charToRaw("test\u01_string")))
    expect_equal(chrkeys_to_rawkeys(c("test_string1","test_string2")),
        list(charToRaw("test_string1"),charToRaw("test_string2")))
})

test_that("chrkeys_to_rawkeys supports NA and NULL", {
    expect_equal(chrkeys_to_rawkeys(c(NA_character_,"test")), list(NULL,charToRaw("test")))
    expect_equal(chrkeys_to_rawkeys(character(0L)),list())
    expect_equal(chrkeys_to_rawkeys(""),list(raw(0L)))
    expect_null(chrkeys_to_rawkeys(NULL))
})

test_that("chrkeys_to_rawkeys supports percent encoding", {
    expect_equal(chrkeys_to_rawkeys("test%64string"), list(charToRaw("test\x64string")))
    expect_equal(chrkeys_to_rawkeys("%00test"), list(c(as.raw(0),charToRaw("test"))))
    expect_equal(chrkeys_to_rawkeys("%FF%ff%00%00%00%00%00%00%30"),
        list(as.raw(c(0xff,0xff,0,0,0,0,0,0,48))))
    expect_equal(chrkeys_to_rawkeys("%ta%0T%af"),
        list(charToRaw("%ta%0T\xaf")))
})

test_that("chrkeys_to_rawkeys supports chunk keys", {
    expect_equal(chrkeys_to_rawkeys(c("@-1:1:0:50","@-1:1:1:47-3","@-1:1:2:50","@-1:1:0:47-3")),
        list(as.raw(c(0xff,0xff,0xff,0xff,1,0,0,0,50)),
             as.raw(c(0xff,0xff,0xff,0xff,1,0,0,0,1,0,0,0,47,3)),
             as.raw(c(0xff,0xff,0xff,0xff,1,0,0,0,2,0,0,0,50)),
             as.raw(c(0xff,0xff,0xff,0xff,1,0,0,0,47,3))
        ))
})

test_that("chrkeys_to_rawkeys treats invalid chunk keys as strings", {
    expect_equal(chrkeys_to_rawkeys("@0:0:0:0a"), list(charToRaw("@0:0:0:0a")))
    expect_equal(chrkeys_to_rawkeys("@0:0:0:0-"), list(charToRaw("@0:0:0:0-")))
    expect_equal(chrkeys_to_rawkeys("@0:0:0:0"), list(charToRaw("@0:0:0:0")))
    expect_equal(chrkeys_to_rawkeys("@0:0:0:"), list(charToRaw("@0:0:0:")))
    expect_equal(chrkeys_to_rawkeys("@0:0:0"), list(charToRaw("@0:0:0")))
    expect_equal(chrkeys_to_rawkeys("@0:0:"), list(charToRaw("@0:0:")))
    expect_equal(chrkeys_to_rawkeys("@0:0"), list(charToRaw("@0:0")))
    expect_equal(chrkeys_to_rawkeys("@0:"), list(charToRaw("@0:")))
    expect_equal(chrkeys_to_rawkeys("@0"), list(charToRaw("@0")))
    expect_equal(chrkeys_to_rawkeys("@"), list(charToRaw("@")))
    expect_equal(chrkeys_to_rawkeys("@0:a:0:0"), list(charToRaw("@0:a:0:0")))
    expect_equal(chrkeys_to_rawkeys("@0:0:0a:0"), list(charToRaw("@0:0:0a:0")))
    expect_equal(chrkeys_to_rawkeys("@0:0:0:44-32"), list(charToRaw("@0:0:0:44-32")))
    expect_equal(chrkeys_to_rawkeys("@0:0:0:100"), list(charToRaw("@0:0:0:100")))
    expect_equal(chrkeys_to_rawkeys("@0:0:-1:44"), list(charToRaw("@0:0:-1:44")))
    expect_equal(chrkeys_to_rawkeys("@0:0:3:44"), list(charToRaw("@0:0:3:44")))
})

test_that("chrkeys_to_rawkeys signals errors", {
    long_string1 <- paste0(rep("%61",10000),collapse='')
    long_string2 <- paste0(rep("a",10000),collapse='')
    expect_error(chrkeys_to_rawkeys(long_string1), "exceeded buffer space")
    expect_error(chrkeys_to_rawkeys(long_string2), "exceeded buffer space")
    expect_error(.Call(Cchrkeys_to_rawkeys, list("a","b")), "not a vector of strings")
})

test_that("rawkeys_to_chrkeys supports strings", {
    expect_equal(rawkeys_to_chrkeys(charToRaw("test_string")),
        "test_string")
    expect_equal(
        rawkeys_to_chrkeys(charToRaw("test\u0a_string")),
        "test%0A_string")
    expect_equal(rawkeys_to_chrkeys(list(charToRaw("test_string1"),charToRaw("test_string2"))),
        c("test_string1","test_string2"))
})

test_that("rawkeys_to_chrkeys supports NA and NULL", {
    expect_equal(rawkeys_to_chrkeys(list(NULL,charToRaw("test"))), c(NA_character_,"test"))
    expect_equal(rawkeys_to_chrkeys(list()),character(0L))
    expect_equal(rawkeys_to_chrkeys(raw(0L)),"")
    expect_null(rawkeys_to_chrkeys(NULL))
})

test_that("rawkeys_to_charkeys supports percent encoding", {
    expect_equal(rawkeys_to_chrkeys(charToRaw("@0:0:0:0")), "%400:0:0:0")
    expect_equal(rawkeys_to_chrkeys(charToRaw("Hello%World")), "Hello%25World")
    expect_equal(rawkeys_to_chrkeys(charToRaw("Hello\x4AWorld")), "HelloJWorld")
    expect_equal(rawkeys_to_chrkeys(charToRaw("Hello\x0FWorld")), "Hello%0FWorld")
    expect_equal(rawkeys_to_chrkeys(charToRaw("Hello\x80World")), "Hello%80World")
})

test_that("rawkeys_to_chrkeys supports chunk keys", {
    expect_equal(rawkeys_to_chrkeys(
        list(as.raw(c(0xff,0xff,0xff,0xff,1,0,0,0,50)),
             as.raw(c(0xff,0xff,0xff,0xff,1,0,0,0,1,0,0,0,47,3)),
             as.raw(c(0xff,0xff,0xff,0xff,1,0,0,0,2,0,0,0,50)),
             as.raw(c(0xff,0xff,0xff,0xff,1,0,0,0,47,3))
        )),
        c("@-1:1:0:50","@-1:1:1:47-3","@-1:1:2:50","@-1:1:0:47-3"))
})

test_that("rawkeys_to_chrkeys treats invalid chunk keys as strings", {
    expect_equal(rawkeys_to_chrkeys(as.raw(c(0,0,0,0,0,0,0,0,0,0,0,0,0,0))),
        "%00%00%00%00%00%00%00%00%00%00%00%00%00%00")
    expect_equal(rawkeys_to_chrkeys(as.raw(c(0,0,0,0,0,0,0,0,3,0,0,0,44,0))),
        "%00%00%00%00%00%00%00%00%03%00%00%00,%00")
    expect_equal(rawkeys_to_chrkeys(as.raw(c(0,0,0,0,0,0,0,0,1,0,0,0,0,0))),
        "%00%00%00%00%00%00%00%00%01%00%00%00%00%00")
    expect_equal(rawkeys_to_chrkeys(as.raw(c(0,0,0,0,0,0,0,0,1,0,0,0,44,32))),
        "%00%00%00%00%00%00%00%00%01%00%00%00,%20")
})

test_that("rawkeys_to_chrkeys signals errors", {
    long_string <- as.raw(rep(97,3000))
    expect_error(rawkeys_to_chrkeys(long_string), "exceeded buffer space")
    expect_error(rawkeys_to_chrkeys("test"), "not a list")
    expect_error(rawkeys_to_chrkeys(list(charToRaw("test"),NULL,"test")), "not a raw type or NULL")
})


test_that("parse_chunk_keys parses correctly",{
    expect_equal(parse_chunk_keys("@0:0:0:47-1"),
        tibble::tibble(key="@0:0:0:47-1",x=0L,z=0L,dimension=0L,tag="SubchunkBlocks",subtag=1L))
})
