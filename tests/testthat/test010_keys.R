library(rbedrock)

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

test_that("charkeys_to_rawkeys supports percent encoding", {
    expect_equal(chrkeys_to_rawkeys("test%64string"), list(charToRaw("test\x64string")))
    expect_equal(chrkeys_to_rawkeys("%00test"), list(c(as.raw(0),charToRaw("test"))))
    expect_equal(chrkeys_to_rawkeys("%FF%ff%00%00%00%00%00%00%30"),
        list(as.raw(c(0xff,0xff,0,0,0,0,0,0,48))))
    expect_equal(chrkeys_to_rawkeys("%ta%0T%af"),
        list(charToRaw("%ta%0T\xaf")))    
})

test_that("charkeys_to_rawkeys supports chunk keys", {
    expect_equal(chrkeys_to_rawkeys(c("@-1:1:0:50","@-1:1:1:47-3","@-1:1:2:50","@-1:1:0:47-3")),
        list(as.raw(c(0xff,0xff,0xff,0xff,1,0,0,0,50)),
             as.raw(c(0xff,0xff,0xff,0xff,1,0,0,0,1,0,0,0,47,3)),
             as.raw(c(0xff,0xff,0xff,0xff,1,0,0,0,2,0,0,0,50)),
             as.raw(c(0xff,0xff,0xff,0xff,1,0,0,0,47,3))
        ))
})

test_that("charkeys_to_rawkeys treats invalid chunk keys as strings", {
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
})

