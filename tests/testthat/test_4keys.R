test_that("chrkeys_to_rawkeys supports strings", {
    expect_equal(
        chrkeys_to_rawkeys("plain:test_string"),
        list(charToRaw("test_string"))
    )
    expect_equal(
        chrkeys_to_rawkeys("plain:test\u01_string"),
        list(charToRaw("test\u01_string"))
    )
    expect_equal(
        chrkeys_to_rawkeys(c("plain:test_string1", "plain:test_string2")),
        list(charToRaw("test_string1"), charToRaw("test_string2"))
    )
})

test_that("chrkeys_to_rawkeys supports NA and NULL", {
    expect_equal(
        chrkeys_to_rawkeys(c(NA_character_, "plain:test")),
        list(NULL, charToRaw("test"))
    )
    expect_equal(chrkeys_to_rawkeys(character(0L)), list())
    expect_equal(chrkeys_to_rawkeys(c("plain:", "")), list(raw(0L), raw(0L)))
    expect_null(chrkeys_to_rawkeys(NULL))
})

test_that("chrkeys_to_rawkeys supports percent encoding", {
    expect_equal(
        chrkeys_to_rawkeys("plain:test%64string"),
        list(charToRaw("test\x64string"))
    )
    expect_equal(
        chrkeys_to_rawkeys("plain:%00test"),
        list(c(as.raw(0), charToRaw("test")))
    )
    expect_equal(
        chrkeys_to_rawkeys("plain:%FF%ff%00%00%00%00%00%00%30"),
        list(as.raw(c(0xff, 0xff, 0, 0, 0, 0, 0, 0, 48)))
    )
    expect_equal(
        chrkeys_to_rawkeys("plain:%ta%0T%af"),
        list(charToRaw("%ta%0T\xaf"))
    )
})

test_that("chrkeys_to_rawkeys treats invalid keys as plain keys", {
    expect_warning(val <- chrkeys_to_rawkeys("xhunk:0:0:0:44"))
    expect_equal(val, list(charToRaw("xhunk:0:0:0:44")))
})

test_that("chrkeys_to_rawkeys supports chunk keys", {
    expect_equal(
        chrkeys_to_rawkeys(c(
            "chunk:-1:1:0:50", "chunk:-1:1:1:47:3",
            "chunk:-1:1:2:50", "chunk:-1:1:0:47:3",
            "chunk:-1:1:0:47:-1", "chunk:-1:1:0:47:-4"
        )),
        list(
            as.raw(c(0xff, 0xff, 0xff, 0xff, 1, 0, 0, 0, 50)),
            as.raw(c(0xff, 0xff, 0xff, 0xff, 1, 0, 0, 0, 1, 0, 0, 0, 47, 3)),
            as.raw(c(0xff, 0xff, 0xff, 0xff, 1, 0, 0, 0, 2, 0, 0, 0, 50)),
            as.raw(c(0xff, 0xff, 0xff, 0xff, 1, 0, 0, 0, 47, 3)),
            as.raw(c(0xff, 0xff, 0xff, 0xff, 1, 0, 0, 0, 47, 255)),
            as.raw(c(0xff, 0xff, 0xff, 0xff, 1, 0, 0, 0, 47, 252))
        )
    )
})

test_that("chrkeys_to_rawkeys treats invalid chunk keys as strings", {
    expect_warning(val <- chrkeys_to_rawkeys("chunk:0:0:0:100"))
    expect_equal(val, list(charToRaw("chunk:0:0:0:100")))
    expect_warning(val <- chrkeys_to_rawkeys("chunk:0:0:-1:44"))
    expect_equal(val, list(charToRaw("chunk:0:0:-1:44")))
    expect_warning(val <- chrkeys_to_rawkeys("chunk:0:0:3:44"))
    expect_equal(val, list(charToRaw("chunk:0:0:3:44")))
    expect_warning(val <- chrkeys_to_rawkeys("chunk:0:0:0:47:-40"))
    expect_equal(val, list(charToRaw("chunk:0:0:0:47:-40")))
    expect_warning(val <- chrkeys_to_rawkeys("chunk:0:0:0:47:40"))
    expect_equal(val, list(charToRaw("chunk:0:0:0:47:40")))

    expect_warning(val <- chrkeys_to_rawkeys("chunk:0:0:0:0a"))
    expect_equal(val, list(charToRaw("chunk:0:0:0:0a")))
    expect_warning(val <- chrkeys_to_rawkeys("chunk:0:0:0:0:-"))
    expect_equal(val, list(charToRaw("chunk:0:0:0:0:-")))
    expect_warning(val <- chrkeys_to_rawkeys("chunk:0:0:0:0:"))
    expect_equal(val, list(charToRaw("chunk:0:0:0:0:")))
    expect_warning(val <- chrkeys_to_rawkeys("chunk:0:0:0:0"))
    expect_equal(val, list(charToRaw("chunk:0:0:0:0")))
    expect_warning(val <- chrkeys_to_rawkeys("chunk:0:0:0:"))
    expect_equal(val, list(charToRaw("chunk:0:0:0:")))
    expect_warning(val <- chrkeys_to_rawkeys("chunk:0:0:0"))
    expect_equal(val, list(charToRaw("chunk:0:0:0")))
    expect_warning(val <- chrkeys_to_rawkeys("chunk:0:0:"))
    expect_equal(val, list(charToRaw("chunk:0:0:")))
    expect_warning(val <- chrkeys_to_rawkeys("chunk:0:0"))
    expect_equal(val, list(charToRaw("chunk:0:0")))
    expect_warning(val <- chrkeys_to_rawkeys("chunk:0:"))
    expect_equal(val, list(charToRaw("chunk:0:")))
    expect_warning(val <- chrkeys_to_rawkeys("chunk:0"))
    expect_equal(val, list(charToRaw("chunk:0")))
    expect_warning(val <- chrkeys_to_rawkeys("chunk:"))
    expect_equal(val, list(charToRaw("chunk:")))
    expect_warning(val <- chrkeys_to_rawkeys("chunk:0:a:0:0"))
    expect_equal(val, list(charToRaw("chunk:0:a:0:0")))
    expect_warning(val <- chrkeys_to_rawkeys("chunk:0:0:0a:0"))
    expect_equal(val, list(charToRaw("chunk:0:0:0a:0")))
})

test_that("chrkeys_to_rawkeys signals errors", {
    long_string1 <- paste0("plain:", paste0(rep("%61", 10000), collapse = ""))
    long_string2 <- paste0("plain:", paste0(rep("a", 10000), collapse = ""))
    expect_error(chrkeys_to_rawkeys(long_string1), "exceeded buffer space")
    expect_error(chrkeys_to_rawkeys(long_string2), "exceeded buffer space")
    expect_error(
        .Call(Cchrkeys_to_rawkeys, list("a", "b")),
        "not a vector of strings"
    )
})

test_that("chrkeys_to_rawkeys supports actor digest keys keys", {
    expect_equal(
        chrkeys_to_rawkeys(c(
            "acdig:-1:1:0", "acdig:-1:1:1",
            "acdig:-1:1:2", "acdig:-1:1:0"
        )),
        list(
            as.raw(c(
                0x64, 0x69, 0x67, 0x70, 0xff, 0xff, 0xff, 0xff,
                1, 0, 0, 0
            )),
            as.raw(c(
                0x64, 0x69, 0x67, 0x70, 0xff, 0xff, 0xff, 0xff,
                1, 0, 0, 0, 1, 0, 0, 0
            )),
            as.raw(c(
                0x64, 0x69, 0x67, 0x70, 0xff, 0xff, 0xff, 0xff,
                1, 0, 0, 0, 2, 0, 0, 0
            )),
            as.raw(c(
                0x64, 0x69, 0x67, 0x70, 0xff, 0xff, 0xff, 0xff,
                1, 0, 0, 0
            ))
        )
    )
})

test_that("chrkeys_to_rawkeys treats invalid actor digest keys as strings", {
    expect_warning(val <- chrkeys_to_rawkeys("acdig:-1:1:1:a"))
    expect_equal(val, list(charToRaw("acdig:-1:1:1:a")))
    expect_warning(val <- chrkeys_to_rawkeys("acdig:-1:1:1:"))
    expect_equal(val, list(charToRaw("acdig:-1:1:1:")))
    expect_warning(val <- chrkeys_to_rawkeys("acdig:-1:1:"))
    expect_equal(val, list(charToRaw("acdig:-1:1:")))
    expect_warning(val <- chrkeys_to_rawkeys("acdig:-1:1"))
    expect_equal(val, list(charToRaw("acdig:-1:1")))
    expect_warning(val <- chrkeys_to_rawkeys("acdig:-1:"))
    expect_equal(val, list(charToRaw("acdig:-1:")))
    expect_warning(val <- chrkeys_to_rawkeys("acdig:-1"))
    expect_equal(val, list(charToRaw("acdig:-1")))
    expect_warning(val <- chrkeys_to_rawkeys("acdig:"))
    expect_equal(val, list(charToRaw("acdig:")))
})

test_that("chrkeys_to_rawkeys supports actor keys", {
    raw_prefix <- charToRaw("actorprefix")
    expect_equal(
        chrkeys_to_rawkeys(c(
            "actor:0123456789abcdef",
            "actor:FEDCBA9876543210"
        )),
        list(
            as.raw(c(
                raw_prefix, 0x01, 0x23, 0x45, 0x67,
                0x89, 0xab, 0xcd, 0xef
            )),
            as.raw(c(
                raw_prefix, 0xfe, 0xdc, 0xba, 0x98,
                0x76, 0x54, 0x32, 0x10
            ))
        )
    )
})

test_that("chrkeys_to_rawkeys treats invalid actor keys as strings", {
    expect_warning(val <- chrkeys_to_rawkeys("actor:0123456789abcdefg"))
    expect_equal(val, list(charToRaw("actor:0123456789abcdefg")))
    expect_warning(val <- chrkeys_to_rawkeys("actor:0123456789"))
    expect_equal(val, list(charToRaw("actor:0123456789")))
    expect_warning(val <- chrkeys_to_rawkeys("actor:100000:-100001:0"))
    expect_equal(val, list(charToRaw("actor:100000:-100001:0")))
    expect_warning(val <- chrkeys_to_rawkeys("actor:local_player"))
    expect_equal(val, list(charToRaw("actor:local_player")))
})

test_that("rawkeys_to_chrkeys supports plain strings", {
    expect_equal(
        rawkeys_to_chrkeys(charToRaw("test_string")),
        "plain:test_string"
    )
    expect_equal(
        rawkeys_to_chrkeys(charToRaw("test\u0a_string")),
        "plain:test%0A_string"
    )
    expect_equal(
        rawkeys_to_chrkeys(list(
            charToRaw("test_string1"),
            charToRaw("test_string2")
        )),
        c("plain:test_string1", "plain:test_string2")
    )
})

test_that("rawkeys_to_chrkeys supports NA and NULL", {
    expect_equal(
        rawkeys_to_chrkeys(list(NULL, charToRaw("test"))),
        c(NA_character_, "plain:test")
    )
    expect_equal(rawkeys_to_chrkeys(list()), character(0L))
    expect_equal(rawkeys_to_chrkeys(raw(0L)), "plain:")
    expect_null(rawkeys_to_chrkeys(NULL))
})

test_that("rawkeys_to_charkeys supports percent encoding", {
    expect_equal(
        rawkeys_to_chrkeys(charToRaw("@0:0:0:0")),
        "plain:@0:0:0:0"
    )
    expect_equal(
        rawkeys_to_chrkeys(charToRaw("Hello%World")),
        "plain:Hello%25World"
    )
    expect_equal(
        rawkeys_to_chrkeys(charToRaw("Hello\x4AWorld")),
        "plain:HelloJWorld"
    )
    expect_equal(
        rawkeys_to_chrkeys(charToRaw("Hello\x0FWorld")),
        "plain:Hello%0FWorld"
    )
    expect_equal(
        rawkeys_to_chrkeys(charToRaw("Hello\x80World")),
        "plain:Hello%80World"
    )
})

test_that("rawkeys_to_chrkeys supports chunk keys", {
    expect_equal(
        rawkeys_to_chrkeys(
            list(
                as.raw(c(0xff, 0xff, 0xff, 0xff, 1, 0, 0, 0, 50)),
                as.raw(c(
                    0xff, 0xff, 0xff, 0xff,
                    1, 0, 0, 0, 1, 0, 0, 0, 47, 3
                )),
                as.raw(c(0xff, 0xff, 0xff, 0xff, 1, 0, 0, 0, 2, 0, 0, 0, 50)),
                as.raw(c(0xff, 0xff, 0xff, 0xff, 1, 0, 0, 0, 47, 3)),
                as.raw(c(0xff, 0xff, 0xff, 0xff, 1, 0, 0, 0, 47, 255)),
                as.raw(c(0xff, 0xff, 0xff, 0xff, 1, 0, 0, 0, 47, 252))
            )
        ),
        c(
            "chunk:-1:1:0:50", "chunk:-1:1:1:47:3", "chunk:-1:1:2:50",
            "chunk:-1:1:0:47:3", "chunk:-1:1:0:47:-1", "chunk:-1:1:0:47:-4"
        )
    )
})

test_that("rawkeys_to_chrkeys treats invalid chunk keys as plain keys", {
    expect_equal(
        rawkeys_to_chrkeys(as.raw(c(
            0, 0, 0, 0, 0, 0, 0, 0,
            0, 0, 0, 0, 0, 0
        ))),
        "plain:%00%00%00%00%00%00%00%00%00%00%00%00%00%00"
    )
    expect_equal(
        rawkeys_to_chrkeys(as.raw(c(
            0, 0, 0, 0, 0, 0, 0, 0,
            3, 0, 0, 0, 44, 0
        ))),
        "plain:%00%00%00%00%00%00%00%00%03%00%00%00,%00"
    )
    expect_equal(
        rawkeys_to_chrkeys(as.raw(c(
            0, 0, 0, 0, 0, 0, 0, 0,
            1, 0, 0, 0, 0, 0
        ))),
        "plain:%00%00%00%00%00%00%00%00%01%00%00%00%00%00"
    )
    expect_equal(
        rawkeys_to_chrkeys(as.raw(c(
            0, 0, 0, 0, 0, 0, 0, 0,
            1, 0, 0, 0, 44, 32
        ))),
        "plain:%00%00%00%00%00%00%00%00%01%00%00%00,%20"
    )
})

test_that("rawkeys_to_chrkeys supports actor keys", {
    prefix <- charToRaw("actorprefix")
    expect_equal(
        rawkeys_to_chrkeys(c(prefix, as.raw(c(0, 0, 0, 0, 0, 0, 0, 0)))),
        "actor:0000000000000000"
    )
    expect_equal(
        rawkeys_to_chrkeys(c(prefix, as.raw(c(110, 0, 0, 0, 0, 107, 0, 0)))),
        "actor:6E000000006B0000"
    )
})

test_that("rawkeys_to_chrkeys treats invalid actor keys as plain keys", {
    prefix <- charToRaw("actorprefix")
    expect_equal(
        rawkeys_to_chrkeys(c(prefix, as.raw(c(0, 0, 0, 0, 0, 0, 0)))),
        "plain:actorprefix%00%00%00%00%00%00%00"
    )
    expect_equal(
        rawkeys_to_chrkeys(c(prefix, as.raw(c(110, 0, 0, 0, 0, 107, 0, 0, 0)))),
        "plain:actorprefixn%00%00%00%00k%00%00%00"
    )
})

test_that("rawkeys_to_chrkeys supports actor digest keys", {
    prefix <- charToRaw("digp")
    expect_equal(
        rawkeys_to_chrkeys(c(prefix, as.raw(c(0, 0, 0, 0, 0, 0, 0, 0)))),
        "acdig:0:0:0"
    )
    expect_equal(
        rawkeys_to_chrkeys(c(prefix, as.raw(c(
            110, 0, 0, 0, 250, 255, 255, 255,
            1, 0, 0, 0
        )))),
        "acdig:110:-6:1"
    )
})

test_that("rawkeys_to_chrkeys treats invalid actor digest keys as plain keys", {
    prefix <- charToRaw("digp")
    expect_equal(
        rawkeys_to_chrkeys(c(prefix, as.raw(c(0, 0, 0, 0, 0, 0, 0)))),
        "plain:digp%00%00%00%00%00%00%00"
    )
    expect_equal(
        rawkeys_to_chrkeys(c(prefix, as.raw(c(
            110, 0, 0, 0, 250, 255, 255, 255,
            1, 0, 0, 0, 0
        )))),
        "plain:digpn%00%00%00%FA%FF%FF%FF%01%00%00%00%00"
    )
})

test_that("rawkeys_to_chrkeys signals errors", {
    long_string <- as.raw(rep(97, 3000))
    expect_error(rawkeys_to_chrkeys(long_string), "exceeded buffer space")
    expect_error(rawkeys_to_chrkeys("test"), "not a list")
    expect_error(
        rawkeys_to_chrkeys(list(charToRaw("test"), NULL, "test")),
        "not a raw type or NULL"
    )
})


test_that("parse_chunk_keys parses correctly", {
    expect_equal(
        parse_chunk_keys("chunk:0:0:0:47:1"),
        tibble::tibble(
            key = "chunk:0:0:0:47:1", x = 0L, z = 0L, dimension = 0L,
            tag = "SubChunkBlocks", subtag = 1L
        )
    )
})
