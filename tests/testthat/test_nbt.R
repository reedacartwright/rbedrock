test_that("read_nbt simplifies values without names", {
    dat <- as_raw(
        tags$compound, rawstr(""),
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
    dat <- as_raw(
        tags$compound, rawstr(""),
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

test_that("unnbt() strips metadata from nbt data", {
    nbt_1 <- nbt_byte(10L)
    nbt_2 <- nbt_compound(A = nbt_int(10), B = nbt_compound(
        nbt_long(10),
        nbt_float(10)
    ))
    nbt_3 <- nbt_compound(nbt_list(c(10, 20, 30), .type = 5L))

    expect_equal(unnbt(nbt_1), 10L)
    expect_equal(unnbt(nbt_2), list(A = 10L, B = list("10", 10)))
    expect_equal(unnbt(nbt_3), list(c(10, 20, 30)))
})

test_that("write_nbt() correctly encodes nbt data", {
    expect_equal(write_nbt(list(test = nbt_byte(10L))), as_raw(
        tags$byte,
        rawstr("test"), 10
    ))

    expect_equal(write_nbt(nbt_byte(10)), as_raw(tags$byte, rawstr(""), 10))
    expect_equal(write_nbt(nbt_short(10)), as_raw(
        tags$short, rawstr(""),
        10, 0
    ))
    expect_equal(write_nbt(nbt_int(10)), as_raw(
        tags$int, rawstr(""),
        10, 0, 0, 0
    ))
    expect_equal(write_nbt(nbt_long(10)), as_raw(
        tags$long, rawstr(""),
        10, 0, 0, 0, 0, 0, 0, 0
    ))
    expect_equal(write_nbt(nbt_float(10)), as_raw(
        tags$float, rawstr(""),
        0, 0, 32, 65
    ))
    expect_equal(write_nbt(nbt_double(10)), as_raw(
        tags$double, rawstr(""),
        0, 0, 0, 0, 0, 0, 36, 64
    ))
    expect_equal(write_nbt(nbt_string("10")), as_raw(
        tags$string, rawstr(""),
        rawstr("10")
    ))
    expect_equal(
        write_nbt(nbt_raw_string(charToRaw("10"))),
        as_raw(tags$string, rawstr(""), rawstr("10"))
    )

    expect_equal(
        write_nbt(nbt_byte_array(c(10, 20))),
        as_raw(tags$byte_array, rawstr(""), 2, 0, 0, 0, 10, 20)
    )
    expect_equal(
        write_nbt(nbt_int_array(c(10, 20))),
        as_raw(tags$int_array, rawstr(""), 2, 0, 0, 0, 10, 0, 0, 0, 20, 0, 0, 0)
    )
    expect_equal(
        write_nbt(nbt_long_array(c(10, 20))),
        as_raw(
            tags$long_array, rawstr(""), 2, 0, 0, 0, 10, 0, 0, 0, 0, 0, 0, 0,
            20, 0, 0, 0, 0, 0, 0, 0
        )
    )

    expect_equal(
        write_nbt(nbt_compound(A = nbt_byte(1), B = nbt_byte(2))),
        as_raw(
            tags$compound, rawstr(""),
            tags$byte, rawstr("A"), 1,
            tags$byte, rawstr("B"), 2,
            tags$end
        )
    )

    expect_equal(
        write_nbt(nbt_compound(nbt_byte(1), nbt_byte(2))),
        as_raw(
            tags$compound, rawstr(""),
            tags$byte, rawstr(""), 1,
            tags$byte, rawstr(""), 2,
            tags$end
        )
    )

    expect_equal(
        write_nbt(nbt_list(c(1, 2), .type = 1)),
        as_raw(tags$list, rawstr(""), tags$byte, 2, 0, 0, 0, 1, 2)
    )
})



test_that("vec_ptype_full returns the right prototype string", {
    expect_equal(vctrs::vec_ptype_full(nbt_byte(1)), "rbedrock_nbt_byte")
    expect_equal(vctrs::vec_ptype_full(nbt_short(1)), "rbedrock_nbt_short")
    expect_equal(vctrs::vec_ptype_full(nbt_int(1)), "rbedrock_nbt_int")
    expect_equal(vctrs::vec_ptype_full(nbt_long(1)), "rbedrock_nbt_long")
    expect_equal(vctrs::vec_ptype_full(nbt_float(1)), "rbedrock_nbt_float")
    expect_equal(vctrs::vec_ptype_full(nbt_double(1)), "rbedrock_nbt_double")
    expect_equal(
        vctrs::vec_ptype_full(nbt_byte_array(1)),
        "rbedrock_nbt_byte_array"
    )
    expect_equal(vctrs::vec_ptype_full(nbt_string("a")), "rbedrock_nbt_string")
    expect_equal(
        vctrs::vec_ptype_full(nbt_raw_string(as.raw(0L))),
        "rbedrock_nbt_raw_string"
    )
    expect_equal(vctrs::vec_ptype_full(nbt_list()), "rbedrock_nbt_list")
    expect_equal(
        vctrs::vec_ptype_full(nbt_compound()),
        "rbedrock_nbt_compound"
    )
    expect_equal(
        vctrs::vec_ptype_full(nbt_int_array(1)),
        "rbedrock_nbt_int_array"
    )
    expect_equal(
        vctrs::vec_ptype_full(nbt_long_array(1)),
        "rbedrock_nbt_long_array"
    )
})
