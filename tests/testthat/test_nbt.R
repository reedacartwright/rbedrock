# nolint start: indentation_linter

# A really big list of NBT values
rawval_args <- list(
    list(b = 1L, "pos", b = 100,
         b = 1L, "neg", b = 254),
    list(b = 2L, "pos", s = 100,
         b = 2L, "neg", s = -2),
    list(b = 3L, "pos", i = 100,
         b = 3L, "neg", i = -2),
    list(b = 4L, "pos", l = "4000000000000000001",
         b = 4L, "neg", l = "-4000000000000000001"),
    list(b = 5L, "pos", f = 36.125,
         b = 5L, "neg", f = -1776.75),
    list(b = 6L, "pos", d = 36.125,
         b = 6L, "neg", d = -1776.75),
    list(b = 7L, "8val", 8L, b = 10:17,
         b = 7L, "1val", 1L, b = 20),
    list(b = 8L, "test", "hello world",
         b = 8L, "empty", "",
         b = 8L, "greek", "\u03B1\u03B2\u03B3",
         b = 8L, "raw", us = 3, b = c(97, 0, 98)),
    list(b = 11L, "8val", 8L, i = 10:17,
         b = 11L, "1val", 1L, i = 20),
    list(b = 12L, "8val", 8L, l = 10:17,
         b = 12L, "1val", 1L, l = 20),
    list(b = 10L, "test",
         b =  1L, "a", b = 1,
         b =  3L, "b", i = 2,
         b =  0L),
    list(b = 9L, "empty", b = 0L, 0L),
    list(b = 9L, "a", b = 1L, 2L, b = 1:2,
         b = 9L, "b", b = 1L, 1L, b = -3),
    list(b = 9L, "a", b = 2L, 2L, s = 1:2,
         b = 9L, "b", b = 2L, 1L, s = -3),
    list(b = 9L, "a", b = 3L, 2L, i = 1:2,
         b = 9L, "b", b = 3L, 1L, i = -3),
    list(b = 9L, "a", b = 4L, 2L, l = 1:2,
         b = 9L, "b", b = 4L, 1L, l = -3),
    list(b = 9L, "a", b = 5L, 2L, f = 1:2,
         b = 9L, "b", b = 5L, 1L, f = -3),
    list(b = 9L, "a", b = 6L, 2L, d = 1:2,
         b = 9L, "b", b = 6L, 1L, d = -3),
    list(b = 9L, "a", b = 7L, 2L,
         2, b = 1:2, 1, b = -3),
    list(b = 9, "abc", b = 8, 3, "a", "b", "c",
         b = 9, "raw", b = 8, 1, us = 3, b = c(97, 0, 98),
         b = 9, "lst", b = 8, 2, "abc", us = 3, b = c(97, 0, 98)),
    list(b = 9L, "a", b = 11L, 2L,
         2, i = 1:2, 1, i = -3),
    list(b = 9L, "a", b = 12L, 2L,
         2, l = 1:2, 1, l = -3),
    list(b = 9, "cmplst", b = 10, 2,
         b = 1, "a", b = 10, b = 0,
         b = 1, "x", b = 20,
         b = 1, "y", b = 21, b = 0),
    list(b = 9, "lstlst", b = 9, 2,
         b = 1, 1, b = 1,
         b = 2, 2, s = -1:0)
    )
rawval_args_u <- unlist(rawval_args, recursive = FALSE)

test_that("read_nbt() and write_nbt() are inverses", {
    rawval_le <- as_raw_le(rawval_args_u)
    rawval_be <- as_raw_be(rawval_args_u)
    rawval_lv <- as_raw_lv(rawval_args_u)

    nbt_val_le <- expect_silent(read_nbt(rawval_le))
    expect_s3_class(nbt_val_le, "rbedrock_nbt_list_of")
    expect_equal(write_nbt(nbt_val_le), rawval_le)

    nbt_val_le <- expect_silent(read_nbt(rawval_le, format = "little"))
    expect_s3_class(nbt_val_le, "rbedrock_nbt_list_of")
    expect_equal(write_nbt(nbt_val_le, format = "little"), rawval_le)

    nbt_val_be <- expect_silent(read_nbt(rawval_be, format = "big"))
    expect_s3_class(nbt_val_be, "rbedrock_nbt_list_of")
    expect_equal(write_nbt(nbt_val_be, format = "big"), rawval_be)

    nbt_val_lv <- expect_silent(read_nbt(rawval_lv, format = "network"))
    expect_s3_class(nbt_val_lv, "rbedrock_nbt_list_of")
    expect_equal(write_nbt(nbt_val_lv, format = "network"), rawval_lv)
})

# nolint end
