test_that("nbt_build() converts correct types", {
  # nbt values pass through unchanged
  expect_equal(nbt_build(nbt_byte(10)), nbt_byte(10))

  # logical/raw to byte
  expect_equal(nbt_build(TRUE), nbt_byte(TRUE))
  expect_equal(nbt_build(as.raw(1)), nbt_byte(TRUE))

  # complex to short
  expect_equal(nbt_build(1i), nbt_short(1L))

  # integer to int
  expect_equal(nbt_build(1L), nbt_int(1L))

  # integer64 to long
  expect_equal(nbt_build(bit64::as.integer64(1L)), nbt_long(1L))

  # real to float
  expect_equal(nbt_build(1.0), nbt_float(1.0))

  # character to string
  expect_equal(nbt_build("1"), nbt_string("1"))

  # logical to byte list
  expect_equal(nbt_build(c(TRUE, FALSE)), nbt_byte_list(c(TRUE, FALSE)))
  expect_equal(nbt_build(I(TRUE)), nbt_byte_list(TRUE))

  # raw to raw strings
  expect_equal(nbt_build(as.raw(c(1, 0))), nbt_raw_string(c(1, 0)))
  expect_equal(nbt_build(I(as.raw(1))), nbt_raw_string(1))

  # complex to short list
  expect_equal(nbt_build(c(1i, 0i)), nbt_short_list(c(1, 0)))
  expect_equal(nbt_build(I(1i)), nbt_short_list(1))

  # integer to int list
  expect_equal(nbt_build(c(1L, 0L)), nbt_int_list(c(1, 0)))
  expect_equal(nbt_build(I(1L)), nbt_int_list(1))

  # integer64 to long list
  expect_equal(
    nbt_build(bit64::as.integer64(c(1L, 0L))),
    nbt_long_list(c(1, 0))
  )
  expect_equal(nbt_build(I(bit64::as.integer64(1L))), nbt_long_list(1))

  # float to float list
  expect_equal(nbt_build(c(1, 0)), nbt_float_list(c(1, 0)))
  expect_equal(nbt_build(I(1)), nbt_float_list(1))

  # character to string list
  expect_equal(nbt_build(c("1", "0")), nbt_string_list(c("1", "0")))
  expect_equal(nbt_build(I("1")), nbt_string_list("1"))

  # NULL to empty list
  expect_equal(nbt_build(NULL), nbt_empty_list())

  # named list to compound
  expect_equal(
    nbt_build(list(T = TRUE, F = FALSE)),
    nbt_compound(T = nbt_byte(TRUE), F = nbt_byte(FALSE))
  )

  # unamed list to compound list
  expect_equal(
    nbt_build(list(list(T = TRUE, F = FALSE))),
    nbt_compound_list(nbt_compound(T = nbt_byte(TRUE), F = nbt_byte(FALSE)))
  )

  # error if unknown type
  expect_error(nbt_build(~1))
  expect_error(nbt_build(nbt_build))
})

test_that("nbt_build_compound() converts correct types", {
  LL <- bit64::as.integer64(1L) # nolint
  obj <- nbt_build_compound(
    byte = TRUE,
    short = 1i,
    int = 1L,
    long = 1 * LL,
    float = 1.0,
    string = "1"
  )
  expected <- nbt_compound(
    byte = nbt_byte(TRUE),
    short = nbt_short(1),
    int = nbt_int(1),
    long = nbt_long(1),
    float = nbt_float(1),
    string = nbt_string("1")
  )
  expect_equal(obj, expected)
})
