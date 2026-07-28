test_that("nbt_compound_list() works with single values", {
  obj <- structure(list(structure(list(T = structure(1, class = c(
    "rbedrock_nbt_byte",
    "rbedrock_nbt_numeric", "rbedrock_nbt_value"
  )), F = structure(0, class = c(
    "rbedrock_nbt_byte",
    "rbedrock_nbt_numeric", "rbedrock_nbt_value"
  ))), class = c(
    "rbedrock_nbt_compound",
    "rbedrock_nbt_value", "rbedrock_nbt_list_of", "list"
  ))), class = c(
    "rbedrock_nbt_compound_list",
    "rbedrock_nbt_list_value", "rbedrock_nbt_value", "rbedrock_nbt_list_of",
    "list"
  ))

  expect_equal(
    nbt_compound_list(nbt_compound(T = nbt_byte(TRUE), F = nbt_byte(FALSE))),
    obj
  )

  expect_equal(
    nbt_compound_list(list(T = nbt_byte(TRUE), F = nbt_byte(FALSE))),
    obj
  )
})

test_that("nbt_compound_list() works with multiple values", {
  obj <- structure(list(structure(list(T = structure(1, class = c(
    "rbedrock_nbt_byte",
    "rbedrock_nbt_numeric", "rbedrock_nbt_value"
  )), F = structure(0, class = c(
    "rbedrock_nbt_byte",
    "rbedrock_nbt_numeric", "rbedrock_nbt_value"
  ))), class = c(
    "rbedrock_nbt_compound",
    "rbedrock_nbt_value", "rbedrock_nbt_list_of", "list"
  )), structure(list(
    T = structure(1, class = c(
      "rbedrock_nbt_byte", "rbedrock_nbt_numeric",
      "rbedrock_nbt_value"
    )), F = structure(0, class = c(
      "rbedrock_nbt_byte",
      "rbedrock_nbt_numeric", "rbedrock_nbt_value"
    ))
  ), class = c(
    "rbedrock_nbt_compound",
    "rbedrock_nbt_value", "rbedrock_nbt_list_of", "list"
  ))), class = c(
    "rbedrock_nbt_compound_list",
    "rbedrock_nbt_list_value", "rbedrock_nbt_value", "rbedrock_nbt_list_of",
    "list"
  ))

  expect_equal(
    nbt_compound_list(
      nbt_compound(T = nbt_byte(TRUE), F = nbt_byte(FALSE)),
      nbt_compound(T = nbt_byte(TRUE), F = nbt_byte(FALSE))
    ),
    obj
  )

  expect_equal(
    nbt_compound_list(
      list(T = nbt_byte(TRUE), F = nbt_byte(FALSE)),
      list(T = nbt_byte(TRUE), F = nbt_byte(FALSE))
    ),
    obj
  )
})
