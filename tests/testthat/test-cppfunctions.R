# Use this script to write tests in R that access C++ functions
test_that("R <-> C++ interface works", {
  expect_equal(int_test(2), 4)
})


test_that("Make 2D array", {
  expect_type(
    simple_array_2D_constructor(1, 1, 1),
    "double"
  )
  expect_type(
    simple_array_2D_constructor(1, 1, NA),
    "double"
  )
  expect_length(
    simple_array_2D_constructor(2, 2, 0),
    4
  )
})

test_that("Access array", {
  # Intended use
  expect_equal(
    simple_array_2D_get_accessor_test(
      simple_array_2D_constructor(2, 2, 0), 1, 1
    ),
    0
  )
  # NA elements
  expect_equal(
    simple_array_2D_get_accessor_test(
      simple_array_2D_constructor(2, 2, NA), 1, 1
    ),
    as.double(NA)
  )
  # Wrong dims
  expect_error(
    simple_array_2D_get_accessor_test(
      simple_array_2D_constructor(2, 2, 0), 3, 1
    ),
    "Trying to access element outside of x or y dim range"
  )
  # Wrong dims
  expect_error(
    simple_array_2D_get_accessor_test(
      simple_array_2D_constructor(2, 2, 0), 2, 2
    ),
    "Trying to access element outside of x or y dim range"
  )
  # Zero indexing cpp style
  expect_equal(
    simple_array_2D_get_accessor_test(
      simple_array_2D_constructor(2, 2, 0), 0, 0
    ),
    0
  )
})
