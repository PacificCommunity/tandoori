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
