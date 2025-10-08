/*
 * This file uses the Catch unit testing library, alongside
 * testthat's simple bindings, to test a C++ function.
 *
 * For your own packages, ensure that your test files are
 * placed within the `src/` folder, and that you include
 * `LinkingTo: testthat` within your DESCRIPTION file.
 */

// All test files should include the <testthat.h>
// header file.
#include <testthat.h>
// #include "../inst/include/tandoori.h"

// Declare exported function
int int_test(int dummy);

context("cpp dummy function") {
  test_that("double function") {
    expect_true(int_test(2) == 4);
    expect_true(int_test(-2) == -4);
    expect_true(int_test(0) == 0);
  }
}



