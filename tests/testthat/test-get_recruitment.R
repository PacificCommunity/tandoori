# I'm pretty sure get_recruitment_orig() is a legacy function.
test_that("get_recruitment_orig() returns error", {
  expect_error(get_recruitment_orig())
})

# I think get_annual_recruitment() is the replacement of get_recruitment_orig()
test_that("get_annual_recruitment() class", {
  expect_equal(class(get_annual_recruitment)[1], "nonstandardGenericFunction")
})

test_that("get_annual_recruitment() accepts simpleBiol", {
  rec <- get_annual_recruitment(bet, year = 2022, srr_devs = srr_devs)

  expect_equal(class(rec)[1], "FLQuant")

  # Additional tests should check the folllowing:
  # year has to be at least minyear + 1
  # year has to be numeric? can it be a character?
  # the necessary slots: n, mat, wt, srr_params, rec_dist
})
