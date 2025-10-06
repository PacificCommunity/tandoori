# I'm pretty sure get_recruitment_orig() is a legacy function.
test_that("get_recruitment_orig() returns error", {
  expect_error(get_recruitment_orig())
})

test_that("get_recruitment_orig() accepts srr_devs with correct dimensions", {
  # Load the package data for now
  # Really, we should have data specifically for testing
  load("../../data/bet_projection_bits.Rdata")

  # Incorrect dimensions
  expect_error(
    get_recruitment_orig(mfcl_pop_n, waa, mat, srr_params, srr_devs, rec_dist),
    "non-conformable arrays"
  )

  # Correct dimensions
  zero_srr_devs <- FLQuant(
    NA,
    dimnames = list(
      year = 2021:2051,
      iter = 1
    )
  )
  zero_srr_devs[, , , , , 1] <- rep(0, length(2021:2051))
  expect_warning(
    get_recruitment_orig(mfcl_pop_n, waa, mat, srr_params, zero_srr_devs, rec_dist),
    "STATS is longer than the extent"
  )
})

# I think get_annual_recruitment() is the replacement of get_recruitment_orig()
test_that("get_annual_recruitment() class", {
  expect_equal(class(get_annual_recruitment)[1], "nonstandardGenericFunction")
})

test_that("get_annual_recruitment() accepts simpleBiol", {
  load("../../data/bet_projection_bits.Rdata")
  biol_flq <- FLQuant(
    NA,
    dimnames = list(
      age = 1:10,
      year = 1:10,
      season = c("Summer", "Autumn", "Winter", "Spring"),
      area = 1:3,
      iter = 1:20
    )
  )
  biol_example <- simpleBiol(
    biol_flq,
    name = "test",
    desc = "Example simpleBiol for testing"
  )
  m(biol_example) <- m
  mat(biol_example) <- mat
  n(biol_example) <- mfcl_pop_n
  movement(biol_example) <- movement
  rec_dist(biol_example) <- rec_dist
  srr_params(biol_example) <- srr_params
  wt(biol_example) <- waa

  dimnames(srr_devs)$year <- 2021:2050

  rec <- get_annual_recruitment(biol_example, year = 2022, srr_devs = srr_devs)

  expect_equal(class(rec)[1], "FLQuant")

  # Additional tests should check the folllowing:
  # year has to be at least minyear + 1
  # year has to be numeric? can it be a character?
  # the necessary slots: n, mat, wt, srr_params, rec_dist
})
