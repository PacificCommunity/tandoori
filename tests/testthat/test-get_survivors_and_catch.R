# test_that("get_survivors_and_catch_orig() basic use", {
#   load("../../data/bet_projection_bits.Rdata")

#   get_survivors_and_catch_orig(effort, pop_n, m, movement, sel, catch_q, fishery_area)
# })


test_that("fmort() works without year and season", {
  fish_flq <- FLQuant(
    NA,
    dimnames = list(
      age = 1:10,
      year = 1:10,
      unit = 1:2,
      season = c("Summer", "Autumn", "Winter", "Spring"),
      iter = 1:20
    )
  )

  # Each fishery is in a single area - so drop the fishery dimension
  # Need to include fishery map inside the object
  fishery_example <- simpleFisheries(fish_flq)

  expect_equal(dim(fmort(fishery_example)), c(10, 10, 2, 4, 1, 20))
})

test_that("fmort() works with year and season", {
  fish_flq <- FLQuant(
    NA,
    dimnames = list(
      age = 1:10,
      year = 1:10,
      unit = 1:2,
      season = c("Summer", "Autumn", "Winter", "Spring"),
      iter = 1:20
    )
  )
  fishery_example <- simpleFisheries(fish_flq)

  expect_equal(
    dim(fmort(fishery_example, year = 1, season = 1)),
    c(10, 1, 2, 1, 1, 20)
  )
})

test_that("fmort() does not work with year but without season", {
  fish_flq <- FLQuant(
    NA,
    dimnames = list(
      age = 1:10,
      year = 1:10,
      unit = 1:2,
      season = c("Summer", "Autumn", "Winter", "Spring"),
      iter = 1:20
    )
  )
  fishery_example <- simpleFisheries(fish_flq)

  expect_error(
    fmort(fishery_example, year = 1),
    "unable to find an inherited method"
  )
})


test_that("get_survivors_and_catch() basic use", {
  fish_flq <- FLQuant(
    NA,
    dimnames = list(
      age = 1:10,
      year = 1:10,
      unit = 1:2,
      season = 1:4,
      iter = 1:20
    )
  )
  biol_flq <- FLQuant(
    NA,
    dimnames = list(
      age = 1:10,
      year = 1:10,
      season = 1:4,
      area = 1:3,
      iter = 1:20
    )
  )
  biol_example <- simpleBiol(
    biol_flq,
    name = "test",
    desc = "Example simpleBiol for testing"
  )
  fishery_example <- simpleFisheries(fish_flq)

  # placeholder
  expect_equal(2, 2)

  # What's going on with fishery_map?
  # Do we want this to be a global variable?
  # get_survivors_and_catch(fishery_example, biol_example, year = 1, season = 1)

  # I think we need to specify the class of year and season to get better error messages from:
  # get_survivors_and_catch(fishery_example, biol_example)
})
