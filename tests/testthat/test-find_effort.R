# tandoori is primarily accessed through find_effort() which calls many
# other functions

test_that("find_effort", {
  # Model info
  season_names <- dimnames(m(bet))$season
  nseasons <- length(season_names)
  nfisheries <- dim(effort(bet_fish))[3]
  nages <- dim(m(bet))[1]
  nareas <- dim(m(bet))[5]
  niters <- dim(m(bet))[6]

  # Projection years
  start_year <- 2022
  final_year <- 2051

  # Target set up
  # Catch only set up to test q
  catch_fisheries <- 1:nfisheries
  effort_fisheries <- numeric(0)
  target <- FLQuant(
    NA,
    dimnames = list(
      year = start_year:final_year,
      season = season_names,
      unit = 1:nfisheries,
      iter = 1:niters
    )
  )
  sq <- apply(mfcl_catch_wt[, ac(2019:2021)], c(1, 3, 4, 5, 6), mean)
  target[, ac(start_year:final_year)] <- sq
  any(target[, ac(2022), , 1] == 0) # Some 0 catch targets
  # Set index fisheries to catch = 1
  target[, ac(start_year:final_year), 33:41] <- 1
  # Are we allowed 0 targets? No. Bombs
  target[target <= 0] <- 1e-9

  # Set type based on effort_ and catch_fisheries
  target_type <- rep(0, nfisheries)
  target_type[effort_fisheries] <- 1

  # Main projection loop
  ycount <- 2022
  scount <- 1
  iter_count <- 1

  # Recruitment different in each iter due to srr_dev!
  annual_rec <- get_annual_recruitment(bet, year = ycount, srr_devs = srr_devs)
  annual_rec_f0 <- get_annual_recruitment(bet, year = ycount, srr_devs = srr_devs, zero_effort = TRUE)
  # Prevent negative values
  annual_rec[annual_rec < 1] <- 1
  annual_rec_f0[annual_rec_f0 < 1] <- 1
  # Put recruitment into place
  n(bet)[1, ac(ycount), ] <- annual_rec
  n0(bet)[1, ac(ycount), ] <- annual_rec_f0

  selq <- sweep(sel(bet_fish), 2:6, catch_q(bet_fish), "*")[, ac(ycount), , scount]
  selq[selq == 0] <- 1e-12
  max_mean_f <- 5
  max_effort <- max_mean_f / apply(selq, c(2:6), mean)

  effout <- find_effort(
    n_pre_move = n(bet)[, ac(ycount), , scount, , iter_count, drop = TRUE],
    m = m(bet)[, ac(ycount), , scount, , iter_count, drop = TRUE],
    waa = catch_wt(bet_fish)[, ac(ycount), , scount, , iter_count, drop = TRUE] / 1000,
    movement = movement(bet)[, , , scount, iter_count, drop = TRUE],
    selq = selq[, , , , , iter_count, drop = TRUE],
    effort_mult_initial = 0.1,
    target = target[, ac(ycount), , scount, , iter_count, drop = TRUE],
    target_type = target_type,
    fishery_area = fishery_map(bet_fish),
    max_effort = max_effort[, , , , , iter_count, drop = TRUE],
    max_solver_iters = 50
  )

  expect_equal(length(effout), 3)
})
