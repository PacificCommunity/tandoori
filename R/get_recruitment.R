#
#  Copyright 2025 Finlay Scott. Distributed under the GPL 2 or later
#  Maintainer: Finlay Scott
#

#' Get recruitment
#'
#' Get recruitment in all seasons and regions of year y+1, given n etc in year y
#' @param pop_n Numbers at age in each season of year y
#' @param waa Weights at age
#' @param mat Maturity at age
#' @param srr_params SRR parameters
#' @param srr_devs Deviates in year y
#' @param rec_dist Proportion of total annual recruitment in each season and region

get_recruitment_orig <- function(
  pop_n,
  waa,
  mat,
  srr_params,
  srr_devs,
  rec_dist
) {
  # Get total annual average SSB in year y
  ssb_total <- seasonMeans(areaSums(quantSums(pop_n * waa * mat))) / 1000
  # Get total rec in year y+1
  # Beverton Holt with bias correction
  total_rec <- (ssb_total * srr_params['a']) /
    (ssb_total + srr_params['b']) *
    exp(srr_params['sigma'] / 2)
  # Apply deviate from year y+1
  total_rec_with_dev <- total_rec - srr_devs
  # Spread over areas and seasons - based on what...
  new_rec <- sweep(rec_dist, c(1, 2, 3, 6), total_rec_with_dev, "*")
  return(new_rec)
}

#' @rdname get_annual_recruitment
#' @description
#' Calculates recruitment in all seasons and areas of a year.
#' Based on the MFCL method of getting total SSB in year-1, calculating total recruitment in year,
#' applying a deviate, then splitting the resulting total annual recruitment across the seasons and areas in year.
#' @param year The year recruitment is to be calculated for.
#' @param srr_devs An FLQuant of total annual recruitment deviates.
#' @param zero_effort If TRUE, use n0 slot to recruitment with no fishing.
#' @aliases get_annual_recruitment get_annual_recruitment-method
#' @examples
#' \dontrun{
#' data(bet_projection_low_catch_bits_simple)
#' annual_rec <- get_annual_recruitment(bet, year=ycount, srr_devs=srr_devs)
#' }
setGeneric("get_annual_recruitment", function(object, ...) {
  standardGeneric("get_annual_recruitment")
})

setMethod(
  "get_annual_recruitment",
  signature(object = "simpleBiol"),
  function(object, year, srr_devs, zero_effort = FALSE) {
    # Check dims of srr_devs
    # Maybe types too - add to dispatch
    # Get ssb in previous year
    # Marginally faster to go by hand, rather than calling ssb() then subsetting year
    if (!zero_effort) {
      ssb_total <- seasonMeans(areaSums(quantSums(
        n(object)[, ac(year - 1)] *
          mat(object)[, ac(year - 1)] *
          wt(object)[, ac(year - 1)]
      ))) /
        1000
    } else {
      ssb_total <- seasonMeans(areaSums(quantSums(
        n0(object)[, ac(year - 1)] *
          mat(object)[, ac(year - 1)] *
          wt(object)[, ac(year - 1)]
      ))) /
        1000
    }
    total_rec <- (ssb_total * srr_params(object)["a"]) /
      (ssb_total + srr_params(object)["b"]) *
      exp(srr_params(object)["sigma"] / 2)
    total_rec_with_dev <- total_rec - srr_devs[, ac(year)]
    # Spread over areas and seasons - based on what...
    new_rec <- sweep(rec_dist(object), c(1, 2, 3, 6), total_rec_with_dev, "*")
    return(new_rec)
  }
)

# data(yft_projection_bits)
#
# ycount <- 3
#
# new_rec <- get_recruitment(pop_n=mfcl_pop_n[,ycount-1], waa=waa[,ycount-1], mat=mat[,ycount-1],
#                            srr_params=srr_params, srr_devs=srr_devs[,ycount-1], rec_dist=rec_dist)
#
# sum(c(mfcl_pop_n[1,ycount]))
# sum(c(new_rec))
#
# (c(mfcl_pop_n[1,ycount]))
# (c(new_rec))
#
# (c(mfcl_pop_n[1,ycount])) / (c(new_rec))
