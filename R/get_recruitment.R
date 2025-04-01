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

get_recruitment <- function(pop_n, waa, mat, srr_params, srr_devs, rec_dist){
  # Get total annual average SSB in year y
  ssb_total <- seasonMeans(areaSums(quantSums(pop_n * waa * mat))) /1000
  # Get total rec in year y+1
  # Beverton Holt with bias correction
  total_rec <- (ssb_total * srr_params['a']) / (ssb_total + srr_params['b']) * exp(srr_params['sigma']/2)
  # Apply deviate from year y+1
  total_rec_with_dev <- total_rec - srr_devs
  # Spread over areas and seasons - based on what...
  new_rec <- sweep(rec_dist, c(1,2,3,6), total_rec_with_dev, "*")
  return(new_rec)
}


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