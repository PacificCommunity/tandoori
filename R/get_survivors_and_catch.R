#  
#  Copyright 2025 Finlay Scott. Distributed under the GPL 2 or later
#  Maintainer: Finlay Scott
#

# Function to calculate N in t+1 and catch in t
# i.e. year and season dimension of objects will be of length 1.
# Give it fishing effort, current N and other things and you get Nt+1 and Ct

# Add man page with expected dims - see example below

get_survivors_and_catch <- function(effort, n, m, mat, movement, sel, catch_q, fishery_area){
  nareas <- dim(n)[5]
  
  # Get f by fishery
  selq <- sweep(sel, c(2,3,4,5,6), catch_q, "*")
  f <- sweep(selq, c(2:6), effort, "*")
  
  # But we also need f by area (so we can calculate total Z in an area)
  farea <- expand(f, area=dimnames(n)$area)
  farea[] <- 0
  for(area in 1:nareas){
    farea[,,fishery_area==area,,area] <- f[,,fishery_area == area]
  }
  # Get total F and Z by area
  total_f <- unitSums(farea) # f by age and area
  total_z <- m + total_f # z by age and area
  # For individual fisheries what is F from that fishery as proportion of total Z on stock?
  fish_fprop <- sweep(farea, c(1,2,4,5,6), total_z, "/")
  
  catch_out <- farea
  catch_out[] <- NA
  
  nages <- dim(n)[1]
  n_out <- n
  n_out[] <- NA
  n_after_move <- n_out
  
  # N and catch by age
  # General rule: movement then death, so death applied to moved population
  for (age_count in 1:nages){
    # Movement happens
    n_after_move[age_count] <- movement[,, age_count] %*% n[age_count,]
    # survivors
    n_out[age_count] <- n_after_move[age_count] * exp(-total_z[age_count,])
    # Apply death to moved population and get catch
    prop_dead <- sweep(fish_fprop[age_count,], c(1,2,4,5,6), (1-exp(-total_z[age_count,])), "*")
    catch_out[age_count, ] <- sweep(prop_dead, c(1,2,4,5,6), n_after_move[age_count], "*")
  }
  
  # Sort out plusgroup
  # move everyone down an age, insert NA at top, and sum last two ages
  n_out[(nages-1),] <- n_out[nages,] + n_out[nages-1,]
  n_out[2:nages,] <- n_out[1:(nages-1),]
  # Recruitment handled separately
  n_out[1,] <- NA
  
  return(list(n_out=n_out, catch_out=catch_out))
}

#data("yft_projection_bits")
## Demo
## Model info
#season_names <- dimnames(m)$season
#nseasons <- length(season_names)
#nfisheries <- nrow(fishery_map)
#nages <- dim(m)[1]
#nareas <- dim(m)[5]
#
## Projection years
#start_year <- 2022
#final_year <- 2051
#
## timestep 
#ycount <- 2
#scount <- 1
#
## Assume we have N in yvount 2, scount 1 (as we have everything else in ycount 1, scount 4 we can ge survivors)
#nt <- mfcl_pop_n[,ycount,,scount]
#mt <- m[,ycount,,scount]
#movementt <- movement[,,,scount]
#selt <- sel # Noting that sel might change over time
#catch_qt <- catch_q[,ycount,,scount]
#fishery_area <- fishery_map$area
#
#
## Back calculate 'true' effort
## Magic up approx effort from F and selq
#effage <- sweep(mfcl_fmort[,ycount,,scount] / selt, c(2:6), catch_qt, "/")
#effage[is.infinite(effage)] <- NA
## Variation in effort across age shows that we have inconsistent info for fmort (or sel)
## For each fishery which age has the maximum F
## Or biggest sel?
#max_fage <- c(apply(mfcl_fmort[,ycount,,scount], c(2:6), which.max))
#effortt <- rep(NA, 37)
#for(fi in 1:37){
#  effortt[fi] <- effage[max_fage[fi],,fi,]
#}
#effortt[is.nan(effortt)] <- 1e-11
#
## Excluding recruitment
#test_nc <- get_survivors_and_catch(effort=effortt,
#                     n = nt, m=mt, mat=matt, movement=movementt,
#                     sel=selt, catch_q=catch_qt, fishery_area=fishery_area)
#
## Check numbers at age
#test_nc[["n_out"]][1:10,,,,1]
#mfcl_pop_n[1:10,ycount,,scount+1,1]
#summary(test_nc[["n_out"]][-1,,,,] / mfcl_pop_n[-1,ycount,,scount+1,])
#
#cwt <- c(areaSums(quantSums(sweep(test_nc[["catch_out"]], c(1,2,4,5,6), waat, "*")))) / 1000
#mfcl_cwt <- c(mfcl_catch_wt[,ycount,,scount])
#summary(cwt / mfcl_cwt)
#(cwt / mfcl_cwt)
## Some of them out by a lot and yet next N is OK
#which.min(cwt / mfcl_cwt)
## Could be those that have tiny catches, but weird effort, and so impact on stock is low
## And that is N is only by area (5), whereas catch is by fishery (37)
#cwt
#
## Try mean waa - closer with this...
#cwt2 <- c(areaSums(quantSums(sweep(test_nc[["catch_out"]], c(1,2,4,5,6), mean_waat, "*")))) / 1000
#summary(cwt2 / mfcl_cwt)
#which.min(cwt2 / mfcl_cwt)






