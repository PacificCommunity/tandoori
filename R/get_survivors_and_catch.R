#  
#  Copyright 2025 Finlay Scott. Distributed under the GPL 2 or later
#  Maintainer: Finlay Scott
#

# Function to calculate N in t+1 and catch in t
# i.e. year and season dimension of objects will be of length 1.
# Give it fishing effort, current N and other things and you get Nt+1 and Ct

# Add man page with expected dims - see example below

#' Get survivors and catch numbers at age in a time step
#'
#' Follows population and fishing dynamics from Multfan-CL.
#' Movement, followed by death (natural mortality and fishing).
#' @param effort Vector of effort
#' @param pop_n N at age
#' @param m Natural mortality at age
#' @param movement Movement rates between model regions
#' @param sel Selectivity at age
#' @param catch_q Catchability
#' @param fishery_area Vector of which model region each fishery operates in 
#' @return A list of survivors and catch numbers at age
#' @export
get_survivors_and_catch <- function(effort, pop_n, m, movement, sel, catch_q, fishery_area){
 
  nareas <- dim(pop_n)[5]
  # Get f by fishery
  selq <- sweep(sel, c(2,3,4,5,6), catch_q, "*")
  f <- sweep(selq, c(2:6), effort, "*")
  
  # But we also need f by area (so we can calculate total Z in an area)
  farea <- expand(f, area=dimnames(pop_n)$area)
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
  
  nages <- dim(pop_n)[1]
  n_out <- pop_n
  n_out[] <- NA
  n_after_move <- n_out
  
  # N and catch by age
  # General rule: movement then death, so death applied to moved population
  for (age_count in 1:nages){
    # Movement happens
    n_after_move[age_count] <- movement[,, age_count] %*% pop_n[age_count,]
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

# New get survivors and catch using FLFisheries and FLBiol

# Keep it simple = E * sel * q
# Takes forever
setGeneric("fmort", function(fisheries, year, season, ...) standardGeneric("fmort"))
setMethod("fmort", signature(fisheries="FLFisheries", year='missing', season='missing'),
  function(fisheries){
    # Output is an FLQuant with unit dim of length nfisheries
    # This is quite dodgy - assumes all FLF objects have same dims
    dmns <- dimnames(catch.sel(fisheries[[1]][[1]]))
    dmns$unit <- 1:length(fisheries)
    fmort <- FLQuant(NA, dimnames=dmns)
    for (fi in 1:length(fisheries)){
      # Assume one catch per fishery at the moment - single stock
      fmort[,,fi,] <- sweep(catch.sel(fisheries[[fi]][[1]]), c(2:6), catch.q(fisheries[[fi]][[1]]) * effort(fisheries[[fi]]), "*")
    }
    return(fmort)
})

setMethod("fmort", signature(fisheries="FLFisheries", year='numeric', season='numeric'),
  function(fisheries, year, season){
    # Output is an FLQuant with unit dim of length nfisheries
    # This is quite dodgy - assumes all FLF objects have same dims
    dmns <- dimnames(catch.sel(fisheries[[1]][[1]])[, ac(year),,season])
    dmns$unit <- 1:length(fisheries)
    fmort <- FLQuant(NA, dimnames=dmns)
    for (fi in 1:length(fisheries)){
      # Assume one catch per fishery at the moment - single stock
      fmort[,,fi,] <- sweep(catch.sel(fisheries[[fi]][[1]])[, ac(year),,season], c(2:6), catch.q(fisheries[[fi]][[1]])[, ac(year),,season] * effort(fisheries[[fi]])[, ac(year),,season], "*")
    }
    return(fmort)
})


setGeneric("get_survivors_and_catch", function(fisheries, biol, ...) standardGeneric("get_survivors_and_catch"))

setMethod("get_survivors_and_catch", signature(fisheries="FLFisheries", biol="FLBiol"),
  function(fisheries, biol, movement, year, season){
    
    # fmort of each fishery in each area
    fm <- fmort(fisheries, year, season)
    #fmort_fishery <- areaSums(fm) # Do we need this?
    fmort_area <- unitSums(fm)
    z_area <- m(biol)[, ac(year),,ac(season)] + fmort_area # z by age and area
    # For individual fisheries what is F from that fishery as proportion of total Z on stock?
    fprop_fishery <- sweep(fm, c(1,2,4,5,6), z_area, "/")
  
  
    nages <- dim(n(biol))[1]
    # Temp objects holder 
    catch_out <- fm
    catch_out[] <- NA
    n_out <- n(biol)[, as.character(year+1),,season]
    n_out[] <- NA
    #n_after_move <- n(biol)[, as.character(year),,season]
    n_after_move[] <- n_out
  
    # N and catch by age
    # General rule: movement then death, so death applied to moved population
    for (age_count in 1:nages){
      # Movement happens
      n_after_move[age_count] <- movement[,, age_count, season] %*% n(biol)[age_count, ac(year),, season]
      # Survivors
      n_out[age_count] <- n_after_move[age_count] * exp(-z_area[age_count,])
      # Apply death to moved population and get catch
      prop_dead <- sweep(fprop_fishery[age_count,], c(1,2,4,5,6), (1-exp(-z_area[age_count,])), "*")
      catch_out[age_count, ] <- sweep(prop_dead, c(1,2,4,5,6), n_after_move[age_count], "*")
    }
  
    # Sort out plusgroup
    # move everyone down an age, insert NA at top, and sum last two ages
    n_out[(nages-1),] <- n_out[nages,] + n_out[nages-1,]
    n_out[2:nages,] <- n_out[1:(nages-1),]
    # Recruitment handled separately
    n_out[1,] <- NA
    
    # Put the outputs into the objects? Or just return them
    return(list(n_out=n_out, catch_out=catch_out))
  })



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
#                     pop_n = nt, m=mt, movement=movementt,
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






