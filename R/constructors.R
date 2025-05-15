#  
#  Copyright 2025 Finlay Scott. Distributed under the GPL 2 or later
#  Maintainer: Finlay Scott
#

# simpleBiol
#' @rdname simpleBiol
#' @aliases simpleBiol simpleBiol-methods
setGeneric('simpleBiol', function(object, ...) standardGeneric('simpleBiol'))


#' @rdname simpleBiol
#' @aliases simpleBiol,missing-method simpleBiol,FLQuant-method simpleBiolcpp-class
setMethod('simpleBiol', signature(object='FLQuant'),
          function(object, ...) {
            args <- list(...)
            # empty object
            object[] <- NA
            units(object) <- "NA"
            # rec_dist, movement and srr_params need some extra work
            # Set up empty movement: nareas x nareas x nages x nseasons x niters
            dims <- dim(object)
            nareas <- dims[5]
            nages <- dims[1]
            nseasons <- dims[4]
            niters <- dims[6]
            dmns <- dimnames(object)
            movement <- array(NA, dim=c(nareas, nareas, nages, nseasons, niters),
                              dimnames=list(to=dmns$area, from=dmns$area, age=dmns$age, season=dmns$season, iter=dmns$iter))
            rec_dist <- FLQuant(NA, dimnames=list(season=dmns$season, area=dmns$area, iter=dmns$iter))
            srr_params <- FLPar(NA, dimnames=list(params=c("a", "b", "steepness", "sigma"), iter=dmns$iter))
            res <- new("simpleBiol", n=object, n0=object, m=object, wt=object, mat=object, movement=movement, rec_dist=rec_dist, srr_params=srr_params)
            # Load given slots
            for(i in names(args)){
              slot(res, i) <- args[[i]]
            }
            return(res)
          }
)

setMethod('simpleBiol', signature(object='missing'),
          function(...) {
            args <- list(...)
            slots <- unlist(lapply(args, function(x) is(x, 'FLQuant')))
            slots <- names(slots[slots])
            # if no FLQuant argument given, then use empty FLQuant
            if(length(slots) == 0){
              object <- FLQuant()
            }
            # if at least 1, use first one
            else if(length(slots) == 1) {
              object <- args[[slots[1]]]
            }
            return(simpleBiol(object, ...))
          }
) 


# simpleFisheries
#' @rdname simpleFisheries
#' @aliases simpleFisheries simpleFisheries-methods
setGeneric('simpleFisheries', function(object, ...) standardGeneric('simpleFisheries'))


#' @rdname simpleFisheries
#' @aliases simpleFisheries,missing-method simpleFisheries,FLQuant-method simpleFisheriescpp-class
setMethod('simpleFisheries', signature(object='FLQuant'),
          function(object, ...) {
            args <- list(...)
            # empty object
            object[] <- NA
            units(object) <- "NA"
            nfisheries <- dim(object)[3]
            fishery_map <- as.numeric(rep(NA, nfisheries))
            res <- new("simpleFisheries", catch_n=object, catch_wt=object, sel=object, catch_q=object[1,], effort=object[1,], fishery_map=fishery_map)
            # Load given slots
            for(i in names(args)){
              slot(res, i) <- args[[i]]
            }
            return(res)
          }
)

setMethod('simpleFisheries', signature(object='missing'),
          function(...) {
            args <- list(...)
            slots <- unlist(lapply(args, function(x) is(x, 'FLQuant')))
            slots <- names(slots[slots])
            # if no FLQuant argument given, then use empty FLQuant
            if(length(slots) == 0){
              object <- FLQuant()
            }
            # if at least 1, use first one
            else if(length(slots) == 1) {
              object <- args[[slots[1]]]
            }
            return(simpleFisheries(object, ...))
          }
) 




