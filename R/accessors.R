#  
#  Copyright 2025 Finlay Scott. Distributed under the GPL 2 or later
#  Maintainer: Finlay Scott
#

# n
#' @rdname simpleBiol
#' @aliases n,simpleBiol-method
setMethod("n", signature(object="simpleBiol"),
          function(object) {
            return(slot(object, "n"))
          }
)

setReplaceMethod("n", signature(object="simpleBiol", value="FLQuant"),
                 function(object, value) {
                   slot(object, "n") <- value
                   return(object)
                 }
)

#' @rdname simpleBiol
#' @aliases n<-,simpleBiol,numeric-method
setReplaceMethod("n", signature(object="simpleBiol", value="numeric"),
                 function(object, value) {
                   slot(object, "n")[] <- value
                   return(object)
                 }
)
# n0
#' @rdname simpleBiol
#' @aliases simpleBiol simpleBiol-methods
setGeneric('n0', function(object, ...) standardGeneric('n0'))

#' @rdname simpleBiol
#' @aliases simpleBiol simpleBiol-methods
setGeneric('n0<-', function(object, value, ...) standardGeneric('n0<-'))

#' @rdname simpleBiol
#' @aliases n0,simpleBiol-method
setMethod("n0", signature(object="simpleBiol"),
          function(object) {
            return(slot(object, "n0"))
          }
)

setReplaceMethod("n0", signature(object="simpleBiol", value="FLQuant"),
                 function(object, value) {
                   slot(object, "n0") <- value
                   return(object)
                 }
)

#' @rdname simpleBiol
#' @aliases n0<-,simpleBiol,numeric-method
setReplaceMethod("n0", signature(object="simpleBiol", value="numeric"),
                 function(object, value) {
                   slot(object, "n0")[] <- value
                   return(object)
                 }
)


# m
#' @rdname simpleBiol
#' @aliases m,simpleBiol-method
setMethod("m", signature(object="simpleBiol"),
          function(object) {
            return(slot(object, "m"))
          }
)

setReplaceMethod("m", signature(object="simpleBiol", value="FLQuant"),
                 function(object, value) {
                   slot(object, "m") <- value
                   return(object)
                 }
)

#' @rdname simpleBiol
#' @aliases m<-,simpleBiol,numeric-method
setReplaceMethod("m", signature(object="simpleBiol", value="numeric"),
                 function(object, value) {
                   slot(object, "m")[] <- value
                   return(object)
                 }
)

# wt
#' @rdname simpleBiol
#' @aliases wt,simpleBiol-method
setMethod("wt", signature(object="simpleBiol"),
          function(object) {
            return(slot(object, "wt"))
          }
)

setReplaceMethod("wt", signature(object="simpleBiol", value="FLQuant"),
                 function(object, value) {
                   slot(object, "wt") <- value
                   return(object)
                 }
)

#' @rdname simpleBiol
#' @aliases wt<-,simpleBiol,numeric-method
setReplaceMethod("wt", signature(object="simpleBiol", value="numeric"),
                 function(object, value) {
                   slot(object, "wt")[] <- value
                   return(object)
                 }
)

# mat
#' @rdname simpleBiol
#' @aliases mat,simpleBiol-method
setMethod("mat", signature(object="simpleBiol"),
          function(object) {
            return(slot(object, "mat"))
          }
)

setReplaceMethod("mat", signature(object="simpleBiol", value="FLQuant"),
                 function(object, value) {
                   slot(object, "mat") <- value
                   return(object)
                 }
)

#' @rdname simpleBiol
#' @aliases mat<-,simpleBiol,numeric-method
setReplaceMethod("mat", signature(object="simpleBiol", value="numeric"),
                 function(object, value) {
                   slot(object, "mat")[] <- value
                   return(object)
                 }
)

# rec_dist
#' @rdname simpleBiol
#' @aliases simpleBiol simpleBiol-methods
setGeneric('rec_dist', function(object, ...) standardGeneric('rec_dist'))

#' @rdname simpleBiol
#' @aliases simpleBiol simpleBiol-methods
setGeneric('rec_dist<-', function(object, value, ...) standardGeneric('rec_dist<-'))

#' @rdname simpleBiol
#' @aliases movement,simpleBiol-method
setMethod("rec_dist", signature(object="simpleBiol"),
          function(object) {
            return(slot(object, "rec_dist"))
          }
)

setReplaceMethod("rec_dist", signature(object="simpleBiol", value="FLQuant"),
                 function(object, value) {
                   slot(object, "rec_dist") <- value
                   return(object)
                 }
)

#' @rdname simpleBiol
#' @aliases rec_dist<-,simpleBiol,numeric-method
setReplaceMethod("rec_dist", signature(object="simpleBiol", value="numeric"),
                 function(object, value) {
                   slot(object, "rec_dist")[] <- value
                   return(object)
                 }
)






# srr_params
#' @rdname simpleBiol
#' @aliases simpleBiol simpleBiol-methods
setGeneric('srr_params', function(object, ...) standardGeneric('srr_params'))

#' @rdname simpleBiol
#' @aliases simpleBiol simpleBiol-methods
setGeneric('srr_params<-', function(object, value, ...) standardGeneric('srr_params<-'))

#' @rdname simpleBiol
#' @aliases srr_params,simpleBiol-method
setMethod("srr_params", signature(object="simpleBiol"),
          function(object) {
            return(slot(object, "srr_params"))
          }
)

setReplaceMethod("srr_params", signature(object="simpleBiol", value="FLPar"),
                 function(object, value) {
                   slot(object, "srr_params") <- value
                   return(object)
                 }
)


#' @rdname simpleBiol
#' @aliases srr_params<-,simpleBiol,numeric-method
setReplaceMethod("srr_params", signature(object="simpleBiol", value="numeric"),
                 function(object, value) {
                   slot(object, "srr_params")[] <- value
                   return(object)
                 }
)

# movement
#' @rdname simpleBiol
#' @aliases simpleBiol simpleBiol-methods
setGeneric('movement', function(object, ...) standardGeneric('movement'))

#' @rdname simpleBiol
#' @aliases simpleBiol simpleBiol-methods
setGeneric('movement<-', function(object, value, ...) standardGeneric('movement<-'))

#' @rdname simpleBiol
#' @aliases movement,simpleBiol-method
setMethod("movement", signature(object="simpleBiol"),
          function(object) {
            return(slot(object, "movement"))
          }
)

setReplaceMethod("movement", signature(object="simpleBiol", value="array"),
                 function(object, value) {
                   slot(object, "movement") <- value
                   return(object)
                 }
)

#' @rdname simpleBiol
#' @aliases movement<-,simpleBiol,numeric-method
setReplaceMethod("movement", signature(object="simpleBiol", value="numeric"),
                 function(object, value) {
                   slot(object, "movement")[] <- value
                   return(object)
                 }
)

# name
#' @rdname simpleBiol
#' @aliases name,simpleBiol-method
setMethod("name", signature(object="simpleBiol"),
          function(object) {
            return(slot(object, "name"))
          }
)

setReplaceMethod("name", signature(object="simpleBiol", value="character"),
                 function(object, value) {
                   slot(object, "name") <- value
                   return(object)
                 }
)

# desc
#' @rdname simpleBiol
#' @aliases desc,simpleBiol-method
setMethod("desc", signature(object="simpleBiol"),
          function(object) {
            return(slot(object, "desc"))
          }
)

setReplaceMethod("desc", signature(object="simpleBiol", value="character"),
                 function(object, value) {
                   slot(object, "desc") <- value
                   return(object)
                 }
)


# catch_n
#' @rdname simpleFisheries
#' @aliases simpleFisheries simpleFisheries-methods
setGeneric('catch_n', function(object, ...) standardGeneric('catch_n'))

#' @rdname simpleFisheries
#' @aliases simpleFisheries simpleFisheries-methods
setGeneric('catch_n<-', function(object, value, ...) standardGeneric('catch_n<-'))

#' @rdname simpleFisheries
#' @aliases catch_n,simpleFisheries-method
setMethod("catch_n", signature(object="simpleFisheries"),
          function(object) {
            return(slot(object, "catch_n"))
          }
)

setReplaceMethod("catch_n", signature(object="simpleFisheries", value="FLQuant"),
                 function(object, value) {
                   slot(object, "catch_n") <- value
                   return(object)
                 }
)

#' @rdname simpleFisheries
#' @aliases catch_n<-,simpleFisheries,numeric-method
setReplaceMethod("catch_n", signature(object="simpleFisheries", value="numeric"),
                 function(object, value) {
                   slot(object, "catch_n")[] <- value
                   return(object)
                 }
)

# catch_wt
#' @rdname simpleFisheries
#' @aliases simpleFisheries simpleFisheries-methods
setGeneric('catch_wt', function(object, ...) standardGeneric('catch_wt'))

#' @rdname simpleFisheries
#' @aliases simpleFisheries simpleFisheries-methods
setGeneric('catch_wt<-', function(object, value, ...) standardGeneric('catch_wt<-'))

#' @rdname simpleFisheries
#' @aliases catch_wt,simpleFisheries-method
setMethod("catch_wt", signature(object="simpleFisheries"),
          function(object) {
            return(slot(object, "catch_wt"))
          }
)

setReplaceMethod("catch_wt", signature(object="simpleFisheries", value="FLQuant"),
                 function(object, value) {
                   slot(object, "catch_wt") <- value
                   return(object)
                 }
)

#' @rdname simpleFisheries
#' @aliases catch_wt<-,simpleFisheries,numeric-method
setReplaceMethod("catch_wt", signature(object="simpleFisheries", value="numeric"),
                 function(object, value) {
                   slot(object, "catch_wt")[] <- value
                   return(object)
                 }
)

# sel
#' @rdname simpleFisheries
#' @aliases simpleFisheries simpleFisheries-methods
setGeneric('sel', function(object, ...) standardGeneric('sel'))

#' @rdname simpleFisheries
#' @aliases simpleFisheries simpleFisheries-methods
setGeneric('sel<-', function(object, value, ...) standardGeneric('sel<-'))

#' @rdname simpleFisheries
#' @aliases sel,simpleFisheries-method
setMethod("sel", signature(object="simpleFisheries"),
          function(object) {
            return(slot(object, "sel"))
          }
)

setReplaceMethod("sel", signature(object="simpleFisheries", value="FLQuant"),
                 function(object, value) {
                   slot(object, "sel") <- value
                   return(object)
                 }
)

#' @rdname simpleFisheries
#' @aliases sel<-,simpleFisheries,numeric-method
setReplaceMethod("sel", signature(object="simpleFisheries", value="numeric"),
                 function(object, value) {
                   slot(object, "sel")[] <- value
                   return(object)
                 }
)

# catch_q
#' @rdname simpleFisheries
#' @aliases simpleFisheries simpleFisheries-methods
setGeneric('catch_q', function(object, ...) standardGeneric('catch_q'))

#' @rdname simpleFisheries
#' @aliases simpleFisheries simpleFisheries-methods
setGeneric('catch_q<-', function(object, value, ...) standardGeneric('catch_q<-'))

#' @rdname simpleFisheries
#' @aliases catch_q,simpleFisheries-method
setMethod("catch_q", signature(object="simpleFisheries"),
          function(object) {
            return(slot(object, "catch_q"))
          }
)

setReplaceMethod("catch_q", signature(object="simpleFisheries", value="FLQuant"),
                 function(object, value) {
                   slot(object, "catch_q") <- value
                   return(object)
                 }
)

#' @rdname simpleFisheries
#' @aliases catch_q<-,simpleFisheries,numeric-method
setReplaceMethod("catch_q", signature(object="simpleFisheries", value="numeric"),
                 function(object, value) {
                   slot(object, "catch_q")[] <- value
                   return(object)
                 }
)

# effort
#' @rdname simpleFisheries
#' @aliases effort,simpleFisheries-method
setMethod("effort", signature(object="simpleFisheries"),
          function(object) {
            return(slot(object, "effort"))
          }
)

setReplaceMethod("effort", signature(object="simpleFisheries", value="FLQuant"),
                 function(object, value) {
                   slot(object, "effort") <- value
                   return(object)
                 }
)

#' @rdname simpleFisheries
#' @aliases effort<-,simpleFisheries,numeric-method
setReplaceMethod("effort", signature(object="simpleFisheries", value="numeric"),
                 function(object, value) {
                   slot(object, "effort")[] <- value
                   return(object)
                 }
)

# fishery_map
#' @rdname simpleFisheries
#' @aliases simpleFisheries simpleFisheries-methods
setGeneric('fishery_map', function(object, ...) standardGeneric('fishery_map'))

#' @rdname simpleFisheries
#' @aliases simpleFisheries simpleFisheries-methods
setGeneric('fishery_map<-', function(object, value, ...) standardGeneric('fishery_map<-'))

#' @rdname simpleFisheries
#' @aliases fishery_map,simpleFisheries-method
setMethod("fishery_map", signature(object="simpleFisheries"),
          function(object) {
            return(slot(object, "fishery_map"))
          }
)

setReplaceMethod("fishery_map", signature(object="simpleFisheries", value="numeric"),
                 function(object, value) {
                   slot(object, "fishery_map") <- value
                   return(object)
                 }
)

#' @rdname simpleFisheries
#' @aliases catch_q<-,simpleFisheries,numeric-method
setReplaceMethod("catch_q", signature(object="simpleFisheries", value="numeric"),
                 function(object, value) {
                   slot(object, "catch_q")[] <- value
                   return(object)
                 }
)

# name
#' @rdname simpleFisheries
#' @aliases name,simpleFisheries-method
setMethod("name", signature(object="simpleFisheries"),
          function(object) {
            return(slot(object, "name"))
          }
)

setReplaceMethod("name", signature(object="simpleFisheries", value="character"),
                 function(object, value) {
                   slot(object, "name") <- value
                   return(object)
                 }
)

# desc
#' @rdname simpleFisheries
#' @aliases desc,simpleFisheries-method
setMethod("desc", signature(object="simpleFisheries"),
          function(object) {
            return(slot(object, "desc"))
          }
)

setReplaceMethod("desc", signature(object="simpleFisheries", value="character"),
                 function(object, value) {
                   slot(object, "desc") <- value
                   return(object)
                 }
)







