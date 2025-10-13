# simpleMP class
# Copyright 2025 Finlay Scott. Distributed under the GPL 2 or later
# Maintainer: Finlay Scott

setClass("simpleMP",
  representation(
    catch_sd = "numeric",
    effort_sd = "numeric",
    hcr = "character",
    hcr_params = "numeric"
  ),
  prototype = prototype(
    catch_sd = numeric(),
    effort_sd = numeric(),
    hcr = "hcr_threshold",
    hcr_params = c(sbsbf0_min = 0.2, sbsbf0_max = 0.5, out_min = 0.2, out_max = 1.0)
  ),
  validity = function(object) {
    # To do
    return(TRUE)
  }
)

#' simpleMP
#'
#' Basic constructor for MFCLMSEControl class
#' @export
simpleMP <- function(hcr = "hcr_threshold", hcr_params = c(hcr_ip_min = 0.2, hcr_ip_max = 0.5, out_min = 0.2, out_max = 1.0), ...) {
  mp <- new("simpleMP")
  slot(mp, "hcr") <- hcr
  slot(mp, "hcr_params") <- hcr_params
  return(mp)
}


#' Accessor methods
#'
#' @rdname accessor-methods
setGeneric("hcr", function(object, ...) standardGeneric("hcr"))
#' @rdname accessor-methods
setMethod("hcr", signature(object = "simpleMP"), function(object) {
  return(slot(object, "hcr"))
})
#' @rdname accessor-methods
setGeneric("hcr<-", function(object, ..., value) standardGeneric("hcr<-"))
#' @rdname accessor-methods
setReplaceMethod(
  "hcr", signature(object = "simpleMP", value = unname(getSlots("simpleMP")["hcr"])),
  function(object, value) {
    slot(object, "hcr") <- value
    return(object)
  }
)

#' @rdname accessor-methods
setGeneric("hcr_params", function(object, ...) standardGeneric("hcr_params"))
#' @rdname accessor-methods
setMethod("hcr_params", signature(object = "simpleMP"), function(object) {
  return(slot(object, "hcr_params"))
})
#' @rdname accessor-methods
setGeneric("hcr_params<-", function(object, ..., value) standardGeneric("hcr_params<-"))
#' @rdname accessor-methods
setReplaceMethod(
  "hcr_params", signature(object = "simpleMP", value = unname(getSlots("simpleMP")["hcr_params"])),
  function(object, value) {
    slot(object, "hcr_params") <- value
    return(object)
  }
)

# Evaluate the HCR
# May need to pass in other parameters - such as when limiting maximum change in output
setGeneric("eval_hcr", function(mp, hcr_ip, ...) standardGeneric("eval_hcr"))
setMethod(
  "eval_hcr", signature(mp = "simpleMP", hcr_ip = "numeric"),
  function(mp, hcr_ip, ...) {
    extra_args <- list(...)
    args <- c(list(hcr_ip = hcr_ip, params = hcr_params(mp)), extra_args)
    return(do.call(hcr(mp), args = args))
  }
)

# Method to get shape coordinates for plotting
#' @rdname accessor-methods
setGeneric("get_hcr_shape", function(object, ...) standardGeneric("get_hcr_shape"))
#' @rdname accessor-methods
setMethod(
  "get_hcr_shape", signature(object = "simpleMP"),
  function(object, hcr_ip) {
    hcr_type <- hcr(object)
    # If constrained - turn type into not constrained and get new params
    hcr_type <- strsplit(hcr_type, "_constrained")[[1]][1]
    names(hcr_params(object))
    hcr_params_cols <- names(hcr_params(object))[
      !(names(hcr_params(object)) %in%
        c("z", "type", "max_change_up", "max_change_down"))
    ]
    hcr_params <- hcr_params(object)[hcr_params_cols]
    # Make a new MP with those params and evaluate it
    newmp <- simpleMP()
    hcr(newmp) <- hcr_type
    hcr_params(newmp) <- hcr_params
    out <- data.frame(hcr_ip = hcr_ip, hcr_op = eval_hcr(newmp, hcr_ip))
    return(out)
  }
)
