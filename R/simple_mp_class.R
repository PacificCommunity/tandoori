# simpleMP class
# Copyright 2025 Finlay Scott. Distributed under the GPL 2 or later
# Maintainer: Finlay Scott

setClass("simpleMP",
  representation(
    catch_sd = 'numeric',
    effort_sd = 'numeric',
    hcr             = 'character',
    hcr_params      = 'numeric'
  ),
  prototype=prototype(
    catch_sd             = numeric(),
    effort_sd             = numeric(),
    hcr             = "hcr_threshold",
    hcr_params      = c(sbsbf0_min = 0.2, sbsbf0_max = 0.5, out_min = 0.2, out_max = 1.0)
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
simpleMP <- function(hcr="hcr_threshold", hcr_params=c(hcr_ip_min = 0.2, hcr_ip_max = 0.5, out_min = 0.2, out_max = 1.0), ...) {
  mp <- new("simpleMP")
  slot(mp, 'hcr') <- hcr
  slot(mp, 'hcr_params') <- hcr_params
  return(mp)
}


# ----  Accessor methods --------------
#' @rdname accessor-methods
setGeneric('hcr', function(object, ...) standardGeneric('hcr'))
#' @rdname accessor-methods
setMethod('hcr', signature(object='simpleMP'), function(object) return(slot(object, 'hcr')))
#' @rdname accessor-methods
setGeneric('hcr<-', function(object, ..., value) standardGeneric('hcr<-'))
#' @rdname accessor-methods
setReplaceMethod('hcr', signature(object='simpleMP', value=unname(getSlots('simpleMP')['hcr'])), 
  function(object, value){slot(object, 'hcr') <- value; return(object)})

#' @rdname accessor-methods
setGeneric('hcr_params', function(object, ...) standardGeneric('hcr_params'))
#' @rdname accessor-methods
setMethod('hcr_params', signature(object='simpleMP'), function(object) return(slot(object, 'hcr_params')))
#' @rdname accessor-methods
setGeneric('hcr_params<-', function(object, ..., value) standardGeneric('hcr_params<-'))
#' @rdname accessor-methods
setReplaceMethod('hcr_params', signature(object='simpleMP', value=unname(getSlots('simpleMP')['hcr_params'])), 
  function(object, value){slot(object, 'hcr_params') <- value; return(object)})

# Evaluate the HCR
# May need to pass in other parameters - such as when limiting maximum change in output
setGeneric('eval_hcr', function(mp, hcr_ip, ...) standardGeneric('eval_hcr'))
setMethod('eval_hcr', signature(mp='simpleMP', hcr_ip='numeric'),
  function(mp, hcr_ip, ...){
    extra_args <- list(...)
    args = c(list(hcr_ip=hcr_ip, params=hcr_params(mp)), extra_args)
    return(do.call(hcr(mp), args=args))
})

# ---------- HCRs ------------------------

# A bunch of HCR functions

#' hcr_threshold
#'
#' Typical HCR function with the parameters hcr_ip_min, hcr_ip_max, out_min, out_max passed as a vector.
#' The default HCR for the MFCLMSEControl class.
#'
#' @param hcr_ip Numeric vector of HCR inputs
#' @param params Numeric vector of HCR parameters.
#'
#' @rdname hcr_funcs
#' @return Numberic vector of the HCR output
hcr_threshold <- function(hcr_ip, params){
  # The standard threshold type of HCR
  if (!all(c('hcr_ip_min', 'hcr_ip_max', 'out_min', 'out_max') %in% names(params))){
    stop("HCR parameter names do not match those in the HCR function\n")
  }
  hcr_ip_min <- params['hcr_ip_min']
  hcr_ip_max <- params['hcr_ip_max']
  out_min <- params['out_min']
  out_max <- params['out_max']
  # Do in steps to prevent further cockups
  # Get the slope
  grad <- (out_max - out_min) / (hcr_ip_max - hcr_ip_min)
  intercept <- out_min - (grad * hcr_ip_min)
  out <- hcr_ip * grad + intercept
  # Apply limits
  out <- unname(pmin(pmax(out_min, out), out_max))
  return(out)
}


#' hcr_asymptotic
#'
#' Instead of a threshold shape this HCR has a lovely curvy shape based on an asymptotic function.
#'
#' @param hcr_ip Numeric vector of HCR inputs
#' @param params Numeric vector of HCR parameters.
#'
#' @rdname hcr_funcs
#' @return Numberic vector of the HCR output
hcr_asymptotic <- function(hcr_ip, params){
  # The standard threshold type of HCR
  if (!all(c('hcr_ip_min', 'hcr_ip_max', 'out_min', 'out_max', 'curve') %in% names(params))){
    stop("HCR parameter names do not match those in the HCR function\n")
  }
  hcr_ip_min <- params['hcr_ip_min']
  hcr_ip_max <- params['hcr_ip_max']
  out_min <- params['out_min']
  out_max <- params['out_max']
  curve <- params['curve']

  # y = a - (a-b) * exp(-c * x)
  # a is maximum attainable y (hcr_ip_max)
  # b is y at x = 0 (hcr_ip_min)
  # c is the curve
  # The following finds a and b for a given curve parameter that passes through
  # (out_min, hcr_ip_min) and (out_max, sbsbsf0_max)
  # Could probably be neater but...

  z1 <- exp(-curve * hcr_ip_min)
  z2 <- exp(-curve * hcr_ip_max)
  p <- (out_max / z2) - (out_min / z1)
  q <- (1 - z2) / z2
  r <- (1 - z1) / z1
  a <- p / (q-r)
  b <- (out_min - a * (1 - z1)) / z1
  out <- a - (a-b)*exp(-curve * hcr_ip)
  out <- pmax(pmin(out_max, out), out_min)
  # Apply limits
  out <- unname(pmin(pmax(out_min, out), out_max))
  return(out)
}

#' hcr_constant
#'
#' An HCR function that just returns the same output irrespective of the SBSBF0 input
#'
#' @rdname hcr_funcs
hcr_constant <- function(hcr_ip, params){
  # A constant output HCR
  if (length(params) > 1){
    stop("Too many parameters in the HCR parameters. There should be only one\n")
  }
  return(rep(unname(params[1]), length(hcr_ip)))
}


#' hcr_hillary_step
#'
#' Hillary step is basically two thresholds on top of each other, side by side
#' Parameters are 'hcr_ip_min', 'hcr_ip_max', 'hcr_ip_step_min', 'hcr_ip_step_max','out_min', 'out_max', 'step_height'.
#' 
#'
#' @param reference_out The reference level that the constraint is applied to.
#'
#' @rdname hcr_funcs

hcr_hillary_step <- function(hcr_ip, params){
  if (!all(c('hcr_ip_min', 'hcr_ip_max', 'hcr_ip_step_min', 'hcr_ip_step_max','out_min', 'out_max', 'step_height') %in% names(params))){
    stop("HCR parameter names do not match those in the HCR function\n")
  }
  # Need to unname them - kind of annoying
  hcr_ip_min <- unname(params['hcr_ip_min'])
  hcr_ip_max <- unname(params['hcr_ip_max'])
  hcr_ip_step_min <- unname(params['hcr_ip_step_min'])
  hcr_ip_step_max <- unname(params['hcr_ip_step_max'])
  out_min <- unname(params['out_min'])
  out_max <- unname(params['out_max'])
  step_height <- unname(params['step_height'])
  
  # Hillary step is basically two thresholds on top of each other, side by side
  out_left_params <- c(hcr_ip_min = hcr_ip_min, hcr_ip_max = hcr_ip_step_min, out_min = out_min, out_max = step_height)
  out_left <- hcr_threshold(hcr_ip, params=out_left_params)
  out_right_params <- c(hcr_ip_min = hcr_ip_step_max, hcr_ip_max = hcr_ip_max, out_min = step_height, out_max = out_max)
  out_right <- hcr_threshold(hcr_ip, params=out_right_params)
  # out is out_left, 
  out <- out_left
  # If we're on the right hand side threshold, use that
  out[hcr_ip > hcr_ip_step_min] <- out_right[hcr_ip > hcr_ip_step_min]
  return(out)
}



#' Generic constrained HCR function
#' 
#' Takes any HCR function as a character string and applies constraint if necessary
#' @param hcr_func Character string of the function name
#' @param hcr_ip Numeric vector of HCR inputs
#' @param params Numeric vector of HCR parameters.
#' @param reference_out The previous output value to compare against
#' @rdname hcr_funcs
#' @return Numberic vector of the HCR output
hcr_constrained <- function(hcr_ip, params, reference_out, hcr_func){
  # Check we have the change parameters
  if (!all(c('max_change_up', 'max_change_down') %in% names(params))){
    stop("Calling constrained HCR but missing 'max_change_up' and 'max_change_down' parameters\n")
  }
  out <- eval(call(hcr_func, hcr_ip=hcr_ip, params=params))
  # Apply constraint
  out <- pmax(pmin(out, reference_out * params['max_change_up']), reference_out * params['max_change_down'])
  return(out)
}

#' hcr_asymptotic_constrained
#'
#' Instead of a threshold shape this HCR has a lovely curvy shape based on an asymptotic function.
#' Also has a maximum constraint change up or down from previous value
#' @param hcr_ip Numeric vector of HCR inputs
#' @param params Numeric vector of HCR parameters.
#' @param reference_out The previous output value to compare against
#'
#' @rdname hcr_funcs
#' @return Numberic vector of the HCR output
hcr_asymptotic_constrained <- function(hcr_ip, params, reference_out){
  out <- hcr_constrained(hcr_ip=hcr_ip, params=params, reference_out=reference_out, hcr_func="hcr_asymptotic")
  return(out)
}

#' hcr_threshold_constrained
#'
#' Like the typical HCR function with the parameters but with a constraint on how much the output is allowed to change from a reference output.
#' Parameters are 'hcr_ip_min', 'hcr_ip_max', 'out_min', 'out_max', 'max_change_up', 'max_change_down'.
#' There is an additional argument 'reference_out' that is the reference output, i.e. the value to which to the new output is compared.
#'
#' @param reference_out The reference level that the constraint is applied to.
#'
#' @rdname hcr_funcs

hcr_threshold_constrained <- function(hcr_ip, params, reference_out){
  out <- hcr_constrained(hcr_ip=hcr_ip, params=params, reference_out=reference_out, hcr_func="hcr_threshold")
  return(out)
}


#' hcr_hillary_step_constrained
#'
#' Like the typical HCR function with the parameters but with a constraint on how much the output is allowed to change from a reference output.
#' Parameters are 'hcr_ip_min', 'hcr_ip_max', 'out_min', 'out_max', 'max_change_up', 'max_change_down'.
#' There is an additional argument 'reference_out' that is the reference output, i.e. the value to which to the new output is compared.
#'
#' @param reference_out The reference level that the constraint is applied to.
#'
#' @rdname hcr_funcs

hcr_hillary_step_constrained <- function(hcr_ip, params, reference_out){
  #browser()
  out <- hcr_constrained(hcr_ip = hcr_ip, params=params, reference_out=reference_out, hcr_func="hcr_hillary_step")
  return(out)
}




#' hcr_asymptotic_hillary_step
#'
#' Hillary step is basically two thresholds on top of each other, side by side
#' Parameters are 'hcr_ip_min', 'hcr_ip_max', 'hcr_ip_step_min', 'hcr_ip_step_max','out_min', 'out_max', 'step_height'.
#' Adding 'curve' to allow for asymptotic out_left
#'
#' @param reference_out The reference level that the constraint is applied to.
#'
#' @rdname hcr_funcs

hcr_asymptotic_hillary_step <- function(hcr_ip, params){
  if (!all(c('hcr_ip_min', 'hcr_ip_max', 'hcr_ip_step_min', 'hcr_ip_step_max','out_min', 'out_max', 'step_height', 'curve') %in% names(params))){
    stop("HCR parameter names do not match those in the HCR function\n")
  }
  # Need to unname them - kind of annoying
  hcr_ip_min <- unname(params['hcr_ip_min'])
  hcr_ip_max <- unname(params['hcr_ip_max'])
  hcr_ip_step_min <- unname(params['hcr_ip_step_min'])
  hcr_ip_step_max <- unname(params['hcr_ip_step_max'])
  out_min <- unname(params['out_min'])
  out_max <- unname(params['out_max'])
  step_height <- unname(params['step_height'])
  curve       <- unname(params['curve'])
  
  # Hillary step is basically two thresholds on top of each other, side by side
  out_left_params <- c(hcr_ip_min = hcr_ip_min, hcr_ip_max = hcr_ip_step_min, out_min = out_min, out_max = step_height, curve=curve)
  out_left <- hcr_asymptotic(hcr_ip, params=out_left_params)
  out_right_params <- c(hcr_ip_min = hcr_ip_step_max, hcr_ip_max = hcr_ip_max, out_min = step_height, out_max = out_max)
  out_right <- hcr_threshold(hcr_ip, params=out_right_params)
  # out is out_left, 
  out <- out_left
  # If we're on the right hand side threshold, use that
  out[hcr_ip > hcr_ip_step_min] <- out_right[hcr_ip > hcr_ip_step_min]
  return(out)
}

#' hcr_hillary_step_constrained
#'
#' Like the typical HCR function with the parameters but with a constraint on how much the output is allowed to change from a reference output.
#' Parameters are 'hcr_ip_min', 'hcr_ip_max', 'out_min', 'out_max', 'max_change_up', 'max_change_down'.
#' There is an additional argument 'reference_out' that is the reference output, i.e. the value to which to the new output is compared.
#'
#' @param reference_out The reference level that the constraint is applied to.
#'
#' @rdname hcr_funcs

hcr_asymptotic_hillary_step_constrained <- function(hcr_ip, params, reference_out){
  out <- hcr_constrained(hcr_ip = hcr_ip, params=params, reference_out=reference_out, hcr_func="hcr_asymptotic_hillary_step")
  return(out)
}

# Method to get shape coordinates for plotting
#' @rdname accessor-methods
setGeneric('get_hcr_shape', function(object, ...) standardGeneric('get_hcr_shape'))
#' @rdname accessor-methods
setMethod('get_hcr_shape', signature(object='simpleMP'),
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
})


