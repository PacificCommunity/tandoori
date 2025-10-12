# A bunch of HCR functions

#' Harvest control rule threshold
#'
#' \strong{Threshold}: typical HCR function with the parameters hcr_ip_min,
#' hcr_ip_max, out_min, out_max passed as a vector.
#' The default HCR for the MFCLMSEControl class.
#'
#' @param hcr_ip Numeric vector of HCR inputs
#' @param params Numeric vector of HCR parameters.
#'
#' @rdname hcr_funcs
#' @return Numberic vector of the HCR output
hcr_threshold <- function(hcr_ip, params) {
  # The standard threshold type of HCR
  if (!all(c("hcr_ip_min", "hcr_ip_max", "out_min", "out_max") %in% names(params))) {
    stop("HCR parameter names do not match those in the HCR function\n")
  }
  hcr_ip_min <- params["hcr_ip_min"]
  hcr_ip_max <- params["hcr_ip_max"]
  out_min <- params["out_min"]
  out_max <- params["out_max"]
  # Do in steps to prevent further cockups
  # Get the slope
  grad <- (out_max - out_min) / (hcr_ip_max - hcr_ip_min)
  intercept <- out_min - (grad * hcr_ip_min)
  out <- hcr_ip * grad + intercept
  # Apply limits
  out <- unname(pmin(pmax(out_min, out), out_max))
  return(out)
}


#' Harvest control rule asymptotic
#'
#' \strong{Asympotic}: instead of a threshold shape this HCR has a lovely curvy shape based on an asymptotic function.
#'
#' @param hcr_ip Numeric vector of HCR inputs
#' @param params Numeric vector of HCR parameters.
#'
#' @rdname hcr_funcs
#' @return Numberic vector of the HCR output
hcr_asymptotic <- function(hcr_ip, params) {
  # The standard threshold type of HCR
  if (!all(c("hcr_ip_min", "hcr_ip_max", "out_min", "out_max", "curve") %in% names(params))) {
    stop("HCR parameter names do not match those in the HCR function\n")
  }
  hcr_ip_min <- params["hcr_ip_min"]
  hcr_ip_max <- params["hcr_ip_max"]
  out_min <- params["out_min"]
  out_max <- params["out_max"]
  curve <- params["curve"]

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
  a <- p / (q - r)
  b <- (out_min - a * (1 - z1)) / z1
  out <- a - (a - b) * exp(-curve * hcr_ip)
  out <- pmax(pmin(out_max, out), out_min)
  # Apply limits
  out <- unname(pmin(pmax(out_min, out), out_max))
  return(out)
}

#' Harvest control rule constant
#'
#' \strong{Constant}: an HCR function that just returns the same output irrespective of the SBSBF0 input
#'
#' @rdname hcr_funcs
hcr_constant <- function(hcr_ip, params) {
  # A constant output HCR
  if (length(params) > 1) {
    stop("Too many parameters in the HCR parameters. There should be only one\n")
  }
  return(rep(unname(params[1]), length(hcr_ip)))
}


#' Harvest control rule Hillary step
#'
#' \strong{Hillary step}: basically two thresholds on top of each other, side by side
#' Parameters are 'hcr_ip_min', 'hcr_ip_max', 'hcr_ip_step_min', 'hcr_ip_step_max','out_min', 'out_max', 'step_height'.
#'
#'
#' @param reference_out The reference level that the constraint is applied to.
#'
#' @rdname hcr_funcs

hcr_hillary_step <- function(hcr_ip, params) {
  if (!all(c("hcr_ip_min", "hcr_ip_max", "hcr_ip_step_min", "hcr_ip_step_max", "out_min", "out_max", "step_height") %in% names(params))) {
    stop("HCR parameter names do not match those in the HCR function\n")
  }
  # Need to unname them - kind of annoying
  hcr_ip_min <- unname(params["hcr_ip_min"])
  hcr_ip_max <- unname(params["hcr_ip_max"])
  hcr_ip_step_min <- unname(params["hcr_ip_step_min"])
  hcr_ip_step_max <- unname(params["hcr_ip_step_max"])
  out_min <- unname(params["out_min"])
  out_max <- unname(params["out_max"])
  step_height <- unname(params["step_height"])

  # Hillary step is basically two thresholds on top of each other, side by side
  out_left_params <- c(hcr_ip_min = hcr_ip_min, hcr_ip_max = hcr_ip_step_min, out_min = out_min, out_max = step_height)
  out_left <- hcr_threshold(hcr_ip, params = out_left_params)
  out_right_params <- c(hcr_ip_min = hcr_ip_step_max, hcr_ip_max = hcr_ip_max, out_min = step_height, out_max = out_max)
  out_right <- hcr_threshold(hcr_ip, params = out_right_params)
  # out is out_left,
  out <- out_left
  # If we're on the right hand side threshold, use that
  out[hcr_ip > hcr_ip_step_min] <- out_right[hcr_ip > hcr_ip_step_min]
  return(out)
}



#' Harvest control rule constraint
#'
#' \strong{Constraint}: takes any HCR function as a character string and applies constraint if necessary
#' @param hcr_func Character string of the function name
#' @param hcr_ip Numeric vector of HCR inputs
#' @param params Numeric vector of HCR parameters.
#' @param reference_out The previous output value to compare against
#' @rdname hcr_funcs
#' @return Numberic vector of the HCR output
hcr_constrained <- function(hcr_ip, params, reference_out, hcr_func) {
  # Check we have the change parameters
  if (!all(c("max_change_up", "max_change_down") %in% names(params))) {
    stop("Calling constrained HCR but missing 'max_change_up' and 'max_change_down' parameters\n")
  }
  out <- eval(call(hcr_func, hcr_ip = hcr_ip, params = params))
  # Apply constraint
  out <- pmax(pmin(out, reference_out * params["max_change_up"]), reference_out * params["max_change_down"])
  return(out)
}

#' Harvest control rule asymptotic with constraint
#'
#' \strong{Constrained asymptotic}: instead of a threshold shape this HCR has a lovely
#' curvy shape based on an asymptotic function.
#' Also has a maximum constraint change up or down from previous value
#' @param hcr_ip Numeric vector of HCR inputs
#' @param params Numeric vector of HCR parameters.
#' @param reference_out The previous output value to compare against
#'
#' @rdname hcr_funcs
#' @return Numberic vector of the HCR output
hcr_asymptotic_constrained <- function(hcr_ip, params, reference_out) {
  out <- hcr_constrained(hcr_ip = hcr_ip, params = params, reference_out = reference_out, hcr_func = "hcr_asymptotic")
  return(out)
}

#' Harvest control rule threshold with constraint
#'
#' \strong{Constrained threshold}: like the typical HCR function with the parameters but with a constraint on
#' how much the output is allowed to change from a reference output. Parameters
#' are 'hcr_ip_min', 'hcr_ip_max', 'out_min', 'out_max', 'max_change_up',
#' 'max_change_down'. There is an additional argument 'reference_out' that is
#' the reference output, i.e. the value to which to the new output is compared.
#'
#' @param reference_out The reference level that the constraint is applied to.
#'
#' @rdname hcr_funcs

hcr_threshold_constrained <- function(hcr_ip, params, reference_out) {
  out <- hcr_constrained(hcr_ip = hcr_ip, params = params, reference_out = reference_out, hcr_func = "hcr_threshold")
  return(out)
}


#' Harvest control rule Hillary step with constraint
#'
#' \strong{Constrained Hillary step}: like the typical HCR function with the
#' parameters but with a constraint on how much the output is allowed to change
#' from a reference output.
#' Parameters are 'hcr_ip_min', 'hcr_ip_max', 'out_min', 'out_max',
#' 'max_change_up', 'max_change_down'.
#' There is an additional argument 'reference_out' that is the reference output,
#' i.e. the value to which to the new output is compared.
#'
#' @param reference_out The reference level that the constraint is applied to.
#'
#' @rdname hcr_funcs

hcr_hillary_step_constrained <- function(hcr_ip, params, reference_out) {
  # browser()
  out <- hcr_constrained(hcr_ip = hcr_ip, params = params, reference_out = reference_out, hcr_func = "hcr_hillary_step")
  return(out)
}




#' Harvest control rule Hillary step with asymptotic curve
#'
#' \strong{Asymptotic Hillary step}: Hillary step is basically two thresholds on
#' top of each other, side by side
#' Parameters are 'hcr_ip_min', 'hcr_ip_max', 'hcr_ip_step_min',
#' 'hcr_ip_step_max','out_min', 'out_max', 'step_height'.
#' Adding 'curve' to allow for asymptotic out_left
#'
#' @param reference_out The reference level that the constraint is applied to.
#'
#' @rdname hcr_funcs

hcr_asymptotic_hillary_step <- function(hcr_ip, params) {
  if (!all(c(
    "hcr_ip_min",
    "hcr_ip_max",
    "hcr_ip_step_min",
    "hcr_ip_step_max",
    "out_min",
    "out_max",
    "step_height",
    "curve"
  ) %in% names(params))) {
    stop("HCR parameter names do not match those in the HCR function\n")
  }
  # Need to unname them - kind of annoying
  hcr_ip_min <- unname(params["hcr_ip_min"])
  hcr_ip_max <- unname(params["hcr_ip_max"])
  hcr_ip_step_min <- unname(params["hcr_ip_step_min"])
  hcr_ip_step_max <- unname(params["hcr_ip_step_max"])
  out_min <- unname(params["out_min"])
  out_max <- unname(params["out_max"])
  step_height <- unname(params["step_height"])
  curve <- unname(params["curve"])

  # Hillary step is basically two thresholds on top of each other, side by side
  out_left_params <- c(
    hcr_ip_min = hcr_ip_min,
    hcr_ip_max = hcr_ip_step_min,
    out_min = out_min,
    out_max = step_height,
    curve = curve
  )
  out_left <- hcr_asymptotic(hcr_ip, params = out_left_params)
  out_right_params <- c(
    hcr_ip_min = hcr_ip_step_max,
    hcr_ip_max = hcr_ip_max,
    out_min = step_height,
    out_max = out_max
  )
  out_right <- hcr_threshold(hcr_ip, params = out_right_params)
  # out is out_left,
  out <- out_left
  # If we're on the right hand side threshold, use that
  out[hcr_ip > hcr_ip_step_min] <- out_right[hcr_ip > hcr_ip_step_min]
  return(out)
}

#' Harvest control rule Hillary step with asymptotic curve and constraint
#'
#' \strong{Constrained asymptotic Hillary step}: like the typical HCR function
#' with the parameters but with a constraint on how much the output is allowed
#' to change from a reference output. Parameters are 'hcr_ip_min', 'hcr_ip_max',
#' 'out_min', 'out_max', 'max_change_up', 'max_change_down'. There is an
#' additional argument 'reference_out' that is the reference output, i.e. the
#' value to which to the new output is compared.
#'
#' @param reference_out The reference level that the constraint is applied to.
#'
#' @rdname hcr_funcs

hcr_asymptotic_hillary_step_constrained <- function(hcr_ip, params, reference_out) {
  out <- hcr_constrained(hcr_ip = hcr_ip, params = params, reference_out = reference_out, hcr_func = "hcr_asymptotic_hillary_step")
  return(out)
}
