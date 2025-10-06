#
#  Copyright 2025 Finlay Scott. Distributed under the GPL 2 or later
#  Maintainer: Finlay Scott
#

# Internal function used by SB and SBF0 methods
# Maybe should not be exported?
# This function fuels all the other methods here.
rolling_mean_and_lagging <- function(flq, mean_nyears, lag_nyears) {
  if ((mean_nyears < 1) | (lag_nyears < 0)) {
    stop("mean_nyears >= 1 and lag_nyears must >= 0")
  }
  # Take a rolling mean over mean_nyears
  flq <- apply(flq, c(1, 3, 4, 5, 6), function(x, n, sides) {
    stats::filter(c(x), rep(1 / n, n), sides = sides)
  }, n = mean_nyears, sides = 1)
  # Shunt everything up by lag_nyears and backfill with NAs
  if (lag_nyears > 0) {
    lagged_flq <- flq[, 1:(dim(flq)[2] - lag_nyears)]
    flq[, (lag_nyears + 1):dim(flq)[2]] <- lagged_flq
    flq[, 1:lag_nyears] <- NA
  }
  return(flq)
}

#---------------- FLQuant methods ---------------------------
setGeneric("SB", function(object, mean_nyears, lag_nyears, ...) standardGeneric("SB"))

# This is the main method that the other SB() methods call
#' @rdname sb_methods
#' @export
setMethod(
  "SB", signature(object = "FLQuant", mean_nyears = "numeric", lag_nyears = "numeric"),
  function(object, mean_nyears, lag_nyears, combine_areas = TRUE, season_means = TRUE) {
    out <- object
    if (season_means == TRUE) {
      out <- seasonMeans(out)
    }
    if (combine_areas == TRUE) {
      out <- areaSums(out)
    }
    out <- rolling_mean_and_lagging(flq = out, mean_nyears = mean_nyears, lag_nyears = lag_nyears)
    return(out)
  }
)

#' @rdname sb_methods
#' @export
setMethod(
  "SB", signature(object = "FLQuant", mean_nyears = "missing", lag_nyears = "missing"),
  function(object, ...) {
    return(SB(object = object, mean_nyears = 1, lag_nyears = 0, ...))
  }
)

#' @rdname sb_methods
#' @export
setMethod(
  "SB", signature(object = "FLQuant", mean_nyears = "numeric", lag_nyears = "missing"),
  function(object, mean_nyears, ...) {
    return(SB(object = object, mean_nyears = mean_nyears, lag_nyears = 0, ...))
  }
)

#' @rdname sb_methods
#' @export
setMethod(
  "SB", signature(object = "FLQuant", mean_nyears = "missing", lag_nyears = "numeric"),
  function(object, lag_nyears, ...) {
    return(SB(object = object, mean_nyears = 1, lag_nyears = lag_nyears, ...))
  }
)

#------------------------------------------------------------------------
# Single rep methods

# SB methods

#' Methods for returns the spawning biomass (fished or unfished) and the depletion
#'
#' Methods for returning the spawning biomass (\code{SB()}), unfished spawning biomass (\code{SBF0()}) and depletion (\code{SBSBF0()}) from a single simpleBiol object.
#' The biomass is averaged over seasons.
#' It is possible to calculate a rolling mean over a specified number of years
#' as well as lag the output.
#' These can specified separately for the SB and SBF0 parts of SBSBF0.
#' The number of years in the outputs will match that of the inputs.
#' The start of the returned FLQuant will be padded with NAs if necessary (for example, where the output has been lagged).
#'
#' Shortcut methods are available (\code{SBlatest()}, \code{SBrecent()}, \code{SBF0recent()} and \code{SBSBF0recent()}). These set the \code{mean_nyears()} and \code{lag_nyears()} parameters to match those used in the assessment reports.
#' \code{SBlatest()} does not calculate a mean or lag the output.
#' \code{SBrecent()} uses a rolling mean of 4 years without a lag.
#' \code{SBF0recent()} uses a rolling mean of 10 years with a lag of 1 year.
#' \code{SBSBF0recent()} is a combination of \code{SBrecent()} and \code{SBF0recent()}.
#' \code{SBSBF0latest()} is a combination of \code{SBlatest()} and \code{SBF0recent()}.
#'
#' @param object A single simpleBiol object
#' @param mean_nyears The number of years to calculate the rolling mean over. A value of 1 is the same as not calculating a mean. For the SB() method the default value is 1.
#' @param lag_nyears The number of years to lag the output. A value of 0 is the same as not lagging the output. For the SB() method the default value is 0.
#' @param combine_areas If TRUE the biomassses in different areas are summed. Default value is TRUE.
#' @param season_means If TRUE the seasonal biomassses are averaged in each year so only annual biomass is returned. Default value is TRUE.
#' @param sb_mean_nyears Equivalent to the argument \code{mean_nyears} for the SB part of the \code{SBSBF0()} method.
#' @param sb_lag_nyears Equivalent to the argument \code{lag_nyears} for the SB part of the \code{SBSBF0()} method.
#' @param sbf0_mean_nyears Equivalent to the argument \code{mean_nyears} for the SBF0 part of the \code{SBSBF0()} method.
#' @param sbf0_lag_nyears Equivalent to the argument \code{lag_nyears} for the SBF0 part of the \code{SBSBF0()} method.
#' @param ... Other arguments.
#'
#' @return An FLQuant
#' @name sb_methods
#' @aliases SB sb_methods
#' @export

#---------------- SBF0 methods ---------------------------


# This is the main method that the other SB() methods call
#' @rdname sb_methods
#' @export
setMethod(
  "SB", signature(object = "simpleBiol", mean_nyears = "numeric", lag_nyears = "numeric"),
  function(object, mean_nyears, lag_nyears, combine_areas = TRUE, season_means = TRUE) {
    out <- quantSums(n(object) * wt(object) * mat(object))
    if (season_means == TRUE) {
      out <- seasonMeans(out)
    }
    if (combine_areas == TRUE) {
      out <- areaSums(out)
    }
    out <- rolling_mean_and_lagging(flq = out, mean_nyears = mean_nyears, lag_nyears = lag_nyears)
    return(out)
  }
)

#' @rdname sb_methods
#' @export
setMethod(
  "SB", signature(object = "simpleBiol", mean_nyears = "missing", lag_nyears = "missing"),
  function(object, ...) {
    return(SB(object = object, mean_nyears = 1, lag_nyears = 0, ...))
  }
)

#' @rdname sb_methods
#' @export
setMethod(
  "SB", signature(object = "simpleBiol", mean_nyears = "numeric", lag_nyears = "missing"),
  function(object, mean_nyears, ...) {
    return(SB(object = object, mean_nyears = mean_nyears, lag_nyears = 0, ...))
  }
)

#' @rdname sb_methods
#' @export
setMethod(
  "SB", signature(object = "simpleBiol", mean_nyears = "missing", lag_nyears = "numeric"),
  function(object, lag_nyears, ...) {
    return(SB(object = object, mean_nyears = 1, lag_nyears = lag_nyears, ...))
  }
)


#---------------- SBF0 methods ---------------------------

#' @rdname sb_methods
#' @export
setGeneric("SBF0", function(object, mean_nyears, lag_nyears, ...) standardGeneric("SBF0"))

#' @rdname sb_methods
#' @export
setMethod(
  "SBF0", signature(object = "simpleBiol", mean_nyears = "numeric", lag_nyears = "numeric"),
  function(object, mean_nyears, lag_nyears, combine_areas = TRUE, season_means = TRUE) {
    out <- quantSums(n0(object) * wt(object) * mat(object))
    if (season_means == TRUE) {
      out <- seasonMeans(out)
    }
    if (combine_areas == TRUE) {
      out <- areaSums(out)
    }
    out <- rolling_mean_and_lagging(flq = out, mean_nyears = mean_nyears, lag_nyears = lag_nyears)
    return(out)
  }
)

#' @rdname sb_methods
#' @export
setMethod(
  "SBF0", signature(object = "simpleBiol", mean_nyears = "missing", lag_nyears = "missing"),
  function(object, ...) {
    return(SBF0(object = object, mean_nyears = 1, lag_nyears = 0, ...))
  }
)

#' @rdname sb_methods
#' @export
setMethod(
  "SBF0", signature(object = "simpleBiol", mean_nyears = "numeric", lag_nyears = "missing"),
  function(object, mean_nyears, ...) {
    return(SBF0(object = object, mean_nyears = mean_nyears, lag_nyears = 0, ...))
  }
)

#' @rdname sb_methods
#' @export
setMethod(
  "SBF0", signature(object = "simpleBiol", mean_nyears = "missing", lag_nyears = "numeric"),
  function(object, lag_nyears, ...) {
    return(SBF0(object = object, mean_nyears = 1, lag_nyears = lag_nyears, ...))
  }
)


#---------------- SBSBF0 methods ---------------------------

#' @rdname sb_methods
#' @export
setGeneric("SBSBF0", function(object, sb_mean_nyears, sb_lag_nyears, sbf0_mean_nyears, sbf0_lag_nyears, ...) standardGeneric("SBSBF0"))

#' @rdname sb_methods
#' @export
setMethod(
  "SBSBF0", signature(object = "simpleBiol", sb_mean_nyears = "numeric", sb_lag_nyears = "numeric", sbf0_mean_nyears = "numeric", sbf0_lag_nyears = "numeric"),
  function(object, sb_mean_nyears, sb_lag_nyears, sbf0_mean_nyears, sbf0_lag_nyears, ...) {
    sbf0 <- SBF0(object = object, mean_nyears = sbf0_mean_nyears, lag_nyears = sbf0_lag_nyears, ...)
    sb <- SB(object = object, mean_nyears = sb_mean_nyears, lag_nyears = sb_lag_nyears, ...)
    out <- sb / sbf0
    return(out)
  }
)

#' @rdname sb_methods
#' @export
setMethod(
  "SBSBF0", signature(object = "simpleBiol", sb_mean_nyears = "missing", sb_lag_nyears = "missing", sbf0_mean_nyears = "missing", sbf0_lag_nyears = "missing"),
  function(object, ...) {
    return(SBSBF0(object = object, sb_mean_nyears = 1, sb_lag_nyears = 0, sbf0_mean_nyears = 1, sbf0_lag_nyears = 0, ...))
  }
)

#---------------- Shortcut methods ---------------------------

#' @rdname sb_methods
#' @export
setGeneric("SBrecent", function(object, ...) standardGeneric("SBrecent"))

#' @rdname sb_methods
#' @export
setMethod(
  "SBrecent", signature(object = "simpleBiol"),
  function(object, ...) {
    return(SB(object = object, mean_nyears = 4, lag_nyears = 0, ...))
  }
)


#' @rdname sb_methods
#' @export
setGeneric("SBlatest", function(object, ...) standardGeneric("SBlatest"))

#' @rdname sb_methods
#' @export
setMethod(
  "SBlatest", signature(object = "simpleBiol"),
  function(object, ...) {
    return(SB(object = object, mean_nyears = 1, lag_nyears = 0, ...))
  }
)

#' @rdname sb_methods
#' @export
setGeneric("SBF0recent", function(object, ...) standardGeneric("SBF0recent"))

#' @rdname sb_methods
#' @export
setMethod(
  "SBF0recent", signature(object = "simpleBiol"),
  function(object, ...) {
    return(SBF0(object = object, mean_nyears = 10, lag_nyears = 1, ...))
  }
)

#' @rdname sb_methods
#' @export
setGeneric("SBSBF0recent", function(object, ...) standardGeneric("SBSBF0recent"))

#' @rdname sb_methods
#' @export
setMethod(
  "SBSBF0recent", signature(object = "simpleBiol"),
  function(object, ...) {
    return(SBSBF0(object = object, sb_mean_nyears = 4, sb_lag_nyears = 0, sbf0_mean_nyears = 10, sbf0_lag_nyears = 1, ...))
  }
)

#' @rdname sb_methods
#' @export
setGeneric("SBSBF0latest", function(object, ...) standardGeneric("SBSBF0latest"))

#' @rdname sb_methods
#' @export
setMethod(
  "SBSBF0latest", signature(object = "simpleBiol"),
  function(object, ...) {
    return(SBSBF0(object = object, sb_mean_nyears = 1, sb_lag_nyears = 0, sbf0_mean_nyears = 10, sbf0_lag_nyears = 1, ...))
  }
)
