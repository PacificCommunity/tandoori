#
#  Copyright 2025 Finlay Scott. Distributed under the GPL 2 or later
#  Maintainer: Finlay Scott
#

#' simpleBiol object for bigeye projections
#'
#' @name bet
#' @docType data
#' @format
#' An object of class simpleBiol with slots:
#' \describe{
#'   \item{n}{Numbers in the population. \code{FLQuant}.}
#'   \item{n0}{Numbers in the population with no fishing. \code{FLQuant}.}
#'   \item{m}{Mortality rate of the population. \code{FLQuant}.}
#'   \item{wt}{Mean weight of an individual. \code{FLQuant}.}
#'   \item{mat}{Proportion of individuals mature. \code{FLQuant}.}
#'   \item{rec_dist}{Distribution of total annual recruitment between seasons and areas. \code{FLQuant}.}
#'   \item{srr_params}{SRR parameters. \code{FLPar}.}
#'   \item{movement}{Movement rates between model areas. \code{array}.}
#'   \item{name}{Name of the object. \code{character}.}
#'   \item{desc}{Brief description of the object. \code{character}.}
#' }
#'
NULL

#' simpleFisheries object for bigeye projections
#'
#' @name bet_fish
#' @docType data
#' @format
#' An object of class simpleFisheries with slots:
#' \describe{
#'   \item{catch_n}{Catch numbers. \code{FLQuant}.}
#'   \item{catch_wt}{Catch weights at age. \code{FLQuant}.}
#'   \item{sel}{Selectivity. \code{FLQuant}.}
#'   \item{catch_q}{Catchability paramsters. \code{FLQuant}.}
#'   \item{effort}{Fishing effort. \code{FLQuant}.}
#'   \item{name}{Name of the object. \code{character}.}
#'   \item{desc}{Brief description of the object. \code{character}.}
#' }
#'
NULL
