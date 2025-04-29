#  
#  Copyright 2025 Finlay Scott. Distributed under the GPL 2 or later
#  Maintainer: Finlay Scott
#

#' Class simpleBiol
#'
#' A simple class for modelling age structured populations.
#'
#' The \code{simpleBiol} class is a representation of a biological fish population.
#' This includes information on abundances, natural mortality and maturity.
#'
#' @name simpleBiol
#' @aliases simpleBiol-class n,simpleBiol-method m,simpleBiol-method wt,simpleBiol-method name,simpleBiol-method desc,simpleBiol-method n<-,simpleBiol,FLQuant-method m<-,simpleBiol,FLQuant-method wt<-,simpleBiol,FLQuant-method name<-,simpleBiol,character-method desc<-,simpleBiol,character-method 
#' @docType class
#' @section Slots: \describe{
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
#' @template Accessors
#' @template Constructors
#' @section Validity: \describe{
#'     \item{Dimensions}{All FLQuant slots must have iters equal to 1 or 'n'.}
#'     \item{Iters}{The dimname for iter[1] should be '1'.}
#'     \item{Dimnames}{The name of the quant dimension must be the same for all FLQuant slots.}
#' }
#' @author Finlay Scott
#' @keywords classes

setClass("simpleBiol",
         representation(
           n        ="FLQuant",
           n0        ="FLQuant",
           m        ="FLQuant",
           wt       ="FLQuant",
           mat      ="FLQuant",
           rec_dist ="FLQuant",
           srr_params = "FLPar",
           movement = "array",
           name     ="character",
           desc     ="character"
           ),
         prototype=prototype(
           n        = FLQuant(),
           n0        = FLQuant(),
           m        = FLQuant(),
           wt       = FLQuant(),
           mat      = FLQuant(),
           rec_dist = FLQuant(),
           srr_params = FLPar(),
           movement = array(),
           name = "",
           desc = ""),
         validity = function(object) {
           # To do
           # Dim of movement should be 5
           # Set up empty movement: nareas x nareas x nages x nseasons x niters
           return(TRUE)
         }
)

#' Class simpleFisheries
#'
#' A simple class for modelling fisheries.
#'
#' The \code{simpleFisheries} class is a simple representation of multiple fisheries.
#' This includes information on abundances, natural mortality and maturity.
#' This class only works for fishing on a single stock.
#' Fisheries are stored in the unit dimension.
#'
#' @name simpleFisheries
#' @aliases simpleFisheries-class 
#' @docType class
#' @section Slots: \describe{
#'   \item{catch_n}{Catch numbers. \code{FLQuant}.}
#'   \item{catch_wt}{Catch weights at age. \code{FLQuant}.}
#'   \item{sel}{Selectivity. \code{FLQuant}.}
#'   \item{catch_q}{Catchability paramsters. \code{FLQuant}.}
#'   \item{effort}{Fishing effort. \code{FLQuant}.}
#'   \item{name}{Name of the object. \code{character}.}
#'   \item{desc}{Brief description of the object. \code{character}.}
#' @template Accessors
#' @template Constructors
#' @section Validity: \describe{
#'     \item{Dimensions}{All FLQuant slots must have iters equal to 1 or 'n'.}
#'     \item{Iters}{The dimname for iter[1] should be '1'.}
#'     \item{Dimnames}{The name of the quant dimension must be the same for all FLQuant slots.}
#' }
#' @author Finlay Scott
#' @keywords classes

setClass("simpleFisheries",
         representation(
           catch_n        ="FLQuant",
           catch_wt       ="FLQuant",
           sel      ="FLQuant",
           catch_q     ="FLQuant", # Could be FLPar?
           effort      ="FLQuant",
           name     ="character",
           desc     ="character"
           ),
         prototype=prototype(
           catch_n        =FLQuant(),
           catch_wt       =FLQuant(),
           sel      =FLQuant(),
           catch_q     =FLQuant(), # Could be FLPar?
           effort     =FLQuant(),
           name = "",
           desc = ""),
         validity = function(object) {
           # To do
           return(TRUE)
         }
)

