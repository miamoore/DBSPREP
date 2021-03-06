#' Calculate Truvada efficacy from DBS measurements
#'
#' @param DBS A number or vector giving Tenofovir-DP dried blood spot measurement(s) in fmols/punch
#' @param gender A character vector indicating the gender from which the DBS measurements are taken
#' @param control An option list of control parameters
#'
#' @examples
#' DBS.measurements = runif(100, min = 300, max = 500)
#' prep.dbs.efficacy(DBS.measurements, gender = "msmtgw")
#' prep.dbs.efficacy(DBS.measurements, gender = "ciswomen")
#'
#' @return A vector of length control$quantiles.to.calc. Quantiles of PrEP efficacy,
#' assuming equal HIV risk across all individuals. By default, returns the 2.5%,
#' 50%, and 97.5% quantiles.
#' @details
#'
#' Provides estimates of Truvada efficacy, 1 - (hazard ratio).
#' The DBS measurements should be provided as fmols of Tenofovir-Disphospate (TFV-DP) per punch.
#'
#' Warning: Intracellular tenofovir can take weeks to build up in red blood cells.
#' Currently, this function is only calibrated to equilibrium levels so only use
#' measurements from individuals who have been assigned daily PrEP for at least
#' ten weeks prior to testing.
#'
#' \code{gender="msmtgw"} denotes men and transgender women who have sex with men.
#' This estimate is uses an exponential curve fit to data
#' from the open label extensions (OLEs) of iPrEX, ATN 082 and the US PrEP
#' safety study. (Grant, 2014, The Lancet)
#'
#' \code{gender="ciswomen"} denotes estimates in cisgender women. This is derived from
#' studies of PrEP among young women in sub-Saharan Africa: Partners PrEP, VOICE,
#' FEM-PrEP, and HPTN 082. The methodology will be presented as an abstract
#' at HIVR4P 2021 (Moore, 2021, R4P).
#'
#' @section Control:
#'
#' \itemize{
#'
#' \item \code{quantiles.to.calc}
#'
#' Default \code{c(.025, .5, .975)}, the quantiles to calculate.
#'
#' \item \code{rm.na}
#'
#' Default \code{TRUE}, should missing DBS measurements be ignored?
#'
#' \item \code{N.new}
#'
#' Default \code{10^3}, number of simulations per batch
#'
#' \item \code{precision}
#'
#' Default \code{.001}, tolerance of the difference of each value between batches.
#'
#' \item \code{maxit}
#'
#' Default \code{10^6}, max number of iterations
#' }
#'
#'
#' @export

prep.dbs.efficacy = function(DBS, gender = c("msmtgw","ciswomen"), control = list()){
  efficacy.func = ifelse(gender=="msmtgw",
                         Truvada.Eff.DBS.Grant, Truvada.Eff.DBS.Moore)

  control = get.control.list(control)
  control$func = efficacy.func
  Truvada.Efficacy.Converge(DBS,control)
}

