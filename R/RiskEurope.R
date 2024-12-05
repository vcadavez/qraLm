#' @title  Risk Europe
#'
#' @description
#' The function [RiskEurope()]  ..
#' @param data A list of four elements see [Lot2LotGen()] function.
#' @param nLots see [Lot2LotGen()] function.
#' @param sizeLot see [Lot2LotGen()] function.
#' @param ProbUnitPos is
#' @param ProbThisLot is
#' @param Virulence  is
#'
#' @importFrom doseresponsemodels DR DRLogNormPoisson DRQuick
#'
#' @export
#'
#' @note notas
#'
#' @examples
#' library(qraLm)
#' sizeLot <- 5
#' nLots <- 3
#' N <- matrix(10^rnorm(sizeLot * nLots, 3, 2), ncol = sizeLot, nrow = nLots)
#' N[1, ] <- 0
#' ProbUnitPos <- rep(0.5, nLots)
#' ProbThisLot <- c(.1, .2, .3)
#' Virulence <- ""
#' # error Error in dimnames(x) <- dn
#' # :length of 'dimnames' [2] not equal to array extent
#' # RiskEurope(N, ProbUnitPos, ProbThisLot, "")
RiskEurope <- function(data = list(),
                       nLots = NULL,
                       sizeLot = NULL,
                       ProbUnitPos,
                       ProbThisLot,
                       Virulence = "") {
  # N : matrix of bacteria
  # P : vector of Prevalence
  # Virulence : "", "LV", "V", "MV"
  # Population in Euroope

  N <- data$N
  ifelse(exists("nLots", data) == TRUE, nLots <- data$nLots, nLots <- nrow(data$N))
  ifelse(exists("sizeLot", data) == TRUE, sizeLot <- data$sizeLot, sizeLot <- ncol(data$N))
  #  nLots <- nrow(N)
  #  sizeLot <- ncol(N)

  Prop <- c(
    9981292, 10507387, 24769674, 26071451, 27917371, 29107545, 67013021,
    68019328, 65803889, 63791535, 24249576, 20921720, 25539929, 15476863
  )
  Prop <- Prop / sum(Prop)
  # Pick the model
  DRmodel <- paste0("EFSA", Virulence)
  # Evaluate the risk for contaminated
  Risk <- array(DRQuick(N,
    model = DRmodel,
    population = 1:14
  ), dim = c(nLots, sizeLot, 14))
  # Build a matrix for weighted mean for tested lots
  ProbThisLot <- nLots * ProbThisLot / sum(ProbThisLot)
  # Mean per lot
  RiskCont <- apply(Risk, c(1, 3), mean)
  # Mean per lot tested
  RiskContTest <- apply(Risk * ProbThisLot, c(1, 3), mean)
  # Multiply by weight populations
  RiskCont <- RiskCont * matrix(Prop, byrow = TRUE, nrow = nLots, ncol = 14)
  RiskContTest <- RiskContTest * matrix(Prop, byrow = TRUE, nrow = nLots, ncol = 14)
  RiskCont <- rowSums(RiskCont)
  RiskContTest <- rowSums(RiskContTest)
  list(RiskCont = mean(RiskCont * ProbUnitPos), RiskContTest = mean(RiskContTest * ProbUnitPos))
}
