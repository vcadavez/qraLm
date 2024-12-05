#' @title Reduction of \emph{L. monocytogenes} due to maceration of fish fillets with gravlax curing agents
#'
#' @description
#' The function [sfMaceration()] describes the effect of macerating the fish fillets with lemon juice, salt, sugar, 
#' black pepper and dill during 72 hours at 4 ºC. Lot-specific log10 reduction values are sampled from a normal distribution.
#'
#' @param data See [Lot2LotGen()] function.
#' @param nLots Number of lots sampled or size of the Monte Carlo simulation (scalar).
#' @param sizeLot Number of units or portions produced in a lot (scalar).
#' @param rGravadMean (`log10`) mean of the normal distribution about the log10 reduction in \emph{L. monocytogenes} in brine-injected
#'  fish fillets due to smoking and 18-24-h maturation (scalar).
#' @param rGravadSd (`log10`) standard deviation of the normal distribution about the log10 reduction in \emph{L. monocytogenes}
#'   in brine-injected fish fillets due to smoking and 18-24-h maturation  (scalar).
#'
#' @return A list of two elements:
#'     \describe{
#'        \item{N}{(CFU) A matrix of size nLots lots by sizeLot units containing the numbers of \emph{L. monocytogenes}
#'            in fish fillets after macerating with gravlax ingredients}
#'        \item{pSurvSmoking}{Probability of a microbial cell to survive the maceration treatment (vector).}
#'            }
#'
#' @author Ursula Gonzales-Barron \email{ubarron@ipb.pt}
#'
#' @keywords log-reduction smoke effect maturation curing
#'
#' @references
#'
#' \insertRef{Lopes2023}{qraLm}
#' \insertRef{extraDistr}{qraLm}
#' \insertRef{Neunlist2005}{qraLm}
#' \insertRef{stats}{qraLm}
#'
#' @importFrom extraDistr rtnorm
#' @importFrom stats rbinom
#' @importFrom Rdpack reprompt
#'
#' @export
#'
#' @note The suggested parameters `rGravadMean=0.7` and `rGravadSd=0.283` defining the normal distribution about the
#' variability in the log10 reduction in \emph{L. monocytogenes} in fish with gravlax ingredients were obtained combining the results
#'from \insertCite{Lopes2023;textual}{qraLm} and \insertCite{Neunlist2005;textual}{qraLm}.
#'
#' @examples
#' rGravadMean=0.7
#' rGravadSd=0.283
#' dat <- Lot2LotGen(
#'                   nLots = 50,
#'                   sizeLot = 100,
#'                   unitSize = 500,
#'                   betaAlpha = 0.5112,
#'                   betaBeta = 9.959,
#'                   C0MeanLog = 1.023,
#'                   C0SdLog = 0.3267,
#'                   propVarInter = 0.7
#'                   )
#' gravadfish <- sfMaceration(dat, 
#'                            rGravadMean=rGravadMean,
#'                            rGravadSd=rGravadSd)
#' hist(gravadfish$N)
#' 
sfMaceration <- function(data = list(),
                      nLots = NULL,
                      sizeLot = NULL,
                      rGravadMean = 0.7,
                      rGravadSd = 0.283) {
  N <- data$N
  # Get the simulation dimension

  if (missing(nLots)) nLots <- nrow(N) # test if nLots was defined
  if (is.null(nLots)) warning("Add 'nLots=#' to function arguments") # test again if nLots is defined

  if (missing(sizeLot)) sizeLot <- ncol(N) # test if sizeLot was defined
  if (is.null(sizeLot)) warning("Add 'sizeLot=#' to function arguments") # test again if sizeLot is defined

  R_gravad <- extraDistr::rtnorm(nLots, rGravadMean, rGravadSd, a = 0)
  pSurvive <- 10^-R_gravad
  N_gravad <- matrix (stats::rbinom(n=nLots*sizeLot, size=N, prob=pSurvive),
                      ncol=sizeLot, nrow=nLots)

  atLeastOneSurvive <- 1 - (1 - pSurvive)^N
  # output
  N <- N_gravad
  ProbUnitPos <- data$ProbUnitPos * rowMeans(atLeastOneSurvive)
  P <- data$P * mean(atLeastOneSurvive)
  
  lotMeans <- rowMeans(N / data$unitSize, na.rm = TRUE)
  unitsCounts <- c((ProbUnitPos/mean(ProbUnitPos)) * (N / data$unitSize))
  
  data$lotMeans <- lotMeans
  data$unitsCounts <- unitsCounts
  data$N <- N
  data$ProbUnitPos <- ProbUnitPos
  data$P <- P
  data$pSurvMaceration <- pSurvive
  return(data)
}
