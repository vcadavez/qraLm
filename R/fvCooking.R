#' @title Microbial reduction due to cooking at home
#'
#' @description
#' The [fvCooking()] function simulates the reduction in \emph{L. monocytogenes} numbers in defrosted (or still frozen) vegetables due to cooking.
#' The function can represent a scenario where the consumer gives the non-RTE frozen vegetables a non-intended use such as direct (uncooked) consumption in salads, smoothies, etc.
#' In that case, the probability of cooking in the argument should be set to a value different from one.
#' The variability in the effect of cooking defrosted (or frozen) vegetables is represented by a triangular distribution,
#' as assumed in \insertCite{EFSA2020;textual}{qraLm}.
#'
#' @param data a list of:
#'    \describe{
#'           \item{`N`}{(`CFU`) A matrix containing the numbers of \emph{L. monocytogenes} per portion of defrosted or frozen vegetables.}
#'           \item{`ProbUnitPos`}{Lot-specific probability of contaminated portions or servings, defrosted or not (vector).}
#'           \item{`P`}{Mean prevalence of contaminated portions of defrosted or frozen vegetables.}
#'           }
#' @param nLots Number of lots sampled or size of the Monte Carlo simulation (scalar).
#' @param sizeLot Number of units or portions produced in a lot (scalar).
#' @param pCooked Probability of cooking the defrosted or frozen vegetables for consumption
#' @param minCook (`log10`) Minimum value of the triangular distribution representing the variability in the reduction of \emph{L. monocytogenes} in the event of cooking
#' @param modeCook (`log10`) Mode value of the triangular distribution representing the variability in the reduction of \emph{L. monocytogenes} in the event of cooking
#' @param maxCook (`log10`) Maximum value of the triangular distribution representing the variability in the reduction of \emph{L. monocytogenes} in the event of cooking
#'
#' @return A list of three elements:
#'     \describe{
#'              \item{`N`}{(`CFU`) A matrix of size `nLots` lots by `sizeLot` units containing the numbers of \emph{L. monocytogenes}
#'               in the portions or servings to be consumed;}
#'              \item{`ProbUnitPos`}{Lot-specific probability of contaminated portions to be consumed (vector);}
#'              \item{`P`}{Mean prevalence of contaminated portions or servings to be consumed (scalar).}
#'              }
#'
#' @author Laurent Guillier
#'
#' @keywords preparation non-intended use
#'
#' @references
#' \insertRef{mc2d}{qraLm}
#'
#' \insertRef{EFSA2020}{qraLm}
#'
#' \insertRef{Willis2020}{qraLm}
#'
#' \insertRef{FSAI2022}{qraLm}
#'
#' @importFrom mc2d rtriang
#' @importFrom extraDistr rtbinom
#' @importFrom stats rbinom
#' @importFrom Rdpack reprompt
#'
#' @export
#'
#' @note To represent variability in the effect of cooking defrosted (or frozen) vegetables, \insertCite{EFSA2020;textual}{qraLm} assumed a
#'  triangular distribution, with parameters \eqn{minCook=1}, \eqn{modeCook=5} and \eqn{maxCook=9}. According to \insertCite{FSAI2022;textual}{qraLm}, 84-89% of the respondants recognised that
#' frozen vegetables need to be cooked prior to consumption, whereas\insertCite{Willis2020;textual}{qraLm} found that 19% of the non-RTE frozen vegetables packages
#' have no indication for cooking. An average value for the probability of cooking of \eqn{p\_cooked=0.825} can be used in this function.
#'
#' @examples
#' library(mc2d)
#' # N0 <- list(N=matrix(stats::rpois(2000, 15),
#' #            ncol=200, nrow=10),
#' #            P=0.12,
#' #            ProbUnitPos = 0.7)
#' #
#' # N_postcooking=fvCooking(N0,
#' #                          pCooked=0.825,
#' #                          minCook=1,
#' #                          modeCook=5,
#' #                          maxCook=9)
#' # hist(N_postcooking$N)
#'
fvCooking <- function(data = list(),
                      nLots = NULL,
                      sizeLot = NULL,
                      pCooked = 0.825,
                      minCook = 1,
                      modeCook = 5,
                      maxCook = 9) {
  if (pCooked == 0) {
    return(data)
  } # Shortcut

  Nbefore <- data$N
  if (any(Nbefore == 0)) warnings("Watch Out: zeroes in N matrix at the entrance of fvCooking")

  ifelse(exists("nLots", data) == TRUE, nLots <- data$nLots, nLots <- nrow(Nbefore))
  ifelse(exists("sizeLot", data) == TRUE, sizeLot <- data$sizeLot, sizeLot <- ncol(Nbefore))

  # nLots <- nrow(Nbefore)
  #  sizeLot <- ncol(Nbefore)

  portions_cooked <- matrix(as.logical(stats::rbinom(n = sizeLot, size = 1, prob = pCooked)),
    nrow = nLots, ncol = sizeLot, byrow = TRUE
  )
  nPortions_cooked <- sum(portions_cooked)


  # For cooked portions only (run on portions for the prevalence)
  log10_cooking <- matrix(mc2d::rtriang(sizeLot, minCook, modeCook, maxCook),
    nrow = nLots, ncol = sizeLot, byrow = TRUE
  )

  pSurvive <- 10^-log10_cooking[portions_cooked]
  # Truncated binomial
  Nafter_cooked <- suppressWarnings(extraDistr::rtbinom(
    n = nPortions_cooked,
    size = Nbefore[portions_cooked],
    prob = 10^-log10_cooking[portions_cooked],
    a = 0
  ))
  # To avoid special cases with extremely low pSurvive
  Nafter_cooked[Nafter_cooked == Nbefore[portions_cooked] & pSurvive < 1E-5] <- 1
  # Merging uncooked and cooked portions
  Nafter <- Nbefore
  Nafter[portions_cooked] <- Nafter_cooked

  # Prob that a serving is contaminated after that step
  p <- (1 - pCooked) + pCooked * mean(1 - (1 - 10^-log10_cooking)^Nbefore)
  probUnitPos <- (1 - pCooked) + pCooked * rowMeans(1 - (1 - 10^(-log10_cooking))^Nbefore)
  # Prevalence (batch to batch) is unaffected
  Pafter <- data$P * p

  
  N <- Nafter
  P <- Pafter
  ProbUnitPos <- data$ProbUnitPos * probUnitPos
  
  lotMeans <- rowMeans(N / data$unitSize, na.rm = TRUE)
  unitsCounts <- c((ProbUnitPos/mean(ProbUnitPos)) * (N / data$unitSize))
  unitsServing <- c((ProbUnitPos/mean(ProbUnitPos)) * (N / data$servingSize))
  
  data$lotMeans <- lotMeans
  data$unitsCounts <- unitsCounts
  data$unitsServing <- unitsServing
  
  data$N <- N
  data$P <- P
  data$ProbUnitPos <- ProbUnitPos
  
  return(data)
}