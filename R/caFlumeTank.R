#' @title Cantaloupe washing in flume tank
#'
#' @description
#' The function [caFlumeTank()] simulates three events taking place during flume tank washing:
#'   \enumerate{
#'     \item the washing of bacteria off the cantaloupe rind at a washing efficiency of `logDecWash`;
#'     \item the elimination of bacteria in the sanitising water at a sanitising efficiency of `log10SaniWash`; and
#'     \item the redistribution of survivors on the cantaloupe rind.
#'     }
#'
#' @param data a list of:
#'     \describe{
#'               \item{`N`}{(`CFU`) A matrix of size `nLots` lots by `sizeLot` units representing the numbers of \emph{L. monocytogenes}
#'               on the rind, from contaminated cultivation lots;}
#'               \item{`P`}{Prevalence of contaminated harvested lots pre-washing (scalar).}
#'             }
#' @param nLots Number of harvested lots of cantaloupe.
#' @param sizeLot Number of cantaloupes in a harvested lot.
#' @param logDecWash (log10) Reduction attained by washing (scalar or vector).
#' @param logDecSani (log10) Reduction attained by sanitising water (scalar or vector).
#' @param b Dispersion factor representing the clustering of surviving cells during redistribution (scalar, vector or matrix).
#'
#' @return
#' A list of two elements of the data objects:
#'     \describe{
#'              \item{`N`}{(`CFU`) A matrix of size `nLots` lots by `sizeLot` units representing the numbers of \emph{L. monocytogenes}
#'              on washed cantaloupes.}
#'              \item{`P`}{Prevalence of contaminated harvested lots post-washing (scalar).}
#'              }
#'
#' @author Regis Pouillot \email{rpouillot.work@gmail.com}
#'
#' @keywords water immersion removal
#'
#' @references
#' \insertRef{mc2d}{qraLm}
#'
#' \insertRef{stats}{qraLm}
#'
#' \insertRef{Nauta2005}{qraLm}
#'
#' @importFrom mc2d rmultinomial rdirichlet
#' @importFrom stats rbinom rmultinom
#' @importFrom utils data
#'
#' @export
#'
#' @note Since surviving cells are distributed in water, a value  of `b` higher than 1 should be used to assume
#' random homogeneous distribution. This dispersion factor is a parameter of the beta-binomial
#'  distribution (\insertCite{Nauta2005;textual}{qraLm}).
#'
#' @examples
#' library(extraDistr)
#' library(mc2d)
#' dat <- caPrimaryProduction(
#'   nLots = 100,
#'   sizeLot = 100,
#'   cantaWeight = 800,
#'   pSoil = 0.089,
#'   pManure = 0.1,
#'   pIrrigRaining = 0.05,
#'   pFoil = 0.5,
#'   rFoil = 0.75,
#'   pIrrig = 0.131
#' )
#' 
#' WashedMelons <- caFlumeTank(dat,
#'   logDecWash = 1.5,
#'   logDecSani = 2,
#'   b = 2.5,
#'   nLots = nLots,
#'   sizeLot = sizeLot
#' )
#' hist(WashedMelons$N)
#'
caFlumeTank <- function(data = list(),
                        logDecWash,
                        logDecSani,
                        b,
                        nLots = NULL,
                        sizeLot = NULL) {
  
  ifelse(exists("nLots", data) == TRUE, nLots <- data$nLots, nLots <- nrow(data$N))
  ifelse(exists("sizeLot", data) == TRUE, sizeLot <- data$sizeLot, sizeLot <- ncol(data$N))
 
  if (!length(logDecWash) %in% c(1, nLots)) stop("logDecWash not conformable size in FlumeTank_CA")
  if (!length(logDecSani) %in% c(1, nLots)) stop("logDecSani not conformable size in FlumeTank_CA")
  if (!length(b) %in% c(1, nLots, nLots * sizeLot)) stop("b not confortable size in FlumeTank_CA")

  # Wash the cantaloupes
  pNotWash <- 10^c(-logDecWash)
  # Those stay on the cantaloupe
  NNotWash <- matrix(stats::rbinom(
    n = nLots * sizeLot,
    size = data$N,
    prob = pNotWash
  ), ncol = sizeLot, nrow = nLots)

  # Sanitize the washed bacteria (those that were removed)
  NWash <- rowSums(data$N) - rowSums(NNotWash)
  pSurvive <- 10^c(-logDecSani)
  NtoDistribute <- stats::rbinom(
    n = nLots,
    size = NWash,
    prob = pSurvive
  )

  # Then redistribute the unsanitized
  if (length(b) == 1) {
    # If b is scalar, this is quicker
    p <- mc2d::rdirichlet(nLots, rep(b, sizeLot))
    N_out <- mc2d::rmultinomial(nLots, NtoDistribute, p)
  } else {
    # if b is a vector, mapply a function
    f <- function(N, b) {
      p <- mc2d::rdirichlet(1, rep(b, sizeLot))
      N_out <- rmultinom(1, N, p)
      return(N_out)
    }
    N_out <- t(mapply(f,
      N = NtoDistribute,
      b = rep(b, length = nLots)
    ))
  }
  # Final are the one that stayed + the one that were redistributed
  N <- NNotWash + N_out
  
  lotMeans <- rowMeans(N / data$cantaWeight, na.rm = TRUE)
  unitsCounts <- c(N / data$cantaWeight)
  data$lotMeans <- lotMeans
  data$unitsCounts <- unitsCounts
  data$N <- N
  
# In case of 0 (per lot)
  # zeroes <- rowSums(Nfinal) == 0
  # if(all(zeroes)) stop("No more bacteria in the system following flume tank")
  # Nfinal[zeroes,] <- Nfinal[sample(which(!zeroes),sum(zeroes), replace=TRUE),]
  #
  # # Adjust prevalence (all washed AND all N0 sanitized)
  # #  All washed, at the cantaloupe level, for the whole lot
  # allWashed <- apply((1-c(pNotWash))^data$N, 1, prod)
  # #  All those N0$N are sanitized
  # allSanitized <- (1-c(pSurvive))^rowSums(data$N)
  # #  At least one not washed and not sanitized (recycle on lots)
  # atLeastOneSurvive <- 1 - allWashed*allSanitized
  # P1 <- data$P * mean(atLeastOneSurvive)

  return(data)
}
