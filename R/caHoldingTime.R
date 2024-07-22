#' @title Holding time of cantaloupes post-harvest
#'
#' @description
#' The function [caHoldingTime()] simulates the survival of \emph{L. monocytogenes} on cantaloupe rind
#' during post-harvest holding time or during any short storage before cantaloupes are washed in the
#' packinghouse. The algorithm calculates decline in  \emph{L. monocytogenes}, and not growth on surfaces,
#' because it is assumed that if any event that injured the rind occurred, it would have been very recent (at harvest),
#' and the holding time would be too short that any growth on the bruised rind would be unlikely or negligible.
#' The user can define a probability `pCooled` that the lot of just-harvested cantaloupes is kept at cold temperatures (4-10 \eqn{^\circ} C).
#' If \eqn{p\_Cooled=0}, the algorithm will evaluate the survival of \emph{L. monocytogenes} on cantaloupes held at ambient temperature (25 \eqn{^\circ} C).

#' @param data a list of:
#'      \describe{
#'                \item{N}{(`CFU`) A matrix of size `nLots` lots by `sizeLot` units representing the numbers of \emph{L. monocytogenes} on
#'                 the cantaloupe rind, from contaminated harvested lots;}
#'                \item{P}{Prevalence of contaminated harvested lots (scalar).}
#'               }
#' @param nLots Number of lots sampled or size of the Monte Carlo simulation (scalar).
#' @param sizeLot Number of units or portions produced in a lot (scalar).
#' @param pCooled Probability that a lot of harvested cantaloupes is cooled (4-10 \eqn{^\circ C}).
#' @param time (`h`) Holding time or storage time of a lot of harvested cantaloupes.
#' @param timePrevious (`h`) Storage time of cantaloupes in previous holding stage(s).
#' @param shape Shape parameter of the Weibull decay model (\eqn{default = 0.6271}, obtained from modelling data from \insertCite{Nyarko2016;textual}{qraLm}).
#' @param meanD410 (day) Mean of the time to first log10 reduction at 4-10 \eqn{^\circ C} (default = 1.1309 day,
#' obtained from modelling data from \insertCite{Nyarko2016;textual}{qraLm}).
#' @param sdD410 (day) Standard deviation of the time to first log10 reduction at 4-10 \eqn{^\circ C} (\eqn{default = 1.770711e-06} day, obtained
#' from modelling data from \insertCite{Nyarko2016;textual}{qraLm}).
#' @param meanD25 (day) Mean of the time to first log10 reduction at 25 \eqn{^\circ} C) (\eqn{default = 2.890015} day, obtained from modelling data
#'  from \insertCite{Nyarko2016;textual}{qraLm}).
#' @param sdD25 (day) Standard deviation of the time to first log10 reduction at 25 \eqn{^\circ} C (\eqn{default = 0.2288748} day, obtained
#' from modelling data from \insertCite{Nyarko2016;textual}{qraLm}).
#'
#' @return A list of two elements of the data objects:
#'     \describe{
#'              \item{N}{(`CFU`) A matrix of size `nLots` lots by `sizeLot` units representing the numbers of \emph{L. monocytogenes} on
#'              cantaloupe after holding `time`;}
#'              \item{P}{Prevalence of contaminated harvested lots after holding `time`;}
#'              \item{timePrevious}{(`h`) Total time the cantaloupes have been stored since harvesting.}
#'              }
#'
#' @author Ursula Gonzales-Barron \email{ubarron@ipb.pt}
#'
#' @keywords decline storage cantaloupe rind
#'
#' @references
#' \insertRef{extraDistr}{qraLm}
#'
#' \insertRef{stats}{qraLm}
#'
#' \insertRef{Nyarko2016}{qraLm}
#'
#' @importFrom stats rbinom
#' @importFrom extraDistr rtnorm
#'
#' @export
#'
#' @note Data on the survival of \emph{L. monocytogenes} on the intact rind of two varieties of cantaloupe stored at 4, 10 and 25 \eqn{^\circ} C were
#' extracted from \insertCite{Nyarko2016;textual}{qraLm}. A non-linear mixed model based on the two-parameter Weibull decay equation with random effects
#' on melon variety showed that the time to achieve the first log10 reduction (`D`) at 25 \eqn{^\circ} C was significantly different from the other two temperatures,
#' while there was not significant difference in `D` between the storage temperatures of 4 and 10 \eqn{^\circ C}. Therefore, the non-linear mixed model was refitted
#' for two temperature classes (cool 4/10 \eqn{^\circ C}) and ambient (25 \eqn{^\circ C}). The shape parameter is fixed for the two temperature classes.
#' Placing random effects in `D` with cantaloupe variety as the clustering variable, the estimates of  `meanD410`, `sdD410`, `meanD25` and `sdD25` were obtained.
#' The present function represents the cultivar-specific variability about `D` at cool or ambient temperature as a normal distribution;
#' and a `D` value is sampled for every lot, assuming a different cantaloupe cultivar.
#' It is assumed that after the holding time, it is not possible that a contaminated lot become free of \emph{L. monocytogenes}. Therefore,
#' if by chance the rinds of all cantaloupes from a contaminated lot end up having zero counts, one cell will be assigned to one cantaloupe of the lot.
#' @examples
#' dat <- caPrimaryProduction(
#'   nLots = 100,
#'     sizeLot = 100)
#' pCooled <- 0.75
#' time <- 4 # hours
#' AfterStorage <- caHoldingTime(dat,
#'   pCooled = pCooled,
#'   time = time,
#'   shape = 0.6271,
#'   meanD410 = 1.1309,
#'   sdD410 = 1.770711e-06,
#'   meanD25 = 2.890015,
#'   sdD25 = 0.2288748
#' )
#' hist(AfterStorage$N, probability = TRUE)
#' lines(density(AfterStorage$N), col = "black", lwd = 2)
caHoldingTime <- function(data = list(),
                          nLots = NULL,
                          sizeLot = NULL,
                          pCooled,
                          time,
                          shape = 0.6271,
                          meanD410 = 1.1309,
                          sdD410 = 1.770711e-06,
                          meanD25 = 2.890015,
                          sdD25 = 0.2288748,
                          timePrevious = NULL) {
  # Get the dimensions
  #  sizeLot <- ncol(data$N)
  #  nLots <- nrow(data$N)

  if (missing(nLots)) nLots <- data$nLots # test if nLots was defined
  if (is.null(nLots)) warning("Add 'nLots=#' to function arguments") # test again if nLots is defined

  if (missing(sizeLot)) sizeLot <- data$sizeLot # test if sizeLot was defined
  if (is.null(sizeLot)) warning("Add 'sizeLot=#' to function arguments") # test again if sizeLot is defined

  ifelse(exists("timePrevious", data) == TRUE, timePrevious <- data$timePrevious, timePrevious <- 0)
  #  if (exists("timePrevious", data)==FALSE) timePrevious <- 0
  #  if(is.null(timePrevious)) timePrevious <- 0

  Batch_cooled <- as.logical(stats::rbinom(n = nLots, size = 1, prob = pCooled))

  Dmean <- rep(meanD25, nLots)
  Dmean[Batch_cooled] <- meanD410

  DSD <- rep(sdD25, nLots)
  DSD[Batch_cooled] <- sdD410

  D <- extraDistr::rtnorm(nLots, Dmean, DSD, a = 0)
  # pSurvive <- 10^-((time/(24*D))^shape)
  # Changed by RP
  pSurvive <- 10^(-((time^shape - timePrevious^shape) / ((24 * D)^shape)))

  N_hold <- matrix(
    stats::rbinom(nLots * sizeLot,
      size = data$N,
      prob = pSurvive
    ),
    ncol = sizeLot, nrow = nLots
  )

  # Resampling in case of row of zeroes and Calculating new prevalence
  # Note: the proportion of cooled vs ambient might change
  # (Ask if we need to evaluate the new probability)
  # Commented out
  zeroes <- rowSums(N_hold) == 0
  N_hold[zeroes, 1] <- 1

  # N_hold[zeroes,] <- N_hold[sample(which(!zeroes),sum(zeroes), replace=TRUE),]
  #
  # allDeath <- (1-c(pSurvive))^data$N
  # atLeastOne  <- 1-apply(allDeath, 1, prod)
  # cPi <- mean(atLeastOne)

  # calculating overall prevalence
  # P <- data$P * cPi
  # data$P <- P

  N <- N_hold
  lotMeans <- rowMeans(N / data$cantaWeight, na.rm = TRUE)
  unitsCounts <- c(N / data$cantaWeight)
  data$lotMeans <- lotMeans
  data$unitsCounts <- unitsCounts
  data$N <- N
  data$timePrevious <- data$timePrevious + time

  return(data)
}
