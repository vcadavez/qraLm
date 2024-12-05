#' @title Storage of raw fish (whole or fillets) during processing
#'
#' @description
#' The function [sfRawFishStorage()] simulates the growth of \emph{L. monocytogenes} in whole fish before any short storage, such as
#' the holding time before filleting at primary processing, or in fish fillets during holding time before secondary processing; and is based on the
#' function [sfGrowthLPD()]. The algorithm is built upon two secondary models: one for lag-phase duration, and the other for growth rate,
#' both as functions of temperature, taken from \insertCite{@Jia2020;textual}{qraLm}. Log-linear growth is assumed until reaching
#' the maximum population density `MPD`. Pert distributions represent the lot-specific variability in holding time and temperature of raw fish.
#'
#' @param data a list of four elements:
#'    \describe{
#'      \item{N}{(CFU) A matrix containing the numbers of \emph{L. monocytogenes} on whole fish before filleting, from contaminated lots;}
#'      \item{ProbUnitPos}{Probability of individual lots being contaminated (vector);}
#'      \item{P}{Prevalence of contaminated lots (scalar);}
#'      \item{workDone}{The work done by bacteria.}
#'    }
#' @param nLots Number of lots sampled or size of the Monte Carlo simulation (scalar).
#' @param sizeLot Number of units or portions produced in a lot (scalar).
#' @param unitSize (g) Weight of a fish unit, whole fish or fish fillet (scalar, vector or matrix).
#' @param MPD (log10 CFU/g) Maximum population density of \emph{L. monocytogenes} on/in raw fish (scalar).
#' @param tempMin (\eqn{^\circ C}) Minimum value of the Pert distribution of variability about the holding temperature (scalar).
#' @param tempMode (\eqn{^\circ C}) Mode value of the Pert distribution of variability about the holding temperature (scalar).
#' @param tempMax (\eqn{^\circ C}) Maximum value of the Pert distribution of variability about the holding temperature (scalar).
#' @param timeMin (h) Minimum value of the Pert distribution of variability about the holding time (scalar).
#' @param timeMode (h) Mode value of the Pert distribution of variability about the holding time (scalar).
#' @param timeMax (h) Maximum value of the Pert distribution of variability about the holding time (scalar).

#' @return A list of four elements:
#'      \describe{
#'              \item{N}{(CFU) A matrix containing the numbers of \emph{L. monocytogenes} on whole fish after the holding time, from contaminated lots;}
#'              \item{ProbUnitPos}{Probability of individual lots being contaminated (vector);}
#'              \item{P}{Prevalence of contaminated lots (scalar);}
#'              \item{workDone}{Total work done during the previous stages including the current storage stage (vector).}
#'              }
#'
#' @author Ursula Gonzales-Barron \email{ubarron@ipb.pt}
#'
#' @keywords stochastic growth lag-phase logistics
#'
#'
#' @references
#'
#' \insertRef{mc2d}{qraLm}
#'
#' \insertRef{Jia2020}{qraLm}
#'
#' @importFrom mc2d rpert
#'
#' @export
#'
#' @note
#' The default value of `MPD=9.2` log10 CFU/g of \emph{L. monocytogenes} in raw fish was taken from \insertCite{@Jia2020;textual}{qraLm}.
#' Parameters for the holding time and temperature distributions should be defined by the user an/or tested in scenarios.
#' In addition to the final \emph{L. monocytogenes} numbers, the function also returns the lot-specific values of `workDone` so that the
#' \emph{L. monocytogenes} growth could be followed up in subsequent storage stages, if needed.
#'
#' @examples
#' tempMin <- -1.5
#' tempMode <- 0.5
#' tempMax <- 3
#' timeMin <- 0.5
#' timeMode <- 2
#' timeMax <- 6
#' MPD <- 9.2
#' nLots <- 1000
#' sizeLot <- 500
#' unitSize <- 1200
#' ## No previous storage stage
#' dat <- list(
#'   N = matrix(
#'     rpois(nLots * sizeLot, 23),
#'     nLots,
#'     sizeLot
#'   ),
#'   ProbUnitPos = rep(0.15, nLots),
#'   P = 0.4,
#'   unitSize = unitSize
#' )
#' SecProc1 <- sfRawFishStorage(dat,
#'   unitSize,
#'   MPD = MPD,
#'   tempMin = tempMin,
#'   tempMode = tempMode,
#'   tempMax = tempMax,
#'   timeMin = timeMin,
#'   timeMode = timeMode,
#'   timeMax = timeMax
#' )
#' hist(SecProc1$N)
#' ## Previous storage stage
#' dat <- list(
#'   N = matrix(rpois(nLots * sizeLot, 23), nLots, sizeLot),
#'   ProbUnitPos = rep(0.15, nLots), workDone = rep(0.5, nLots),
#'   P = 0.4,
#'   unitSize = unitSize
#' )
#' SecProc2 <- sfRawFishStorage(dat,
#'   unitSize = 1200,
#'   MPD = MPD,
#'   tempMin = tempMin,
#'   tempMode = tempMode,
#'   tempMax = tempMax,
#'   timeMin = timeMin,
#'   timeMode = timeMode,
#'   timeMax = timeMax
#' )
#' hist(SecProc2$N)
#'
sfRawFishStorage <- function(data = list(),
                             unitSize = NULL,
                             nLots = NULL,
                             sizeLot = NULL,
                             MPD = NULL,
                             tempMin,
                             tempMode,
                             tempMax,
                             timeMin,
                             timeMode,
                             timeMax) {
  # To evaluate the growth during transportation
  N_t <- data$N
  # Get the dimensions
  if (missing(nLots)) nLots <- nrow(N_t) # test if nLots was defined
  if (is.null(nLots)) warning("Add 'nLots=#' to function arguments") # test again if nLots is defined

  if (missing(sizeLot)) sizeLot <- ncol(N_t) # test if sizeLot was defined
  if (is.null(sizeLot)) warning("Add 'sizeLot=#' to function arguments") # test again if sizeLot is defined

  if (missing(unitSize)) unitSize <- data$unitSize # test if unitSize was defined
  if (is.null(unitSize)) warning("Add 'unitSize=#' to function arguments") # test again if unitSize is defined

  if (missing(MPD)) MPD <- data$MPD # test if MPD was defined
  if (is.null(MPD)) warning("Add 'MPD=#' to function arguments") # test again if MPD is defined
  
  #  sizeLot <- ncol(N_t)
  #  nLots <- nrow(N_t)
  if (is.null(data$workDone)) workDone <- 0 else workDone <- data$workDone

  if (!all(c(
    length(MPD), length(tempMin), length(tempMode), length(tempMode),
    length(timeMin), length(timeMode), length(timeMax), length(workDone)
  )
  %in% c(1, nLots, nLots * sizeLot))) {
    stop("unconformable size of parameters in RawFishStorage")
  }

  # sampling values for every batch arranging into vectors
  Temp_v <- mc2d::rpert(nLots, min = tempMin, mode = tempMode, max = tempMax, shape = 4)
  time_v <- mc2d::rpert(nLots, min = timeMin, mode = timeMode, max = timeMax, shape = 4)

  # Calculates final counts N (CFU) in contaminated fillets and the work done
  results <- sfGrowthLPD(data,
                         unitSize = unitSize,
                         MPD = MPD,
                         Temp = Temp_v,
                         time = time_v,
                         aLM = 0.0581,
                         Tmin = 1.3,
                         ALM = 0.84,
                         mLM = 1.11,
                         workDone = workDone
                         )
  #data$results <- results
  N <- ceiling(results$N)
  ProbUnitPos <- results$ProbUnitPos
  P <- results$P
  lotMeans <- rowMeans(N / unitSize, na.rm = TRUE)
  unitsCounts <- c((ProbUnitPos/mean(ProbUnitPos)) * (N / unitSize))
  
  data$lotMeans <- lotMeans
  data$unitsCounts <- unitsCounts
  
  data$N <- N
  data$ProbUnitPos <- ProbUnitPos
  data$P <- P
  data$MPD <- MPD
  data$unitSize <- unitSize
  data$workDone <- results$workDone
 #data$results <- NULL
  #  data$N <- ceiling(results$N)
  #  data$workDone <- results$workDone
  return(data)
}
