#' @title Transport of RTE diced cantaloupe packs from the end of processing to retail
#'
#' @description
#' The function [caTrans2RetRTE()] simulates the growth of \emph{L. monocytogenes} in RTE diced cantaloupe during cold transport
#' to retail, and is based on the function [caGrowthBaranyi()]. The algorithm considers that RTE diced cantaloupe packs
#'  from every lot are subjected
#' to the same initial `q0` (related to lag phase), the same transportation temperature (`Temp`), and the same transportation `time`.
#' The normal distribution with parameters `lnQ0Mean` and `lnQ0Sd` represents the variability about `ln_q0`, a parameter
#' related to the physiological state of cells in the Baranyi ad Roberts' growth model. The uncertainty about `EGR5` is
#' represented by a normal distribution with parameters `meanEGR5` and `sdEGR5`. Pert distributions represent the
#' lot-specific variability in transport time and temperature.
#'
#' @param data a list of:
#'    \describe{
#'      \item{N}{(`CFU`) A matrix containing the numbers of \emph{L. monocytogenes} in packs of RTE diced cantaloupe before transport,
#'       from contaminated lots.}
#'      \item{P}{Prevalence of contaminated lots (scalar).}
#'      }
#' @param tempMin (\eqn{^\circ C}) Minimum transportation temperature (scalar).
#' @param tempMode (\eqn{^\circ C}) Mode of the transportation temperature (scalar).
#' @param tempMax (\eqn{^\circ C}) Maximum transportation temperature (scalar).
#' @param timeMin (`h`) Minimum transportation time (scalar).
#' @param timeMode (`h`) Mode of the transportation time (scalar).
#' @param timeMax (`h`) Maximum transportation time (scalar).
#' @param lnQ0Mean Mean of the natural log of `Q0` (suggested \eqn{default=-0.096728} at \eqn{time=0}) (scalar).
#' @param lnQ0Sd Standard deviation of the natural log of `Q0` (suggested \eqn{default=0.063930} at \eqn{time=0}) (scalar).
#' @param MPD (\eqn{log10\ CFU/g}) Maximum population density of \emph{L. monocytogenes} in cantaloupe flesh (scalar).
#' @param unitSize (`g`) Weight of a pack of cantaloupe dices (scalar).
#' @param meanEGR5 (\eqn{h^{-1}}) Mean exponential growth rate of \emph{L. monocytogenes} in cantaloupe flesh at
#' 5 \eqn{^\circ C} (suggested \eqn{default=0.03557288\ h^{-1}}) (scalar).
#' @param seEGR5 (\eqn{h^{-1}}) Standard error about the mean of the exponential growth rate of \emph{L. monocytogenes} in
#' cantaloupe flesh at 5 \eqn{^\circ C} (suggested \eqn{default=0.004\ h^{-1}}) (scalar).
#' @param Tmin (\eqn{^\circ C}) Nominal minimum temperature for growth of \emph{L. monocytogenes} in cantaloupe flesh
#' (suggested \eqn{default=-2.0196\ ^\circ C}) (scalar).
#'
#' @return A list of five elements:
#'     \describe{
#'              \item{N}{(`CFU`) A matrix containing the numbers of \emph{L. monocytogenes} in packs of RTE diced cantaloupe after transport,
#'               from contaminated lots;}
#'              \item{P}{Prevalence of RTE diced cantaloupe lots contaminated with \emph{L. monocytogenes} (scalar);}
#'              \item{lnQt}{Natural log of the `Q` parameter at the end of transportation (matrix);}
#'              \item{lotEGR5}{(\eqn{h^{-1}}) Growth rate of \emph{L. monocytogenes} in cantaloupe flesh specific to every lot (vector);}
#'              \item{unitSize}{(`g`) Weight of a pack of cantaloupe dices.}
#'              }
#' @author Ursula Gonzales-Barron \email{ubarron@ipb.pt}
#'
#' @keywords stochastic growth lag-phase logistics Baranyi
#'
#' @references
#'
#' \insertRef{Ding2013}{qraLm}
#'
#' \insertRef{extraDistr}{qraLm}
#'
#' \insertRef{mc2d}{qraLm}
#'
#' \insertRef{SantAna2014}{qraLm}
#'
#' \insertRef{Scolforo2017}{qraLm}
#'
#' \insertRef{stats}{qraLm}
#'
#' @importFrom stats rnorm
#' @importFrom extraDistr rtnorm
#' @importFrom mc2d rpert
#'
#' @export

#' @note
#' `meanEGR5`, `seEGR5` and `Tmin` were determined from fitting a square-root model to data extracted from multiple sources, as described in
#' more detail in the function [caGrowthBaranyi()]. `lnQ0Mean` and `lnQ0Sd` were determined by running a Monte Carlo simulation to data in canary melon
#' consisting of growth rate and lag phase duration obtained at different temperatures. Such data were extracted from \insertCite{Scolforo2017;textual}{qraLm}.
#' The suggested parameters for the Pert distribution of transportation temperature are taken from \insertCite{Ding2013;textual}{qraLm} and \insertCite{SantAna2014;textual}{qraLm}:
#' \eqn{Temp\_min=3\ ^\circ C}, \eqn{Temp\_mode=5\ ^\circ C} and \eqn{Temp\_max=10.3\ ^\circ C}. Parameters for the transportation time distribution should be defined by the user.
#' In addition to the final \emph{L. monocytogenes} numbers, the function also returns the lot-specific values of `lotEGR5` and `lnQt` so that the
#' \emph{L. monocytogenes} growth could be followed up in subsequent logistics stages.
#'
#' @examples
#' timeMin <- 2
#' timeMode <- 5
#' timeMax <- 9
#' nLots <- 1000
#' sizeLot <- 250
#' dat <- list(
#'   N = matrix(rpois(nLots * sizeLot, 23), nLots, sizeLot),
#'   P = 0.4,
#'   unitSize = 200
#' )
#' ArriveRetail <- caTrans2RetRTE(dat,
#'   MPD = 8.5,
#'   meanEGR5 = 0.03557288,
#'   seEGR5 = 0.004,
#'   Tmin = -2.0196,
#'   tempMin = 3,
#'   tempMode = 5,
#'   tempMax = 10.3,
#'   timeMin = timeMin,
#'   timeMode = timeMode,
#'   timeMax = timeMax,
#'   lnQ0Mean = -0.096728,
#'   lnQ0Sd = 0.063930
#' )
#' str(ArriveRetail)
#' hist(ArriveRetail$N)
caTrans2RetRTE <- function(data = list(),
                           MPD,
                           unitSize = NULL,
                           meanEGR5,
                           seEGR5,
                           Tmin,
                           tempMin, tempMode, tempMax,
                           timeMin, timeMode, timeMax,
                           lnQ0Mean, lnQ0Sd) {
  if (missing(unitSize)) unitSize <- data$unitSize # test if unitSize was defined
  if (is.null(unitSize)) warning("Add 'unitSize=#' to function arguments") # test again if unitSize is defined

  # To evaluate the growth during transportation
  N_t <- data$N
  # Get the dimensions
  Number_packs <- ncol(N_t)
  old_nLots <- nrow(N_t)

  if (!all(c(
    length(MPD), length(meanEGR5), length(seEGR5), length(Tmin),
    length(tempMin), length(tempMode), length(tempMax), length(timeMin),
    length(timeMode), length(timeMax), length(lnQ0Mean),
    length(lnQ0Sd)
  ) %in% c(1, old_nLots, old_nLots * Number_packs))) {
    stop("unconformable size of parameters")
  }

  # sampling values for every batch arranging into vectors
  lotEGR5 <- extraDistr::rtnorm(old_nLots, meanEGR5, seEGR5, a = 0)
  Temp_v <- mc2d::rpert(old_nLots, min = tempMin, mode = tempMode, max = tempMax, shape = 4)
  time_v <- mc2d::rpert(old_nLots, min = timeMin, mode = timeMode, max = timeMax, shape = 4)
  Q0_v <- exp(stats::rnorm(old_nLots, lnQ0Mean, lnQ0Sd))

  # Calculates final counts N (`CFU`) in contaminated packs and places zero in non-contaminated packs

  results <- caGrowthBaranyi(data,
    MPD = MPD,
    unitSize = data$unitSize,
    EGR5 = lotEGR5,
    Tmin = Tmin,
    Temp = Temp_v,
    time = time_v,
    Q0 = Q0_v
  )

  Nt_matrix <- ceiling(matrix(results$Nt, ncol = Number_packs, nrow = old_nLots))
  lnQt_matrix <- matrix(results$lnQt, ncol = Number_packs, nrow = old_nLots)

  N <- Nt_matrix
  lotMeans <- rowMeans(N / unitSize, na.rm = TRUE)
  unitsCounts <- c(N / unitSize)
  data$lotMeans <- lotMeans
  data$unitsCounts <- unitsCounts
  
  data$N <- N
  data$lnQt <- lnQt_matrix
  data$lotEGR5 <- lotEGR5
  data$Tmin <- Tmin
  data$MPD <- MPD
  return(data)
}
