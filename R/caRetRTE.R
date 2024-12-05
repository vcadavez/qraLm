#' @title Display of RTE diced cantaloupe packs at retail
#'
#' @description
#' The function [caRetRTE()] simulates the growth of \emph{L. monocytogenes} in RTE diced cantaloupe during display
#' at retail, and is based on the function [caGrowthBaranyi()] The algorithm considers that RTE diced cantaloupe packs
#' from every lot are subjected to the same `lnQt` (passed from the previous logistic stage) and the same retail temperature (`Temp`).
#' Retail time is sampled at the unit level.
#' The input `data` provides the algorithm with the lot-specific values of `EGR5` and the. Pert distributions
#' represent the variability in transport time and temperature.
#'
#' @param data a list of:
#'    \describe{
#'      \item{N}{(`CFU`) A matrix containing the numbers of \emph{L. monocytogenes} in packs of RTE diced cantaloupe at the start of retail, from contaminated lots;}
#'      \item{P}{Prevalence of contaminated lots (scalar);}
#'      \item{lnQt}{Natural log of the `Q` parameter at the start of retail (matrix);}
#'      \item{lotEGR5}{(\eqn{h^{-1}}) Growth rate of \emph{L. monocytogenes} in cantaloupe flesh specific to every lot (vector);}
#'      \item{unitSize}{(`g`) Weight of a pack of cantaloupe dices.}
#'      }
#' @param tempMin (\eqn{^\circ} C) Minimum retail temperature (scalar).
#' @param tempMode (\eqn{^\circ} C) Mode of the retail temperature (scalar).
#' @param tempMax (\eqn{^\circ} C) Maximum retail temperature (scalar).
#' @param timeMin (`h`) Minimum retail time (scalar).
#' @param timeMode (`h`) Mode of the retail time (scalar).
#' @param timeMax (`h`) Maximum retail time (scalar).
#' @param MPD (log10 CFU/g) Maximum population density of \emph{L. monocytogenes} in cantaloupe flesh (scalar).
#' @param Tmin (\eqn{^\circ} C) Nominal minimum temperature for growth of \emph{L. monocytogenes} in cantaloupe flesh (suggested \eqn{default=-2.0196\ ^\circ C}) (scalar).
#'
#' @return A list of five elements:
#'     \describe{
#'              \item{N}{(`CFU`) A matrix containing the numbers of \emph{L. monocytogenes} in packs of RTE diced cantaloupe
#'              at the end of retail, from contaminated lots;}
#'              \item{P}{Prevalence of RTE diced cantaloupe lots contaminated with \emph{L. monocytogenes} (scalar);}
#'              \item{lnQt}{Natural log of the `Q` parameter at the end of retail (matrix);}
#'              \item{lotEGR5}{(\eqn{h^{-1}}) Growth rate of \emph{L. monocytogenes} in cantaloupe flesh specific to every lot (vector);}
#'              \item{unitSize}{(`g`) Weight of a pack of cantaloupe dices.}
#'              }
#'
#' @author Ursula Gonzales-Barron \email{ubarron@ipb.pt}
#'
#' @keywords stochastic growth lag-phase logistics Baranyi
#'
#' @references
#'
#' \insertRef{Derens2015}{qraLm}
#'
#' \insertRef{mc2d}{qraLm}
#'
#' @importFrom mc2d rpert
#'
#' @export
#'
#' @note
#' The suggested parameters for the Pert distribution of retail temperature are taken from \insertCite{Derens2015;textual}{qraLm}:
#' \eqn{Temp\_min=1.4\ ^\circ C}, \eqn{Temp\_mode=5.6\ ^\circ C} and \eqn{Temp\_max=9.8\ ^\circ C}. The parameter \eqn{Tmin=-2.0196\ ^\circ C} was determined from fitting
#' a square-root model to data extracted from multiple sources (refer to the function [caGrowthBaranyi()]).
#' Parameters for the retail time distribution should be defined by the user and/or tested in scenarios. In addition to the
#' final \emph{L. monocytogenes} numbers, the function also returns the values of `lotEGR5` and `lnQt`
#' so that the \emph{L. monocytogenes} growth could be followed up in subsequent logistics stages.
#'
#' @examples
#' library(extraDistr)
#' dat <- caPrimaryProduction(
#'                            nLots = 100,
#'                            sizeLot = 100)
#'                            dat$unitSize = 900
#'                            dat$lotEGR5 = 0.3
#'                            dat$lnQt = 0.2
#' str(dat)
#' EndRetail <- caRetRTE(dat,
#'                       MPD = 10,
#'                       Tmin = -2.0196,
#'                       tempMin = 1.4, 
#'                       tempMode = 5.6, 
#'                       tempMax = 9.8,
#'                       timeMin = 2,
#'                       timeMode = 5,
#'                       timeMax = 9
#'                       )
#' str(EndRetail)
#' str(EndRetail$lnQt)
#'
caRetRTE <- function(data = list(),
                     MPD = NULL,
                     Tmin = -2.0196,
                     tempMin,
                     tempMode,
                     tempMax,
                     timeMin,
                     timeMode,
                     timeMax) {
  if (missing(MPD)) MPD <- data$MPD # test if MPD was defined
  if (is.null(MPD)) warning("Add 'MPD=#' to function arguments") # test again if MPD is defined

    # To evaluate the growth during retail
  N_t <- data$N
  # Get the dimensions
  Number_packs <- ncol(N_t)
  old_nLots <- nrow(N_t)

  # if(missing(lotEGR5)) lotEGR5 <- data$lotEGR5 #test if lotEGR5 was defined
  # if(is.null(lotEGR5)) warning("Add 'lotEGR5=#' to function arguments") #test again if lotEGR5 is defined
  #
  # if(missing(lnQt)) lnQt <- data$lnQt #test if lnQt was defined
  # if(is.null(lnQt)) warning("Add 'lnQt=#' to function arguments") #test again if lnQt is defined
  #

  if (!all(c(
    length(MPD), length(data$lotEGR5), length(Tmin),
    length(tempMin), length(tempMode), length(tempMax), length(timeMin),
    length(timeMode), length(timeMax), length(data$lnQt)
  )
  %in% c(1, old_nLots, old_nLots * Number_packs))) {
    stop("unconformable size of parameters")
  }

  # sampling values for every batch arranging into vectors
  Temp_v <- mc2d::rpert(old_nLots, min = tempMin, mode = tempMode, max = tempMax, shape = 4)
  time_v <- mc2d::rpert(old_nLots * Number_packs, min = timeMin, mode = timeMode, max = timeMax, shape = 4)
  Q0_v <- exp(data$lnQt)

  # Calculates final numbers N (`CFU`) in contaminated packs and places zero in non-contaminated packs
  # if(missing(unitSize)) unitSize <- data$unitSize #test if unitSize was defined
  # if(is.null(unitSize)) warning("Add 'unitSize=#' to function arguments") #test again if unitSize is defined
  #
  if (missing(Tmin)) Tmin <- data$Tmin # test if Tmin was defined
  if (is.null(Tmin)) warning("Add 'Tmin=#' to function arguments") # test again if Tmin is defined

  results <- caGrowthBaranyi(data,
                             MPD = MPD,
                             unitSize = data$unitSize,
                             EGR5 = data$lotEGR5,
                             Tmin = Tmin,
                             Temp = Temp_v,
                             time = time_v,
                             Q0 = Q0_v
                             )

  Nt_matrix <- ceiling(matrix(results$Nt, ncol = Number_packs, nrow = old_nLots))
  lnQt_matrix <- matrix(results$lnQt, ncol = Number_packs, nrow = old_nLots)

  N <- Nt_matrix
  lotMeans <- rowMeans(N / data$unitSize, na.rm = TRUE)
  unitsCounts <- c(N / data$unitSize)
  data$lotMeans <- lotMeans
  data$unitsCounts <- unitsCounts
  data$N <- N
  data$lnQt <- lnQt_matrix
  return(data)
}
