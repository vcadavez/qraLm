#' @title Transport of RTE diced cantaloupe packs from retail to home
#'
#' @description
#' The function [caRet2HomeRTE()] simulates the growth of \emph{L. monocytogenes} in RTE diced cantaloupe during transport
#' from retail to home, and is based on the function [caGrowthBaranyi()]. The algorithm samples transportation time and temperature
#' at the unit level since they depend on the consumer. The input `data` provides the algorithm with the lot-specific values of `EGR5`
#' and the unit-specific values of `lnQt` obtained from the previous logistics stage. A Gamma distribution represent
#' the variability in transport time, whereas a Pert distribution the variability in transport temperature.
#'
#' @param data a list of:
#' \describe{
#'    \item{N}{(`CFU`) A matrix containing the numbers of \emph{L. monocytogenes} in packs of RTE diced cantaloupe at the
#'      end of retail, from contaminated lots;}
#'    \item{P}{Prevalence of contaminated lots (scalar);}
#'    \item{lnQt}{Natural log of the `Q` parameter at the end of retail (matrix);}
#'    \item{lotEGR5}{(\eqn{h^{-1}}) Growth rate of \emph{L. monocytogenes} in cantaloupe flesh specific to every lot (vector);}
#'    \item{unitSize}{(`g`) Weight of a pack of cantaloupe dices.}
#'    }
#' @param tempMin (\eqn{^\circ C}) Minimum transportation temperature (scalar).
#' @param tempMode (\eqn{^\circ C}) Mode of the transportation temperature (scalar).
#' @param tempMax (\eqn{^\circ C}) Maximum transportation temperature (scalar).
#' @param timeShape Shape parameter of the gamma distribution representing transportation time in hours (scalar) (suggested \eqn{default=6.2}).
#' @param timeScale Scale parameter of the gamma distribution representing transportation time in hours (scalar) (suggested \eqn{default=8.2}).
#' @param MPD (log10 CFU/g) Maximum population density of \emph{L. monocytogenes} in cantaloupe flesh (scalar).
#' @param Tmin (\eqn{^\circ C}) Nominal minimum temperature for growth of \emph{L. monocytogenes} in cantaloupe flesh (suggested \eqn{default=-2.0196\ ^\circ C}) (scalar).
#'
#' @return A list of five elements:
#'     \describe{
#'         \item{N}{(`CFU`) A matrix containing the numbers of \emph{L. monocytogenes} in packs of RTE diced cantaloupe at the end of transportation,
#'               from contaminated lots;}
#'         \item{P}{Prevalence of RTE diced cantaloupe lots contaminated with \emph{L. monocytogenes} (scalar);}
#'         \item{lnQt}{Natural log of the `Q` parameter at the end of transportation (matrix);}
#'         \item{lotEGR5}{(\eqn{h^{-1}}) Growth rate of \emph{L. monocytogenes} in cantaloupe flesh specific to every lot (vector);}
#'         \item{unitSize}{(`g`) Weight of a pack of cantaloupe dices.}
#'              }
#'
#' @author Ursula Gonzales-Barron \email{ubarron@ipb.pt}
#'
#' @keywords stochastic growth lag-phase logistics Baranyi
#'
#' @references
#'
#' \insertRef{mc2d}{qraLm}
#'
#' \insertRef{stats}{qraLm}
#'
#' @importFrom mc2d rpert
#' @importFrom stats rgamma
#'
#' @export
#'
#' @note
#' The parameter \eqn{Tmin=-2.0196\ ^\circ C} was determined from fitting a square-root model to data extracted from multiple sources
#' (refer to the function [caGrowthBaranyi()]). Parameters for the distributions of the transportation conditions
#' should be defined by the user and/or tested in scenarios.
#' In addition to the final \emph{L. monocytogenes} numbers, the function also returns the values of `lotEGR5` and `lnQt`
#' so that the \emph{L. monocytogenes} growth could be followed up in subsequent logistics stages.
#'
#' @examples
#' tempMin <- 3
#' tempMode <- 5
#' tempMax <- 12
#' Tmin <- -2.0196
#' nLots <- 1000
#' sizeLot <- 250
#' N <- matrix(230, nLots, sizeLot)
#' N[5, ] <- 0
#' lnQt <- matrix(2.3, nLots, sizeLot)
#' lnQt[5, ] <- 0
#' dat <- list(
#'   N = N, lnQt = lnQt,
#'   lotEGR5 = extraDistr::rtnorm(nLots, 0.03557288, 0.004, a = 0),
#'   P = 0.4, unitSize = 200
#' )
#'
#' ArriveHome <- caRet2HomeRTE(dat,
#'   MPD = 8.5,
#'   Tmin - 2.0196,
#'   tempMin = tempMin,
#'   tempMode = tempMode,
#'   tempMax = tempMax,
#'   timeShape = 6.2,
#'   timeScale = 8.2
#' )
#' hist(ArriveHome$N)
#'
caRet2HomeRTE <- function(data = list(),
                          MPD = NULL,
                          Tmin = -2.0196,
                          tempMin,
                          tempMode,
                          tempMax,
                          timeShape,
                          timeScale) {
  # To evaluate the growth during retail
  N_t <- data$N
  # Get the dimensions
  Number_packs <- ncol(N_t)
  old_nLots <- nrow(N_t)

  if (missing(MPD)) MPD <- data$MPD # test if MPD was defined
  if (is.null(MPD)) warning("Add 'MPD=#' to function arguments") # test again if MPD is defined

  if (!all(c(
    length(MPD), length(data$lotEGR5), length(Tmin),
    length(tempMin), length(tempMode), length(tempMax), length(timeShape),
    length(timeScale), length(data$lnQt)
  )
  %in% c(1, old_nLots, old_nLots * Number_packs))) {
    stop("unconformable size of parameters")
  }

  # sampling values for every batch arranging into vectors
  Temp_v <- mc2d::rpert(old_nLots * Number_packs, min = tempMin, mode = tempMode, max = tempMax, shape = 4)
  time_v <- stats::rgamma(old_nLots * Number_packs, timeShape, timeScale)
  Q0_v <- exp(data$lnQt)

  # Calculates final counts N (`CFU`) in contaminated packs and places zero in non-contaminated packs

  results <- caGrowthBaranyi(data,
    MPD = data$MPD,
    unitSize = data$unitSize,
    EGR5 = data$lotEGR5,
    Tmin = data$Tmin,
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
