#' @title Storage of RTE diced cantaloupe packs at home
#'
#' @description
#' The function [caHomeRTE()] simulates the growth of \emph{L. monocytogenes} in RTE diced cantaloupe at home,
#' and is based on the function  [caGrowthBaranyi()]. The algorithm samples home storage time and temperature
#' at the unit level since they depend on the consumer. The input `data` provides the algorithm with the lot-specific values of `EGR5`
#' and the unit-specific values of `lnQt` obtained from the previous logistics stage. Pert distributions represent the variability
#' in home storage time and temperature.
#'
#' @param data a list of:
#'     \describe{
#'               \item{N}{(`CFU`) A matrix containing the numbers of \emph{L. monocytogenes} in packs of RTE diced cantaloupe at the end of
#'                transport to retail, from contaminated lots;}
#'               \item{P}{Prevalence of contaminated lots (scalar);}
#'               \item{lnQt}{Natural log of the `Q` parameter at the end of transport to retail (matrix);}
#'               \item{lotEGR5}{(\eqn{h^{-1}}) Growth rate of \emph{L. monocytogenes} in cantaloupe flesh specific to every lot (vector);}
#'               \item{unitSize}{(`g`) Weight of a pack of cantaloupe dices.}
#'              }
#' @param tempMin (\eqn{^\circ C}) Minimum retail temperature (scalar) (suggested \eqn{value=3.10\ ^\circ C}) (scalar).
#' @param tempMode (\eqn{^\circ C}) Mode of the retail temperature (scalar) (suggested \eqn{value=6.64\ ^\circ C}) (scalar).
#' @param tempMax (\eqn{^\circ} C) Maximum retail temperature (scalar) (suggested \eqn{value=11.3\ ^\circ C}) (scalar).
#' @param timeMin (`h`) Minimum retail time (scalar).
#' @param timeMode (`h`) Mode of the retail time (scalar).
#' @param timeMax (`h`) Maximum retail time (scalar).
#' @param MPD (log10 CFU/g) Maximum population density of \emph{L. monocytogenes} in cantaloupe flesh (scalar).
#' @param Tmin (\eqn{^\circ C}) Nominal minimum temperature for growth of \emph{L. monocytogenes} in
#'    cantaloupe flesh (suggested \eqn{value=-2.0196\ ^\circ C}) (scalar).
#'
#' @return A list of two elements:
#'     \describe{
#'              \item{N}{(`CFU`) A matrix containing the numbers of \emph{L. monocytogenes} in packs of RTE diced
#'              cantaloupe at the point of consumption, from contaminated lots;}
#'              \item{P}{Prevalence of RTE diced cantaloupe lots contaminated with \emph{L. monocytogenes} (scalar).}
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
#' \insertRef{Carrasco2010}{qraLm}
#'
#' \insertRef{Ding2013}{qraLm}
#'
#' \insertRef{Nauta2003}{qraLm}
#'
#' @importFrom mc2d rpert
#'
#' @export

#' @note
#' The suggested parameters for the Pert distribution of retail temperature are taken from \insertCite{Carrasco2010;textual}{qraLm},
#' \insertCite{Ding2013;textual}{qraLm} and \insertCite{Nauta2003;textual}{qraLm}: \eqn{tempMin=3.10\ ^\circ C}, \eqn{Temp\_mode=6.64\ ^\circ C}
#' and \eqn{Temp\_max=11.3\ ^\circ C}.
#' The parameter \eqn{Tmin=-2.0196\ ^\circ C} was determined from fitting a square-root model to data extracted from multiple sources
#' (refer to the function [caGrowthBaranyi()]).
#' Parameters for the home storage time distribution should be defined by the user and/or tested in scenarios.
#'
#' @examples
#' timeMin <- 0.25
#' timeMode <- 3
#' timeMax <- 24
#' nLots <- 1000
#' sizeLot <- 250
#' N <- matrix(320, nLots, sizeLot)
#' N[2, ] <- 0
#' lnQt <- matrix(3.5, nLots, sizeLot)
#' lnQt[2, ] <- 0
#' data <- list(
#'   N = N, lnQt = lnQt,
#'   lotEGR5 = extraDistr::rtnorm(nLots, 0.03557288, 0.004, a = 0),
#'   P = 0.4, unitSize = 200
#' )
#' AtConsumption <- caHomeRTE(data,
#'   MPD = 8.5, Tmin = -2.0196,
#'   tempMin = 3.1, tempMode = 6.64, tempMax = 11.3,
#'   timeMin, timeMode, timeMax
#' )
#' hist(AtConsumption$N)
caHomeRTE <- function(data = list(),
                      MPD = NULL,
                      Tmin = -2.0196,
                      tempMin,
                      tempMode,
                      tempMax,
                      timeMin,
                      timeMode,
                      timeMax) {
  # To evaluate the growth at home
  N_t <- data$N
  # Get the dimensions
  Number_packs <- ncol(N_t)
  old_nLots <- nrow(N_t)

  if (missing(MPD)) MPD <- data$MPD # test if MPD was defined
  if (is.null(MPD)) warning("Add 'MPD=#' to function arguments") # test again if MPD is defined

  if (!all(c(
    length(MPD), length(data$lotEGR5), length(Tmin),
    length(tempMin), length(tempMode), length(tempMax), length(timeMin),
    length(timeMode), length(timeMax), length(data$lnQt)
  )
  %in% c(1, old_nLots, old_nLots * Number_packs))) {
    stop("unconfortable size of parameters")
  }

  # sampling values for every batch arranging into vectors
  Temp_v <- mc2d::rpert(old_nLots * Number_packs, min = tempMin, mode = tempMode, max = tempMax, shape = 4)
  time_v <- mc2d::rpert(old_nLots * Number_packs, min = timeMin, mode = timeMode, max = timeMax, shape = 4)
  Q0_v <- exp(data$lnQt)

  # Calculates final counts N (`CFU`) in contaminated packs and places zero in non-contaminated packs
  # if(missing(nLots)) nLots <- nrow( data$N) #test if nLots was defined
  # if(is.null(nLots)) warning("Add 'nLots=#' to function arguments") #test again if nLots is defined
  #
  # if(missing(sizeLot)) sizeLot <- ncol( data$N) #test if sizeLot was defined
  # if(is.null(sizeLot)) warning("Add 'sizeLot=#' to function arguments") #test again if sizeLot is defined
  #
  # if(missing(unitSize)) unitSize <- data$unitSize #test if unitSize was defined
  # if(is.null(unitSize)) warning("Add 'unitSize=#' to function arguments") #test again if unitSize is defined

  results <- caGrowthBaranyi( # N0 = N_t,
    data,
    MPD = MPD,
    unitSize = data$unitSize,
    EGR5 = data$lotEGR5,
    Tmin = Tmin,
    Temp = Temp_v,
    time = time_v,
    Q0 = Q0_v
  )

  Nt_matrix <- ceiling(matrix(results$Nt, ncol = Number_packs, nrow = old_nLots))

  # converting the matrix containing zeros to a non-zero matrix
  # calculating prevalence of contaminated units P_units
  # Pi_0_index <- Nt_matrix == 0
  # Pi_0 <- sum(Pi_0_index)/(Number_packs*old_nLots)
  # P_units <- data$P*(1-Pi_0)
  # #resampling to remove zeros
  # Nt_matrix[Pi_0_index] <- Nt_matrix[sample(which(!Pi_0_index),
  #                                           sum(Pi_0_index), replace=TRUE)]
  N <- Nt_matrix
  lotMeans <- rowMeans(N / data$unitSize, na.rm = TRUE)
  unitsCounts <- c(N / data$unitSize)
  data$lotMeans <- lotMeans
  data$unitsCounts <- unitsCounts
  data$N <- N
  data$lnQt <- data$lotEGR5 <- NULL #  not needed anymore
  return(data)
}
