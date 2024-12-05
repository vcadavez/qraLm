#' @title Growth of \emph{L. monocytogenes} in cantaloupe flesh
#'
#' @description
#' The function [caGrowthBaranyi()] estimates the numbers of \emph{L. monocytogenes} in cantaloupe flesh (\emph{i.e.}, dices, slices)
#' kept at a constant temperature `Temp` at the end of the logistics stage, `time`. The growth rate at a given temperature
#' is calculated from a square-root type model defined by the minimum temperature for growth (`Tmin`) and the exponential growth rate
#' at 5 \eqn{^{\circ}C} (`EGR5`). The primary growth model with lag phase of \insertCite{Baranyi1994;textual}{qraLm} is used to
#' estimate the numbers of \emph{L. monocytogenes} at `time`. The value of the physiological state of cells,
#' `q0`, must be provided. The return of the natural logarithm of `Q`
#' at `time` enables the reuse of this function for multiple transport and storage stages.
#'
#' @param data A list of four elements see [caPrimaryProduction()] function.
#' @param Temp (\eqn{^{\circ}C}) Temperature  of cantaloupe flesh at the logistic stage.
#' @param time (`h`) Time of the logistic stage.
#' @param Q0 Initial parameter `Q` of the Baranyi and Roberts' model related to lag phase.
#' @param MPD (\eqn{log_{10}\ CFU/g}) Maximum population density of \emph{L. monocytogenes} in cantaloupe flesh.
#' @param unitSize (`g`) Weight of a pack of cantaloupe dices.
#' @param EGR5 (\eqn{h^{-1}}) Growth rate of \emph{L. monocytogenes} in cantaloupe flesh at 5 \eqn{^{\circ}C}.
#' @param Tmin (\eqn{^{\circ}C}) Nominal minimum temperature for growth of \emph{L. monocytogenes}in cantaloupe flesh.
#'
#' @return A list of two elements:
#'     \describe{
#'              \item{`Nt`}{Numbers (`CFU`) of \emph{L. monocytogenes} in cantaloupe flesh at the end of the stage (scalar, vector or matrix);}
#'              \item{`lnQt`}{natural logarithm of the lag-phase parameter `Q` at the end of the stage (scalar, vector or matrix).}
#'             }
#'
#' @author Ursula Gonzales-Barron \email{ubarron@ipb.pt}
#'
#' @keywords RTE melon dices slices pre-cut Baranyi
#'
#' @references
#'
#' \insertRef{stats}{qraLm}
#'
#' \insertRef{Danyluk2014}{qraLm}
#'
#' \insertRef{Fang2013}{qraLm}
#'
#' \insertRef{Farber1998}{qraLm}
#'
#' \insertRef{Guzel2017}{qraLm}
#'
#' \insertRef{Hong2014}{qraLm}
#'
#' \insertRef{Moreira2019}{qraLm}
#'
#' \insertRef{Patil2017}{qraLm}
#'
#' \insertRef{Ukuku2002}{qraLm}
#'
#' @export
#'
#' @note The parameters `Tmin` and `EGR5` were obtained by fitting a square-root model to growth data
#' of \emph{L. monocytogenes} in cantaloupe flesh at temperature below 30 \eqn{^\circ C}, extracted
#' from \insertCite{Danyluk2014;textual}{qraLm}, \insertCite{Fang2013;textual}{qraLm},
#' \insertCite{Farber1998;textual}{qraLm}, \insertCite{Guzel2017;textual}{qraLm}, \insertCite{Hong2014;textual}{qraLm},
#' \insertCite{Moreira2019;textual}{qraLm}, \insertCite{Patil2017;textual}{qraLm} and \insertCite{Ukuku2012;textual}{qraLm}.
#' \eqn{Tmin = -2.0196\ ^\circ C} (\eqn{standard\ error = 0.576\ ^\circ C}) and \eqn{EGR5 = 0.035573\ h^{-1}}
#' (\eqn{standard\ error = 0.004\ h^{-1}}). A zero is returned in the output elements when \eqn{N=0}.
#'
#' @examples
#'
#' # Cells with a low Q0
#' dat <- Lot2LotGen(
#'   nLots = 50,
#'   sizeLot = 100,
#'   unitSize = 500,
#'   betaAlpha = 0.5112,
#'   betaBeta = 9.959,
#'   C0MeanLog = 1.023,
#'   C0SdLog = 0.3267,
#'   propVarInter = 0.7
#' )
#' Low <- caGrowthBaranyi(dat,
#'   Temp = 12,
#'   MPD = 8,
#'   unitSize = 250,
#'   EGR5 = 0.035573,
#'   Tmin = -2.0196,
#'   Q0 = 0.01,
#'   time = 5
#' )
#' hist(Low$Nt)
#' # Cells with a high Q0
#' High <- caGrowthBaranyi(dat,
#'   Temp = 12,
#'   MPD = 8,
#'   unitSize = 250,
#'   EGR5 = 0.035573,
#'   Tmin = -2.0196,
#'   Q0 = 3,
#'   time = 5
#' )
#' hist(High$Nt)
########################################################################
caGrowthBaranyi <- function(data = list(),
                            Temp,
                            MPD,
                            unitSize = NULL,
                            EGR5,
                            Tmin,
                            Q0,
                            time) {
  # "caGrowthBaranyi" returns N(t) and ln_Q(t) at time=t
  N0 <- data$N

  if (missing(unitSize)) unitSize <- data$unitSize # test if unitSize was defined
  if (is.null(unitSize)) warning("Add 'unitSize=#' to function arguments") # test again if unitSize is defined

  mu <- ifelse(Temp < Tmin,
    0,
    EGR5 * ((Temp - Tmin) / (5 - Tmin))^2
  )
  A <- time + 1 / mu * log((exp(-mu * time) + Q0) / (1 + Q0))
  log_Nt <- log(N0) + mu * A - log(1 + (exp(mu * A) - 1) / exp(log(10^MPD * unitSize) - log(N0)))
  Nt <- exp(log_Nt) # output concentration in CFU at time t
  # Need to set as the same length as N0 for all the logistics steps
  lnQt <- rep(mu * time + log(Q0), length.out = length(N0)) # output lnQ(t) at time t

  Nt[N0 == 0] <- 0
  lnQt[N0 == 0] <- 0
  data$Nt <- Nt
  data$lnQt <- lnQt
  return(data)
  #  return (list(Nt=Nt,lnQt=lnQt))
}
