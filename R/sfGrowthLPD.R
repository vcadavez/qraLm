#' @title Growth of \emph{L. monocytogenes} in raw fish

#' @description
#' The function [sfGrowthLPD()] simulates the growth of \emph{L. monocytogenes} in raw fish (salmon) considering lag phase duration.
#' Parameters of the secondary models predicting specific growth rate and lag phase are taken from \insertCite{Jia2020;textual}{qraLm}.
#' @param data See [Lot2LotGen()] function.
#' @param unitSize (g) Weight of a raw fish unit (whole or fillet).
#' @param MPD (log10 CFU/g) Maximum population density of \emph{L. monocytogenes} in raw fish, taken from \insertCite{Jia2020;textual}{qraLm}.
#' @param Temp (\eqn{^\circ C}) Temperature of raw fish fillet during holding time or storage.
#' @param time (h) Length of holding time or storage.
#' @param aLM Coefficient of the secondary model for growth rate, taken from \insertCite{Jia2020;textual}{qraLm}.
#' @param Tmin (\eqn{^\circ C}) Minimum temperature for growth of \emph{L. monocytogenes} in raw fish, taken from \insertCite{Jia2020;textual}{qraLm}.
#' @param ALM Coefficient of the model for lag phase duration in hours, taken from \insertCite{Jia2020;textual}{qraLm}.
#' @param mLM Exponent of the model for lag phase duration in hours, taken from \insertCite{Jia2020;textual}{qraLm}.
#' @param workDone Work done during the stages prior to the current storage stage. Should be set to zero if there was no previous
#' storage phase accounted for.
#' @return A list of two elements:
#'     \describe{
#'              \item{N}{(CFU) Numbers of \emph{L. monocytogenes} in fish fillet after storage;}
#'              \item{workDone}{Work done during the previous stages including the current storage stage.}
#' }
#'
#' @author Regis Pouillot \email{rpouillot.work@gmail.com} and Ursula Gonzales-Barron \email{ubarron@ipb.pt}
#'
#' @keywords salmon lag-phase holding-time transportation
#'
#' @references
#'
#' \insertRef{Daelman2013}{qraLm}
#'
#' \insertRef{Jia2020}{qraLm}
#'
#' @export
#'
#' @note The work-to-be-done concept from \insertCite{Daelman2013;textual}{qraLm} was implemented
#' to keep track of the lapse of the lag phase or the remaining lag phase during multiple storage stages.
#'
#' @examples
#' dat <- Lot2LotGen(
#'   nLots = 100,
#'   sizeLot = 100,
#'   unitSize = 500,
#'   betaAlpha = 0.5112,
#'   betaBeta = 9.959,
#'   C0MeanLog = 1.023,
#'   C0SdLog = 0.3267,
#'   propVarInter = 0.7
#' )
#' N <- matrix(1:12, ncol = 4, nrow = 3)
#' dat <- list(N = N, unitSize = 500)
#'
#' # Example of the first stage growth is taking place
#' aLM <- 0.0581 # parameter for mu (h-1)
#' Tmin <- 1.3 # parameter for mu (h-1)
#' ALM <- 0.84 # parameter for LPD (h)
#' mLM <- 1.11 # parameter for LPD (h)
#' MPD <- 9.2 # log10CFU/g # value taken from Jia et al. (2020)
#' time <- 11
#' Temp <- 2
#'
#' first <- sfGrowthLPD(dat,
#'   MPD = MPD,
#'   Temp = 15,
#'   time = 11
#' )
#'
#' # Example with previous stage where growth took place
#' second <- sfGrowthLPD(dat,
#'   MPD = MPD,
#'   Temp = 23,
#'   time = 11,
#'   workDone = first$workDone
#' )
#'
#' # On matrix
#' third <- sfGrowthLPD(dat,
#'   MPD = MPD,
#'   Temp = c(10, 25, 30),
#'   time = 11
#' )
########################################################################
sfGrowthLPD <- function(data = list(),
                        unitSize = NULL,
                        MPD=NULL,
                        Temp,
                        time,
                        aLM = 0.0581,
                        Tmin = 1.3,
                        ALM = 0.84,
                        mLM = 1.11,
                        workDone = 0) {
  if (missing(MPD)) MPD <- data$MPD # test if MPD was defined
  if (is.null(MPD)) warning("Add 'MPD=#' to function arguments") # test again if MPD is defined
  
  if (missing(unitSize)) unitSize <- data$unitSize # test if unitSize was defined
  if (is.null(unitSize)) warning("Add 'unitSize=#' to function arguments") # test again if unitSize is defined

  N0 <- data$N
  # Note: the whole function is vectorized for all parameters
  # mu and lambda for the present stage from Jia et al. (2020)
  mu <- ifelse(Temp < Tmin,
    0,
    ((aLM * (Temp - Tmin)^0.75)^2)
  ) # in h-1
  EGR <- mu / log(10) # in log10 CFU/h
  lambda <- exp(ALM) * mu^(-mLM) # in h

  # Dealing with the lag over multiple steps
  # See, e.g. Daelman et al. IJFM, 166 (2013) 433-499
  # Work to be done at this step
  wk <- mu * lambda
  # Work to be done corrected by the work already done
  # Note: if mu == 0, then lag == Inf and wk = NA. So need to change
  wprimk <- ifelse(mu != 0, wk - workDone, Inf)
  # update the work done for next step
  Dk <- time * mu
  workDone <- workDone + Dk

  # Effective lag is 0 if the work to be done at this step is already done
  # Otherwise it is the remaining lag, after considering the work already done
  effective_lag <- ifelse(wprimk < 0, 0, wprimk / mu)
  # Effective growth is time-effective_lag (with a minimum of 0)
  effective_time_growth <- pmax(time - effective_lag, 0)

  # Growth during time of growth
  N_out <- 10^(log10(N0) + EGR * (effective_time_growth))
  # Cap with MPD
  N_out <- pmin(N_out, (10^MPD) * unitSize)
  # if(is.array(N_out)) workDone <- matrix(workDone, ncol=ncol(N_out), nrow=nrow(N_out))
  
  # output
  N <- N_out
  lotMeans <- rowMeans(N / data$unitSize, na.rm = TRUE)
  unitsCounts <- c((data$ProbUnitPos/mean(data$ProbUnitPos)) * (N / data$unitSize))
  
  data$lotMeans <- lotMeans
  data$unitsCounts <- unitsCounts
  data$N <- N
  data$workDone <- workDone
  return(data)
}
