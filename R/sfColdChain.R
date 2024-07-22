#' @title Cold chain logistics for ready-to-eat seafood
#'
#' @description
#' The function [sfColdChain()] simulates the growth of \emph{L. monocytogenes}, as affected by lactic acid bacteria (LAB) populations,
#' in RTE seafood during the various stages of the cold chain logistics, including transportation to retail, display at retail and transportation
#' to home; and relies on the growth kinetic functions sfMejlholmDalgaard, [sfMejlholmDalgaardLAB()] and [sfGrowthJameson()].
#' The function [sfColdChain()] can be used for home storage as well, by changing only the parameters defining the time and
#' temperature Pert distributions. For every lot, the algorithm samples time and temperature from Pert distributions defined by the
#' parameters `timeMin`, `timeMode`, `timeMax`, and `tempMin`, `tempMode`, `tempMax`, respectively, provided by the user.
#' The specific growth rates for \emph{L. monocytogenes} and LAB are determined evaluating the intrinsic and extrinsic characteristics
#' of the seafood product (sampled from the function [sfCharacteristics()]) in the Mejholm and Daalgard's existing secondary
#' models for both bacterial groups (\insertCite{Mejlholm2009}{qraLm}, \insertCite{Mejlholm2010}{qraLm}, \insertCite{Mejlholm2013}{qraLm}
#' and \insertCite{Mejlholm2015}{qraLm}). The parameters of the two growth rate models related to reference growth rate, temperature,
#' water activity, pH and concentrations of phenol, nitrites, CO2 at equilibrium, undissociated lactic acid, undissociated diacetate,
#' undissociated acetic acid, undissociated benzoic acid, undissociated citric and undissociated sorbic acid, for \emph{L. monocytogenes}
#' and lactic acid bacteria, are set by default to those fitted and validated by \insertCite{Mejlholm2010;textual}{qraLm} and
#'  \insertCite{Mejlholm2013;textual}{qraLm}, respectively.
#' However, any of the parameters of the growth rate models can be altered or updated. The numbers of \emph{L. monocytogenes} in the
#' RTE seafood product, as affected by LAB, are then estimated applying the extended Jameson-effect competition model published
#' in \insertCite{Gimenez2004;textual}{qraLm} using the interaction `gamma` parameter proposed by \insertCite{Moller2013;textual}{qraLm}.
#'
#' @param data a list of:
#'     \describe{
#'        \item{`N`}{(`CFU`) A matrix containing the numbers of \emph{L. monocytogenes} in packs of RTE seafood at the end of transport to retail, from contaminated lots.}
#'        \item{`P`}{Prevalence of contaminated lots (scalar).}
#'        \item{`N0LAB`}{(`CFU`) Optional: A matrix containing the numbers of LAB in packs of RTE seafood at the end of transport to retail, from contaminated lots. If missing, it will be generated from default parameters.}
#'        \item{`lnQ0Lm`}{Optional: A matrix containing the natural logarithm of q0 for \emph{L. monocytogenes} in packs of RTE seafood. If missing, it will be generated from default parameters.}
#'        \item{`lnQ0LAB`}{Optional: A matrix containing the natural logarithm of q0 for LAB in packs of RTE seafood. If missing, it will be generated from default parameters.}
#'        \item{`MPDLm`}{(`log10 CFU/g`) Optional: A matrix containing the maximum population density of \emph{L. monocytogenes} in packs of RTE seafood. If missing, it will be generated from default parameters.}
#'        \item{`MPDLAB`}{(`log10 CFU/g`) Optional: A matrix containing the log10 of MPD for LAB in packs of RTE seafood. If missing, it will be generated from default parameters.}
#'          }
#' @param corTimeTemp define 
#' @param nLots Number of lots sampled or size of the Monte Carlo simulation (scalar).
#' @param sizeLot Number of units or portions produced in a lot (scalar).
#' @param tempMin (\eqn{^\circ C}) Minimum storage temperature of the RTE seafood product (scalar). Will be used in a Pert distribution.
#' @param tempMode (\eqn{^\circ C}) Mode of storage temperature of the RTE seafood product (scalar).
#' @param tempMax (\eqn{^\circ C}) Maximum storage temperature of the RTE seafood product (scalar).
#' @param timeMin (`h`) Minimum storage time of the RTE seafood product (scalar). Will be used in a Pert distribution.
#' @param timeMode (`h`) Mode of storage time of the RTE seafood product (scalar).
#' @param timeMax (`h`) Maximum storage time of the RTE seafood product (scalar).
#'
#' @param variability level of variability for time and temperature: "lot", "column" or "portion".
#'
#' @param RTE Intrinsic and extrinsic characteristics of the RTE seafood product, provided using the \code{\link{sfCharacteristics}} function
#' @param unitSize (`g`) Weight of the contents of the RTE seafood product

#' @param N0LABmin (`log10 CFU`) Minimum value of the Pert distribution representing variability in the numbers of LAB at `time=0` (scalar). Will be then used as a power of 10.
#' @param N0LABmode (`log10 CFU`) Mode value of the Pert distribution representing variability in the numbers of LAB at `time=0` (scalar).
#' @param N0LABmax (`log10 CFU`) Maximum value of the Pert distribution representing variability in the numbers of LAB at `time=0` (scalar).
#' @param intralotSdN0LAB (`log10 CFU`) Intra-lot standard deviation to produce variability in the numbers of LAB (scalar).
#' @param lnQ0LABmin Minimum value of the Pert distribution representing variability in the natural logarithm of the parameter `q0` for LAB (scalar).
#' @param lnQ0LABmode Mode value of the Pert distribution representing variability in the natural logarithm of the parameter `q0` for LAB (scalar).
#' @param lnQ0LABmax Maximum value of the Pert distribution representing variability in the natural logarithm of the parameter `q0` for LAB (scalar).
#'
#' @param MPDLABmin (`log10 CFU/g`) Minimum value of the Pert distribution representing the variability in the maximum population density of LAB (scalar).
#' @param MPDLABmode (`log10 CFU/g`) Mode value of the Pert distribution representing the variability in the maximum population density of LAB (scalar).
#' @param MPDLABmax (`log10 CFU/g`) Maximum value of the Pert distribution representing the variability in the maximum population density of LAB (scalar).

#' @param MPDLmmin (`log10 CFU/g`) Minimum value of the Pert distribution representing the variability in the maximum population density of \emph{L. monocytogenes} (scalar).
#' @param MPDLmmode (`log10 CFU/g`) Mode value of the Pert distribution representing the variability in the maximum population density of \emph{L. monocytogenes} (scalar).
#' @param MPDLmmax (`log10 CFU/g`) Maximum value of the Pert distribution representing the variability in the maximum population density of \emph{L. monocytogenes} (scalar).

#' @param mumaxrefLm (`1/h`) Maximum growth rate of \emph{L. monocytogenes} at the reference temperature \code{TrefLm} (scalar).
#' @param TminLm (\eqn{^\circ C}) Minimum temperature for growth of \emph{L. monocytogenes} (scalar).
#' @param TrefLm (\eqn{^\circ C}) Reference temperature for \emph{L. monocytogenes} (scalar).
#' @param awminLm  Minimum water activity for growth of \emph{L. monocytogenes} (scalar).
#' @param pHminLm  Minimum pH for growth of \emph{L. monocytogenes} (scalar).
#' @param pheMaxLm  (`ppm`) Minimum inhibitory concentration of phenol for \emph{L. monocytogenes} (scalar).
#' @param NITmaxLm (`ppm`) Minimum inhibitory concentration of nitrites for \emph{L. monocytogenes} (scalar).
#' @param CO2maxLm (`ppm`) Minimum inhibitory concentration of CO2 for \emph{L. monocytogenes} (scalar).
#' @param micLACuLm (`mM`) Minimum inhibitory concentration of undissociated lactic acid for \emph{L. monocytogenes} (scalar).
#' @param micDACuLm (`mM`) Minimum inhibitory concentration of undissociated diacetate for \emph{L. monocytogenes} (scalar).
#' @param micAACuLm (`mM`) Minimum inhibitory concentration of undissociated acetic acid for \emph{L. monocytogenes} (scalar).
#' @param micBACuLm (`mM`) Minimum inhibitory concentration of undissociated benzoic acid for \emph{L. monocytogenes} (scalar).
#' @param micCACuLm (`mM`) Minimum inhibitory concentration of undissociated citric acid for \emph{L. monocytogenes} (scalar).
#' @param micSACuLm (`mM`) Minimum inhibitory concentration of undissociated sorbic acid for \emph{L. monocytogenes} (scalar).
#'
#' @param mumaxrefLAB (`1/h`) Maximum growth rate of LAB at the reference temperature \code{TrefLAB} (scalar).
#' @param TminLAB (\eqn{^\circ C}) Minimum temperature for growth of LAB (scalar).
#' @param TrefLAB (\eqn{^\circ C}) Reference temperature for LAB (scalar).
#' @param awminLAB Minimum water activity for growth of LAB (scalar).
#' @param pHminLAB Minimum pH for growth of LAB (scalar).
#' @param pheMaxLAB (`ppm`) Minimum inhibitory concentration of phenol for LAB (scalar).
#' @param NITmaxLAB (`ppm`) Minimum inhibitory concentration of nitrites for LAB (scalar).
#' @param CO2maxLAB (`ppm`) Minimum inhibitory concentration of CO2 for LAB (scalar).
#' @param micLACuLAB (`Mm`) Minimum inhibitory concentration of undissociated lactic acid for LAB (scalar).
#' @param micDACuLAB (`Mm`) Minimum inhibitory concentration of undissociated diacetate for LAB (scalar).
#' @param micAACuLAB (`Mm`) Minimum inhibitory concentration of undissociated acetic acid for LAB (scalar).
#' @param micBACuLAB (`Mm`) Minimum inhibitory concentration of undissociated benzoic acid for LAB (scalar).
#' @param micCACuLAB (`Mm`) Minimum inhibitory concentration of undissociated citric acid for LAB (scalar).
#' @param micSACuLAB (`Mm`) Minimum inhibitory concentration of undissociated sorbic acid for LAB (scalar).
#
#' @param gamma Gamma parameter for the Jameson effect function. If 1, converges to classical Jameson effect (scalar).
#' @param lim (`log10 CFU/g`) Limit to consider interactions in the Jameson effect (scalar).
#' @param step (`h`) Integration step (scalar).
#'
#' @return A list of six elements:
#'     \describe{
#'        \item{`N`}{(`CFU`) A matrix containing the numbers of \emph{L. monocytogenes} in packs of RTE seafood after storage,
#'           from contaminated lots;}
#'        \item{`P`}{Prevalence of RTE seafood lots contaminated with \emph{L. monocytogenes} (scalar);}
#'        \item{`N0LAB`}{(`CFU`) A matrix containing the numbers of LAB in packs of RTE seafood;}
#'        \item{`lnQ0Lm`}{A matrix containing the natural logarithm of `q0` of \emph{L. monocytogenes} in packs of RTE seafood;}
#'        \item{`lnQ0LAB`}{A matrix containing the natural logarithm of `q0` of LAB in packs of RTE seafood;}
#'        \item{`SF`}{An object containing the intrinsic and extrinsic characteristics of the RTE seafood product.}
#'              }
#'
#' @author Regis Pouillot \email{rpouillot.work@gmail.com}
#'
#' @keywords logistics stochastic growth lag-phase Baranyi competition
#'
#' @references
#' \insertRef{Endrikat2010}{qraLm}
#' \insertRef{extraDistr}{qraLm}
#' \insertRef{FDAFSIS2003}{qraLm}
#' \insertRef{Gimenez2004}{qraLm}
#' \insertRef{mc2d}{qraLm}
#' \insertRef{Mejlholm2009}{qraLm}
#' \insertRef{Mejlholm2010}{qraLm}
#' \insertRef{Mejlholm2013}{qraLm}
#' \insertRef{Mejlholm2015}{qraLm}
#' \insertRef{Moller2013}{qraLm}
#' \insertRef{Pouillot2007}{qraLm}
#' \insertRef{stats}{qraLm}
#'
#' @importFrom stats qnorm
#' @importFrom extraDistr rtnorm
#' @importFrom mc2d rpert
#'
#' @export

#' @note
#' This function can be used multiple times. The microbial kinetic parameters `MPD` and `ln_q0` for \emph{L. monocytogenes} and
#' LAB are sampled only once. Information for building the variability distributions about retail time and temperature was obtained from
#' \insertCite{Pouillot2007;textual}{qraLm} and \insertCite{FDAFSIS2003;textual}{qraLm}.
#' For Home storage, the Pert distribution for temperature can be defined using the following parameters: `tempMin = qnorm(0.025, 7, 3)`,
#' `tempMode = 7`, and `tempMax = qnorm(0.975, 7, 3)`, which are reformulated from the `normal(7.0, 3.0)` used in
#'  \insertCite{Pouillot2007;textual}{qraLm}.
#' The Pert distribution for home storage time can be defined using the following parameters:
#' `timeMin = qweibull(0.025,shape = 1.14, scale = 18.39) * 24`
#' `timeMode = 18.39*((1.14-1)/1.14)^(1/1.14) * 24`
#' `timeMax = qweibull(0.975,shape = 1.14, scale = 18.39) * 24`
#' which are reformulated from the `Weibull(1.14, 18.39)` days used in \insertCite{Endrikat2010;textual}{qraLm}.
#'
#' @examples
#' 
#' sizeLot <- 100
#' size_mc <- 100
#' N <- matrix(round(10^rnorm(size_mc * sizeLot, 0, 3)),
#'             ncol = sizeLot,
#'             nrow = size_mc
#'             )
#'
#' dat <- list(N = N, unitSize = 500)
#' RTE <- sfCharacteristics(size_mc)
#' str(RTE)
#' dat1 <- sfColdChain(dat,
#'                     RTE = RTE
#'                     )
#' str(dat1)
#'
#'
sfColdChain <- function(data = list(),
                        RTE, # RTE characteristics
                        nLots = NULL,
                        sizeLot = NULL,
                        unitSize = NULL,
                        # time temperature profile
                        tempMin = 0.288, # From Pouillot et al, 2007
                        tempMode = 4.60,
                        tempMax = 8.912,
                        timeMin = 12, # From FDA/FSIS 2003
                        timeMode = 144,
                        timeMax = 360,
                        variability = "lot",
                        corTimeTemp = 0,
                        # LAB characteristics
                        N0LABmin = 1.5,
                        N0LABmode = 2.78,
                        N0LABmax = 4.1,
                        intralotSdN0LAB = 0,
                        lnQ0LABmin = -12, # from Lm. To be changed
                        lnQ0LABmode = -2.73,
                        lnQ0LABmax = 1.26,
                        MPDLABmin = 8.0, # To be changed
                        MPDLABmode = 8.5,
                        MPDLABmax = 9.0,
                        # LM characteristics
                        MPDLmmin = 6.6,
                        MPDLmmode = 7.36,
                        MPDLmmax = 8.2,
                        # Other parameters (with defaults)
                        mumaxrefLm = 0.419,
                        TminLm = -2.83,
                        TrefLm = 25,
                        awminLm = 0.923,
                        pHminLm = 4.97,
                        pheMaxLm = 32,
                        NITmaxLm = 350,
                        CO2maxLm = 3140,
                        micLACuLm = 3.79,
                        micDACuLm = 4.8,
                        micAACuLm = 10.3,
                        micBACuLm = 0.349,
                        micCACuLm = 2.119,
                        micSACuLm = 1.896,
                        mumaxrefLAB = 0.583,
                        TminLAB = -5.25,
                        TrefLAB = 25,
                        awminLAB = 0.928,
                        pHminLAB = 4.24,
                        pheMaxLAB = 40.3,
                        NITmaxLAB = 2780,
                        CO2maxLAB = 6691,
                        micLACuLAB = 12.0,
                        micDACuLAB = 33.3,
                        micAACuLAB = 10.3,
                        micBACuLAB = 1.51,
                        micCACuLAB = 10.3,
                        micSACuLAB = 12.6,
                        gamma = 1,
                        lim = 1,
                        step = 1) {
  if (missing(nLots)) nLots <- nrow(data$N) # test if nLots was defined
  if (is.null(nLots)) warning("Add 'nLots=#' to function arguments") # test again if nLots is defined

  if (missing(sizeLot)) sizeLot <- ncol(data$N) # test if sizeLot was defined
  if (is.null(sizeLot)) warning("Add 'sizeLot=#' to function arguments") # test again if sizeLot is defined

  if (missing(unitSize)) unitSize <- data$unitSize # test if unitSize was defined
  if (is.null(unitSize)) warning("Add 'unitSize=#' to function arguments") # test again if unitSize is defined

  #  nLots <- nrow(data$N)
  #  sizeLot <- ncol(data$N)

  # If bacteria characteristics are not provided, draw the distribution.
  # Else: keep the current values
  if (is.null(data$lnQ0Lm)) {
    # From Couvert et al, 2010
    # Please check
    h0 <- extraDistr::rtnorm(nLots, mean = 2.8, sd = 4.6, a = 0)
    data$lnQ0Lm <- log(1 / (exp(h0) - 1)) # was pmin(1/(exp(h0)-1), 5)
  }
  # MPDLm
  if (is.null(data$MPDLm)) {
    data$MPDLm <- mc2d::rpert(n = nLots, min = MPDLmmin, mode = MPDLmmode, max = MPDLmmax)
  }
  # MPDLAB
  if (is.null(data$MPDLAB)) {
    data$MPDLAB <- mc2d::rpert(n = nLots, min = MPDLABmin, mode = MPDLABmode, max = MPDLABmax)
  }
  # lnQ0LAB
  if (is.null(data$lnQ0LAB)) {
    data$lnQ0LAB <- mc2d::rpert(nLots, min = lnQ0LABmin, mode = lnQ0LABmode, max = lnQ0LABmax)
  }
  # N0LAB per lot.
  if (is.null(data$N0LAB)) {
    meanN0LAB <- mc2d::rpert(n = nLots, min = N0LABmin, mode = N0LABmode, max = N0LABmax)
    # Include a bit of variability per portion
    data$N0LAB <- round(10^stats::rnorm(nLots * sizeLot, mean = meanN0LAB, sd = intralotSdN0LAB))
  }

  # Draw time temperature distribution (per lot)
  if (variability == "lot") {
    Temp <- mc2d::rpert(n = nLots, min = tempMin, mode = tempMode, max = tempMax)
    time <- mc2d::rpert(n = nLots, min = timeMin, mode = timeMode, max = timeMax)
  } else if (variability == "column") {
    Temp <- matrix(mc2d::rpert(n = sizeLot, min = tempMin, mode = tempMode, max = tempMax),
      nrow = nLots, ncol = sizeLot, byrow = TRUE
    )
    time <- matrix(mc2d::rpert(n = sizeLot, min = timeMin, mode = timeMode, max = timeMax),
      nrow = nLots, ncol = sizeLot, byrow = TRUE
    )
  } else if (variability == "portion") {
    Temp <- mc2d::rpert(n = nLots * sizeLot, min = tempMin, mode = tempMode, max = tempMax)
    time <- mc2d::rpert(n = nLots * sizeLot, min = timeMin, mode = timeMode, max = timeMax)
  }

  if (corTimeTemp != 0) {
    tT <- mc2d::cornode(cbind(c(time), c(Temp)), target = corTimeTemp)
    Temp <- tT[, 2]
  }

  # Get the muLm
  muLm <- sfMejlholmDalgaard(Temp,
    aw = RTE$aw,
    NaCl = RTE$NaCl,
    pH = RTE$pH,
    P = RTE$P,
    CO2equi = RTE$CO2equi,
    NIT = RTE$NIT,
    aaWph = RTE$aaWph,
    baWph = RTE$baWph,
    caWph = RTE$caWph,
    daWph = RTE$daWph,
    laWph = RTE$laWph,
    saWph = RTE$saWph,
    Tmin = TminLm,
    Tref = TrefLm,
    awmin = awminLm,
    pHmin = pHminLm,
    pheMax = pheMaxLm,
    NITmax = NITmaxLm,
    CO2max = CO2maxLm,
    micLACu = micLACuLm,
    micDACu = micDACuLm,
    micAACu = micAACuLm,
    micBACu = micBACuLm,
    micCACu = micCACuLm,
    micSACu = micSACuLm,
    mumaxref = mumaxrefLm
  )

  # Get the muLAB
  muLAB <- sfMejlholmDalgaardLAB(Temp,
    aw   = RTE$aw,
    NaCl = RTE$NaCl,
    pH   = RTE$pH,
    P    = RTE$P,
    CO2equi = RTE$CO2equi,
    NIT      = RTE$NIT,
    aaWph    = RTE$aaWph,
    baWph    = RTE$baWph,
    caWph    = RTE$caWph,
    daWph    = RTE$daWph,
    laWph    = RTE$laWph,
    saWph    = RTE$saWph,
    Tmin     = TminLAB,
    Tref     = TrefLAB,
    awmin    = awminLAB,
    pHmin    = pHminLAB,
    pheMax   = pheMaxLAB,
    NITmax   = NITmaxLAB,
    CO2max   = CO2maxLAB,
    micLACu  = micLACuLAB,
    micDACu  = micDACuLAB,
    micAACu  = micAACuLAB,
    micBACu  = micBACuLAB,
    micCACu  = micCACuLAB,
    micSACu  = micSACuLAB,
    mumaxref = mumaxrefLAB
  )

  # Growth
  Growth <- sfGrowthJameson(time,
    unitSize = unitSize,
    N0Lm = data$N,
    N0LAB = data$N0LAB,
    q0Lm = exp(data$lnQ0Lm),
    q0LAB = exp(data$lnQ0LAB),
    muLm = muLm,
    muLAB = muLAB,
    MPDLm = data$MPDLm,
    MPDLAB = data$MPDLAB,
    gamma = gamma,
    lim = lim,
    step = step
  )
# output
  N <- Growth$N1Lm
  
  lotMeans <- rowMeans(N / data$unitSize, na.rm = TRUE)
  unitsCounts <- c((data$ProbUnitPos/mean(data$ProbUnitPos)) * (N / data$unitSize))
  
  data$lotMeans <- lotMeans
  data$unitsCounts <- unitsCounts
  
  data$N <- N
  data$lnQ0Lm <- Growth$lnQtLm
  data$N0LAB <- Growth$N1LAB
  data$lnQ0LAB <- Growth$lnQtLAB
  return(data)
}
