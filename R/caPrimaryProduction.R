#' @title Contamination of cantaloupe rind from soil and irrigation water
#'
#' @description
#' [caPrimaryProduction()] is a function that assesses the contamination of cantaloupes through soil and irrigation water.
#' It considers soil and water contamination characteristics such as prevalence (`pSoil` and `pIrrig`)
#' and distributions of \emph{L. monocytogenes} concentration in soil (`cSoilLogMin`, `cSoilLogMode` and `cSoilLogMax`
#' and water (`cIrrigLogMin` and `cIrrigLogMax`) as inputs.
#' `pSoil` and `pIrrig` have to be chosen by the user according to existing data of prevalence; whereas `pSoil` is conditional to several risk factors.
#' The identified risk factors (\insertCite{Background2022}{qraLm}) are:
#'  \enumerate{
#'    \item irrigation in the previous days before harvesting; and
#'    \item the use of organic fertilizer
#'    }
#' which affect `pSoil` as associated odds-ratios (`F_irrig_rain` and `fManure`). In this context, the user has to define the proportion of fields
#' practicing irrigation prior to harvest or undergoing rain (`p_irrig_rain`), and the proportion of fields using organic amendments (`pManure`).
#' Default values for `c_soil` and `c_irrig` have been proposed based on \insertCite{Background2022;textual}{qraLm}. `c_irrig` is
#' conditional to water sources contaminated with \emph{L. monocytogenes}.
#'
#' @param nLots Size of the Monte Carlo simulation (scalar).
#' @param sizeLot Number of cantaloupes per cultivation lot or field (scalar).
#' @param pSoil Prevalence of contamination of soil (provided by the user, `default=0.089` according to \insertCite{Strawn2013a;textual}{qraLm}).
#' @param fManure Odds-ratio estimate associated to use or organic amendment in soil (`default=7.0` according to \insertCite{Strawn2013b;textual}{qraLm}).
#' @param pManure Proportion of fields using organic amendments (provided by user, `default=0.5`)
#' @param fIrrigRaining Odds-ratio estimate associated to use of irrigation and raining events up to `2` days before harvest (`default=25.0` according
#'  to \insertCite{Weller2015;textual}{qraLm}).
#' @param pIrrigRaining Proportion of fields that undergo irrigation or raining just previous harvest (provided by user, `default=0.1`).
#' @param cSoilLogMin (`log10 CFU/g`) Minimum value of the triangular distribution describing variability of concentration
#' (according to \insertCite{Dowe1997;textual}{qraLm}: `default=-1` log10\ CFU/g).
#' @param cSoilLogMode (`log10 CFU/g`) Mode value of the triangular distribution describing variability of concentration
#'  (according to \insertCite{Dowe1997;textual}{qraLm}: `default=0.6` log10\ CFU/g).
#' @param cSoilLogMax (`log10 CFU/g`) Maximum value of the triangular distribution describing variability of concentration
#' (according to  \insertCite{Dowe1997;textual}{qraLm}: `default=1.48` log10\ CFU/g).
#' @param qSoilMin (`g`) Minimum value of the triangular distribution describing variability of quantity of soil deposited on cantaloupe (`default=0.05` g).
#' @param qSoilMode (`g`) Mode value of the triangular distribution describing variability of quantity of soil deposited on cantaloupe (`default=0.5` g).
#' @param qSoilMax (`g`) Maximum value of the triangular distribution describing variability of quantity of soil deposited on cantaloupe (`default=5` g).
#' @param pFoil Proportion of fields grown in foil (e.g. plastic mulch) (`default=0.5`).
#' @param rFoil Reduction fraction of the quantity of soil transferred to rind (`default=default EKE: 0.9`).
#' @param pIrrig prevalence of contamination in irrigation water (provided by the user, `default=0.131` according to \insertCite{Raschle2021;textual}{qraLm})
#' @param cIrrigLogMin (`log CFU/L`) min value of the uniform distribution (provided by the user, `default=-1.52` according to \insertCite{Sharma2020;textual}{qraLm})
#' @param cIrrigLogMax (`log CFU/L`)  max value of the uniform distribution (provided by the user, `default=1.04` according to \insertCite{Sharma2020;textual}{qraLm})
#' @param cantaWeight (`g`) weight of a cantaloupe
#' @param pWaterGainMin minimum value of the fraction of water gain (`ml`) relative to the cantaloupe weight in `g` (`default=0.0`)
#' @param pWaterGainMax maximum value of the fraction of water gain (`ml`) relative to the cantaloupe weight in `g`
#' (`default=0.004` according to \insertCite{Richards2004;textual}{qraLm})
#'
#' @return A list of two elements:
#'   \describe{
#'      \item{`N`}{(`CFU`) A matrix of size `nLots` by `sizeLot` containing the numbers of \emph{L. monocytogenes} cells on cantaloupe at harvest;}
#'      \item{`P`}{Prevalence of field lots of cantaloupes contaminated with \emph{L. monocytogenes} cells at harvest (scalar).}
#'           }
#'
#' @author Laurent Guillier
#'
#' @keywords soil, irrigation, splash, transfer, cross-contamination
#'
#' @family data generation
#'
#' @references
#' \insertRef{stats}{qraLm}
#' \insertRef{mc2d}{qraLm}
#' \insertRef{Background2022}{qraLm}
#' \insertRef{Dowe1997}{qraLm}
#' \insertRef{Raschle2021}{qraLm}
#' \insertRef{Richards2004}{qraLm}
#' \insertRef{Sharma2020}{qraLm}
#' \insertRef{Strawn2013a}{qraLm}
#' \insertRef{Strawn2013b}{qraLm}
#' \insertRef{Weller2015}{qraLm}
#'
#' @importFrom stats runif
#' @importFrom mc2d rtriang
#'
#' @export
#'
#' @note The prevalence in soil, `pSoil`, must be provided by the user according to existing data. \insertCite{Background2022;textual}{qraLm} lists various prevalence
#' estimates in different regions of the world. A default value of 0.089 \insertCite{Strawn2013a}{qraLm} is taken.
#' `pSoil` is conditional to several risk factors; yet this functions is based upon two risk factors:
#'  \enumerate{
#'    \item irrigation or rain occurring prior to harvesting, whose odds-ratio has been estimated at `25.0` when taking place `24` hours before harvesting \insertCite{Weller2015}{qraLm}; and
#'    \item application of organic fertilizer, whose odds-ratio has been estimated at `7.0` when manure is applied within `1` year \insertCite{Strawn2013b}{qraLm}.
#'    }
#' The distribution about the concentration of \emph{L. monocytogenes} in soil is represented as a triangular distribution, using data from
#' \insertCite{Dowe1997;textual}{qraLm}, who reported a mean of `4.0` MPN \emph{L. monocytogenes} \ g soil, with a 95% confidence interval of `<1.0-28` MPN/g.
#' \insertCite{Background2022;textual}{qraLm} lists many estimates of \emph{L. monocytogenes} in water environments.
#' The estimate of `0.131` provided by \insertCite{Raschle2021;textual}{qraLm} has been chosen as default.
#' The distribution about the concentration of \emph{L. monocytogenes} in irrigation water is represented as a uniform distribution,
#' using the minimum and maximum values from \insertCite{Sharma2020;textual}{qraLm}, who reported `<0.03` to `11` MPN \emph{L. monocytogenes} / L water.
#' The algorithm assumes that the amount of water deposited on the cantaloupe rind after the last irrigation is as a percentage of the cantaloupe weight,
#' and is sampled from a uniform distribution of parameters between a minimum value of zero and a maximum value of `0.004`, taken from \insertCite{Richards2004;textual}{qraLm}.
#'
#' @examples
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
#' hist(dat$N)
#'
caPrimaryProduction <- function(nLots,
                                sizeLot,
                                cantaWeight = 1000,
                                pSoil = 0.089,
                                pManure = 0.5,
                                fManure = 7,
                                cSoilLogMin = -1.0,
                                cSoilLogMode = 0.6,
                                cSoilLogMax = 1.48,
                                qSoilMin = 0.05,
                                qSoilMode = 0.5,
                                qSoilMax = 5,
                                pFoil = 0.5,
                                rFoil = 0.9,
                                fIrrigRaining = 25,
                                pIrrigRaining = 0.1,
                                pIrrig = 0.131,
                                cIrrigLogMin = -1.52,
                                cIrrigLogMax = 1.04,
                                pWaterGainMin = 0,
                                pWaterGainMax = 0.004) {
  # Assess the contamination (P and N) coming from soil
  conta_soil <- caSoil2rind(
                            nLots=nLots,
                            sizeLot = sizeLot,
                            pSoil = pSoil,
                            fManure = fManure,
                            pManure = pManure,
                            fIrrigRaining = fIrrigRaining,
                            pIrrigRaining = pIrrigRaining,
                            cSoilLogMin = cSoilLogMin,
                            cSoilLogMode = cSoilLogMode,
                            cSoilLogMax = cSoilLogMax,
                            qSoilMin = qSoilMin,
                            qSoilMode = qSoilMode,
                            qSoilMax = qSoilMax,
                            pFoil = pFoil,
                            rFoil = rFoil
                            )

  # Assess the contamination (P and N) coming from irrigation water
  conta_irrig <- caIrrig2rind(
                              nLots = nLots,
                              sizeLot = sizeLot,
                              pIrrig = pIrrig,
                              cIrrigLogMin = cIrrigLogMin,
                              cIrrigLogMax = cIrrigLogMax,
                              cantaWeight = cantaWeight,
                              pWaterGainMin = pWaterGainMin,
                              pWaterGainMax = pWaterGainMax
                              )

#  caPrimaryProduction <- c(as.list(environment()), list(...))
  # Resulting prevalence of both events
  p_from_soil <- conta_soil$P # probability from soil
  p_from_irrig <- conta_irrig$P # probability from irrigation water

  # Calculate the overall prevalence that includes either soil, irrigation water, or both soil & irrigation water
  # assuming independence
  P0 <- p_from_soil + p_from_irrig - p_from_soil * p_from_irrig


  ## Preparing the contamination matrix

  # probability of observing only soil
  p_only_soil <- p_from_soil * (1 - p_from_irrig)

  # probability of observing only water
  p_only_irrig <- p_from_irrig * (1 - p_from_soil)

  # probability of observing both soil and irrigation water contamination
  p_soil_and_irrig <- p_from_soil * p_from_irrig

  # Building matrix N
  N0 <- NULL

  n_only_soil <- round(nLots * p_only_soil / P0)
  if (n_only_soil > 0) {
    N_only_soil <- conta_soil$N[1:n_only_soil, ]
    N0 <- rbind(N0, N_only_soil)
  }

  n_only_irrig <- round(nLots * p_only_irrig / P0)
  if (n_only_irrig > 0) {
    N_only_irrig <- conta_irrig$N[1:n_only_irrig, ]
    N0 <- rbind(N0, N_only_irrig)
  }

  conta_both <- conta_soil$N + conta_irrig$N

  n_soil_irrig <- nLots - n_only_soil - n_only_irrig
  if (n_soil_irrig > 0) {
    N_soil_irrig <- conta_both[1:n_soil_irrig, ]
    N0 <- rbind(N0, N_soil_irrig)
  }

  # Shuffle
  shuffle <- sample(nLots)
  Origin <- c(
    rep("soil", n_only_soil),
    rep("irrig", n_only_irrig),
    rep("soil_irrig", n_soil_irrig)
  )
  Origin <- Origin[shuffle]
  N0 <- N0[shuffle, ]
  
  lotMeans <- rowMeans(N0 / cantaWeight, na.rm = TRUE)
  unitsCounts <- c(N0 / cantaWeight)

  data <- list(
#               caPrimaryProduction = caPrimaryProduction,
               lotMeans = lotMeans,
               unitsCounts = unitsCounts,
               N = N0,
               P = P0,
               Origin = Origin,
               nLots = nLots,
               sizeLot = sizeLot,
               cantaWeight = cantaWeight
               )
  class(data) <- "qraLm"
  return(data)
}
