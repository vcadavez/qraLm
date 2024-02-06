#' @title Sampling of the RTE smoked fish characteristics
#'
#' @description
#' The function [sfCharacteristics()] samples from multiple Pert distributions the
#' intrinsic and extrinsic characteristics of RTE smoked seafood, which affects microbial growth; namely,
#' water activity, pH, contents of salt, phenol, nitrites, acetic acid, benzoic acid, citric acid, diacetate,
#' lactic acid and sorbic acid, and concentration of `CO_2` in equilibrium in the package.
#' Since sampling is carried out at lot level; all the RTE seafood units of a lot have the same characteristics.
#'
#' @param nLots Number of lots or size of the Monte Carlo simulation
#' @param awminSF Minimum water activity of RTE. If null,it will be calculated from NaCl concentration.
#' @param awmodeSF Mode of water activity of RTE.
#' @param awmaxSF Maximum water activity of RTE.
#' @param NaClminSF (%) Minimum NaCl of RTE
#' @param NaClmodeSF (%) Mode of NaCl of RTE
#' @param NaClmaxSF (%) Maximum NaCl of RTE
#' @param pHminSF Minimum pH of RTE
#' @param pHmodeSF Mode of pH of RTE
#' @param pHmaxSF Maximum pH of RTE
#' @param PminSF (ppm) Minimum phenol concentration in RTE
#' @param PmodeSF (ppm) Mode of phenol concentration in RTE
#' @param PmaxSF (ppm) Maximum phenol concentration in RTE
#' @param CO2equilibriumminSF (proportion) Minimum `CO_2` concentration in atmosphere in RTE package (e.g. .0.25)
#' @param CO2equilibriummodeSF (proportion) Mode of `CO_2` concentration in atmosphere in RTE package
#' @param CO2equilibriummaxSF (proportion) Maximum `CO_2` concentration in atmosphere in RTE package
#' @param NITminSF (ppm) Minimum nitrites concentration in RTE
#' @param NITmodeSF (ppm) Mode of nitrites concentration in RTE
#' @param NITmaxSF (ppm) Maximum nitrites concentration in RTE
#' @param aaWphminSF (ppm) Minimum acetic acid concentration in RTE
#' @param aaWphmodeSF (ppm) Mode of acetic acid concentration in RTE
#' @param aaWphmaxSF (ppm) Maximum acetic acid concentration in RTE
#' @param baWphminSF (ppm) Minimum benzoic acid concentration in RTE
#' @param baWphmodeSF (ppm) Mode benzoic acid concentration in RTE
#' @param baWphmaxSF (ppm) Maximum benzoic acid concentration in RTE
#' @param caWphminSF (ppm) Minimum citric acid concentration in RTE
#' @param caWphmodeSF (ppm) Mode of citric acid concentration in RTE
#' @param caWphmaxSF (ppm) Maximum citric acid concentration in RTE
#' @param daWphminSF (ppm) Minimum diacetate concentration in RTE
#' @param daWphmodeSF (ppm) Mode of diacetate concentration in RTE
#' @param daWphmaxSF (ppm) Maximum diacetate concentration in RTE
#' @param laWphminSF (ppm) Minimum lactic acid concentration in RTE
#' @param laWphmodeSF (ppm) Mode of lactic acid concentration in RTE
#' @param laWphmaxSF (ppm) Maximum lactic acid concentration in RTE
#' @param saWphminSF (ppm) Minimum sorbic acid concentration in RTE
#' @param saWphmodeSF (ppm) Mode of sorbic acid concentration in RTE
#' @param saWphmaxSF (ppm) Maximum sorbic acid concentration in RTE
#'
#' @return A list containing vectors `aw`, `NaCl`, `pH`, P, `CO2equi`, `NIT`, `aaWph`, `baWph`,
#' `caWph`, `daWph`, `laWph`, `saWph` to be used in the growth functions [sfMejlholmDalgaard()] and
#' [sfMejlholmDalgaardLAB()] for estimating specific growth rates of \emph{L. monocytogenes} and lactic
#' acid bacteria, respectively, in RTE seafood.
#'
#' @author Regis Pouillot \email{rpouillot.work@gmail.com}
#'
#' @keywords seafood
#'
#' @references
#'
#' \insertRef{Background2022}{qraLm}
#' \insertRef{EFSA2017}{qraLm}
#' \insertRef{mc2d}{qraLm}
#' @importFrom mc2d rpert
#'
#' @export
#'
#' @note All parameters end with `SF` not to be confounded with growth parameters. If parameters for water activity `aw` are `NULL`,
#' `aw` will be evaluated from NaCl concentration in the growth functions. The default parameters of the Pert distributions describing
#' variability in `pH`, contents of `salt`, `phenol`, `diacetate`, `lactic acid`, and concentration of `CO_2` in equilibrium for smoked seafood,
#' were taken from \insertCite{Background2022;textual}{qraLm} (Table 3.25) and \insertCite{EFSA2017;textual}{qraLm}.
#'
#' @examples
#' N <- matrix(round(10^rnorm(100 * 100, 0, 3)),
#'   ncol = 100, nrow = 100
#' )
#'
#' dat <- list(N = N, nLots = 1000)
#' dat <- sfCharacteristics(dat)
#' str(dat)
#'
sfCharacteristics <- function(nLots = data$nLots,
                              # RTE seafood characteristics
                              awminSF = NULL,
                              awmodeSF = NULL,
                              awmaxSF = NULL,
                              NaClminSF = 1.5,
                              NaClmodeSF = 3.4,
                              NaClmaxSF = 5.3,
                              PminSF = 5.0,
                              PmodeSF = 10,
                              PmaxSF = 22,
                              pHminSF = 5.8,
                              pHmodeSF = 6.1,
                              pHmaxSF = 6.5,
                              CO2equilibriumminSF = 0.25,
                              CO2equilibriummodeSF = 0.25,
                              CO2equilibriummaxSF = 0.30,
                              NITminSF = 0,
                              NITmodeSF = 0,
                              NITmaxSF = 0,
                              aaWphminSF = 0,
                              aaWphmodeSF = 0,
                              aaWphmaxSF = 0,
                              baWphminSF = 0,
                              baWphmodeSF = 0,
                              baWphmaxSF = 0,
                              caWphminSF = 0,
                              caWphmodeSF = 0,
                              caWphmaxSF = 0,
                              daWphminSF = 500,
                              daWphmodeSF = 1500,
                              daWphmaxSF = 1900,
                              laWphminSF = 6000,
                              laWphmodeSF = 12000,
                              laWphmaxSF = 28000,
                              saWphminSF = 0,
                              saWphmodeSF = 0,
                              saWphmaxSF = 0) {
  #  if(missing(nLots)) nLots <- nrow(data$N) #test if nLots was defined

  if (is.null(awminSF)) {
    aw <- NULL
    NaCl <- mc2d::rpert(nLots, min = NaClminSF, mode = NaClmodeSF, max = NaClmaxSF)
  } else {
    NaCl <- NULL
    aw <- mc2d::rpert(nLots, min = awminSF, mode = awmodeSF, max = awmaxSF)
  }

  P <- mc2d::rpert(nLots, min = PminSF, mode = PmodeSF, max = PmaxSF)
  pH <- mc2d::rpert(nLots, min = pHminSF, mode = pHmodeSF, max = pHmaxSF)
  CO2equi <- mc2d::rpert(nLots,
    min = CO2equilibriumminSF,
    mode = CO2equilibriummodeSF,
    max = CO2equilibriummaxSF
  )
  NIT <- mc2d::rpert(nLots, min = NITminSF, mode = NITmodeSF, max = NITmaxSF)
  aaWph <- mc2d::rpert(nLots, min = aaWphminSF, mode = aaWphmodeSF, max = aaWphmaxSF)
  baWph <- mc2d::rpert(nLots, min = baWphminSF, mode = baWphmodeSF, max = baWphmaxSF)
  caWph <- mc2d::rpert(nLots, min = caWphminSF, mode = caWphmodeSF, max = caWphmaxSF)
  daWph <- mc2d::rpert(nLots, min = daWphminSF, mode = daWphmodeSF, max = daWphmaxSF)
  laWph <- mc2d::rpert(nLots, min = laWphminSF, mode = laWphmodeSF, max = laWphmaxSF)
  saWph <- mc2d::rpert(nLots, min = saWphminSF, mode = saWphmodeSF, max = saWphmaxSF)

  # Provide an object with the RTE seafood characteristics to be passed in the next function
  SmokedFish <- list(
    aw = aw, NaCl = NaCl, pH = pH, P = P, CO2equi = CO2equi,
    NIT = NIT, aaWph = aaWph, baWph = baWph, caWph = caWph,
    daWph = daWph, laWph = laWph, saWph = saWph
  )

  return(SmokedFish)
}
