#' @title Dose-response model function for the qraLm models
#'
#' @description
#' This function provides the marginal probability of invasive listeriosis
#' in a given `population` for a given `Dose` in `CFU` using the
#' `JEMRA`, the `Pouillot`, the `Fritsch` or the `EFSA` dose-response models
#' or the model developed within this project (`EFSAMV`,`EFSAV`,`EFSALV`) (see References).
#'
#' @param data a list of (a minima):
#'    \describe{
#'           \item{`N`}{(`CFU`) A matrix containing the numbers of \emph{L. monocytogenes} per portion of defrosted or frozen vegetables.}
#'           }
#' @param model either `JEMRA`, `Pouillot`, `Fritsch`, `EFSA`, `EFSAMV`,`EFSAV` or `EFSALV`
#' @param population considered population (scalar).
#' @param Poisson if `TRUE`, assume that `Dose` is the mean of a Poisson distribution.
#' (actual LogNormal Poisson). If `FALSE` (default), assume that `Dose` is the actual number of bacteria.
#'
#' @details
#' see [doseresponsemodels::DR()] or [doseresponsemodels::DRQuick()] for details on population and models.
#'
#'
#' @author Regis Pouillot, Vasco Cadavez
#'
#' @keywords dose-response model
#'
#' @references
#'
#' \insertRef{EFSA2018}{qraLm}
#' \insertRef{FAO-WHO2004}{qraLm}
#' \insertRef{Fritsch2018}{qraLm}
#' \insertRef{Pouillot2015}{qraLm}
#'
#' @importFrom doseresponsemodels DR
#' @importFrom doseresponsemodels DRLogNormPoisson
#' @importFrom doseresponsemodels DRQuick
#'
#' @return the data object with added:
#'     \describe{
#'              \item{`Risk`}{A matrix of risk, of size similar to `N`}
#'              \item{`Model`}{the Model}
#'              \item{`Population`}{the Population}
#'              }
#'
#' @export
#'
#' @note This function uses (for all model but `JEMRA`) a linear approximation (`approxfun`)
#' from the exact [doseresponsemodels::DR()] model evaluated on \eqn{Dose = c(0,10^{seq(-5,12,length=1701)})}
#' (if `Poisson=TRUE`) or \eqn{c(0,10^{seq(0,12,length=2000)})} (if `Poisson=FALSE`).
#' Any Dose lower or higher than these ranges will lead to `NA`.
#'
#' @examples
#' data <- list(N = matrix(10^runif(100, 0, 5), ncol = 25))
#' DRForModel(data, "JEMRA", 1)
#' DRForModel(data, "EFSAMV", 10)
#'
DRForModel <- function(data = list(), model = "JEMRA", population = 1, Poisson = FALSE) {
  if (length(model) > 1) stop("model is not vectorized in DRForModel")
  if (length(population) > 1) stop("population is not vectorized in DRForModel")
  # if JEMRA: use the original model
  risk <- DRQuick(data$N, model = model, population = population, Poisson = Poisson)
  # back to matrix
  Risk <- matrix(risk,
                 ncol = ncol(data$N),
                 nrow = nrow(data$N)
  )
  
  if (exists("ProbUnitPos", data) == TRUE) {
    lotMeanRisk <- rowMeans(Risk * data$ProbUnitPos) 
  } else {
    probunitpos <- rep(1, nrow(data$N))
    lotMeanRisk <- rowMeans(Risk * probunitpos)
  }

  if (exists("ProbUnitPos", data) == TRUE) {
    servingRisk <- Risk * data$ProbUnitPos
  } else {
    probunitpos <- rep(1, nrow(data$N))
    servingRisk <- Risk * probunitpos
  }
  
  data$Risk <- Risk
  data$lotMeanRisk <- lotMeanRisk
  data$servingRisk <- servingRisk
  data$Model <- model
  data$Population <- population

  return(data)
}