#' Print summary MC results at Lot level
#'
#' @title printPrevContUnits function to print the prevalence of contaminated units 
#' @param x qraLm object. See [Lot2LotGen()]
#' @param ... optional plot parameters passed to the plot function
#' @author Vasco Cadavez
#'
#' @importFrom stats weighted.mean
#' @importFrom Hmisc wtd.quantile
#' @importFrom DT datatable
#' @importFrom DT formatSignif
#'
#' @examples
#' prod <- Lot2LotGen(
#'                    nLots        = 1000,
#'                    sizeLot      = 1000,
#'                    unitSize     = 500,
#'                    betaAlpha    = 0.5112,
#'                    betaBeta     = 9.959,
#'                    C0MeanLog    = 1.023,
#'                    C0SdLog      = 0.3267,
#'                    propVarInter = 0.7,
#'                    Poisson      = FALSE
#' )
#'
#' printPrevContUnits.qraLm(prod)
#'
#' @export
#'
printPrevContUnits.qraLm <- function(x, ...) {
  # if (class(x)!= "qraLm")
  #   stop("object is not of class 'qraLm'")
  if (exists("ProbUnitPos", x) == TRUE) {
      prev <- mean(x$ProbUnitPos * rowMeans(x$N !=0))
  } else {
    probunitpos <- rep(1, nrow(x$N))
    prev <- stats::weighted.mean(probunitpos * (x$N != 0),
                                 w=rep(1,length(x$N)))
  }
print(round(prev,4))
}
