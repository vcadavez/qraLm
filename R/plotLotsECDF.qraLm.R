#' Plot ECDF curve
#'
#' @title plotLotsECDF Generic plot function to plot the ECDF
#' @param x qraLm object see [Lot2LotGen()]
#' @param ... Optional plot parameters passed to the function
#' @author Vasco Cadavez
#'
#' @importFrom stats weighted.mean
#' @importFrom Hmisc wtd.quantile
#' @importFrom DT datatable
#'
#' @examples
#'
#' prod <- Lot2LotGen(
#'                    nLots = 1000,
#'                    sizeLot = 1000,
#'                    unitSize = 500,
#'                    betaAlpha = 0.5112,
#'                    betaBeta = 9.959,
#'                    C0MeanLog = 1.023,
#'                    C0SdLog = 0.3267,
#'                    propVarInter = 0.7
#'                    )
#' prod1 <- Lot2LotGen(
#'                     nLots = 1000,
#'                     sizeLot = 1000,
#'                     unitSize = 500,
#'                     betaAlpha = 0.9112,
#'                     betaBeta = 2.959,
#'                     C0MeanLog = 2.023,
#'                     C0SdLog = 0.5267,
#'                     propVarInter = 0.7
#'                     )
#'
#' plotLotsECDF.qraLm(prod1)
#'
#' p1 <- plotLotsECDF.qraLm(prod)
#' p2 <- plotLotsECDF.qraLm(prod1)
#' par(mfrow = c(1, 2))
#' p1
#' p2
#' par(mfrow = c(1, 1)) # reset this parameter
#'
#' @export
#'
plotLotsECDF.qraLm <- function(x, ...) {
  # if (class(x)!= "qraLm")
  #   stop("object is not of class 'qraLm'") https://github.com/ejanalysis/analyze.stuff
  cfu <- NULL

  if (exists("unitSize", x) == TRUE) {
    lotN <- rowMeans(x$N / x$unitSize)
  } else {
    lotN <- rowMeans(x$N)
  }

  if (exists("ProbUnitPos", x) == TRUE) {
    Clot <- lotN * x$ProbUnitPos # compute cells/g per lot (concentration)
  } else {
    Clot <- lotN
  }

  df <- data.frame(cfu = Clot) # weighted cels/g per lot


  plot <- ggplot2::ggplot(df, ggplot2::aes(x = cfu)) +
    ggplot2::theme_minimal() +
    ggplot2::stat_ecdf()
  p <- plotly::ggplotly(plot)
  return(p)
}
