#' Plot ECDF curve
#'
#' @title plotUnitsECDF Generic plot function to plot the ECDF
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
#' plotUnitsECDF.qraLm(prod1)
#'
#' p1 <- plotUnitsECDF.qraLm(prod)
#' p2 <- plotUnitsECDF.qraLm(prod1)
#' par(mfrow = c(1, 2))
#' p1
#' p2
#' par(mfrow = c(1, 1)) # reset this parameter
#'
#' @export
#'
plotUnitsECDF.qraLm <- function(x, ...) {
  # if (class(x)!= "qraLm")
  #   stop("object is not of class 'qraLm'")
  if (exists("unitSize", x) == TRUE) {
    lotN <- c(x$N / x$unitSize)
  } else {
    lotN <- c(x$N / x$cantaWeight, na.rm = TRUE)
  }

  df <- data.frame(cfu = lotN) # weighted cels/g per lot


  plot <- ggplot2::ggplot(df, ggplot2::aes(x = cfu)) +
    ggplot2::theme_minimal() +
    ggplot2::stat_ecdf() +
    ggplot2::xlab("CFU/g in a contaminated unit") +
    ggplot2::ylab("Probability")
  p <- plotly::ggplotly(plot)
  return(p)
}
