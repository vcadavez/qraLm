#' Print MC results
#'
#' @title Generic print function to print the MC results
#' @param x qraLm object. See [Lot2LotGen()]
#' @param ... optional plot parameters passed to the plot function
#' @author Vasco Cadavez
#'
#' @examples
#'
#' prod <- Lot2LotGen(
#'   nLots = 1000,
#'   sizeLot = 1000,
#'   unitSize = 500,
#'   betaAlpha = 0.5112,
#'   betaBeta = 9.959,
#'   C0MeanLog = 1.023,
#'   C0SdLog = 0.3267,
#'   propVarInter = 0.7
#' )
#' printMC.qraLm(prod)
#'
#' @export
#'
printMC.qraLm <- function(x, ...) {
  # if (class(x)!= "qraLm")
  #   stop("object is not of class 'qraLm'")

  if (exists("ProbUnitPos", x) == TRUE) {
    cat("List composed of:\n")
    cat("N: ", utils::head(x$N[, 1:5]))

    cat("\n")
    cat("ProbUnitPos: ", utils::head(x$ProbUnitPos))

    cat("\n")
    cat("P: ", x$P)

    cat("\n")
    cat("betaGen: ", utils::head(x$betaGen))
  } else {
    cat("List composed of:\n")
    cat("N: ", utils::head(x$N[, 1:5]))

    cat("\n")
    cat("P: ", x$P)
  }
}
