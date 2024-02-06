#' Print summary MC units results
#'
#' @title summaryUnits Generic function to print the within lots (between units) MC summary statistics
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
#' dat <- Lot2LotGen(
#'   nLots = 500,
#'   sizeLot = 500,
#'   unitSize = 500,
#'   betaAlpha = 0.5112,
#'   betaBeta = 9.959,
#'   C0MeanLog = 1.023,
#'   C0SdLog = 0.3267,
#'   propVarInter = 0.7
#' )
#' df <- fvPortioning(dat, servingSize = 150, unitSize = 500, bPort = 1)
#'
#' summaryUnits.qraLm(dat)
#'
#' @export
#'
summaryUnits.qraLm <- function(x, ...) {
  # if (class(x)!= "qraLm")
  #   stop("object is not of class 'qraLm'")
  if (exists("ProbUnitPos", x) == TRUE) {
    cfuUnit <- x$ProbUnitPos * (x$N / x$unitSize)
  } else {
    cfuUnit <- x$N / x$unitSize
  }

  index <- which(cfuUnit == 0)

  if (length(index) == 0) {
    posUnits <- cfuUnit
  } else {
    posUnits <- cfuUnit[-index]
  }

  NStatsMin <- min(posUnits)
  NStatsMax <- max(posUnits)
  NStatsMedian <- stats::median(posUnits)
  NStatsMean <- mean(posUnits, na.rm = TRUE)
  Q2.5 <- stats::quantile(posUnits, probs = c(0.025), na.rm = TRUE)
  Q97.5 <- stats::quantile(posUnits, probs = c(0.975), na.rm = TRUE)

  Counts <- rbind(
    unname(NStatsMin),
    unname(Q2.5),
    unname(NStatsMean),
    unname(NStatsMedian),
    unname(Q97.5),
    unname(NStatsMax)
  )
  log10Counts <- log10(Counts)
  Statistics <- c("Minimum", "pct 2.5th", "Mean", "Median", "pct 97.5th", "Maximum")
  MCstats <- data.frame(Statistics, Counts, log10Counts)
  names(MCstats) <- c("Statistics", "CFU/g", "log10 CFU/g")
  DT::datatable(MCstats,
    caption = "Summary statistics: within lots (between units) mean counts",
    class = "display",
    fillContainer = FALSE,
    options = list(dom = "t")
  ) |>
    DT::formatSignif(columns = c("CFU/g", "log10 CFU/g"), digits = 4)
}
