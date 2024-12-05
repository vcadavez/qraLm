#' Print summary MC units results
#'
#' @title summaryUnits Generic function to print the within lots (between units) MC summary statistics
#' @param x qraLm object. See [Lot2LotGen()]
#' @param ... optional plot parameters passed to the plot function
#' @author Vasco Cadavez
#'
#' @importFrom stats weighted.mean quantile
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
#'
#' summaryUnits.qraLm(dat)
#'
#' @export
#'
summaryUnits.qraLm <- function(x, ...) {
  # if (class(x)!= "qraLm")
  #   stop("object is not of class 'qraLm'")

 unitsCounts <- c(x$unitsCounts)

  index <- which(unitsCounts==0)
  if (length(index)==0) {
    PosUnits <- unitsCounts
    } else {
    PosUnits <- unitsCounts[-index]
  }

  NStatsMin    <- min(PosUnits)
  NStatsMax    <- max(PosUnits)
  NStatsMedian <- quantile(PosUnits, probs=c(0.5))
  NStatsMean   <- mean(PosUnits)
  Q2.5         <- quantile(PosUnits, probs=c(0.025))
  Q97.5        <- quantile(PosUnits, probs=c(0.975))

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
    caption = "Summary statistics: counts in contaminated units",
    class = "display",
    fillContainer = FALSE,
    options = list(dom = "t")
  ) |>
    DT::formatSignif(columns = c("CFU/g", "log10 CFU/g"), digits = 6)
}
