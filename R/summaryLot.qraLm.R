#' Print summary MC results
#'
#' @title summaryLot Generic print function to print the between lots MC summary statistics
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
#'   nLots        = 1000,
#'   sizeLot      = 1000,
#'   unitSize     = 500,
#'   betaAlpha    = 0.5112,
#'   betaBeta     = 9.959,
#'   C0MeanLog    = 1.023,
#'   C0SdLog      = 0.3267,
#'   propVarInter = 0.7,
#'   Poisson      = FALSE
#' )
#'
#' summaryLot.qraLm(prod)
#'
#' @export
#'
summaryLot.qraLm <- function(x, ...) {
  # if (class(x)!= "qraLm")
  #   stop("object is not of class 'qraLm'")
  if (exists("unitSize", x) == TRUE) {
    lotN <- rowMeans(x$N / x$unitSize, na.rm = TRUE)
  } else {
    lotN <- rowMeans(x$N, na.rm = TRUE)
  }

  if (exists("ProbUnitPos", x) == TRUE) {
    NStats <- Hmisc::wtd.quantile(lotN,
      weights = x$ProbUnitPos,
      probs = c(0.0, 0.5, 0.025, 0.975, 1.0),
      normwt = TRUE, na.rm = TRUE
    )
    NStatsMean <- stats::weighted.mean(lotN, w = x$ProbUnitPos, na.rm = TRUE)
  } else {
    NStats <- Hmisc::wtd.quantile(lotN,
      weights = rep(1, nrow(x$N)),
      probs = c(0.00, 0.0, 0.025, 0.975, 1.00),
      normwt = TRUE, na.rm = TRUE
    )
    NStatsMean <- mean(lotN, na.rm = TRUE)
  }

  NStatsMin <- NStats[1]
  NStatsMedian <- NStats[2]
  Q2.5 <- NStats[3]
  Q97.5 <- NStats[4]
  NStatsMax <- NStats[5]
  Counts <- rbind(
    unname(NStatsMin),
    unname(Q2.5),
    unname(NStatsMean),
    unname(NStatsMedian),
    unname(Q97.5),
    unname(NStatsMax)
  )
  #  log10Counts <- round(log10(ceiling(Counts)), digits=4)
  log10Counts <- round(log10(Counts), digits = 4)
  Statistics <- c("Minimum", "pct 2.5th", "Mean", "Median", "pct 97.5th", "Maximum")
  MCstats <- data.frame(Statistics, Counts, log10Counts)


  if (exists("ProbUnitPos", x) == TRUE) {
    names(MCstats) <- c("Statistics", "CFU/g", "log10 CFU/g")
    DT::datatable(MCstats,
      caption = "Summary statistics: between lots mean counts",
      class = "display",
      fillContainer = FALSE,
      options = list(dom = "t")
    ) |>
      DT::formatSignif(columns = c("CFU/g", "log10 CFU/g"), digits = 4)
  } else {
    names(MCstats) <- c("Statistics", "CFU/Melon", "log10 CFU/Melon")

    DT::datatable(MCstats,
      caption = "Summary statistics: between lots mean counts",
      class = "display",
      fillContainer = FALSE,
      options = list(dom = "t")
    ) |>
      DT::formatSignif(columns = c("CFU/Melon", "log10 CFU/Melon"), digits = 4)
  }
}
