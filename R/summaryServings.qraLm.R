#' Print summary of MC results
#'
#' @title summaryServings Generic function to print the within lots (between servings) MC summary statistics
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
#' dat1 <- fvDefrost(dat,
#'   Temp = 8,
#'   time = 2,
#'   MPD = 8.00,
#'   Tmin = -1.18,
#'   meanEGR5 = 0.0117,
#'   sdEGR5 = 0.00816,
#'   servingSize = 50,
#'   pDefrost = 0
#' )
#'
#' #summaryServings.qraLm(dat1)
#'
#' @export
#'
summaryServings.qraLm <- function(x, ...) {
  # if (class(x)!= "qraLm")
  #   stop("object is not of class 'qraLm'")
  
    unitsServing <- c(x$unitsServing)
  
  index <- which(unitsServing==0)
  if (length(index)==0) {
    PosServings <- unitsServing
  } else {
    PosServings <- unitsServing[-index]
  }
  
  NStatsMin    <- min(PosServings)
  NStatsMax    <- max(PosServings)
  NStatsMedian <- quantile(PosServings, probs=c(0.5))
  NStatsMean   <- mean(PosServings)
  Q2.5         <- quantile(PosServings, probs=c(0.025))
  Q97.5        <- quantile(PosServings, probs=c(0.975))
  
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
    caption = "Summary statistics: counts in contaminated servings",
    class = "display",
    fillContainer = FALSE,
    options = list(dom = "t")
  ) |>
    DT::formatSignif(columns = c("CFU/g", "log10 CFU/g"), digits = 4)
}
