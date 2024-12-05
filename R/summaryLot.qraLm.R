#' Print summary MC results at Lot level
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
#' summaryLot.qraLm(prod)
#'
#' @export
#'
summaryLot.qraLm <- function(x, ...) {
  # if (class(x)!= "qraLm")
  #   stop("object is not of class 'qraLm'")
  
  lotMeans <- x$lotMeans
   
  NStats <- Hmisc::wtd.quantile(lotMeans,
                                weights = x$ProbUnitPos,
                                probs = c(0.00, 0.50, 0.025, 0.975, 1.0),
                                normwt = TRUE, na.rm = TRUE)
      
  NStatsMean <- stats::weighted.mean(lotMeans, 
                                      w = x$ProbUnitPos,
                                      na.rm = TRUE)

  NStatsMin    <- NStats[1]
  NStatsMedian <- NStats[2]
  Q2.5         <- NStats[3]
  Q97.5        <- NStats[4]
  NStatsMax    <- NStats[5]
  
  Counts <- rbind(
                  unname(NStatsMin),
                  unname(Q2.5),
                  unname(NStatsMean),
                  unname(NStatsMedian),
                  unname(Q97.5),
                  unname(NStatsMax)
                  )
  # logs function
  log_var <- function(x) {
    ifelse(x != 0, log10(x), 0)
  }
  
   log10Counts <- round(log_var(Counts), digits = 6)
  Statistics <- c("Minimum", "pct 2.5th", "Mean", "Median", "pct 97.5th", "Maximum")
  MCstats <- data.frame(Statistics, Counts, log10Counts)

  if (exists("ProbUnitPos", x) == TRUE) {
    names(MCstats) <- c("Statistics", "CFU/g", "log10 CFU/g")
    DT::datatable(MCstats,
                  caption = "Summary statistics: mean counts in contaminated lots",
                  class = "display",
                  fillContainer = FALSE,
                  options = list(dom = "t")) |>
      DT::formatSignif(columns = c("CFU/g", "log10 CFU/g"), digits = 4)
  } else {
    names(MCstats) <- c("Statistics", "CFU/Melon", "log10 CFU/Melon")

    DT::datatable(MCstats,
                  caption = "Summary statistics: mean counts in contaminated lots",
                  class = "display",
                  fillContainer = FALSE,
                  options = list(dom = "t")) |>
      DT::formatSignif(columns = c("CFU/Melon", "log10 CFU/Melon"), digits = 4)
  }
}
