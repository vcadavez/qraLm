#' Print summary MC risk results
#'
#' @title summaryRisk generic function to print the risk summary statistics
#' @param x qraLm object. See [Lot2LotGen()]
#' @param ... optional plot parameters passed to the plot function
#' @author Vasco Cadavez
#'
#' @importFrom stats weighted.mean
#' @importFrom Hmisc wtd.quantile
#' @importFrom DT datatable
#' @importFrom DT formatSignif
#' @importFrom matrixStats rowMeans2 rowWeightedMeans
#'  
#' @examples
#' dat <- Lot2LotGen(
#'                   nLots = 500,
#'                   sizeLot = 500,
#'                   unitSize = 500,
#'                   betaAlpha = 0.5112,
#'                   betaBeta = 9.959,
#'                   C0MeanLog = 1.023,
#'                   C0SdLog = 0.3267,
#'                   propVarInter = 0.7
#'                   )
#' DRmodel = "JEMRA"
#' population = 2
#' res <- DRForModel(dat, 
#'                   model=DRmodel,
#'                   population = population)
#' 
#' summaryRisk.qraLm(res)
#'
#' @export
#'
summaryRisk.qraLm <- function(x, ...) {
  # if (class(x)!= "qraLm")
  #   stop("object is not of class 'qraLm'")
  if (exists("ProbUnitPos", x) == TRUE) {
    wRiskLotMean <- matrixStats::rowWeightedMeans(x$Risk, W = x$ProbUnitPos)
   } else {
    wRiskLotMean <- matrixStats::rowMeans2(x$Risk)
  }

  log_risk = function(x){
             ifelse(x!=0, log10(x), 0)
             }
  
  wRiskLotMeanLog <-  log_risk(wRiskLotMean)
  
riskMin <- min(wRiskLotMeanLog)
riskMax <- max(wRiskLotMeanLog)
riskMedian <- stats::median(wRiskLotMeanLog)
riskMean <- mean(wRiskLotMeanLog, na.rm = TRUE)
riskQ2.5 <- stats::quantile(wRiskLotMeanLog, probs = c(0.025), na.rm = TRUE)
 riskQ97.5 <- stats::quantile(wRiskLotMeanLog, probs = c(0.975), na.rm = TRUE)

  risk <- rbind(
                unname(riskMin),
                unname(riskQ2.5),
                unname(riskMean),
                unname(riskMedian),
                unname(riskQ97.5),
                unname(riskMax)
                )
  Statistics <- c("Minimum", "pct 2.5th", "Mean", "Median", "pct 97.5th", "Maximum")
  Riskstats <- data.frame(Statistics, risk)
  names(Riskstats) <- c("Statistics", "log10 Risk")
  DT::datatable(Riskstats,
                caption = "Summary statistics of Risk per lot",
                class = "display",
                fillContainer = FALSE,
                options = list(dom = "t") ) |>
    DT::formatSignif(columns = c("log10 Risk"), digits = 4)
}
