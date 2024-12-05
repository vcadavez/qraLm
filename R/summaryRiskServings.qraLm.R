#' Print summary MC risk results per contamiunated serving
#'
#' @title summaryRiskServings generic function to print the risk summary statistics
#' @param x qraLm object. See [Lot2LotGen()]
#' @param ... optional plot parameters passed to the plot function
#' @author Vasco Cadavez
#'
#' @importFrom stats weighted.mean quantile
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
#' summaryRiskServings.qraLm(res)
#'
#' @export
#'
summaryRiskServings.qraLm <- function(x, ...) {
  # if (class(x)!= "qraLm")
  #   stop("object is not of class 'qraLm'")
  
 servingRisk <- c(x$servingRisk!=0)
  
 servingRiskLog <-  log10(servingRisk)
  
riskMin <- min(servingRiskLog)
riskMax <- max(servingRiskLog)
riskMedian <- stats::quantile(servingRiskLog, probs = c(0.50), na.rm = TRUE)
riskMean <- mean(servingRiskLog, na.rm = TRUE)
riskQ2.5 <- stats::quantile(servingRiskLog, probs = c(0.025), na.rm = TRUE)
 riskQ97.5 <- stats::quantile(servingRiskLog, probs = c(0.975), na.rm = TRUE)

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
                caption = "Summary statistics of risk per contaminated serving",
                class = "display",
                fillContainer = FALSE,
                options = list(dom = "t") ) |>
    DT::formatSignif(columns = c("log10 Risk"), digits = 4)
}
