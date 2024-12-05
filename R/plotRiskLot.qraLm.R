#' Plot risk distribution per contaminated lot
#'
#' @title Generic plot function for risk
#' @param x qraLm object see [Lot2LotGen()]
#' @param ... optional plot parameters passed to the plot function
#' @author Vasco Cadavez
#'
#' @importFrom dplyr %>%
#' @importFrom stats density
#' @importFrom stats weighted.mean
#' @importFrom Hmisc wtd.quantile
#' @importFrom DT datatable
#' @importFrom plotly subplot ggplotly plot_ly
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
#' DRmodel <- "JEMRA"
#' population <- 2
#' risk <- DRForModel(prod,
#'                    model = DRmodel,
#'                    population = population)
#' str(risk)
#' plotLotRisk.qraLm(risk)
#'
#' @export
#'
plotLotRisk.qraLm <- function(x, ...) {
  
  lotMeanRisk <- x$lotMeanRisk
  
  lotMeanRiskLog <-  log10(lotMeanRisk)
  
 
  df <- data.frame(logRisk=lotMeanRiskLog)
  
  histo <- plot_ly(x = ~df$logRisk,
                   type = "histogram", nbinsx = 30,          
                   histnorm = "probability")

  box <- plot_ly(x = ~df$logRisk, type = "box") 

  plot <- plotly::subplot(box, histo,
                          nrows = 2,
                          heights = c(0.2,0.8),
                          shareX = TRUE, 
                          titleX = TRUE)
  return(plot)
}
