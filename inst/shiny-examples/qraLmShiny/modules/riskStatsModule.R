# Ui
riskStatsUI <- function(id) {
  ns <- NS(id)
  DT::dataTableOutput(ns("riskstats"))
}  

# server
riskStatsServer <- function(id, data) {
  moduleServer(id, function(input, output, session) {
  
  output$riskstats <- DT::renderDataTable({
    
      wRiskLotMean <- data()$lotMeanRisk
    
     log_risk = function(x){
     ifelse(x!=0, log10(x), 0)
   }
    wRiskLotMeanlog  <- log10(wRiskLotMean)
    

    RiskLotMin    <- min(wRiskLotMean)
    RiskLotMax    <- max(wRiskLotMean)
    RiskLotMedian <- quantile(wRiskLotMean, probs = c(0.5))
    RiskLotMean   <- mean(wRiskLotMean)
    RiskLotQ2.5   <- quantile(wRiskLotMean, probs = c(0.025))
    RiskLotQ97.5  <- quantile(wRiskLotMean, probs = c(0.975))
  
    logRiskLotMin    <- min(wRiskLotMeanlog)
    logRiskLotMax    <- max(wRiskLotMeanlog)
    logRiskLotMedian <- quantile(wRiskLotMeanlog, probs = c(0.5))
    logRiskLotMean   <- mean(wRiskLotMeanlog)
    logRiskLotQ2.5   <- quantile(wRiskLotMeanlog, probs = c(0.025))
    logRiskLotQ97.5  <- quantile(wRiskLotMeanlog, probs = c(0.975))
  
    Stats      <- c(RiskLotMin, RiskLotMax, RiskLotMedian,
                    RiskLotMean, RiskLotQ2.5, RiskLotQ97.5)
    logStats   <- c(logRiskLotMin, logRiskLotMax, logRiskLotMedian,
                    logRiskLotMean, logRiskLotQ2.5, logRiskLotQ97.5)
    StatsNames <- c("Minimum","Maximum","Median","Mean", "pct 2.5th", "pct 97.5th")
    
    riskStats <- data.frame(StatsNames, Stats, logStats)
    names(riskStats) <- c("Statistics", "Mean risk per lot", "Mean risk per lot (log10)")
    
    dt <- DT::datatable(riskStats, 
                        class = 'cell-border stripe',
                        extensions= 'Buttons',
                        rownames = FALSE,
                        options = list(dom = 'Blrt',
                                      lengthMenu = list(c(6, 12, -1), c(6, 12, "All"))))  |>
      DT::formatRound(c("Mean risk per lot (log10)"), digits = 5) |>
      DT::formatSignif(c("Mean risk per lot"), digits = 5)
      return(dt)
      })
  })
}