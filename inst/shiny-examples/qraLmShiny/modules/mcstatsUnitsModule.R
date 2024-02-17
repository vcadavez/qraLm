## Summary statistics of contaminated units 
# define the Ui
mcstatsUnitsUI <- function(id) {
  ns <- NS(id)
    DT::dataTableOutput(ns("mcstats_units"))
}  

# server
mcstatsUnitsServer <- function(id, data) {
  moduleServer(id, function(input, output, session) {
 
    output$mcstats_units <-  DT::renderDataTable({
#   cat("variability of contamination in units (CFU/g)\n")
    
 if (exists("ProbUnitPos", data())==TRUE) {
    cfuUnit <- data()$ProbUnitPos*(data()$N/data()$unitSize)
  } else if (exists("unitSize", data())==TRUE)  {
    cfuUnit <- data()$N/data()$unitSize
  } else{
    cfuUnit <- data()$N
  }
         
       index <- which(cfuUnit==0)
         if (length(index)==0) {
           posUnits <- cfuUnit
          } else {
            posUnits <- cfuUnit[-index]
          }    
      
      NStatsMin    <- min(posUnits)
      NStatsMax    <- max(posUnits)
      NStatsMedian <- median(posUnits)
      NStatsMean   <- mean(posUnits)
      Q2.5  <- quantile(posUnits, probs = c(0.025), na.rm=TRUE)
      Q97.5 <- quantile(posUnits, probs = c(0.975), na.rm=TRUE)
      
      Counts <-rbind(unname(NStatsMin),
                     unname(Q2.5),
                     unname(NStatsMean),
                     unname(NStatsMedian),
                     unname(Q97.5),
                     unname(NStatsMax))
                     
                     
      logCounts <- round(log10(Counts), digits=4)
      Statistics <- c("Minimum", "pct 2.5th", "Mean", "Median", "pct 97.5th", "Maximum")
      MCstats <- data.frame(Statistics, Counts, logCounts)
      names(MCstats) <- c("Statistics", "CFU/g", "log10 CFU/g" )
      dt <- DT::datatable(MCstats,
#                          caption = "Summary statistics: within lots (between units) counts", 
                          class = "cell-border stripe", 
                          extensions= 'Buttons',
                          rownames = FALSE,
                          options = list(dom = 'Blrt')) %>%
        DT::formatSignif(columns = c("CFU/g","log10 CFU/g"), digits = 4) %>%
        DT::formatRound(columns = c("CFU/g","log10 CFU/g"), 4) 
      return(dt)
      })
    })
}
