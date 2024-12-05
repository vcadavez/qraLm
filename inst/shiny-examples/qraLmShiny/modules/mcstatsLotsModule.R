# UI
mcstatsLotsUI <- function(id) {
  ns <- NS(id)
    DT::dataTableOutput(ns("mcstats_lots"))
}  

# SERVER
mcstatsLotsServer <- function(id, data) {
  moduleServer(id, function(input, output, session) {
 
    output$mcstats_lots <-  DT::renderDataTable({
      lotN <- data()$lotMeans
      if (exists("ProbUnitPos", data()) == TRUE) {
        NStats <- Hmisc::wtd.quantile(lotN,
                                      weights = data()$ProbUnitPos,
                                      probs = c(0.00, 0.50, 0.025, 0.975, 1.0),
                                      normwt = TRUE, na.rm = TRUE)
        
        NStatsMean <- stats::weighted.mean(lotN, w = data()$ProbUnitPos, na.rm = TRUE)
      } else {
        probunitpos <- rep(1, nrow(data()$N))
        NStats <- Hmisc::wtd.quantile(lotN,
                                      weights = probunitpos,
                                      probs = c(0.00, 0.50, 0.025, 0.975, 1.00),
                                      normwt = TRUE, na.rm = TRUE)
        
        NStatsMean <- stats::weighted.mean(lotN, w = probunitpos, na.rm = TRUE)
      }

      NStatsMin    <- NStats[1]
      NStatsMedian <- NStats[2]
      Q2.5         <- NStats[3]
      Q97.5        <- NStats[4]
      NStatsMax    <- NStats[5]
      Counts       <- rbind(
                            unname(NStatsMin),
                            unname(Q2.5),
                            unname(NStatsMean),
                            unname(NStatsMedian),
                            unname(Q97.5),
                            unname(NStatsMax)
                            )
      
  #    logs function
      log_var <- function(x) {
         ifelse(x != 0, log10(x), 0)
       }
       logCounts <- round(log_var(Counts), digits=5)
       #logCounts <- log10(Counts)
        
      Statistics <- c("Minimum", "pct 2.5th", "Mean", "Median", "pct 97.5th", "Maximum")
      MCstats <- data.frame(Statistics, Counts, logCounts)
      
      if (exists("ProbUnitPos", data())==TRUE) {
      names(MCstats) <- c("Statistics", "CFU/g", "log10 CFU/g" )
      dt <- DT::datatable(MCstats,
#                   caption = "Summary statistics: between lots LM counts", 
                    class = "cell-border stripe", 
                    extensions= 'Buttons',
                    rownames = FALSE,
                    options = list(dom = 'Blrt')) |>
        DT::formatSignif(columns = c("CFU/g","log10 CFU/g"), digits = 4) |>
        DT::formatRound(columns = c("CFU/g","log10 CFU/g"), 4) 
      return(dt)
      } else {
      names(MCstats) <- c("Statistics", "CFU/Melon", "log10 CFU/Melon" )
      dt <- DT::datatable(MCstats,
#                    caption = "Summary statistics: between lots LM counts", 
                    class = "cell-border stripe", 
                    extensions= 'Buttons',
                    rownames = FALSE,
                    options = list(dom = 'Blrt')) |>
        DT::formatSignif(columns = c("CFU/Melon","log10 CFU/Melon"), digits = 4) |>
        DT::formatRound(columns = c("CFU/Melon","log10 CFU/Melon"), 4)
      return(dt)
      }
      })
    })
}
