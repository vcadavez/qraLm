## LM Concentration in the contaminated lots #
# Define the UI
countsLotsDistUI <- function(id) {
  ns <- NS(id)
   plotlyOutput(ns("counts_lots_dist"))
}  

# server
countsLotsDistServer <- function(id, data) {
  moduleServer(id, function(input, output, session) {
  
  output$counts_lots_dist <- renderPlotly({
    # cat("variability of contamination in lots (CFU/g)\n")
    CFU <- data()$ProbUnitPos * (rowSums(data()$N)/(data()$nLots*data()$unitSize))
    CFU <- data.frame(CFU=CFU)
    
   plot1 <- ggplot2::ggplot(data = CFU, aes(x = "", y = CFU)) +
     ggplot2::geom_boxplot(color="blue", fill="blue", alpha=0.5, width=1.5) +
     ggplot2::theme_classic() +
     ggplot2::coord_flip()
   
     plot2 <- ggplot2::ggplot(CFU, aes(x=CFU, y=after_stat(count),
                     position = "identity", binwidth = 1, 
                     color = "blue")) +
       ggplot2::geom_histogram( color="blue", fill="grey", bins=10, binwidth = 0.25) +
       ggplot2::theme_classic()
     
     plot1 <- plotly::ggplotly(plot1, width = 500, height = 500)
     plot2 <- plotly::ggplotly(plot2, width = 500, height = 500)
     
     plot <- plotly::subplot(plot1, plot2, nrows = 2,
                            shareX = TRUE, titleX = TRUE) |> 
      layout(title = "",
             xaxis = list(title = "Counts (CFU/g)"),
             yaxis = list(title = "Frequency"),
             margin = list(l = 50, r = 50, t = 50, b = 150)
             )
    return(plot)
      })
   })
}