## LM Concentration in the contaminated lots #
# Define the UI
countsUnitsDistUI <- function(id) {
  ns <- NS(id)
   plotlyOutput(ns("counts_units_dist"))
}  

# server
countsUnitsDistServer <- function(id, data) {
  moduleServer(id, function(input, output, session) {
  output$counts_units_dist <- renderPlotly({
   
    CFU <- data()$unitsCounts
    
    index <- which(CFU==0)
      if (length(index)==0) {
       PosServings <- CFU
     } else {
       PosServings <- CFU[-index]
     }
    df <- data.frame(counts=PosServings)
  
    histo <- plot_ly(x = ~df$counts, 
                     type = "histogram", nbinsx = 25,          
                     histnorm = "probability")
    
    box <- plot_ly(x = ~df$counts, type = "box") 
    
    plot <- plotly::subplot(box, histo,
                            nrows = 2,
                            heights = c(0.2,0.8),
                            shareX = TRUE, 
                            titleX = TRUE) |>
      plotly::layout(title = "", showlegend = FALSE,
                     xaxis = list(title = "CFU/g"),
                     yaxis = list(title = "Probability")
                     )
    return(plot)
      })
   })
}
