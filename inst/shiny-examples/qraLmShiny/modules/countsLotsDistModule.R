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
  
    CFU <- data()$lotMeans
    df <- data.frame(CFU=CFU)
    
    histo <- plot_ly(x = ~df$CFU,
                     type = "histogram", nbinsx = 25,          
                     histnorm = "probability")
    
    box <- plot_ly(x = ~df$CFU, type = "box") 
    
    plot <- plotly::subplot(box, histo,
                            nrows = 2,
                            heights = c(0.2,0.8),
                            shareX = TRUE, 
                            titleX = TRUE) |>
      plotly::layout(title = "", showlegend = FALSE,
                     xaxis = list(title = "CFU/g"),
                     yaxis = list(title = "Probability"))
    
    return(plot)
      })
   })
}