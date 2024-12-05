### Variability in within-lot prevalence

# Ui
prevWithinlotsUI <- function(id) {
  ns <- NS(id)
 
   plotlyOutput(ns("prev_within_lot"), height = 'auto', width = 'auto')
  
}  

# server
prevWithinlotsServer <- function(id, data) {
  moduleServer(id, function(input, output, session) {
  
  output$prev_within_lot <- renderPlotly({
   
#    isolate({
      df <- data.frame(betaGen=data()$betaGen)
      p <- ggplot(df, aes(x=betaGen, y=after_stat(density))) +
           theme_minimal() +
           geom_histogram(fill="brown", bins=20)
      
      plot <- ggplotly(p) |>
        plotly::layout(title = "",
                     xaxis = list(title = "Frequency"),
                     yaxis = list(title = "Prevalence"),
                     margin = list(l = 50, r = 50, t = 50, b = 150)
      )
      return(plot)
      #})
    })
  })
}