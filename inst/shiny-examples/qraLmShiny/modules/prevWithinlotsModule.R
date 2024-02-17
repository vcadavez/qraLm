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
           geom_histogram(fill="brown", bins=20) + 
           labs(x = "Prevalence", y = "Frequency")
      
      
      plot <- ggplotly(p)
      return(plot)
      #})
    })
  })
}