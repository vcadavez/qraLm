## LM Concentration in the contaminated lots (theoretical line)
# Ui
betaDistUI <- function(id) {
  ns <- NS(id)
  plotlyOutput(ns("betadist"), height = 'auto', width = 'auto')
}  

# server
betaDistServer <- function(id, data) {
  moduleServer(id, function(input, output, session) {
  
  output$betadist <- renderPlotly({
   
    #isolate({
      
     p <- ggplot(data(), aes(x=prev, y=prob)) +
            theme_minimal() +
            geom_line( col="blue") +
            labs(x = "Prevalence", y = "Probability")
     
              
     plot <- ggplotly(p)
      
      return(plot)
      
      })
    #})
  })

}
