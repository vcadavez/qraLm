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
      
     p <- ggplot2::ggplot(data(), aes(x=prev, y=prob)) +
            theme_minimal() +
            geom_line( col="blue" ) +
            labs(x = "Prevalence", y = "Probability")
     
              
     plot <- plotly::ggplotly(p, width = 500, height = 500)  |>
       plotly::layout(title = "",
                      xaxis = list(title = "Probability"),
                      yaxis = list(title = "Prevalence"),
                      margin = list(l = 50, r = 50, t = 50, b = 150)
       )
      
      return(plot)
      
      })
    #})
  })

}
