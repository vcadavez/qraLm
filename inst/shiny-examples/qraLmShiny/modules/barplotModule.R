### Proportion of remaining contaminated lots
# Ui
barplotUI <- function(id) {
  ns <- NS(id)
   plotlyOutput(ns("barPlot"), height = 'auto', width = 'auto')
} 

# server
barplotServer <- function(id, data, Stage) {
  moduleServer(id, function(input, output, session) {
  
  output$barPlot <- renderPlotly({
#    isolate({
    # make data
    dat <- data.frame(
                      Stage=Stage, 
                      P=data()$P
                      )
    
    bar <- ggplot(data=dat, aes(x=Stage, y=P)) +
              geom_bar(stat="identity", width=0.35, fill="steelblue") +
              theme_minimal() +
              xlab("") + 
              ylab("Prevalence") +
              theme_ipsum() +
              theme(legend.position="none")
    
    plot <- ggplotly(bar)
    return(plot)
#   })
   })
  })
}