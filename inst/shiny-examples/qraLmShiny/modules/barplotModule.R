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
    
    bar <- ggplot2::ggplot(data=dat, aes(x=Stage, y=P)) +
              geom_bar(stat="identity", width=0.35, fill="steelblue") +
              theme_minimal() +
              xlab("") + 
              ylab("Prevalence") +
              theme_ipsum() +
              theme(legend.position="none")
    
    plot <- plotly::ggplotly(bar, width = 500, height = 500) |>
    plotly::layout(title = "",
                   xaxis = list(title = ""),
                   yaxis = list(title = "Prevalence"),
                   margin = list(l = 50, r = 50, t = 50, b = 150)
    )
    return(plot)
#   })
   })
  })
}