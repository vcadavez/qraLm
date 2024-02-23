### Proportion of remaining contaminated lots
# Ui
boxplotUI <- function(id) {
  ns <- NS(id)
   plotlyOutput(ns("boxPlot"), height = 'auto', width = 'auto')
}  

# server
boxplotServer <- function(id, data) {
  moduleServer(id, function(input, output, session) {
  
  output$boxPlot <- renderPlotly({
#    isolate({
    lotsSums <- rowSums(data()$N)
    weights <- data()$ProbUnitPos
    
    df <- data.frame(Counts=ceiling(wtd.rowSums(x=lotsSums, wts=weights, na.rm = TRUE)))
    
    log_risk = function(x){
      ifelse(x!=0, log10(x), 0)
    }
    
    df$logCounts <- log_risk(df$Counts)
    
    p <- ggplot2::ggplot(df, 
            aes(y=Counts)) +
            theme_minimal() +
            geom_boxplot()
      
    plot <- plotly::ggplotly(p, width = 500, height = 500)  |>
      plotly::layout(title = "",
                     xaxis = list(title = ""),
                     yaxis = list(title = "Counts"),
                     margin = list(l = 50, r = 50, t = 50, b = 150)
      )
    
      
    return(plot)
      
#    })
    
   })
  })
}