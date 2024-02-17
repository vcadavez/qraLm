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
    
    df <- data.frame(Counts=log10(ceiling(wtd.rowSums(x=lotsSums,
                                                   wts=weights, na.rm = TRUE))))
    p <- ggplot(df, 
            aes(y=Counts)) +
            theme_minimal() +
            geom_boxplot()
      
    plot <- ggplotly(p)
      
    return(plot)
      
#    })
    
   })
  })
}