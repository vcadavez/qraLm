## LM Concentration in the contaminated lots
# Ui
logCountsDistUI <- function(id) {
  ns <- NS(id)
 
   plotlyOutput(ns("logCountsDist"), height = 'auto', width = 'auto')
  
}  

# server
logCountsDistServer <- function(id, data) {
  moduleServer(id, function(input, output, session) {
  
  output$logCountsDist <- renderPlotly({
    
    Nlot <- wtd.rowSums(x=data()$N,
                        wts=data()$ProbUnitPos, 
                        na.rm = TRUE)
    
    df <- data.frame(Counts=Nlot/(data()$sizeLot*data()$unitSize))
        
    
    plot1 <- ggplotly(
                ggplot(df, aes(y=log10(Counts))) +
                   theme_minimal() +
                   geom_boxplot(fill="blue", alpha=0.2, width=1.5) +
                   ylab('Counts (CFU/g)') +
                   xlab('')
                )
        
    
    plot2 <- ggplotly(
                ggplot(df, aes(x=log10(Counts))) +
                  theme_minimal() +
                  geom_histogram(color="blue", bins=25) +
                  xlab('Counts (CFU/g)') +
                  ylab('Frequency')
                )
        
  plotly::subplot(list(plot1, plot2))

      })
   })

}
