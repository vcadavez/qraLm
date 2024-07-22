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
    
    Nlot <- data()$lotMeans

    df <- data.frame(Counts=Nlot)
    
    log_risk = function(x){
      ifelse(x!=0, log10(x), 0)
    }
    
    df$logCounts <- log_risk(df$Counts)
    
    plot1 <- ggplot2::ggplot(df, aes(y=logCounts)) +
                   theme_minimal() +
                   geom_boxplot(fill="blue", alpha=0.2, width=1.5) +
                   ylab('Counts (CFU/g)') +
                   xlab('')

    plot2 <- ggplot2::ggplot(df, aes(x=logCounts)) +
                  theme_minimal() +
                  geom_histogram(color="blue", bins=25) +
                  xlab('Counts (CFU/g)') +
                  ylab('Frequency')
    
    plot1 <- ggplotly(plot2, width = 500, height = 500)
    plot2 <- ggplotly(plot2, width = 500, height = 500)

    plotly::subplot(plot1, plot2,
                    nrows = 2,
                    shareX = TRUE) |>
     plotly::layout(title = "",
                   xaxis = list(title = "Counts (CFU/g)"),
                   yaxis = list(title = "Frequency"),
                   margin = list(l = 50, r = 50, t = 50, b = 150)
    )

      })
   })

}
