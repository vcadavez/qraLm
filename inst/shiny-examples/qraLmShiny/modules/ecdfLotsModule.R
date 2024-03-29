### Proportion of remaining contaminated lots
# define the Ui
ecdfLotsUI <- function(id) {
  ns <- NS(id)
   plotlyOutput(ns("ecdf_lots"), height = 'auto', width = 'auto')
  }  

# server
ecdfLotsServer <- function(id, data) {
  moduleServer(id, function(input, output, session) {
  
  output$ecdf_lots <- renderPlotly({
#    isolate({
#    cat("variability of contamination in lots (CFU/g)\n")
       
     if (exists("unitSize", data()) == TRUE) {
       lotN <- rowMeans(data()$N/data()$unitSize)
           } else{
             lotN <- rowMeans(data()$N)
           }
           
     if (exists("ProbUnitPos", data())==TRUE) {
     Clot <- lotN*data()$ProbUnitPos  # compute cells/g per lot (concentration)
     } else {
     Clot <- lotN 
     }
    
    df <- data.frame(Counts= Clot) # weighted cels/g per lot
    
    # logs function
    log_var <- function(x) {
      ifelse(x != 0, log10(x), 0)
    }
    
    df$logCounts <- log_var(df$Counts)
    p <- ggplot2::ggplot(df, aes(x=logCounts)) +
             theme_minimal() +
             stat_ecdf(geom = "step", col="blue", linewidth=1.5)
    
    plot <- plotly::ggplotly(p, width = 800, height = 500) |> 
        plotly::layout(title = "",
               xaxis = list(title = "Counts (log10 CFU/g)"),
               yaxis = list(title = "Cumulative Probability"),
               margin = list(l = 50, r = 50, t = 50, b = 150)
               )
      return(plot)
#    })
    
   })
  })
}
