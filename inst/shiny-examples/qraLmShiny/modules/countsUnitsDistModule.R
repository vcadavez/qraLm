## LM Concentration in the contaminated lots #
# Define the UI
countsUnitsDistUI <- function(id) {
  ns <- NS(id)
   plotlyOutput(ns("counts_units_dist"))
}  

# server
countsUnitsDistServer <- function(id, data) {
  moduleServer(id, function(input, output, session) {
  output$counts_units_dist <- renderPlotly({
   
    # assessment in CFU per gram in a contaminated serving (unit base)
    #index <- which(data()$N==0)
    CFU <- c(data()$N / data()$unitSize)  # compute cells/g per unit (concentration)
    CFU <- data.frame(CFU=CFU[CFU>0])
    plot1 <- ggplotly(
      ggplot(data = CFU, aes(x = "", y = CFU)) +
        geom_boxplot(color="blue", fill="blue", alpha=0.5, width=1.5) +
        theme_minimal() +
        coord_flip() +
        xlab("")
    )
    plot2 <- ggplotly(
      ggplot(CFU, aes(x=CFU, y=(..count..)/sum(..count..),
                      position = "identity", binwidth = 1, 
                      color = "blue")) +
        geom_histogram( color="blue", fill="grey", bins=10, binwidth = 0.25) +
        theme_classic() +
        xlab("CFU/g")
    )
    plot <- plotly::subplot(plot1, plot2, nrows = 2,
                            shareX = TRUE, titleX = TRUE)
    return(plot)
      })
   })
}