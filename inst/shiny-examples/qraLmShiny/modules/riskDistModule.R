# UI
riskDistUI <- function(id) {
  ns <- NS(id)
  plotlyOutput(ns("risk_dist"))
}

# Server
riskDistServer <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    output$risk_dist <- renderPlotly({

      wRiskLotMean <- data()$lotMeanRisk
      
      wRiskLotMeanlog <- log10(wRiskLotMean)
      df <- data.frame(risklog = wRiskLotMeanlog)
      
      box <- plotly::plot_ly(x = ~df$risklog, 
                             type = "box", name="log10 Risk")
      histo <- plotly::plot_ly(x = ~df$risklog, 
                               type = "histogram",
                               histnorm = "probability")
      plot <- plotly::subplot(box, histo,
                              nrows = 2,
                              heights = c(0.3, 0.7),
                              shareX = TRUE, shareY = TRUE) |>
        plotly::layout(
          title = "", 
          showlegend = FALSE,
          xaxis = list(title = "log10 Risk"),
          yaxis = list(
            title = "Probability",
            titlefont = list(
                             size = 14,      # Adjust the font size
                             color = "black" # Adjust the font color
                             ),
            tickfont = list(
              size = 12,      # Adjust the tick font size
              color = "black" # Adjust the tick font color
            )
          ),
          margin = list(l = 50, r = 50, t = 50, b = 150)
        )
      return(plot)
    })
  })
}
