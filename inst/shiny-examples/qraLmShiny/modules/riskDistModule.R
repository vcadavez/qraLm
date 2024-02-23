# Ui
riskDistUI <- function(id) {
  ns <- NS(id)
    plotlyOutput(ns("risk_dist"))
}  

# server
riskDistServer <- function(id, data) {
  moduleServer(id, function(input, output, session) {
  
    output$risk_dist <-  renderPlotly ({
      
      wRiskLotMean <- matrixStats::rowWeightedMeans(data()$Risk, W = data()$ProbUnitPos)

      log_risk = function(x){
        ifelse(x!=0, log10(x), 0)
      }
      
      df <- data.frame(risklog=log_risk(wRiskLotMean))
      
      # axis styles
      ax <- list(
                 title = "",
                 zeroline = FALSE,
                 showline = FALSE,
                 showticklabels = TRUE,
                 showgrid = FALSE
                 )
      
      aax <- list(
                 title = "",
                 zeroline = FALSE,
                 showline = FALSE,
                 showticklabels = FALSE,
                 showgrid = FALSE
                 )
      # box on top / hist bottom

      box <-  plotly::plot_ly(df, 
                              x = ~risklog, 
                              type = "box", 
                              name = "log10 Risk",
                              width = 500,
                              height = 500) %>%
        plotly::layout(showlegend=FALSE,
                       xaxis = ax,
                       yaxis = ax)
      
      histo <- plotly::plot_ly(df,
                               x = ~risklog, 
                               type = "histogram",
                               histnorm = "probability",
                               name = "log10 Risk",
                               width = 500, 
                               height = 500) %>%
        plotly::layout(showlegend = FALSE, 
                       xaxis = ax, 
                       yaxis = ax)
      plot <- plotly::subplot(box, histo,
                              nrows = 2,
                              shareX = TRUE) |>
        plotly::layout(title = "",
                       xaxis = list(title = "log10 Risk"),
                       yaxis = list(title = "Probability"),
                       margin = list(l = 50, r = 50, t = 50, b = 150)
                      )
        return(plot)
        })
  })
}