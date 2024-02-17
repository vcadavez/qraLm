## plot distribution
# Ui
histUI <- function(id) {
  ns <- NS(id)
  plotOutput(ns("histograms"),  width = "100%", height = "600px")
}  

# server
histServer <- function(id, data) {
  moduleServer(id, function(input, output, session) {
  
  output$histograms <- renderPlot({
   
    df <- as.data.frame(do.call(cbind, data()))
    names(df) <- c("NaCl", "pH", "P", "CO2equi", "NIT", "aaWph", 
                   "baWph", "caWph", "daWph", "laWph", "saWph")
    df$NIT <- NULL
    df$aaWph <- NULL
    df$baWph <- NULL
    df$caWph <- NULL
    df$saWph <- NULL 

    par(mfrow=c(3,2), mar=c(2, 4, 2, 2)) # 12 columns
    
    plotDataFrame <- function(dataFrame){
      df <- dataFrame
      ln <- length(names(dataFrame))
      for(i in 1:ln){
        mname <- substitute(df[,i])
        if(is.factor(df[,i])){
          plot(df[,i], main=names(df)[i])}
        else{hist(df[,i],
                  main=names(df)[i], 
                  freq=FALSE,
                  xlim=c(min(df[,i]), max(df[,i])),
                  plot=TRUE,
                  breaks = "Freedman-Diaconis", col = "blue", border = "brown")}
      }
    }
    plotDataFrame(df)
#    return(plot)
    })
  })
}