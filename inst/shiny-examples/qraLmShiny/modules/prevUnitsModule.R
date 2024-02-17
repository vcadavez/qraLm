## Prevalence of contaminated units

# define th Ui
prevUnitsUI <- function(id) {
  ns <- NS(id)
  verbatimTextOutput(ns("prev_units"))
}  

# server
 
prevUnitsServer <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    
    output$prev_units <-  renderPrint({
#   cat("Prevalence of contaminated units\n")
      
          P <- mean( data()$P * rowMeans(data()$N != 0))
          return(P)
      })
    })
}
