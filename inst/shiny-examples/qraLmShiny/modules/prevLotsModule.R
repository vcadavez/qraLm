## Summary statistics of contaminated lots 

# define th Ui
prevLotsUI <- function(id) {
  ns <- NS(id)
  verbatimTextOutput(ns("prev_lots"))
}  

# server
 
prevLotsServer <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    
    output$prev_lots <-  renderPrint({
#      cat("Prevalence of contaminated lots\n")

      return(data()$P)
    })
    })
}
