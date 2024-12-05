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
      if (exists("ProbUnitPos", data()) == TRUE) {
        prevUnits <- stats::weighted.mean(data()$ProbUnitPos * (data()$N != 0),
                             w=rep(1,length(data()$N)))
      } else {
        probunitpos <- rep(1, nrow(data()$N))
        prevUnits <- stats::weighted.mean(probunitpos * (data()$N != 0),
                                     w=rep(1,length(data()$N)))
      }
      return(prevUnits)
    })
    
  })
}
