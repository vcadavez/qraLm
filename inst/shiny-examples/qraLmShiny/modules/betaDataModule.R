## LM Concentration in the contaminated lots (theoretical line)

# Ui
betaDataUI <- function(id) {
  ns <- NS(id)
 }  

# server
betaDataServer <- function(id) {
  moduleServer(id, function(input, output, session) {
  
       df <- reactive({
         prev = seq(0, 1, length=input$size_lot)
         prob = stats::dbeta(seq(0, 1, length=input$size_lot), 
                                         shape1=input$beta_alpha, 
                                         shape2=input$beta_beta)
         df <- data.frame(
                          prev=prev,
                          prob=prob)
       })
      return(df)
      })
  }
