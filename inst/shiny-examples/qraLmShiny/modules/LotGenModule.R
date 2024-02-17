# define th Ui
LotGenUI <- function(id) {
  ns <- NS(id)
  
    tagList(
      # UI elements for user input (e.g., sliders, checkboxes, etc.)
      sliderInput(ns("n_lots"),     "Number of lots",
                  value = 1000, min = 1000, max = 5000, step=500),
      sliderInput(ns("size_lot"),     "Lot size (Number of units)",
                  value = 1000, min = 1000, max = 5000, step=500),
      sliderInput(ns("unit_size"),    "Unit size (g):", 
                  value = 500, min = 100, max = 1000, step=100),
      sliderInput(ns("prev"),   "prevalence of contaminated lots", 
                  value = 0.057, min = 0.0, max = 1.0, step=0.10),
      sliderInput(ns("log_mean_c"),     "Mean of Counts (log10 CFU/g)", 
                  value = 1.023, min = 0.10, max = 5.000, step=0.100),
      sliderInput(ns("log_sd_c"),       "St. dev. of Counts (log10 CFU/g)",
                  value = 0.3267, min = 0.00, max = 2.00, step=0.100)  )
}

# define the server
LotGenServer <- function(id) {
  moduleServer(id, function(input, output, session) {

observeEvent( input$simulate , {
  
  df <- reactive({
    df <- LotGen(nLots     = input$n_lots,
                 sizeLot   = input$size,
                 unitSize  = input$unit_size,
                 P         = input$prev,
                 C0MeanLog = input$log_mean_c,
                 C0SdLog   = input$log_sd_c)
    })
  return(df)
  })
    
})
}
