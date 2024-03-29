sf_Production_ui <- function(id) {
  fluidRow(
    column(6,
           h4("Prevalence of contaminated lots"), prevLotsUI("sf_prev_lots"),
           h4("Between lots LM counts"), mcstatsLotsUI("sf_lot_stats"),
           #      h4("Between lots Counts distribution (CFU/g)"), countsLotsDistUI("sf_lot_counts")
           h4("Variability in the prevalence of contaminated lots"), betaDistUI("sf_beta")
    ),
    column(6,
           h4("Prevalence of contaminated units"), prevUnitsUI("sf_prev_units"),
           h4("Within lots/Between units LM counts"), mcstatsUnitsUI("sf_unit_stats"),
           #      h4("Counts distribution per unit (CFU/g)"), countsUnitsDistUI("sf_unit_counts")
           h4("Variability in within-lot prevalence"), prevWithinlotsUI("sf_within_lots")
    ),
    column(12,
           h4("ECDF Plot"), ecdfLotsUI("sf_ecdf")
    )
  )
}

sf_Production_server <- function(input, output, session, suffix) {
  ns <- NS(suffix)
  id <- ns("Production")
  output[[id]] <- renderUI({ sf_Production_ui(id) })
  
  prefix <- "smokedfish-sidebar-inputs-"
  datFish <- reactive({ generate_datFish(input, prefix) })
  datBeta <- reactive({ generate_datBeta(input, prefix) })
  
  prevLotsServer("sf_prev_lots", data = datFish)
  prevUnitsServer("sf_prev_units", data = datFish)
  mcstatsLotsServer("sf_lot_stats", data = datFish)
  mcstatsUnitsServer("sf_unit_stats", data = datFish)
  countsLotsDistServer("sf_lot_counts", data = datFish)
  countsUnitsDistServer("sf_unit_counts", data = datFish)
  betaDistServer("sf_beta", data = datBeta)
  prevWithinlotsServer("sf_within_lots", data = datFish)
  ecdfLotsServer("sf_ecdf", data = datFish)
  
  return(datFish)
}

generate_datFish <- function(input, prefix) { #first stage, no input
  df <- Lot2LotGen(
    nLots        = get_input_value(input, prefix, "n_lots"),
    sizeLot      = get_input_value(input, prefix, "size_lot"),
    unitSize     = get_input_value(input, prefix, "unit_size_prod"),
    betaAlpha    = get_input_value(input, prefix, "beta_alpha"),
    betaBeta     = get_input_value(input, prefix, "beta_beta"),
    C0MeanLog    = get_input_value(input, prefix, "log_mean_c"),
    C0SdLog      = get_input_value(input, prefix, "log_sd_c"),
    propVarInter = get_input_value(input, prefix, "prop_var_inter"),
    Poisson      = get_input_value(input, prefix, "poisson")
  )
  return(df)
}

generate_datBeta <- function(input, prefix) {
    prev <- seq(0, 1, length = get_input_value(input, prefix, "size_lot"))
    prob <- stats::dbeta(prev, 
                         shape1 = get_input_value(input, prefix, "beta_alpha"), 
                         shape2 = get_input_value(input, prefix, "beta_beta"))
  data.frame(prev = prev, prob = prob)
}

sf_ProductionInputs_ui <- function(id) {  
  ns <- NS(id)  
   div(  
   id = ns("Production"),  
#  tagList(  
    numericInput(ns("seed"), 
                 "Set a random seed", value = 12345),  
    numericInput(ns("n_lots"), 
                 "nLots: number of lots",
                 value = 1000,  min = 1000, max = 10000, step = 500),
    numericInput(ns("size_lot"), 
                 "sizeLot: number of fishes",
                 value = 500,  min = 500, max = 5000, step = 500),
    numericInput(ns("unit_size_prod"),
                 "unitSize: size of the fish (g)",
                 value = 4000,  min = 3000, max = 6000, step = 500),
    sliderInput(ns("beta_alpha"), 
                "betaAlpha: Beta distribution parameter", 
                value = 0.874, min = 0.0, max = 3.0, step = 0.001),  
    sliderInput(ns("beta_beta"), 
                "betaBeta: Beta distribution parameter",
                value = 5.88, min = 0.0, max = 15.0, step = 0.01),  
    sliderInput(ns("prop_var_inter"),
                "propVarInter: between-lot variance (%)", 
                value = 0.5, min = 0.0, max = 1.0, step = 0.1),  
    sliderInput(ns("log_mean_c"), 
                "C0MeanLog: mean of counts (log10 CFU/g)",
                value = -1.0, min = -2, max = 2, step = 0.25),  
    sliderInput(ns("log_sd_c"),
                "C0SdLog: std of counts (log10 CFU/g)",
                value = 1.0, min = 0, max = 2.00, step = 0.10),
    checkboxInput(ns("poisson"), 
                  "Poisson: Poisson distribuition", value = TRUE)
#    ) 
   )
}
