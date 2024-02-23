fv_Production_ui <- function(id) {
  fluidRow(
    column(6,
           h4("Prevalence of contaminated lots"), prevLotsUI("fv_prev_lots_prod"),
           h4("Between lots LM counts"), mcstatsLotsUI("fv_lots_mcstats_prod"),
           h4("Between lots Counts distribution"), countsLotsDistUI("fv_counts_lots_dist_prod"),
           h4("Variability between-lot prevalence"), betaDistUI("fv_beta_dist")
    ),
    column(6,
           h4("Prevalence of contaminated units"), prevUnitsUI("fv_prev_units_prod"),
           h4("Within lots/Between units LM counts"), mcstatsUnitsUI("fv_units_mcstats_prod"),
           h4("Within lots/Between units LM counts distribution"), countsUnitsDistUI("fv_counts_units_dist_prod"),
           h4("Variability in within-lot prevalence"), prevWithinlotsUI("fv_prob_dist")
    ),
    column(12, h4("Empirical Cumulative Distribution Function (ECDF)"), ecdfLotsUI("fv_ecdf_prob"))
  )
}

fv_Production_server <- function(input, output, session, suffix) {
  ns <- NS(suffix)
  id <- ns("Production")
  output[[id]] <- renderUI({ fv_Production_ui(id) })
  
  prefix <- "frozenvegetables-sidebar-inputs-"
  datProd <- reactive({ generate_datProd(input, prefix) })
  datBeta <- reactive({ generate_datBeta(input, prefix) })
  
  prevLotsServer("fv_prev_lots_prod", data=datProd)
  prevUnitsServer("fv_prev_units_prod", data=datProd)
  mcstatsLotsServer("fv_lots_mcstats_prod", data=datProd)
  mcstatsUnitsServer("fv_units_mcstats_prod", data=datProd)
  countsLotsDistServer("fv_counts_lots_dist_prod", data=datProd)
  countsUnitsDistServer("fv_counts_units_dist_prod", data=datProd)
  betaDistServer("fv_beta_dist", data=datBeta)
  prevWithinlotsServer("fv_prob_dist", data=datProd)
  ecdfLotsServer("fv_ecdf_prob", data=datProd)
  
  return(datProd)
}

generate_datProd <- function(input, prefix) { #first stage, no input
  df <- Lot2LotGen(
    nLots        = get_input_value(input, prefix, "n_lots"),
    sizeLot      = get_input_value(input, prefix, "size_lot"),
    unitSize     = get_input_value(input, prefix, "unit_size"),
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

fv_ProductionInputs_ui <- function(id) {  
  ns <- NS(id)  
  div(  
  id = ns("Production"),  
#  tagList(  
    numericInput(ns("seed"), "Set a random seed", value = 12345),  
    numericInput(ns("n_lots"), "nLots: Number of lots",
                 value = 5000,  min = 1000, max = 10000, step = 1000),
    numericInput(ns("size_lot"), "sizeLot: Number of units",
                 value = 1000,  min = 1000, max = 5000, step = 500),
    numericInput(ns("unit_size"), "unitSize: Size of the units (g)",
                 value = 500,  min = 500, max = 5000, step = 100),
    sliderInput(ns("beta_alpha"), "betaAlpha: Alpha parameter of the Beta distribution", 
                value = 0.5112, min = 0.1, max = 3.0, step = 0.10),  
    sliderInput(ns("beta_beta"), "betaBeta: Beta parameter of the Beta distribution", 
                value = 1.9918, min = 0.1, max = 3.0, step = 0.10),  
    sliderInput(ns("prop_var_inter"), "propVarInter: Prop. of between-lot variance (%)", 
                value = 0.7, min = 0.0, max = 1.0, step = 0.1),  
    sliderInput(ns("log_mean_c"), "C0MeanLog: Mean of Counts (log10 CFU/g)",
                value = 1.023, min = -2, max = 2, step = 0.25),  
    sliderInput(ns("log_sd_c"), "C0SdLog: St. dev. of Counts (log10 CFU/g)", 
                value = 0.3267, min = 0, max = 2.00, step = 0.10),
    checkboxInput(ns("poisson"), label = "Poisson: Definition??", value = FALSE)
#  )  
 )   
}