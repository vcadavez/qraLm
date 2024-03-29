fv_Testing_ui <- function(id) {
  ns = NS(id)
  fluidPage(
#    h3("Variability of contamination in lots"),
    
      fluidRow(
           column(6,
                  h4("Prevalence of contaminated lots"), prevLotsUI("fv_prev_lots_test"),
                  h4("Between lots LM counts"), mcstatsLotsUI("fv_lots_mcstats_test")
                  #                  h4("Between lots LM Counts distribution"), countsLotsDistUI("fv_counts_lots_dist_test")
                  ),
           column(6,
                  h4("Prevalence of contaminated units"), prevUnitsUI("fv_prev_units_test"),
                  h4("Within lots/Between units LM counts"),  mcstatsUnitsUI("fv_units_mcstats_test")
                  #                  h4("Within lots/Between units LM counts distribution"), countsUnitsDistUI("fv_counts_units_dist_test")
           ),
           column(12,
                  h4("Empirical Cumulative Distribution Function (ECDF)"), ecdfLotsUI("fv_ecdf_prob_test")
                  )
           )
  )
  }

fv_Testing_server <- function(input, output, session, suffix, datPart) {
  ns <- NS(suffix)
  id <- ns("Testing")

  output[[id]] <- renderUI({ fv_Testing_ui(id) })
  
  prefix <- "frozenvegetables-sidebar-inputs-"
  datTest <- reactive({ generate_datTest(input, prefix, datPart) })
  
  
  prevLotsServer("fv_prev_lots_test",                     data=datTest)
  prevUnitsServer("fv_prev_units_test",                   data=datTest)
  mcstatsLotsServer("fv_lots_mcstats_test",          data=datTest)
  mcstatsUnitsServer("fv_units_mcstats_test",        data=datTest)
  countsLotsDistServer("fv_counts_lots_dist_test",   data=datTest)
  countsUnitsDistServer("fv_counts_units_dist_test", data=datTest)
  ecdfLotsServer("fv_ecdf_prob_test",                     data=datTest)
  return(datTest)
}

generate_datTest <- function(input, prefix, datPart) {
  set.seed(get_input_value(input, prefix, "seed"))
  df <- fvTesting(
                  datPart(),
                  nTested = get_input_value(input, prefix, "n_tested"),
                  gTested = get_input_value(input, prefix, "g_tested"),
                  MTested = get_input_value(input, prefix, "m_tested"),
                  cTested = get_input_value(input, prefix, "c_tested"),
                  pLotTested = get_input_value(input, prefix, "p_lot_tested"),
                  Se = get_input_value(input, prefix, "se"),
                  gTestedEnum = get_input_value(input, prefix, "g_tested_enumeration"),
                  iterSub = NULL
  )
  return(df)
}

fv_TestingInputs_ui <- function(id) {
  ns <- NS(id)
  div(
  id = ns("Testing"),
#  tagList(
    sliderInput(ns("n_tested"), 
                "nTested: Sample size", value=5, min=1, max=10, step=1),
    sliderInput(ns("g_tested"),
                "gTested: Sample weight tested (g)", value=25, min=25, max=100, step=5),
    sliderInput(ns("m_tested"), 
                "mTested: Maximum concentration accepted (CFU/g)", value=0, min=0, max=1, step=0.5),
    sliderInput(ns("c_tested"), 
                "cTested: Maximum number of samples", value=0, min=0, max=20, step=5),
    sliderInput(ns("p_lot_tested"), 
                "pLotTested: Proportion of lots tested", value=0, min=0, max=1, step=0.1),
    sliderInput(ns("se"), 
                "Se: Sensibility of the test", value=1, min=0, max=1.0, step=0.10),
    sliderInput(ns("g_tested_enumeration"),
                "gTestedEnum: Sample weight tested for enumeration (g)", value=10, min=10, max=100, step=10),
# )
)
}

