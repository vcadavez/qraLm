ca_Testing_ui <- function(id) {
  ns <- NS(id)
  
  fluidRow(
    column(6, 
           h4("Prevalence of contaminated lots"), prevLotsUI("prev_lots_testing"),
           h4("Between lots LM counts"), mcstatsLotsUI("lots_mcstats_testing")
           #           h4("Between lots LM counts distribution"), countsLotsDistUI("counts_lots_dist_testing")
    ),
    column(6, 
           h4("Prevalence of contaminated units"), prevUnitsUI("prev_units_testing"),
           h4("Within lots/Between units LM counts"), mcstatsUnitsUI("units_mcstats_testing")
           #           h4("Within lots/Between units distribution"), countsUnitsDistUI("counts_units_dist_testing")
    ),
    column(12, 
           h4("ECDF plot"), ecdfLotsUI("ecdf_prob_testing")
    )
  )
}

ca_Testing_server <- function(input, output, session, suffix, datPartitioning) {
  ns <- NS(suffix)
  id <- ns("Testing")
  
  output[[id]] <- renderUI({ ca_Testing_ui(id) })
  
  prefix <- "rtecantaloupe-sidebar-inputs-"
  datTesting <- reactive({ generate_datTesting(input, prefix, datPartitioning) })
  
  prevLotsServer("prev_lots_testing",                data=datTesting)
  prevUnitsServer("prev_units_testing",              data=datTesting)
  mcstatsLotsServer("lots_mcstats_testing",          data=datTesting)
  mcstatsUnitsServer("units_mcstats_testing",        data=datTesting)
  countsLotsDistServer("counts_lots_dist_testing",   data=datTesting)
  countsUnitsDistServer("counts_units_dist_testing", data=datTesting)
  ecdfLotsServer("ecdf_prob_testing",                data=datTesting)
  return(datTesting)
}

generate_datTesting <- function(input, prefix, datPartitioning) {
  set.seed(get_input_value(input, prefix, "seed"))
  df <- caTesting(
    datPartitioning(),
    nTested     = get_input_value(input, prefix, "n_tested"),
    gTested	    = get_input_value(input, prefix, "g_tested"),
    MTested     = get_input_value(input, prefix, "m_tested"),
    cTested     = get_input_value(input, prefix, "c_tested"),
    pLotTested  = get_input_value(input, prefix, "p_lot_tested"),
    Se          = get_input_value(input, prefix, "se"),
#    unitSize    = get_input_value(input, prefix, "unit_size"),
#    sizeLot     = get_input_value(input, prefix, "size_lot"),
    gTestedEnum = get_input_value(input, prefix, "g_tested_enum"),
    backToSublot = TRUE,
    iterSub=NULL
    )
  return(df)
}

ca_TestingInputs_ui <- function(id) {
  ns <- NS(id)
  div(
  id = ns("Testing"),
#  tagList(
    sliderInput(ns("n_tested"),
                "nTested: Sample size or number of units tested",
                value = 10, min = 0.0, max = 20, step=5),
    sliderInput(ns("g_tested"),
                "gTested: Sample weight tested per unit",
                value = 10, min = 5, max = 50, step=5),
    sliderInput(ns("m_tested"),
                "mTested: Maximum concentration accepted in a sample",
                value = 2, min = 0, max = 5, step=1),
    sliderInput(ns("c_tested"),
                "cTested: Maximum number of samples accepted between",
                value = 1, min = 0, max = 10, step=1),
    sliderInput(ns("p_lot_tested"),
                "pLotTested: Proportion of lots subjected to sampling and testing",
                value = 0.1, min = 0, max = 1, step=0.1),
    sliderInput(ns("se"),
                "Se: Sensibility of the test or probability to detect",
                value = 0.1, min = 0, max = 1, step=0.050),
    sliderInput(ns("g_tested_enum"),
                "gTestedEnum: Sample weight tested for enumeration",
                value = 10, min = 5, max = 20, step=5)
#    )
  )
}