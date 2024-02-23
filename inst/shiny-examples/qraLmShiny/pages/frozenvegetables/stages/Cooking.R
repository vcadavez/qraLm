fv_Cooking_ui <- function(id) {
  ns = NS(id)

  fluidPage(
#    h3("Variability of contamination in lots"),
    fluidRow(
    column(6, 
           h4("Prevalence of contaminated lots"), prevLotsUI("fv_prev_lots_cook"),
           h4("Between lots LM counts"), mcstatsLotsUI("fv_lots_mcstats_cook")
#           h4("Between lots LM counts distribution"), countsLotsDistUI("fv_counts_lots_dist_cook"),
           ),
    column(6, 
           h4("Prevalence of contaminated units"), prevUnitsUI("fv_prev_units_cook"),
           h4("Within lots/Between units LM counts"), mcstatsUnitsUI("fv_units_mcstats_cook")
#           h4("Within lots/Between units LM Counts distribution"), countsUnitsDistUI("fv_counts_units_dist_cook")
           ),
    column(12, 
           h4("Empirical Cumulative Distribution Function (ECDF)"), ecdfLotsUI("fv_ecdf_prob_cook")
           )
    )
    )
}

fv_Cooking_server <- function(input, output, session, suffix, datDefrost) {
  ns <- NS(suffix)
  id <- ns("Cooking")

  output[[id]] <- renderUI({ fv_Cooking_ui(id) })
  
  prefix <- "frozenvegetables-sidebar-inputs-"
  datCook <- reactive({ generate_datCook(input, prefix, datDefrost) })
  
  prevLotsServer("fv_prev_lots_cook",                data=datCook)
  prevUnitsServer("fv_prev_units_cook",              data=datCook)
  mcstatsLotsServer("fv_lots_mcstats_cook",          data=datCook)
  mcstatsUnitsServer("fv_units_mcstats_cook",        data=datCook)
  countsLotsDistServer("fv_counts_lots_dist_cook",   data=datCook)
  countsUnitsDistServer("fv_counts_units_dist_cook", data=datCook)
  ecdfLotsServer("fv_ecdf_prob_cook",                data=datCook)
  return(datCook)
}

generate_datCook <- function(input, prefix, datDefrost) {
  set.seed(get_input_value(input, prefix, "seed"))
  df <- fvCooking(
                 datDefrost(),
                 pCooked = get_input_value(input, prefix, "p_cooked"),
                 minCook = get_input_value(input, prefix, "min_cook"),
                 modeCook = get_input_value(input, prefix, "mode_cook"),
                 maxCook = get_input_value(input, prefix, "max_cook")
  )
  return(df)
}

fv_CookingInputs_ui <- function(id) {
  ns <- NS(id)
  div(
  id = ns("Cooking"),
#  tagList(
    sliderInput(ns("p_cooked"), "pCooked: Probability of cooking",      
                value=0.40, min=0.00, max=1.00, step=0.10),
    sliderInput(ns("min_cook"), "minCook: Minimum cooking time (min)", 
                value=1, min=0, max=10, step=0.25),
    sliderInput(ns("mode_cook"), "modeCook: Mode of cooking time (min)",
                value=5, min=0, max=10, step=0.25),
    sliderInput(ns("max_cook"), "maxCook: Maximum cooking time (min)",  
                value=9, min=0, max=20, step=0.25) 
#    )
  )
}



