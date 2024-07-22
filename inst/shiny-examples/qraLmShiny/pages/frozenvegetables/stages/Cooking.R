fv_Cooking_ui <- function(id) {
  ns = NS(id)

  fluidPage(
#    h3("Variability of contamination in lots"),
    fluidRow(
    column(6, 
           h5("Prevalence of contaminated lots"), prevLotsUI("fv_prev_lots_cook"),
           h5("Mean counts in contaminated lots"), mcstatsLotsUI("fv_lots_mcstats_cook")#,
#           h5("Distribution of between-lot mean counts"), countsLotsDistUI("fv_counts_lots_dist_cook"),
           ),
    column(6, 
           h5("Prevalence of contaminated units"), prevUnitsUI("fv_prev_units_cook"),
           h5("Counts in contaminated units"), mcstatsUnitsUI("fv_units_mcstats_cook"),
#           h5("Distribution of between-unit counts"), countsUnitsDistUI("fv_counts_units_dist_cook")
           ),
    column(12, 
           h5("Cumulative distribution of mean counts in contaminated lots"), ecdfLotsUI("fv_ecdf_prob_cook")
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
                value=1.0, min=0.00, max=1.00, step=0.05),
    sliderInput(ns("min_cook"), "minCook: Minimum LM log reduction", 
                value=1, min=0, max=5, step=0.25),
    sliderInput(ns("mode_cook"), "modeCook: Mode of LM log reduction",
                value=5, min=2, max=8, step=0.25),
    sliderInput(ns("max_cook"), "maxCook: Maximum LM log reduction",  
                value=9, min=5, max=9, step=0.25) 
#    )
  )
}



