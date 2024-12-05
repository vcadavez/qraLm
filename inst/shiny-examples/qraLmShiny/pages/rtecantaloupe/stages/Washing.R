ca_Washing_ui <- function(id) {
  ns <- NS(id)
  
  fluidRow(
    column(6, 
           h5("Prevalence of contaminated lots"), prevLotsUI("prev_lots_washing"),
           h5("Mean counts in contaminated lots"), mcstatsLotsUI("lots_mcstats_washing")
           #           h5("Distribution of between-lot mean counts"), countsLotsDistUI("counts_lots_dist_washing")
    ),
    column(6, 
           h5("Prevalence of contaminated units"), prevUnitsUI("prev_units_washing"),
           h5("Counts in contaminated units"), mcstatsUnitsUI("units_mcstats_washing")
           #           h5("Distribution of between-unit counts"), countsUnitsDistUI("counts_units_dist_washing")
    ),
    column(12, 
           h5("Empirical Cumulative Distribution Function (ECDF)"), ecdfLotsUI("ecdf_prob_washing")
    )
  )
}

ca_Washing_server <- function(input, output, session, suffix, datStoring) {
  ns <- NS(suffix)
  id <- ns("Washing")
  
  output[[id]] <- renderUI({ ca_Washing_ui(id) })
  
  prefix <- "rtecantaloupe-sidebar-inputs-"
  datWashing <- reactive({ generate_datWashing(input, prefix, datStoring) })
  
  prevLotsServer("prev_lots_washing",                data=datWashing)
  prevUnitsServer("prev_units_washing",              data=datWashing)
  mcstatsLotsServer("lots_mcstats_washing",          data=datWashing)
  mcstatsUnitsServer("units_mcstats_washing",        data=datWashing)
  countsLotsDistServer("counts_lots_dist_washing",   data=datWashing)
  countsUnitsDistServer("counts_units_dist_washing", data=datWashing)
  ecdfLotsServer("ecdf_prob_washing",                data=datWashing)
  return(datWashing)
}

generate_datWashing <- function(input, prefix, datStoring) {
  set.seed(get_input_value(input, prefix, "seed"))
  df <- caFlumeTankCC(
    datStoring(),
    probCCW      = get_input_value(input, prefix, "prob_ccw"),
    logWaterMin  = get_input_value(input, prefix, "log_water_min"),
    logWaterMode = get_input_value(input, prefix, "log_water_mode"),
    logWaterMax  = get_input_value(input, prefix, "log_water_max"),
    pWaterGain   = get_input_value(input, prefix, "p_water_gain"),
    bWater       = get_input_value(input, prefix, "b_water")
    )
  return(df)
}

ca_WashingInputs_ui <- function(id) {
  ns <- NS(id)
  div(
  id = ns("Washing"),
#  tagList(
    sliderInput(ns("prob_ccw"),
                "probCCW: Probability that water of flume tank is contaminated",
                value = 0, min = 0, max = 1, step=0.1),
    sliderInput(ns("log_water_min"),
                "logWaterMin: Minimal concentration of LM in water of flume tank",
                value = 1, min = 0, max = 2, step=0.25),
    sliderInput(ns("log_water_mode"),
                "logWaterMode: Mode concentration of LM in water of flume tank",
                value = 1, min = 0, max = 2, step=0.25),
    sliderInput(ns("log_water_max"),
                "logWaterMax: Maximum concentration of LM in water of flume tank",
                value = 5, min = 2, max = 10, step=0.25),
    sliderInput(ns("p_water_gain"),
                "pWaterGain: Fraction of water gain (ml) relative to the cantaloupe weight (g)",
                value = 0.004, min = 0.0, max = 0.009, step=0.001),
    sliderInput(ns("b_water"),
                "bWater: Dispersion factor - clustering of cells during washing)",
                value = 1, min = 0, max = 2, step=0.2)
#    )
   )
}
