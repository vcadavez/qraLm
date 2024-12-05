sf_Smoking_ui <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    fluidRow(
      column(6,
             h5("Prevalence of contaminated lots"), prevLotsUI("sf_prev_lots_smoke"),
             h5("Mean counts in contaminated lots"), mcstatsLotsUI("sf_mcstats_lots_smoke"),
             h5("Distribution of between-lot mean counts"), countsLotsDistUI("sf_counts_lots_dist_smoke")
      ),
      column(6, 
             h5("Prevalence of contaminated units"), prevUnitsUI("sf_prev_units_smoke"),
             h5("Counts in contaminated units"), mcstatsUnitsUI("sf_mcstats_units_smoke"),
             h5("Distribution of between-unit counts"), countsUnitsDistUI("sf_counts_units_dist_smoke")
             ),
      column(12,
           h5("Cumulative distribution of mean counts in contaminated lots"), ecdfLotsUI("sf_ecdf_prob_smoke")
    )
   )
  )
}

sf_Smoking_server <- function(input, output, session, suffix, datBrinesalt) {
  ns <- NS(suffix)
  id <- ns("Smoking")
  
  output[[id]] <- renderUI({ sf_Smoking_ui(id) })
  
  prefix <- "smokedfish-sidebar-inputs-"
  datSmoke = reactive({ generate_datSmoke(input, prefix, datBrinesalt) })
  
  prevLotsServer("sf_prev_lots_smoke", data = datSmoke)
  prevUnitsServer("sf_prev_units_smoke", data = datSmoke)
  mcstatsLotsServer("sf_mcstats_lots_smoke", data = datSmoke)
  mcstatsUnitsServer("sf_mcstats_units_smoke", data = datSmoke)
  countsLotsDistServer("sf_counts_lots_dist_smoke", data = datSmoke)
  countsUnitsDistServer("sf_counts_units_dist_smoke", data = datSmoke)
  ecdfLotsServer("sf_ecdf_prob_smoke", data = datSmoke)
  
  return(datSmoke)
}

generate_datSmoke <- function(input, prefix, datBrinesalt) {
  df <- sfSmoking(
                  datBrinesalt(),
                  rBrineMean   = get_input_value(input, prefix, "r_brine_mean"),   # 0.871
                  rBrineSd     = get_input_value(input, prefix, "r_brine_sd"),     # 0.807
                  rDrysaltMean = get_input_value(input, prefix, "r_drysalt_mean"), # 1.093
                  rDrysaltSd   = get_input_value(input, prefix, "r_drysalt_sd"),   # 0.532
                  )
  return(df)
}

sf_SmokingInputs_ui <- function(id) {  
  ns <- NS(id)  
  div(  
  id = ns("Smoking"),   
#    tagList(
sliderInput(ns("r_brine_mean"), 
            "rBrineMean: Mean of the normal distribution about log10 reduction in LM in brined fillets (log10)",
            value = 0.871, min = 0.25, max = 1.50, step = 0.01),
sliderInput(ns("r_brine_sd"), 
            "rBrineSd: Std of the normal distribution about the log10 reduction in LM in brineed fillets (log10)",
            value = 0.807, min = 0.25, max = 1.50, step = 0.01),
sliderInput(ns("r_drysalt_mean"), 
            "rDrysaltMean: Mean of the normal distribution about log10 reduction in LM in dry-salted fillets (log10)",
            value = 1.093, min = 0.25, max = 1.50, step = 0.01),
sliderInput(ns("r_drysalt_sd"), 
            "rDrysaltSd: Std of the normal distribution about the log10 reduction in LM in dry-salted fillets (log10)",
            value = 0.532, min = 0.25, max = 1.50, step = 0.01)
)
}
