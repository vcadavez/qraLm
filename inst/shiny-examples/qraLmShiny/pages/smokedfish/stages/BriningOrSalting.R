sf_BriningOrSalting_ui <- function(id) {
  ns <- NS(id)
  fluidRow(
    column(6,
           h5("Prevalence of contaminated lots"), prevLotsUI("sf_prev_lots_brine_salt"),
           h5("Mean counts in contaminated lots"), mcstatsLotsUI("sf_mcstats_lots_brine_salt"),
           h5("Distribution of between-lot mean counts"), countsLotsDistUI("sf_counts_lots_dist_brine_salt")
      ),
    column(6,
           h5("Prevalence of contaminated units"), prevUnitsUI("sf_prev_units_brine_salt"),
           h5("Counts in contaminated units"), mcstatsUnitsUI("sf_mcstats_units_brine_salt"),
           h5("Distribution of between-unit counts"), countsUnitsDistUI("sf_counts_units_dist_brine_salt")
           ),
    column(12,
           h5("Cumulative distribution of mean counts in contaminated lots"), ecdfLotsUI("sf_ecdf_prob_brine_salt")
    )
  )
}

sf_BriningOrSalting_server <- function(input, output, session, suffix, datHold) {
  ns <- NS(suffix)
  id <- ns("BriningOrSalting")
  
  output[[id]] <- renderUI({ sf_BriningOrSalting_ui(id) })
  
  prefix <- "smokedfish-sidebar-inputs-"
  datBrineSalt = reactive({ generate_datBrineSalt(input, prefix, datHold) })
  
  prevLotsServer("sf_prev_lots_brine_salt", data = datBrineSalt)
  prevUnitsServer("sf_prev_units_brine_salt", data = datBrineSalt)
  mcstatsLotsServer("sf_mcstats_lots_brine_salt", data = datBrineSalt)
  mcstatsUnitsServer("sf_mcstats_units_brine_salt", data = datBrineSalt)
  countsLotsDistServer("sf_counts_lots_dist_brine_salt", data = datBrineSalt)
  countsUnitsDistServer("sf_counts_units_dist_brine_salt", data = datBrineSalt)
  ecdfLotsServer("sf_ecdf_prob_brine_salt", data = datBrineSalt)
  
  return(datBrineSalt)
}

generate_datBrineSalt <- function(input, prefix, datHold) {
df <- sfBrineORsaltCC(datHold(),
                      pBrine         = get_input_value(input, prefix, "p_brine"),
                      pccBrine       = get_input_value(input, prefix, "pcc_brine"),
                      volInjMin      = get_input_value(input, prefix, "vol_inj_min"),
                      volInjMode     = get_input_value(input, prefix, "vol_inj_mode"),
                      volInjMax      = get_input_value(input, prefix, "vol_inj_max"),
                      concBrineMin   = get_input_value(input, prefix, "conc_brine_min"),
                      concBrineMode  = get_input_value(input, prefix, "conc_brine_mode"),
                      concBrineMax   = get_input_value(input, prefix, "conc_brine_max"),
                      pccSmearing    = get_input_value(input, prefix, "pcc_smearing"),
                      trSmearingMean = -0.29,
                      trSmearingSd   = 0.31,
                      nSurface       = get_input_value(input, prefix, "n_surface"))
  
  return(df)
}

sf_BriningOrSaltingInputs_ui <- function(id) {  
  ns <- NS(id)  
   div(  
     id = ns("BriningOrSalting"),   
#    tagList(  
      sliderInput(ns("p_brine"), 
                  "pBrine: Prob. that a lot is salted by brining (%)",
                  value = 1, min = 0, max = 1, step = 0.05),
      sliderInput(ns("pcc_brine"), 
                  "pccBrine: Prob. that the brine is contaminated with LM (%)",
                  value = 0.135, min = 0, max = 1, step = 0.005),
      sliderInput(ns("vol_inj_min"), 
                  "volInjMin: Minimum volume of brine (ml)",
                  value = 25, min = 1, max = 30, step = 1),
      sliderInput(ns("vol_inj_mode"), 
                  "volInjMode: Mode of the volume of brine (ml)",
                  value = 35, min = 15, max = 50, step = 5),
      sliderInput(ns("vol_inj_max"), 
                  "volInjMax: Maximum volume of brine (ml)",
                  value = 100, min = 50, max = 150, step = 10),
      sliderInput(ns("conc_brine_min"), 
                  "concBrineMin: Minimum concentration of LM in brine (CFU/ml)",
                  value = 0, min = 0, max = 5, step = 0.01),
      sliderInput(ns("conc_brine_mode"),
                  "concBrineMode: Mode of concentration of LM in brine (CFU/ml)",
                  value = 0.015, min = 0.0, max = 10, step = 0.005),
      sliderInput(ns("conc_brine_max"),
                  "concBrineMax: Maximum concentration of LM in brine (CFU/ml)",
                  value = 0.060, min = 0.05, max = 20, step = 0.05),
      sliderInput(ns("pcc_smearing"), 
                  "pccSmearing: Probability of cross-contamination (%)",
                  value = 0.03, min = 0.0, max = 1, step = 0.01),
       sliderInput(ns("n_surface"), 
                   "nSurface: Numbers of LM on surfaces in contact with a fillet (CFU)",
                  value = 10, min = 10, max = 1000, step = 10)
#    )  
   )
}