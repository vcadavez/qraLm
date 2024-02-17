sf_BriningOrSalting_ui <- function(id) {
  ns <- NS(id)
  fluidRow(
    column(6,
           h4("Prevalence of contaminated lots"), prevLotsUI("sf_prev_lots_brine_salt"),
           h4("Between lots LM counts"), mcstatsLotsUI("sf_mcstats_lots_brine_salt")
           #           h4("Between lots LM Counts distribution"), countsLotsDistUI("sf_counts_lots_dist_brine_salt")
      ),
    column(6,
           h4("Prevalence of contaminated units"), prevUnitsUI("sf_prev_units_brine_salt"),
           h4("Within lots/Between units LM counts"), mcstatsUnitsUI("sf_mcstats_units_brine_salt")
           #           h4("Within lots/Between units distribution"), countsUnitsDistUI("sf_counts_units_dist_brine_salt")
           ),
    column(12,
           h4("ECDF plot"), ecdfLotsUI("sf_ecdf_prob_brine_salt")
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
                  "pBrine: Prob. that a lot is salted by brinning",
                  value = 1, min = 0, max = 1, step = 0.1),
      sliderInput(ns("pcc_brine"), 
                  "pccBrine: Prob. that the brine is contaminated with LM",
                  value = 0.135, min = 0, max = 1, step = 0.001),
      sliderInput(ns("vol_inj_min"), 
                  "volInjMin: Minimum volume of brine (ml)",
                  value = 25, min = 0, max = 50, step = 5),
      sliderInput(ns("vol_inj_mode"), 
                  "volInjMode: Mode of the volume of brine (ml)",
                  value = 35, min = 25, max = 50, step = 5),
      sliderInput(ns("vol_inj_max"), 
                  "volInjMax: Maximum volume of brine (ml)",
                  value = 100, min = 50, max = 150, step = 5),
      sliderInput(ns("conc_brine_min"), 
                  "concBrineMin: Minimum concentration of LM in brine (CFU/ml)",
                  value = 0, min = 0, max = 5, step = 0.015),
      sliderInput(ns("conc_brine_mode"),
                  "concBrineMode: Mode of concentration of LM in brine (CFU/ml)",
                  value = 0.015, min = 0, max = 5, step = 0.015),
      sliderInput(ns("conc_brine_max"),
                  "concBrineMax: Maximum concentration of LM in brine (CFU/ml)",
                  value = 0.060, min = 0, max = 5, step = 0.015),
      sliderInput(ns("pcc_smearing"), 
                  "pccSmearing: Probability of cross-contamination",
                  value = 0.25, min = 0, max = 1, step = 0.10),
       sliderInput(ns("n_surface"), 
                   "nSurface: Numbers of LM on surfaces in contact with a fillet (CFU)",
                  value = 1000, min = 0, max = 5000, step = 100)
#    )  
   )
}