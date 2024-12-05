sf_ColdChain_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    fluidRow(
      column(6,
             h5("Prevalence of contaminated lots"), prevLotsUI("sf_prev_lots_coldchain"),
             h5("Mean counts in contaminated lots"), mcstatsLotsUI("sf_mcstats_lots_coldchain"),
             h5("Distribution of between-lot mean counts"), countsLotsDistUI("sf_counts_lots_dist_coldchain")
      ),
      column(6,
             h5("Prevalence of contaminated units"), prevUnitsUI("sf_prev_units_coldchain"),
             h5("Counts in contaminated units"), mcstatsUnitsUI("sf_mcstats_units_coldchain"),
             h5("Distribution of between-unit counts"), countsUnitsDistUI("sf_counts_units_dist_coldchain")
      ),
      column(12,
             h5("Cumulative distribution of mean counts in contaminated lots"), ecdfLotsUI("sf_ecdf_prob_coldchain")
      )
    )
  )
}

sf_ColdChain_server <- function(input, output, session, suffix, datPack, RTE) {
  ns <- NS(suffix)
  id <- ns("ColdChain")
  
  output[[id]] <- renderUI({ sf_ColdChain_ui(id) })
  
  prefix <- "smokedfish-sidebar-inputs-"
  
  datColdChain = reactive({ generate_datColdChain(input, prefix, datPack, RTE) })
  
  prevLotsServer("sf_prev_lots_coldchain", data = datColdChain)
  prevUnitsServer("sf_prev_units_coldchain", data = datColdChain)
  mcstatsLotsServer("sf_mcstats_lots_coldchain", data = datColdChain)
  mcstatsUnitsServer("sf_mcstats_units_coldchain", data = datColdChain)
  countsLotsDistServer("sf_counts_lots_dist_coldchain", data = datColdChain)
  countsUnitsDistServer("sf_counts_units_dist_coldchain", data = datColdChain)
  ecdfLotsServer("sf_ecdf_prob_coldchain", data = datColdChain)
  
  return(datColdChain)
}


generate_datColdChain <- function(input, prefix, datPack, RTE) {
  set.seed(get_input_value(input, prefix, "seed"))
  df1 <- sfColdChain(
                    datPack(),
                    RTE = RTE(),
#    unitSize =  datPack()$unitSize,
    tempMin  = get_input_value(input, prefix, "temp_min_cc"),
    tempMode = get_input_value(input, prefix, "temp_mode_cc"),
    tempMax  = get_input_value(input, prefix, "temp_max_cc"),
    timeMin  = get_input_value(input, prefix, "time_min_cc"),
    timeMode = get_input_value(input, prefix, "time_mode_cc"),
    timeMax  = get_input_value(input, prefix, "time_max_cc"),
    variability =  get_input_value(input, prefix, "Variability_cc"),
    corTimeTemp = get_input_value(input, prefix, "cor_time_temp"),
    N0LABmin  = get_input_value(input, prefix, "N0_LAB_min"),
    N0LABmode = get_input_value(input, prefix, "N0_LAB_mode"),
    N0LABmax  = get_input_value(input, prefix, "N0_LAB_max"),
    intralotSdN0LAB = 0,
    lnQ0LABmin  = -12,
    lnQ0LABmode = -2.73,
    lnQ0LABmax  = 1.26,
    MPDLABmin   = get_input_value(input, prefix, "MPD_LAB_min"),
    MPDLABmode  = get_input_value(input, prefix, "MPD_LAB_mode"),
    MPDLABmax   = get_input_value(input, prefix, "MPD_LAB_max"),
    MPDLmmin    = get_input_value(input, prefix, "MPD_Lm_min"),
    MPDLmmode   = get_input_value(input, prefix, "MPD_Lm_mode"),
    MPDLmmax    = get_input_value(input, prefix, "MPD_Lm_max"),
    mumaxrefLm  = 0.419,
    TminLm      = -2.83,
    TrefLm      = 25,
    awminLm = 0.923,
    pHminLm = 4.97,
    pheMaxLm = 32,
    NITmaxLm = 350,
    CO2maxLm = 3140,
    micLACuLm = 3.79,
    micDACuLm = 4.8,
    micAACuLm = 10.3,
    micBACuLm = 0.349,
    micCACuLm = 2.119,
    micSACuLm = 1.896,
    mumaxrefLAB = 0.583,
    TminLAB   = -5.25,
    TrefLAB    = 25,
    awminLAB   = 0.928,
    pHminLAB   = 4.24,
    pheMaxLAB  = 40.3,
    NITmaxLAB  = 2780,
    CO2maxLAB  = 6691,
    micLACuLAB = 12,
    micDACuLAB = 33.3,
    micAACuLAB = 10.3,
    micBACuLAB = 1.51,
    micCACuLAB = 10.3,
    micSACuLAB = 12.6,
    gamma       = 1,
    lim         = 1,
    step        = 1
  )
  return(df1)
}


sf_ColdChainInputs_ui <- function(id) {  
  ns <- NS(id)  
   div(  
   id = ns("ColdChain"),   
#  tagList(
    sliderInput(ns("temp_min_cc"),     "tempMin: Minimum storage temperature (ºC)",
                value = 0.28, min = 0, max = 4, step=0.02),
    sliderInput(ns("temp_mode_cc"),     "tempMode: Mode storage temperature (ºC)",
                value = 4.6, min = 0, max = 7, step=0.1),
    sliderInput(ns("temp_max_cc"),     "tempMax: Maximum storage temperature (ºC)",
                value = 7, min = 4, max = 12, step=0.1),
    sliderInput(ns("time_min_cc"),     "timeMin: Minimum storage time (h)",
                value = 12, min = 0, max = 24, step=2),
    sliderInput(ns("time_mode_cc"),     "timeMode: Mode storage time (h)",
                value = 144, min = 0, max = 200, step=2),
    sliderInput(ns("time_max_cc"),     "timeMax: Maximum storage time (h)",
                value = 720, min = 0, max = 1000, step=10),
    selectInput(ns("Variability_cc"),     "variability: Variability for time and temperature",
                choices = c("lot", "column", "portion"), selected=c("lot")),
    sliderInput(ns("cor_time_temp"),     "corTimeTemp: Correlation time/temperature",
                value = -0.16, min = -1.0, max = 1.0, step=0.02),
    sliderInput(ns("N0_LAB_min"),     "N0LABmin: Minimum LAB counts (log10 CFU/g)",
                value = -1.0, min = -2, max = 4, step=0.1),
    sliderInput(ns("N0_LAB_mode"),     "N0LABmode: Mode of LAB counts (log10 CFU/g)",
                value = 0.28, min = -2 , max = 6, step=0.1),
    sliderInput(ns("N0_LAB_max"),     "N0LABmax: Maximum LAB counts (log10 CFU/g)",
                value = 1.6, min = 1.0 , max = 10, step=0.1),
    sliderInput(ns("MPD_LAB_min"),     "MPDLABmin: Minimum MPD of LAB (⁠log10 CFU/g)⁠",
                value = 8, min = 4 , max = 10, step=0.25),
    sliderInput(ns("MPD_LAB_mode"),     "MPDLABmode: Mode of MPD of LAB (⁠log10 CFU/g)",
                value = 8.5, min = 4 , max = 10, step=0.25),
    sliderInput(ns("MPD_LAB_max"),     "MPDLABmax: Maximum  MPD of LAB (⁠log10 CFU/g)",
                value = 9, min = 4 , max = 10, step=0.25),
    sliderInput(ns("MPD_Lm_min"),     "MPDLmmin: Minimum  MPD of Lm (⁠log10 CFU/g)⁠",
                value = 6.6, min = 2 , max = 10, step=0.2),
    sliderInput(ns("MPD_Lm_mode"),     "MPDLmmode: Mode of   MPD of Lm (⁠log10 CFU/g)",
                value = 7.4, min = 2 , max = 10, step=0.2),
    sliderInput(ns("MPD_Lm_max"),     "MPDLmmax: Maximum   MPD of Lm (⁠log10 CFU/g)",
                value = 8.2, min = 2 , max = 10, step=0.2)
#  ) 
  )
}
