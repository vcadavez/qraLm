sf_Home_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
  fluidRow(
  column(6,
         h5("Prevalence of contaminated lots"), prevLotsUI("sf_prev_lots_home"),
         h5("Mean counts in contaminated lots"), mcstatsLotsUI("sf_prod_lots_mcstats_home"),
         h5("Distribution of between-lot mean counts"), countsLotsDistUI("sf_prod_counts_lots_dist_home")
         ),
  column(6,
         h5("Prevalence of contaminated units"), prevUnitsUI("sf_prev_units_home"),
         h5("Counts in contaminated units"), mcstatsUnitsUI("sf_prod_units_mcstats_home"),
         h5("Distribution of between-unit counts"), countsUnitsDistUI("sf_prod_counts_units_dist_home")
         ),
  column(12,
         h5("Cumulative distribution of mean counts in contaminated lots"), ecdfLotsUI("sf_ecdf_prob_home")
    )
  )
  )
}

sf_Home_server <- function(input, output, session, suffix, datColdchain, RTE) {
  ns <- NS(suffix)
  id <- ns("Home")

  output[[id]] <- renderUI({ sf_Home_ui(id) })

  prefix <- "smokedfish-sidebar-inputs-"

  datHome = reactive({ generate_datHome(input, prefix, datColdchain, RTE) })

  prevLotsServer("sf_prev_lots_home",                      data=datHome)
  prevUnitsServer("sf_prev_units_home",                    data=datHome)
  mcstatsLotsServer("sf_prod_lots_mcstats_home",           data=datHome)
  mcstatsUnitsServer("sf_prod_units_mcstats_home",         data=datHome)
  countsLotsDistServer("sf_prod_counts_lots_dist_home",    data=datHome)
  countsUnitsDistServer("sf_prod_counts_units_dist_home",  data=datHome)
  ecdfLotsServer("sf_ecdf_prob_home",                      data=datHome)

  return(datHome)
}

generate_datHome <- function(input, prefix, datColdchain, RTE) {
df <- sfColdChain(
    datColdchain(),
    # RTE characteristics
    RTE = RTE(),
#    unitSize =  datColdchain()$unitSize,
    tempMin  = get_input_value(input, prefix, "temp_min_h"),
    tempMode = get_input_value(input, prefix, "temp_mode_h"),
    tempMax  = get_input_value(input, prefix, "temp_max_h"),
    timeMin  = get_input_value(input, prefix, "time_min_h"),
    timeMode = get_input_value(input, prefix, "time_mode_h"),
    timeMax  = get_input_value(input, prefix, "time_max_h"),
    variability =  get_input_value(input, prefix, "Variability_h"),
    corTimeTemp = get_input_value(input, prefix, "cor_time_temp_h"),
    intralotSdN0LAB = 0,
    lnQ0LABmin      = -12,
    lnQ0LABmode     = -2.73,
    lnQ0LABmax      = 1.26,
    mumaxrefLm      = 0.419, # mumaxrefLm
    TminLm          = -2.83,
    TrefLm          = 25,
    awminLm         = 0.923,
    pHminLm         = 4.97,
    pheMaxLm        = 32,
    NITmaxLm        = 350,
    CO2maxLm        = 3140,
    micLACuLm       = 3.79,
    micDACuLm       = 4.8,
    micAACuLm       = 10.3,
    micBACuLm       = 0.349,
    micCACuLm       = 2.119,
    micSACuLm       = 1.896,
    mumaxrefLAB     = 0.583,
    TminLAB         = -5.25,
    TrefLAB         = 25,
    awminLAB        = 0.928,
    pHminLAB        = 4.24,
    pheMaxLAB       = 40.3,
    NITmaxLAB       = 2780,
    CO2maxLAB       = 6691,
    micLACuLAB      = 12,
    micDACuLAB      = 33.3,
    micAACuLAB     = 10.3,
    micBACuLAB     = 1.51,
    micCACuLAB   = 10.3,
    micSACuLAB   = 12.6,
    gamma        = 1,
    lim          = 1,
    step         = 1
    )
  return(df)
}

sf_HomeInputs_ui <- function(id) {  
  ns <- NS(id)  
  div(  
  id = ns("Home"),   
#  tagList(
    sliderInput(ns("temp_min_h"),     "tempMin: Minimum storage temperature (ºC)",
                value = 1.12, min = 0, max = 5, step=0.02),
    sliderInput(ns("temp_mode_h"),     "tempMode: Mode storage temperature (ºC)",
                value = 7, min = 0, max = 7, step=0.25),
    sliderInput(ns("temp_max_h"),     "tempMax: Maximum storage temperature (ºC)",
                value = 13, min = 5, max = 15, step=0.25),
    sliderInput(ns("time_min_h"),     "timeMin: Minimum storage time (h)",
                value = 17.5, min = 0, max = 20, step=0.5),
    sliderInput(ns("time_mode_h"),     "timeMode: Mode storage time (h)",
                value = 70, min = 10, max = 100, step=2.0),
    sliderInput(ns("time_max_h"),     "timeMax: Maximum storage time (h)",
                value = 840, min = 100, max = 1000, step=10),
selectInput(ns("Variability_h"),     "variability: Variability for time and temperature",
            choices = c("lot", "column", "portion"), selected=c("column")),
sliderInput(ns("cor_time_temp_h"),     "corTimeTemp: Correlation time/temperature",
            value = -0.12, min = -1.0, max = 1.0, step=0.02)
#  ) 
   )
}
