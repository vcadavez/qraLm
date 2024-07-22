ca_RTEStorage_ui <- function(id) {
  ns <- NS(id)
  fluidRow(
    column(6, 
           h5("Prevalence of contaminated lots"), prevLotsUI("prev_lots_rtestorage"),
           h5("Mean counts in contaminated lots"), mcstatsLotsUI("lots_mcstats_rtestorage")
           #           h5("Distribution of between-lot mean counts"), countsLotsDistUI("counts_lots_dist_rtestorage")
    ),
    column(6, 
           h5("Prevalence of contaminated units"), prevUnitsUI("prev_units_rtestorage"),
           h5("Counts in contaminated units"), mcstatsUnitsUI("units_mcstats_rtestorage")
           #           h5("Distribution of between-unit counts"), countsUnitsDistUI("counts_units_dist_rtestorage")
    ),
    column(12, 
           h5("Empirical Cumulative Distribution Function (ECDF)"), ecdfLotsUI("ecdf_prob_rtestorage")
    )
  )
}

ca_RTEStorage_server <- function(input, output, session, suffix, datTransport) {
  ns <- NS(suffix)
  id <- ns("RTEStorage")
  
  output[[id]] <- renderUI({ ca_RTEStorage_ui(id) })
  
  prefix <- "rtecantaloupe-sidebar-inputs-"
  datRTEStorage <- reactive({ generate_datRTEStorage(input, prefix, datTransport) })
  
  prevLotsServer("prev_lots_rtestorage",                data=datRTEStorage)
  prevUnitsServer("prev_units_rtestorage",              data=datRTEStorage)
  mcstatsLotsServer("lots_mcstats_rtestorage",          data=datRTEStorage)
  mcstatsUnitsServer("units_mcstats_rtestorage",        data=datRTEStorage)
  countsLotsDistServer("counts_lots_dist_rtestorage",   data=datRTEStorage)
  countsUnitsDistServer("counts_units_dist_rtestorage", data=datRTEStorage)
  ecdfLotsServer("ecdf_prob_rtestorage",                data=datRTEStorage)
  return(datRTEStorage)
}

generate_datRTEStorage <- function(input, prefix, datRTEStorage) {
  set.seed(get_input_value(input, prefix, "seed"))
  df <- caRetRTE(
                 datRTEStorage(),
                 Tmin     = -2.0196,
                 tempMin  = get_input_value(input, prefix, "temp_min"),
                 tempMode = get_input_value(input, prefix, "temp_mod"),
                 tempMax  = get_input_value(input, prefix, "temp_max"),
                 timeMin  = get_input_value(input, prefix, "time_min"),
                 timeMod  = get_input_value(input, prefix, "time_mod"),
                 timeMax  = get_input_value(input, prefix, "time_max")
                 )
  return(df)
}

ca_RTEStorageInputs_ui <- function(id) {
  ns <- NS(id)
  div(
  id = ns("RTEStorage"),
#  tagList(
    sliderInput(ns("temp_min"), "tempMin: Minimum retail temperature",
                value = 3, min = 1, max = 5, step=0.5),
    sliderInput(ns("temp_mod"), "tempMode: Mode retail temperature",
                value = 5, min = 2.0, max = 8, step=0.5),
    sliderInput(ns("temp_max"), "tempMax: Max retail temperature",
                value = 10, min = 5, max = 15, step=0.5),
    sliderInput(ns("time_min"), "timeMin: Minimum retail time",
                value = 2, min = 1, max = 5, step=0.5),
    sliderInput(ns("time_mod"), "timeMode: Mode retail time",
                value = 5, min = 3, max = 10, step=0.5),
    sliderInput(ns("time_max"), "timeMax: Max retail time",
                value = 9, min = 6, max = 15, step=0.5)
#    )
  )
}
