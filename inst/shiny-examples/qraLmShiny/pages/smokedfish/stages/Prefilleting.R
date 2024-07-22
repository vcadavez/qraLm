sf_Prefilleting_ui <- function(id) {
  ns <- NS(id)

  fluidPage(
    fluidRow(
      column(6,
      h5("Prevalence of contaminated lots"), prevLotsUI("sf_prev_lots_prefill"),
      h5("Mean counts in contaminated lots"), mcstatsLotsUI("sf_lot_mcstats_prefill"),  
      h5("Summary statistics - Mean between units"), countsLotsDistUI("sf_lot_counts_prefill")
      ),
      column(6,
             h5("Prevalence of contaminated units"), prevUnitsUI("sf_prev_units_prefill"),
             h5("Counts in contaminated units"), mcstatsUnitsUI("sf_unit_mcstats_prefill"),
             h5("Distribution of between-unit counts"), countsUnitsDistUI("sf_unit_counts_prefill")
    ),
    column(12,
      h5("Cumulative distribution of mean counts in contaminated lots"), ecdfLotsUI("sf_ecdf_prefill")
    )
  )
  )
}

sf_Prefilleting_server <- function(input, output, session, suffix, datFish) {
  ns <- NS(suffix)
  id <- ns("Prefilleting")

  output[[id]] <- renderUI({ sf_Prefilleting_ui(id) })

  prefix <- "smokedfish-sidebar-inputs-"
  datPrefill <- reactive({ generate_datPrefill(input, prefix, datFish) })

  prevLotsServer("sf_prev_lots_prefill", data = datPrefill)
  prevUnitsServer("sf_prev_units_prefill", data = datPrefill)
  mcstatsLotsServer("sf_lot_mcstats_prefill", data = datPrefill)
  mcstatsUnitsServer("sf_unit_mcstats_prefill", data = datPrefill)
  countsLotsDistServer("sf_lot_counts_prefill", data = datPrefill)
  countsUnitsDistServer("sf_unit_counts_prefill", data = datPrefill)
  ecdfLotsServer("sf_ecdf_prefill", data = datPrefill)

  return(datPrefill)
}

generate_datPrefill <- function(input, prefix, datFish) {
  set.seed(get_input_value(input, prefix, "seed"))
  df <- sfRawFishStorage(
    datFish(),
    MPD = get_input_value(input, prefix, "mpd"),
    tempMin = get_input_value(input, prefix, "temp_min"),
    tempMode = get_input_value(input, prefix, "temp_mode"),
    tempMax = get_input_value(input, prefix, "temp_max"),
    timeMin = get_input_value(input, prefix, "time_min"),
    timeMode = get_input_value(input, prefix, "time_mode"),
    timeMax = get_input_value(input, prefix, "time_max")
  )
  return(df)
}

sf_PrefilletingInputs_ui <- function(id) {
  ns <- NS(id)
   div(
     id = ns("Prefilleting"),
#    tagList(
      sliderInput(ns("mpd"), "MPD: MPD of LM in raw fish (log10 CFU/g)",
                  value = 9.2, min = 5, max = 10, step = 0.1),
      sliderInput(ns("temp_min"), "tempMin: Minimum holding temperature (ºC)", 
                  value = -2, min = -4, max = 5, step =0.20),
      sliderInput(ns("temp_mode"), "tempMode: Mode holding temperature (ºC)",
                  value = 0, min = -3, max = 8, step = 0.20),
      sliderInput(ns("temp_max"), "tempMax. Maximum holding temperature (ºC)",
                  value = 4, min = 1, max = 10, step = 0.20),
      sliderInput(ns("time_min"), "timeMin: Minimum holding time (h)",     
                  value = 0.5, min = 0.0, max = 12.0, step = 0.5),
      sliderInput(ns("time_mode"), "timeMode: Mode of the holding time (h)",
                  value = 2, min = 1, max = 16, step = 0.5),
      sliderInput(ns("time_max"), "timeMax: Maximum holding time (h)",
                  value = 6, min = 5, max = 24, step = 0.5)
#    )
   )
}
