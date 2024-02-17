ca_StoredDices_ui <- function(id) {
  ns <- NS(id)
  
  fluidRow(
    column(6, 
           h4("Prevalence of contaminated lots"), prevLotsUI("prev_lots_storeddices"),
           h4("Between lots LM counts"), mcstatsLotsUI("lots_mcstats_storeddices")
           #           h4("Between lots LM counts distribution"), countsLotsDistUI("counts_lots_dist_storeddices")
    ),
    column(6, 
           h4("Prevalence of contaminated units"), prevUnitsUI("prev_units_storeddices"),
           h4("Within lots/Between units LM counts"), mcstatsUnitsUI("units_mcstats_storeddices")
           #           h4("Within lots/Between units distribution"), countsUnitsDistUI("counts_units_dist_storeddices")
    ),
    column(12, 
           h4("ECDF plot"), ecdfLotsUI("ecdf_prob_storeddices")
    )
  )
}

ca_StoredDices_server <- function(input, output, session, suffix, datConsumersTransport) {
  ns <- NS(suffix)
  id <- ns("StoredDices")
  
  output[[id]] <- renderUI({ ca_StoredDices_ui(id) })
  
  prefix <- "rtecantaloupe-sidebar-inputs-"
  datStoredDices <- reactive({ generate_datStoredDices(input, prefix, datConsumersTransport) })
  
  prevLotsServer("prev_lots_storeddices",                data=datStoredDices)
  prevUnitsServer("prev_units_storeddices",              data=datStoredDices)
  mcstatsLotsServer("lots_mcstats_storeddices",          data=datStoredDices)
  mcstatsUnitsServer("units_mcstats_storeddices",        data=datStoredDices)
  countsLotsDistServer("counts_lots_dist_storeddices",   data=datStoredDices)
  countsUnitsDistServer("counts_units_dist_storeddices", data=datStoredDices)
  ecdfLotsServer("ecdf_prob_storeddices",                data=datStoredDices)
  return(datStoredDices)
}

generate_datStoredDices <- function(input, prefix, datConsumersTransport) {
  set.seed(get_input_value(input, prefix, "seed"))
  df <- caHomeRTE(
                      datConsumersTransport(),
                      Tmin      = -2.0196,
                      tempMin   = get_input_value(input, prefix, "temp_min_h"),
                      tempMode  = get_input_value(input, prefix, "temp_mode_h"),
                      tempMax   = get_input_value(input, prefix, "temp_max_h"),
                      timeMin  = get_input_value(input, prefix, "time_min_h"),
                      timeMod  = get_input_value(input, prefix, "time_mode_h"),
                      timeMax  = get_input_value(input, prefix, "time_max_h")    )
  return(df)
}

ca_StoredDicesInputs_ui <- function(id) {
  ns <- NS(id)
  div(
  id = ns("StoredDices"),
#  tagList(
    sliderInput(ns("temp_min_h"),
                "tempMin: Minimum retail temperature (ºC)",
                value = 3.1, min = 2, max = 5, step=0.5),
    sliderInput(ns("temp_mode_h"),
                "tempMode: Mode retail temperature (ºC)",
                value = 6.64, min = 5, max = 10, step=0.5),
    sliderInput(ns("temp_max_h"),
                "tempMax: Max retail temperature (ºC)",
                value = 11.1, min = 8, max = 15, step=0.5),
    sliderInput(ns("time_min_h"),
                "timeMin: Minimum retail time (h)",
                value = 3, min = 0.0, max = 5, step=2),
    sliderInput(ns("time_mode_h"),
                "timeMode: Mode retail time (h)",
                value = 24, min = 10, max = 30, step=2),
    sliderInput(ns("time_max_h"),
                "timeMax: Max retail time (h)",
                value = 120, min = 100, max = 150, step=2)
#    )
  )
}