ca_Transport_ui <- function(id) {
  ns <- NS(id)
  
  fluidRow(
    column(6, 
           h5("Prevalence of contaminated lots"), prevLotsUI("prev_lots_transport"),
           h5("Mean counts in contaminated lots"), mcstatsLotsUI("lots_mcstats_transport")
           #           h5("Distribution of between-lot mean counts"), countsLotsDistUI("counts_lots_dist_transport")
    ),
    column(6, 
           h5("Prevalence of contaminated units"), prevUnitsUI("prev_units_transport"),
           h5("Counts in contaminated units"), mcstatsUnitsUI("units_mcstats_transport")
           #           h5("Distribution of between-unit counts"), countsUnitsDistUI("counts_units_dist_transport")
    ),
    column(12, 
           h5("Empirical Cumulative Distribution Function (ECDF)"), ecdfLotsUI("ecdf_prob_transport")
    )
  )
}

ca_Transport_server <- function(input, output, session, suffix, datTesting) {
  ns <- NS(suffix)
  id <- ns("Transport")
  
  output[[id]] <- renderUI({ ca_Transport_ui(id) })
  
  prefix <- "rtecantaloupe-sidebar-inputs-"
  datTransport <- reactive({ generate_datTransport(input, prefix, datTesting) })
  
  prevLotsServer("prev_lots_transport",                data=datTransport)
  prevUnitsServer("prev_units_transport",              data=datTransport)
  mcstatsLotsServer("lots_mcstats_transport",          data=datTransport)
  mcstatsUnitsServer("units_mcstats_transport",        data=datTransport)
  countsLotsDistServer("counts_lots_dist_transport",   data=datTransport)
  countsUnitsDistServer("counts_units_dist_transport", data=datTransport)
  ecdfLotsServer("ecdf_prob_transport",                data=datTransport)
  return(datTransport)
}

generate_datTransport <- function(input, prefix, datTransport) {
  set.seed(get_input_value(input, prefix, "seed"))
  df <- caTrans2RetRTE(
    datTransport(),
    MPD = 8,
    lnQ0Mean = -0.096728,
    lnQ0Sd = 0.063930,
    meanEGR5 = 0.03557288,
    seEGR5 = 0.004,
    Tmin = -2.0196,
    tempMin  = get_input_value(input, prefix, "temp_min"),
    tempMode = get_input_value(input, prefix, "temp_mod"),
    tempMax  = get_input_value(input, prefix, "temp_max"),
    timeMin  = get_input_value(input, prefix, "time_min"),
    timeMod  = get_input_value(input, prefix, "time_mod"),
    timeMax  = get_input_value(input, prefix, "time_max")
    )
  return(df)
}

ca_TransportInputs_ui <- function(id) {
  ns <- NS(id)
  div(
  id = ns("Transport"),
#  tagList(
    sliderInput(ns("temp_min"),
                "tempMin: Minimum transportation temperature (ºC)",
                value = 3, min = 0.0, max = 5, step=0.5),
    sliderInput(ns("temp_mod"),
                "tempMode: Mode transportation temperature (ºC)",
                value = 5, min = 2.0, max = 10, step=0.5),
    sliderInput(ns("temp_max"),
                "tempMax: Max transportation temperature (ºC)",
                value = 10.3, min = 5, max = 15, step=0.5),
    sliderInput(ns("time_min"),
                "timeMin: Minimum transportation time (h)",
                value = 2, min = 0.0, max = 5, step=0.5),
    sliderInput(ns("time_mod"),
                "timeMode: Mode transportation time (h)",
                value = 5, min = 2.0, max = 10, step=0.5),
    sliderInput(ns("time_max"),
                "timeMax: Max transportation time (h)",
                value = 9, min = 5, max = 15, step=0.5)
#    )
  )
}
