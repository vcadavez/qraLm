ca_ConsumersTransport_ui <- function(id) {
  ns <- NS(id)
  
  fluidRow(
    column(6, 
           h5("Prevalence of contaminated lots"), prevLotsUI("prev_lots_consumerstransport"),
           h5("Mean counts in contaminated lots"), mcstatsLotsUI("lots_mcstats_consumerstransport")
           #           h5("Distribution of between-lot mean counts"), countsLotsDistUI("counts_lots_dist_consumerstransport")
    ),
    column(6, 
           h5("Prevalence of contaminated units"), prevUnitsUI("prev_units_consumerstransport"),
           h5("Counts in contaminated units"), mcstatsUnitsUI("units_mcstats_consumerstransport")
           #           h5("Distribution of between-unit counts"), countsUnitsDistUI("counts_units_dist_consumerstransport")
    ),
    column(12, 
           h5("Empirical Cumulative Distribution Function (ECDF)"), ecdfLotsUI("ecdf_prob_consumerstransport")
    )
  )
}

ca_ConsumersTransport_server <- function(input, output, session, suffix, datRTEStorage) {
  ns <- NS(suffix)
  id <- ns("ConsumersTransport")
  
  output[[id]] <- renderUI({ ca_ConsumersTransport_ui(id) })
  
  prefix <- "rtecantaloupe-sidebar-inputs-"
  datConsumersTransport <- reactive({ generate_datConsumersTransport(input, prefix, datRTEStorage) })
  
  prevLotsServer("prev_lots_consumerstransport",                data=datConsumersTransport)
  prevUnitsServer("prev_units_consumerstransport",              data=datConsumersTransport)
  mcstatsLotsServer("lots_mcstats_consumerstransport",          data=datConsumersTransport)
  mcstatsUnitsServer("units_mcstats_consumerstransport",        data=datConsumersTransport)
  countsLotsDistServer("counts_lots_dist_consumerstransport",   data=datConsumersTransport)
  countsUnitsDistServer("counts_units_dist_consumerstransport", data=datConsumersTransport)
  ecdfLotsServer("ecdf_prob_consumerstransport",                data=datConsumersTransport)
  return(datConsumersTransport)
}

generate_datConsumersTransport <- function(input, prefix, datConsumersTransport) {
  set.seed(get_input_value(input, prefix, "seed"))
  df <- caRet2HomeRTE(
                      datConsumersTransport(),
                      Tmin      = -2.0196,
                      tempMin   = get_input_value(input, prefix, "temp_min"),
                      tempMode  = get_input_value(input, prefix, "temp_mod"),
                      tempMax   = get_input_value(input, prefix, "temp_max"),
                      timeShape = get_input_value(input, prefix, "time_shape"),
                      timeScale = get_input_value(input, prefix, "time_scale")
    )
  return(df)
}

ca_ConsumersTransportInputs_ui <- function(id) {
  ns <- NS(id)
  div(
  id = ns("ConsumersTransport"),
#  tagList(
    sliderInput(ns("temp_min"),
                "tempMin: Minimum retail temperature",
                value = 7, min = 5, max = 14, step=0.5),
    sliderInput(ns("temp_mod"),
                "tempMode: Mode retail temperature",
                value = 15, min = 10, max = 20, step=0.5),
    sliderInput(ns("temp_max"),
                "tempMax: Max retail temperature",
                value = 30, min = 25, max = 35, step=0.5),
    sliderInput(ns("time_shape"),
                "timeShape: Shape parameter of the gamma distribution",
                value = 6.2, min = 5.0, max = 10, step=0.2),
    sliderInput(ns("time_scale"),
                "timeScale: Scale parameter of the gamma distribution",
                value = 8.2, min = 6.0, max = 12, step=0.2)
#   )
  )
}
