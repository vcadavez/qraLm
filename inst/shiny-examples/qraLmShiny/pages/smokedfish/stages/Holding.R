sf_Holding_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
  fluidRow(
    column(6,
           h5("Prevalence of contaminated lots"), prevLotsUI("sf_prev_lots_hold"),
           h5("Mean counts in contaminated lots"), mcstatsLotsUI("sf_prod_lots_mcstats_hold"),
           h5("Distribution of between-lot mean counts"), countsLotsDistUI("sf_prod_counts_lots_dist_hold")
           ),
    column(6,
            h5("Prevalence of contaminated units"), prevUnitsUI("sf_prev_units_hold"),
            h5("Counts in contaminated units"), mcstatsUnitsUI("sf_prod_units_mcstats_hold"),
            h5("Distribution of between-unit counts"), countsUnitsDistUI("sf_prod_counts_units_dist_hold")
            ),
    column(12,
          h5("Cumulative distribution of mean counts in contaminated lots"), ecdfLotsUI("sf_ecdf_prob_hold")
          )
     )
  )
}

sf_Holding_server <- function(input, output, session, suffix, datFill) {
  ns <- NS(suffix)
  id <- ns("Holding")
  output[[id]] <- renderUI({ sf_Holding_ui(id) })

  prefix <- "smokedfish-sidebar-inputs-"
  datHold <- reactive({ generate_datHold(input, prefix, datFill) })

  prevLotsServer("sf_prev_lots_hold", data = datHold)
  prevUnitsServer("sf_prev_units_hold", data = datHold)
  mcstatsLotsServer("sf_prod_lots_mcstats_hold", data = datHold)
  mcstatsUnitsServer("sf_prod_units_mcstats_hold", data = datHold)
  countsLotsDistServer("sf_prod_counts_lots_dist_hold", data = datHold)
  countsUnitsDistServer("sf_prod_counts_units_dist_hold", data = datHold)
  ecdfLotsServer("sf_ecdf_prob_hold", data = datHold)

  return(datHold)
}

generate_datHold <- function(input, prefix, datFill) {
  set.seed(get_input_value(input, prefix, "seed"))
  df <- sfRawFishStorage(
    datFill(),
#    MPD = get_input_value(input, prefix, "mpd"), #  read from data
#    unitSize = get_input_value(input, prefix, "unit_size_hold"), #  read from data
    tempMin  = get_input_value(input, prefix, "temp_min_hold"),
    tempMode = get_input_value(input, prefix, "temp_mode_hold"),
    tempMax  = get_input_value(input, prefix, "temp_max_hold"),
    timeMin  = get_input_value(input, prefix, "time_min_hold"),
    timeMode = get_input_value(input, prefix, "time_mode_hold"),
    timeMax  = get_input_value(input, prefix, "time_max_hold")
    )
  return(df)
}

sf_HoldingInputs_ui <- function(id) {  
  ns <- NS(id)  
  div(  
    id = ns("Holding"),  
#    tagList(
       sliderInput(ns("temp_min_hold"),  
                   "tempMin: Minimum holding temperature (ºC)",  
                    value = -2, min = -4, max = 5, step =0.20),
      sliderInput(ns("temp_mode_hold"), 
                  "tempMode: Mode of holding temperature (ºC)", 
                  value = 0, min = -3, max = 8, step = 0.20),
      sliderInput(ns("temp_max_hold"),  
                  "tempMax: Maximum holding temperature (ºC)", 
                  value = 4, min = 1, max = 10, step = 0.20),
      sliderInput(ns("time_min_hold"),  
                  "timeMin: Minimum holding time (h)",
                  value = 1.0, min = 0.0, max = 12.0, step = 0.5),
      sliderInput(ns("time_mode_hold"), 
                  "timeMode: Mode of holding time (h)",
                  value = 2, min = 1, max = 16, step = 0.5),
      sliderInput(ns("time_max_hold"),  "timeMax. Maximum holding time (h)",
                  value = 6, min = 5, max = 24, step = 0.5)
#    )
  )   
}  
