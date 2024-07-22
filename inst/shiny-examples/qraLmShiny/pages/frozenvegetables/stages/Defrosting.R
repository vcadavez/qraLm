fv_Defrosting_ui <- function(id) {
  ns = NS(id)
  fluidPage(
#    h3("Variability of contamination in lots"),
  fluidRow(
    column(6,
           h5("Prevalence of contaminated lots"), prevLotsUI("fv_prev_lots_defrost"),
           h5("Mean counts in contaminated lots"), mcstatsLotsUI("fv_lots_mcstats_defrost"),
           h5("Distribution of between-lot mean counts"), countsLotsDistUI("fv_counts_lots_dist_defrost")
           ),
    column(6,
           h5("Prevalence of contaminated units"), prevUnitsUI("fv_prev_units_defrost"),
           h5("Counts in contaminated units"), mcstatsUnitsUI("fv_units_mcstats_defrost"),
           h5("Distribution of between-unit counts"), countsUnitsDistUI("fv_counts_units_dist_defrost")
           ),
    column(12,
           h5("Cumulative distribution of mean counts in contaminated lots"), ecdfLotsUI("fv_ecdf_prob_defrost")
           )
    )
  )
  }

fv_Defrosting_server <- function(input, output, session, suffix, datPort) {
  ns <- NS(suffix)
  id <- ns("Defrosting")

  output[[id]] <- renderUI({ fv_Defrosting_ui(id) })
  
  prefix <- "frozenvegetables-sidebar-inputs-"
  datDefrost <- reactive({ generate_datDefrost(input, prefix, datPort) })
  

  prevLotsServer("fv_prev_lots_defrost",                data=datDefrost)
  prevUnitsServer("fv_prev_units_defrost",              data=datDefrost)
  mcstatsLotsServer("fv_lots_mcstats_defrost",          data=datDefrost)
  mcstatsUnitsServer("fv_units_mcstats_defrost",        data=datDefrost)
  countsLotsDistServer("fv_counts_lots_dist_defrost",   data=datDefrost)
  countsUnitsDistServer("fv_counts_units_dist_defrost", data=datDefrost)
  ecdfLotsServer("fv_ecdf_prob_defrost",                data=datDefrost)
  return(datDefrost)
}

generate_datDefrost <- function(input, prefix, datPort) {
  set.seed(get_input_value(input, prefix, "seed"))
  df <- fvDefrost(
                  datPort(),
                  Temp     = get_input_value(input, prefix, "temp_defrost"),
                  time     = get_input_value(input, prefix, "time_defrost"),
                  MPD      = get_input_value(input, prefix, "mpd"), # 8.00
                  Tmin     = -1.18,
                  meanEGR5 = 0.0117,
                  sdEGR5   = 0.00816,
                  pDefrost = get_input_value(input, prefix, "p_defrost")
  )
  return(df)
}

fv_DefrostingInputs_ui <- function(id) {
  ns <- NS(id)
  div(
  id = ns("Defrosting"),
#  tagList(
    sliderInput(ns("temp_defrost"), "Temp: Defrosting temperature (ºC)", value=25, min=5, max=35, step=1),
    sliderInput(ns("time_defrost"), "time: Defrosting time (h)",         value=2,  min=0, max=24, step=1.0),
    sliderInput(ns("mpd"), "MPD of LM in blanched vegetables",           value=8,  min=5, max=10, step=0.2),
    sliderInput(ns("p_defrost"), "pDefrost: Probability of defrosting",  value=0.20,  min=0, max=1, step=0.05) 
   #)
)
}