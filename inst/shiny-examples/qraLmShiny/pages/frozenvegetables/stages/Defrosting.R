fv_Defrosting_ui <- function(id) {
  ns = NS(id)
  fluidPage(
#    h3("Variability of contamination in lots"),
  fluidRow(
    column(6,
           h4("Prevalence of contaminated lots"), prevLotsUI("fv_prev_lots_defrost"),
           h4("Between lots LM counts"), mcstatsLotsUI("fv_lots_mcstats_defrost")
#           h4("Between lots LM Counts distribution"), countsLotsDistUI("fv_counts_lots_dist_defrost")
           ),
    column(6,
           h4("Prevalence of contaminated units"), prevUnitsUI("fv_prev_units_defrost"),
           h4("Within lots/Between units LM counts"), mcstatsUnitsUI("fv_units_mcstats_defrost")
           #           h4("Within lots/Between units LM counts distribution"), countsUnitsDistUI("fv_counts_units_dist_defrost")
           ),
    column(12,
           h4("Empirical Cumulative Distribution Function (ECDF)"), ecdfLotsUI("fv_ecdf_prob_defrost")
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
    sliderInput(ns("temp_defrost"), "Temp: Defrosting temperature (ºC)", value=8, min=4, max=20, step=1),
    sliderInput(ns("time_defrost"), "time: Defrost time (h)", value=2, min=0, max=5, step=0.5),
    sliderInput(ns("mpd"), "MPD of LM in blanched vegetables", value=8, min=5, max=12, step=0.15),
    sliderInput(ns("p_defrost"), "pDefrost: Probability of defrosting", value=0, min=0, max=1, step=0.1) 
   #)
)
}