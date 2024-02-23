fv_Blanching_ui <- function(id) {
  ns <- NS(id)

  fluidRow(
    column(6, 
           h4("Prevalence of contaminated lots"), prevLotsUI("prev_lots_blanch"),
           h4("Between lots LM counts"), mcstatsLotsUI("lots_mcstats_blanch")
#           h4("Between lots LM counts distribution"), countsLotsDistUI("counts_lots_dist_blanch")
           ),
    column(6, 
           h4("Prevalence of contaminated units"), prevUnitsUI("prev_units_blanch"),
           h4("Within lots/Between units LM counts"), mcstatsUnitsUI("units_mcstats_blanch")
#           h4("Within lots/Between units distribution"), countsUnitsDistUI("counts_units_dist_blanch")
           ),
    column(12, 
           h4("Empirical Cumulative Distribution Function (ECDF)"), ecdfLotsUI("ecdf_prob_blanch")
           )
    )
}

fv_Blanching_server <- function(input, output, session, suffix, datProd) {
  ns <- NS(suffix)
  id <- ns("Blanching")

  output[[id]] <- renderUI({ fv_Blanching_ui(id) })
  
  prefix <- "frozenvegetables-sidebar-inputs-"
  datBlanch <- reactive({ generate_datBlanch(input, prefix, datProd) })
  
  prevLotsServer("prev_lots_blanch",                data=datBlanch)
  prevUnitsServer("prev_units_blanch",              data=datBlanch)
  mcstatsLotsServer("lots_mcstats_blanch",          data=datBlanch)
  mcstatsUnitsServer("units_mcstats_blanch",        data=datBlanch)
  countsLotsDistServer("counts_lots_dist_blanch",   data=datBlanch)
  countsUnitsDistServer("counts_units_dist_blanch", data=datBlanch)
  ecdfLotsServer("ecdf_prob_blanch",                data=datBlanch)
  return(datBlanch)
}

generate_datBlanch <- function(input, prefix, datProd) {
  set.seed(get_input_value(input, prefix, "seed"))
  df <- fvBlanching(
                    datProd(),
                    tempBlanch  = get_input_value(input, prefix, "temp_blanch"),
                    timeBlanch  = get_input_value(input, prefix, "time_blanch"),
                    logDrefMean =  -1.78,
                    logDrefSd   =  0.252,
                    zT          = 6.06
  )
  return(df)
}

fv_BlanchingInputs_ui <- function(id) {
  ns <- NS(id)
   div(
   id = ns("Blanching"),
#  tagList(
     sliderInput(ns("temp_blanch"),
                 "tempBlanch: Temperature of blanching (ºC)",
                  value = 83.0, min = 75.0, max = 90.0, step=1.0),
      sliderInput(ns("time_blanch"),
                  "timeBlanch: Duration of blanching (min)",
                   value = 0.75, min = 0.25, max = 1.50, step=0.10)
#     )
  )
}