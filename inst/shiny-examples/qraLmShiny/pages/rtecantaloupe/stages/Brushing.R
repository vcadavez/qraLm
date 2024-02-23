ca_Brushing_ui <- function(id) {
  ns <- NS(id)
  
  fluidRow(
    column(6, 
           h4("Prevalence of contaminated lots"), prevLotsUI("prev_lots_brushing"),
           h4("Between lots LM counts"), mcstatsLotsUI("lots_mcstats_brushing")
           #           h4("Between lots LM counts distribution"), countsLotsDistUI("counts_lots_dist_brushing")
    ),
    column(6, 
           h4("Prevalence of contaminated units"), prevUnitsUI("prev_units_brushing"),
           h4("Within lots/Between units LM counts"), mcstatsUnitsUI("units_mcstats_brushing")
           #           h4("Within lots/Between units distribution"), countsUnitsDistUI("counts_units_dist_brushing")
    ),
    column(12, 
           h4("Empirical Cumulative Distribution Function (ECDF)"), ecdfLotsUI("ecdf_prob_brushing")
    )
  )
}

ca_Brushing_server <- function(input, output, session, suffix, datHarvest) {
  ns <- NS(suffix)
  id <- ns("Brushing")
  
  output[[id]] <- renderUI({ ca_Brushing_ui(id) })
  
  prefix <- "rtecantaloupe-sidebar-inputs-"
  datBrush <- reactive({ generate_datBrush(input, prefix, datHarvest) })
  
  prevLotsServer("prev_lots_brushing",                data=datBrush)
  prevUnitsServer("prev_units_brushing",              data=datBrush)
  mcstatsLotsServer("lots_mcstats_brushing",          data=datBrush)
  mcstatsUnitsServer("units_mcstats_brushing",        data=datBrush)
  countsLotsDistServer("counts_lots_dist_brushing",   data=datBrush)
  countsUnitsDistServer("counts_units_dist_brushing", data=datBrush)
  ecdfLotsServer("ecdf_prob_brushing",                data=datBrush)
  return(datBrush)
}

generate_datBrush <- function(input, prefix, datHarvest) {
  set.seed(get_input_value(input, prefix, "seed"))
  df <- caBrush(
    datHarvest(),
    logDecBrush  = get_input_value(input, prefix, "log_dec_brush")
    )
  return(df)
}

ca_BrushingInputs_ui <- function(id) {
  ns <- NS(id)
  div(
  id = ns("Brushing"),
#  tagList(
    sliderInput(ns("log_dec_brush"),
                "logDecBrush: Mean log10 reduction attained by brushing",
                value = 0, min = 0, max = 10, step=1)
#  )
  )
}
