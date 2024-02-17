sf_Filleting_ui <- function(id) {
  ns <- NS(id)
  fluidPage(  
  fluidRow(
    column(6,
           h4("Prevalence of contaminated lots"), prevLotsUI("sf_prev_lots_fill"),
           h4("Between lots LM counts"), mcstatsLotsUI("sf_lot_stats_fill")
           #           h4("Between lots LM counts distribution"), countsLotsDistUI("sf_lot_counts_fill")
           ),
     column(6,      
           h4("Prevalence of contaminated units"), prevUnitsUI("sf_prev_units_fill"),
           h4("Within lots/Between units LM counts"), mcstatsUnitsUI("sf_unit_stats_fill")
           #           h4("Within lots/Between units distribution"), countsUnitsDistUI("sf_unit_counts_fill")
           ),
     column(12,       
           h4("ECDF Plot"), ecdfLotsUI("sf_ecdf_fill")
           )
    )
  )
}

sf_Filleting_server <- function(input, output, session, suffix, datPrefill) {
  ns <- NS(suffix)
  id <- ns("Filleting")
  
  output[[id]] <- renderUI({ sf_Filleting_ui(id) })
  
  prefix <- "smokedfish-sidebar-inputs-"
  datFill <- reactive({ generate_datFill(input, prefix, datPrefill) })
  
  prevLotsServer("sf_prev_lots_fill", data = datFill)
  prevUnitsServer("sf_prev_units_fill", data = datFill)
  mcstatsLotsServer("sf_lot_stats_fill", data = datFill)
  mcstatsUnitsServer("sf_unit_stats_fill", data = datFill)
  countsLotsDistServer("sf_lot_counts_fill", data = datFill)
  countsUnitsDistServer("sf_unit_counts_fill", data = datFill)
  ecdfLotsServer("sf_ecdf_fill", data = datFill)
  
  return(datFill)
}

generate_datFill <- function(input, prefix, datPrefill) {
  df <- sfSlicer(datPrefill(),
                 wSlices     = get_input_value(input, prefix, "w_slices_f"),
                 initSlicer  = get_input_value(input, prefix, "init_slicer_f"),
                 aParamLoc   = 0.07,
                 aParamScale = 0.03,
                 aParamMax   = 0.50,
                 eMean       = -2.12,
                 eSd         = 0.85
                 )
  return(df)
}

sf_FilletingInputs_ui <- function(id) {  
  ns <- NS(id)  
   div(  
     id = ns("Filleting"),
#    tagList(  
      sliderInput(ns("w_slices_f"), "wSlices: Slice weight (g)",
                  value = 1300, min = 500, max = 2000, step = 100),
      sliderInput(ns("init_slicer_f"), "initSlicer: Slicer initial contamination (CFU)",
                  value = 100, min = 0, max = 1000, step = 25)
#      )  
   )
}
