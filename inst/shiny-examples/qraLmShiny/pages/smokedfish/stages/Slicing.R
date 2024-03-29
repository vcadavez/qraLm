sf_Slicing_ui <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    fluidRow(
    column(6,
           h4("Prevalence of contaminated lots"), prevLotsUI("sf_prev_lots_slice"),
           h4("Between lots LM counts"), mcstatsLotsUI("sf_mcstats_lots_slice")
           #           h4("Between lots LM counts distribution"), countsLotsDistUI("sf_counts_lots_dist_slice")
           ),
    column(6,
          h4("Prevalence of contaminated units"), prevUnitsUI("sf_prev_units_slice"),
          h4("Within lots/Between units LM counts"), mcstatsUnitsUI("sf_mcstats_units_slice")
          #          h4("Within lots/Between units distribution"), countsUnitsDistUI("sf_counts_units_dist_slice")
          ),
    column(12,
           h4("Empirical Cumulative Distribution Function (ECDF)"), ecdfLotsUI("sf_ecdf_prob_slice")
    )
  )
  )
}

sf_Slicing_server <- function(input, output, session, suffix, datSmoke) {
  ns <- NS(suffix)
  id <- ns("Slicing")
  
  output[[id]] <- renderUI({ sf_Slicing_ui(id) })
  
  prefix <- "smokedfish-sidebar-inputs-"
  datSlice = reactive({ generate_datSlice(input, prefix, datSmoke) })
  
  prevLotsServer("sf_prev_lots_slice", data = datSlice)
  prevUnitsServer("sf_prev_units_slice", data = datSlice)
  mcstatsLotsServer("sf_mcstats_lots_slice", data = datSlice)
  mcstatsUnitsServer("sf_mcstats_units_slice", data = datSlice)
  countsLotsDistServer("sf_counts_lots_dist_slice", data = datSlice)
  countsUnitsDistServer("sf_counts_units_dist_slice", data = datSlice)
  ecdfLotsServer("sf_ecdf_prob_slice", data = datSlice)
  return(datSlice)
}

generate_datSlice <- function(input, prefix, datSmoke) {
  df <- sfSlicer(datSmoke(),
                 wSlices     = get_input_value(input, prefix, "w_slices_s"),
                 initSlicer  = get_input_value(input, prefix, "init_slicer_s"),
                 aParamLoc   = 0.07,
                 aParamScale = 0.03,
                 aParamMax   = 0.50,
                 eMean       = -2.12,
                 eSd         = 0.85
                 )
  
  return(df)
}

sf_SlicingInputs_ui <- function(id) {  
  ns <- NS(id)  
  div(  
  id = ns("Slicing"),   
#    tagList(  
      sliderInput(ns("w_slices_s"), 
                  "wSlices: Slices weight (g)",
                  value = 32.5, min = 10, max = 50, step = 2.5),
      sliderInput(ns("init_slicer_s"), 
                  "initSlicer: Slicer initial contamination (CFU)",
                  value = 100, min = 0, max = 1000, step = 100)
#    )  
   )
}