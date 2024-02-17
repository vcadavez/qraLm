ca_Storing_ui <- function(id) {
  ns <- NS(id)
  
  fluidRow(
    column(6, 
           h4("Prevalence of contaminated lots"), prevLotsUI("prev_lots_storing"),
           h4("Between lots LM counts"), mcstatsLotsUI("lots_mcstats_storing")
           #           h4("Between lots LM counts distribution"), countsLotsDistUI("counts_lots_dist_storing")
    ),
    column(6, 
           h4("Prevalence of contaminated units"), prevUnitsUI("prev_units_storing"),
           h4("Within lots/Between units LM counts"), mcstatsUnitsUI("units_mcstats_storing")
           #           h4("Within lots/Between units distribution"), countsUnitsDistUI("counts_units_dist_storing")
    ),
    column(12, 
           h4("ECDF plot"), ecdfLotsUI("ecdf_prob_storing")
    )
  )
}

ca_Storing_server <- function(input, output, session, suffix, datBrush) {
  ns <- NS(suffix)
  id <- ns("Storing")
  
  output[[id]] <- renderUI({ ca_Storing_ui(id) })
  
  prefix <- "rtecantaloupe-sidebar-inputs-"
  datStoring <- reactive({ generate_datStoring(input, prefix, datBrush) })
  
  prevLotsServer("prev_lots_storing",                data=datStoring)
  prevUnitsServer("prev_units_storing",              data=datStoring)
  mcstatsLotsServer("lots_mcstats_storing",          data=datStoring)
  mcstatsUnitsServer("units_mcstats_storing",        data=datStoring)
  countsLotsDistServer("counts_lots_dist_storing",   data=datStoring)
  countsUnitsDistServer("counts_units_dist_storing", data=datStoring)
  ecdfLotsServer("ecdf_prob_storing",                data=datStoring)
  return(datStoring)
}

generate_datStoring <- function(input, prefix, datBrush) {
  set.seed(get_input_value(input, prefix, "seed"))
  df <- caHoldingTime(
    datBrush(),
    pCooled  = get_input_value(input, prefix, "p_cooled"),
    time     = get_input_value(input, prefix, "time_sto"),
    shape = 0.6271,
    meanD410 = 1.1309,
    sdD410 = 1.770711e-06, 
    meanD25 = 2.890015,
    sdD25 = 0.2288748
    )
  return(df)
}

ca_StoringInputs_ui <- function(id) {
  ns <- NS(id)
  div(
  id = ns("Storing"),
#  tagList(
    sliderInput(ns("p_cooled"),
                "pCooled: Probability that a lot of cantaloupes is cooled",
                value = 0, min = 0, max = 1, step=0.1),
    sliderInput(ns("time_sto"),
                "time: Storage time of a lot of cantaloupes (h)",
                value = 6, min = 0, max = 10, step=1)
#    )
 )
}
