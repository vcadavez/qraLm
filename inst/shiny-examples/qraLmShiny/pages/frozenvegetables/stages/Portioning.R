fv_Portioning_ui <- function(id) {
  ns = NS(id)
  
  fluidPage(
#    h3("Variability of contamination in lots"),
      fluidRow(
           column(6,
                  h4("Prevalence of contaminated lots"), prevLotsUI("fv_prev_lots_port"),
                  h4("Between lots LM counts"), mcstatsLotsUI("fv_lots_mcstats_port")
                  #                  h4("Between lots LM counts distribution"), countsLotsDistUI("fv_counts_lots_dist_port")
                  ),
           column(6,
                  h4("Prevalence of contaminated units"),     prevUnitsUI("fv_prev_units_port"),
                  h4("Within lots/Between units LM counts"),  mcstatsUnitsUI("fv_units_mcstats_port")
                  #                  h4("Within lots/Between units LM counts distribution"), countsUnitsDistUI("fv_counts_units_dist_port")
           ),
           column(12,
             h4("ECDF plot"), ecdfLotsUI("fv_ecdf_prob_port")
             )
           )
  )
}
fv_Portioning_server <- function(input, output, session, suffix, datTest) {
  ns <- NS(suffix)
  id <- ns("Portioning")

  output[[id]] <- renderUI({ fv_Portioning_ui(id) })
  
  prefix <- "frozenvegetables-sidebar-inputs-"
  datPortfv <- reactive({ generate_datPortfv(input, prefix, datTest) })
  

  prevLotsServer("fv_prev_lots_port",                data=datPortfv)
  prevUnitsServer("fv_prev_units_port",              data=datPortfv)
  mcstatsLotsServer("fv_lots_mcstats_port",          data=datPortfv)
  mcstatsUnitsServer("fv_units_mcstats_port",        data=datPortfv)
  countsLotsDistServer("fv_counts_lots_dist_port",   data=datPortfv)
  countsUnitsDistServer("fv_counts_units_dist_port", data=datPortfv)
  ecdfLotsServer("fv_ecdf_prob_port",                data=datPortfv)
  return(datPortfv)
}

generate_datPortfv <- function(input, prefix, datTest) {
  set.seed(get_input_value(input, prefix, "seed"))
  df <- fvPortioning(
                     datTest(),
                     servingSize = get_input_value(input, prefix, "serving_size_port"),
                     bPort = get_input_value(input, prefix, "b_port")
  )
  return(df)
}


fv_PortioningInputs_ui <- function(id) {
  ns <- NS(id)
  div(
  id = ns("Portioning"),
# tagList(
    sliderInput(ns("serving_size_port"), 
                "servingSize: Weight portion taken from a pack (g):", 
                value=50, min=25, max=500, step=25),
    sliderInput(ns("b_port"), 
                "bPort: Dispersion in the pack:",
                value=1, min=0, max=2, step=0.05) 
#    )
 )
}
