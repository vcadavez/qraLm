sf_Portioning_ui <- function(id) {
  ns <- NS(id)

  fluidPage(
    fluidRow(
      column(6,
      h5("Prevalence of contaminated lots"), prevLotsUI("sf_prev_lots_port"),
      h5("Mean counts in contaminated lots"), mcstatsLotsUI("sf_lot_mcstats_port") , 
      h5("Distribution of between-lot mean counts"), countsLotsDistUI("sf_lot_counts_port")
      ),
      column(6,
             h5("Prevalence of contaminated units"), prevUnitsUI("sf_prev_units_port"),
             h5("Counts in contaminated units"), mcstatsUnitsUI("sf_unit_mcstats_port"),
             h5("Distribution of between-unit counts"), countsUnitsDistUI("sf_unit_counts_port")
    ),
    column(12,
      h5("Cumulative distribution of mean counts in contaminated lots"), ecdfLotsUI("sf_ecdf_port")
    )
  )
  )
}

sf_Portioning_server <- function(input, output, session, suffix, datHome) {
  ns <- NS(suffix)
  id <- ns("Portioning")

  output[[id]] <- renderUI({ sf_Portioning_ui(id) })

  prefix <- "smokedfish-sidebar-inputs-"
  datPort <- reactive({ generate_datPort(input, prefix, datHome) })

  prevLotsServer("sf_prev_lots_port", data = datPort)
  prevUnitsServer("sf_prev_units_port", data = datPort)
  mcstatsLotsServer("sf_lot_mcstats_port", data = datPort)
  mcstatsUnitsServer("sf_unit_mcstats_port", data = datPort)
  countsLotsDistServer("sf_lot_counts_port", data = datPort)
  countsUnitsDistServer("sf_unit_counts_port", data = datPort)
  ecdfLotsServer("sf_ecdf_port", data = datPort)

  return(datPort)
}
generate_datPort <- function(input, prefix, datHome) {
  set.seed(get_input_value(input, prefix, "seed"))
  df <- sfPortioning(
    datHome(),
    servingSize = get_input_value(input, prefix, "serving_size"),
    bPortSF = get_input_value(input, prefix, "b_port_sf")
  )
  return(df)
}

sf_PortioningInputs_ui <- function(id) {
  ns <- NS(id)
  div(
  id = ns("Portioning"),
#    tagList(
      sliderInput(ns("serving_size"), 
                  "servingSize: Portion taken from a pack (g)",
                  value = 32.5, min = 32.5, max = 260, step = 32.5),
      sliderInput(ns("b_port_sf"), 
                  "bPortSF: dispersion factor of cells within the package", 
                  value = 1, min = 0.2, max = 2, step = 0.1)
#      )
  )
}
