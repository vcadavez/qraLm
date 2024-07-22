ca_Harvest_ui <- function(id) {
  ns <- NS(id)
  
  fluidRow(
    column(6, 
           h5("Prevalence of contaminated lots"), prevLotsUI("prev_lots_harvest"),
           h5("Mean counts in contaminated lots"), mcstatsLotsUI("lots_mcstats_harvest")
           #           h5("Distribution of between-lot mean counts"), countsLotsDistUI("counts_lots_dist_harvest")
    ),
    column(6, 
           h5("Prevalence of contaminated units"), prevUnitsUI("prev_units_harvest"),
           h5("Counts in contaminated units"), mcstatsUnitsUI("units_mcstats_harvest")
           #           h5("Distribution of between-unit counts"), countsUnitsDistUI("counts_units_dist_harvest")
    ),
    column(12, 
           h5("Empirical Cumulative Distribution Function (ECDF)"), ecdfLotsUI("ecdf_prob_harvest")
    )
  )
}

ca_Harvest_server <- function(input, output, session, suffix, datCanta) {
  ns <- NS(suffix)
  id <- ns("Harvest")
  
  output[[id]] <- renderUI({ ca_Harvest_ui(id) })
  
  prefix <- "rtecantaloupe-sidebar-inputs-"
  datHarvest <- reactive({ generate_datHarvest(input, prefix, datCanta) })
  
  prevLotsServer("prev_lots_harvest",                data=datHarvest)
  prevUnitsServer("prev_units_harvest",              data=datHarvest)
  mcstatsLotsServer("lots_mcstats_harvest",          data=datHarvest)
  mcstatsUnitsServer("units_mcstats_harvest",        data=datHarvest)
  countsLotsDistServer("counts_lots_dist_harvest",   data=datHarvest)
  countsUnitsDistServer("counts_units_dist_harvest", data=datHarvest)
  ecdfLotsServer("ecdf_prob_harvest",                data=datHarvest)
  return(datHarvest)
}

generate_datHarvest <- function(input, prefix, datCanta) {
  set.seed(get_input_value(input, prefix, "seed"))
  df <- caHarvestCC(
    datCanta(),
    probCCH = get_input_value(input, prefix, "prob_cch"),
    trMean  = -1.42,
    trSd    = 0.52,
    nPlas  = get_input_value(input, prefix, "n_plas")
    )
  return(df)
}

ca_HarvestInputs_ui <- function(id) {
  ns <- NS(id)
  div(
  id = ns("Harvest"),
#  tagList(
    sliderInput(ns("prob_cch"),
                "probCCH: Probability of cross-contamination during harvest",
                value = 0.25, min = 0, max = 1, step=0.1),
     sliderInput(ns("n_plas"),
                "nPlas: Numbers of LM on food contact surfaces",
                value = 9, min = 0, max = 20, step=1)
#  )
  )
}
