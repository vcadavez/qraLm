ca_Partitioning_ui <- function(id) {
  ns <- NS(id)
  
  fluidRow(
    column(6, 
           h5("Prevalence of contaminated lots"), prevLotsUI("prev_lots_partitioning"),
           h5("Mean counts in contaminated lots"), mcstatsLotsUI("lots_mcstats_partitioning")
           #           h5("Distribution of between-lot mean counts"), countsLotsDistUI("counts_lots_dist_partitioning")
    ),
    column(6, 
           h5("Prevalence of contaminated units"), prevUnitsUI("prev_units_partitioning"),
           h5("Counts in contaminated units"), mcstatsUnitsUI("units_mcstats_partitioning")
           #           h5("Distribution of between-unit counts"), countsUnitsDistUI("counts_units_dist_partitioning")
    ),
    column(12, 
           h5("Empirical Cumulative Distribution Function (ECDF)"), ecdfLotsUI("ecdf_prob_partitioning")
    )
  )
}

ca_Partitioning_server <- function(input, output, session, suffix, datDicing) {
  ns <- NS(suffix)
  id <- ns("Partitioning")
  
  output[[id]] <- renderUI({ ca_Partitioning_ui(id) })
  
  prefix <- "rtecantaloupe-sidebar-inputs-"
  datPartitioning <- reactive({ generate_datPartitioning(input, prefix, datDicing) })
  
  prevLotsServer("prev_lots_partitioning",                data=datPartitioning)
  prevUnitsServer("prev_units_partitioning",              data=datPartitioning)
  mcstatsLotsServer("lots_mcstats_partitioning",          data=datPartitioning)
  mcstatsUnitsServer("units_mcstats_partitioning",        data=datPartitioning)
  countsLotsDistServer("counts_lots_dist_partitioning",   data=datPartitioning)
  countsUnitsDistServer("counts_units_dist_partitioning", data=datPartitioning)
  ecdfLotsServer("ecdf_prob_partitioning",                data=datPartitioning)
  return(datPartitioning)
}

generate_datPartitioning <- function(input, prefix, datDicing) {
  set.seed(get_input_value(input, prefix, "seed"))
  df <- caPartitioningCC(
    datDicing(),
    probCCDice    = get_input_value(input, prefix, "prob_cc_dice"),
    trDicerMean	  = -1.42,
    trDicerSd     = 0.52,
    nDicer        = get_input_value(input, prefix, "n_dicer"),
    b             = get_input_value(input, prefix, "b_canta"),
    unitSize      = get_input_value(input, prefix, "unit_size_dic")
    )
  return(df)
}


ca_PartitioningInputs_ui <- function(id) {
  ns <- NS(id)
  div(
  id = ns("Partitioning"),
#  tagList(
    sliderInput(ns("prob_cc_dice"),
                "probCCDice: Probability of cross-contamination from the dicing machine",
                value = 0.5, min = 0.0, max = 1.0, step=0.1),
    sliderInput(ns("n_dicer"),
                "nDicer: Numbers of LM on the surface of the dicing machine",
                value = 100, min = 0, max = 500, step=100),
     sliderInput(ns("unit_size_dic"),
                 "unitSize: Weight of a pack of cantaloupe dices",
                 value = 200, min = 100, max = 500, step=50),
    sliderInput(ns("b_canta"),
                "Dispersion factor of the beta distribution",
                value = 1, min = 1, max = 2, step=0.05)
#    )
  )
}
