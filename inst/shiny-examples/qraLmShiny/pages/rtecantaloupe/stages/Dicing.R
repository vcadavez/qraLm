ca_Dicing_ui <- function(id) {
  ns <- NS(id)
  
  fluidRow(
    column(6, 
           h5("Prevalence of contaminated lots"), prevLotsUI("prev_lots_dicing"),
           h5("Mean counts in contaminated lots"), mcstatsLotsUI("lots_mcstats_dicing")
           #           h5("Distribution of between-lot mean counts"), countsLotsDistUI("counts_lots_dist_dicing")
    ),
    column(6, 
           h5("Prevalence of contaminated units"), prevUnitsUI("prev_units_dicing"),
           h5("Counts in contaminated units"), mcstatsUnitsUI("units_mcstats_dicing")
           #           h5("Distribution of between-unit counts"), countsUnitsDistUI("counts_units_dist_dicing")
    ),
    column(12, 
           h5("Empirical Cumulative Distribution Function (ECDF)"), ecdfLotsUI("ecdf_prob_dicing")
    )
  )
}

ca_Dicing_server <- function(input, output, session, suffix, datWashing) {
  ns <- NS(suffix)
  id <- ns("Dicing")
  
  output[[id]] <- renderUI({ ca_Dicing_ui(id) })
  
  prefix <- "rtecantaloupe-sidebar-inputs-"
  datDicing <- reactive({ generate_datDicing(input, prefix, datWashing) })
  
  prevLotsServer("prev_lots_dicing",                data=datDicing)
  prevUnitsServer("prev_units_dicing",              data=datDicing)
  mcstatsLotsServer("lots_mcstats_dicing",          data=datDicing)
  mcstatsUnitsServer("units_mcstats_dicing",        data=datDicing)
  countsLotsDistServer("counts_lots_dist_dicing",   data=datDicing)
  countsUnitsDistServer("counts_units_dist_dicing", data=datDicing)
  ecdfLotsServer("ecdf_prob_dicing",                data=datDicing)
  return(datDicing)
}

generate_datDicing <- function(input, prefix, datWashing) {
  set.seed(get_input_value(input, prefix, "seed"))
  df <- caDicing(
    datWashing(),
    minTR = 0.087,
    modeTR = 0.55,
    maxTR = 2.82,
    cantaSurface  = 580,
    cantaRindFree = get_input_value(input, prefix, "canta_rind_free"),
    sizeSublot = get_input_value(input, prefix, "size_sublot")
    )
  return(df)
}

ca_DicingInputs_ui <- function(id) {
  ns <- NS(id)
  div(
  id = ns("Dicing"),
#  tagList(
sliderInput(ns("canta_rind_free"),
            "cantaRindFree: Weight of a seedless rind-free cantaloupe (%)",
            value = 950, min = 600, max = 1000, step=50),
    sliderInput(ns("size_sublot"),
                "sizeSublot: Number of cantaloupes to be diced",
                value = 500, min = 50, max = 1000, step=50)
#    )
  )
}
