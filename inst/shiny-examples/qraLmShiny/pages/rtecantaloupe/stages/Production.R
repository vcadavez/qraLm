ca_Production_ui <- function(id) {
  fluidRow(
    column(6,
      h5("Prevalence of contaminated lots"), prevLotsUI("ca_prev_lots"),
      h5("Mean counts in contaminated lots"), mcstatsLotsUI("ca_lot_stats"),
      h5("Distribution of between-lot mean counts"), countsLotsDistUI("ca_lot_counts")
    ),
    column(6,
      h5("Prevalence of contaminated units"), prevUnitsUI("ca_prev_units"),
      h5("Counts in contaminated units"), mcstatsUnitsUI("ca_unit_stats"),
      h5("Distribution of between-unit counts"), countsUnitsDistUI("ca_unit_counts")
    ),
    column(12,
      h5("Cumulative distribution of mean counts in contaminated lots"), ecdfLotsUI("ca_ecdf")
    )
  )
}

ca_Production_server <- function(input, output, session, suffix) {
  ns <- NS(suffix)
  id <- ns("Production")
  output[[id]] <- renderUI({ ca_Production_ui(id) })

  prefix <- "rtecantaloupe-sidebar-inputs-"
  datCanta <- reactive({ generate_datCanta(input, prefix) })

  prevLotsServer("ca_prev_lots",          data = datCanta)
  prevUnitsServer("ca_prev_units",        data = datCanta)
  mcstatsLotsServer("ca_lot_stats",       data = datCanta)
  mcstatsUnitsServer("ca_unit_stats",     data = datCanta)
  countsLotsDistServer("ca_lot_counts",   data = datCanta)
  countsUnitsDistServer("ca_unit_counts", data = datCanta)
  prevWithinlotsServer("ca_within_lots",  data = datCanta)
  ecdfLotsServer("ca_ecdf",               data = datCanta)

  return(datCanta)
}

generate_datCanta <- function(input, prefix) { #first stage, no input
  df <- caPrimaryProduction(
    nLots        = get_input_value(input, prefix, "n_lots"),
    sizeLot      = get_input_value(input, prefix, "size_lot"),
    cantaWeight  = get_input_value(input, prefix, "canta_weight"),
    pSoil        = get_input_value(input, prefix, "p_soil"),
    fManure      = get_input_value(input, prefix, "f_manure"),
    pManure      = get_input_value(input, prefix, "p_manure"),
    fIrrigRaining= get_input_value(input, prefix, "f_irrig_raining"),
    pIrrigRaining= get_input_value(input, prefix, "p_irrig_raining"),
    cSoilLogMin  = get_input_value(input, prefix, "c_soil_log_min"),
    cSoilLogMode = get_input_value(input, prefix, "c_soil_log_mode"),
    cSoilLogMax  = get_input_value(input, prefix, "c_soil_log_max"),
    qSoilMin     = get_input_value(input, prefix, "q_soil_min"),
    qSoilMode    = get_input_value(input, prefix, "q_soil_mode"),
    qSoilMax     = get_input_value(input, prefix, "q_soil_max"),
    pFoil        = get_input_value(input, prefix, "p_foil"),
    rFoil        = get_input_value(input, prefix, "r_foil"),
    pIrrig       = get_input_value(input, prefix, "p_irrig"),
    cIrrigLogMin = get_input_value(input, prefix, "c_irrig_log_min"),
    cIrrigLogMax = get_input_value(input, prefix, "c_irrig_log_max"),
    pWaterGainMin= get_input_value(input, prefix, "p_water_gain_min"),
    pWaterGainMax= get_input_value(input, prefix, "p_water_gain_max")
    )
  return(df)
}

ca_ProductionInputs_ui <- function(id) {  
  ns <- NS(id)  
  div(  
  id = ns("Production"),  
#    tagList(  
      numericInput(ns("seed"), "Set a random seed", 
                   value = 12345),  
      numericInput(ns("n_lots"), "nLots: Number of lots",
                   value = 500,  min = 500, max = 5000, step = 500),
      numericInput(ns("size_lot"), "lotSize: Number of units in a lot",
                   value = 1000,  min = 1000, max = 5000, step = 500),
      numericInput(ns("canta_weight"), "cantaweight: Cantaloupe weight (g)", 
                   value = 1000,  min = 500, max = 1500, step = 100),
      sliderInput(ns("p_soil"), "pSoil: Prevalence of contamination of soil",
                  value = 0.089, min = 0.0, max = 1.0, step = 0.01),
      sliderInput(ns("p_manure"), "pManure: Proportion of fields using organic amendments",
                  value = 0.5, min = 0.0, max = 1.0, step = 0.10),
      sliderInput(ns("f_manure"), "fManure: Odds-ratio estimate associated to organic amendment", 
            value = 7, min = 0.0, max = 10.0, step = 1.0),
      sliderInput(ns("c_soil_log_min"), "cSoilLogMin: Minimum value variability of concentration",
                  value = -1, min = -2, max = 2, step = 0.25),
      sliderInput(ns("c_soil_log_mode"), "cSoilLogMode: Mode value of the variability of concentration",
                  value = 0.6, min = -1, max = 2, step = 0.25),
      sliderInput(ns("c_soil_log_max"), "cSoilLogMax: Maximum value of the variability of concentration",
                  value = 1.48, min = -1, max = 3, step = 0.25),
      sliderInput(ns("q_soil_min"), "qSoilMin: Minimum value of the variability of quantity of soil deposited on cantaloupe",
                  value = 0.05, min = 0, max = 3, step = 0.05),
      sliderInput(ns("q_soil_mode"), "qSoilMode: Mode value of the variability of quantity of soil deposited on cantaloupe",
                  value = 0.5, min = 0, max = 3, step = 0.05),
      sliderInput(ns("q_soil_max"), "qSoilMax: Maximum value of the variability of quantity of soil deposited on cantaloupe",
                  value = 5, min = 0, max = 10, step = 0.05),
      sliderInput(ns("p_foil"), "pFoil: Proportion of fields grown in foil (e.g. plastic mulch)",
                  value = 0.5, min = 0, max = 1, step = 0.1),      
      sliderInput(ns("r_foil"), "rFoil: Reduction fraction of the quantity of soil transferred to rind",
                  value = 0.9, min = 0, max = 1, step = 0.1),
      sliderInput(ns("f_irrig_raining"), "fIrrigRaining: Odds-ratio irrigation and raining events", 
                  value = 25, min = 0.0, max = 50, step = 1.0),
      sliderInput(ns("p_irrig_raining"),  "pIrrigRaining: Proportion of fields irrigated or raining just previous harvest",
                  value = 0.1, min = 0.0, max = 1, step = 0.1),
      sliderInput(ns("p_irrig"), "pIrrig: Prevalence of contamination in irrigation water",
                  value = 0.131, min = 0, max = 1, step = 0.1),
      sliderInput(ns("c_irrig_log_min"), "cIrrigLogMin: Minimum value of the uniform distribution",
                  value = -0.152, min = -1, max = 1, step = 0.1),
      sliderInput(ns("c_irrig_log_max"), "cIrrigLogMax: Maximum value of the uniform distribution",
                  value = 1.04, min = -1, max = 2, step = 0.1),
      sliderInput(ns("p_water_gain_min"), "pWaterGainMin: Minimum value of the fraction of water gain (ml)",
                  value = 0, min = 0, max = 0.25, step = 0.01),
      sliderInput(ns("p_water_gain_max"), "pWaterGainMax: Maximum value of the fraction of water gain (ml)",
                  value = 0.004, min = 0, max = 0.5, step = 0.01)
#      )  
  )   
}
