sf_rteCharacteristics_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    fluidRow(
      column(12,
             h5("Fish characteristics"), dataUI("sf_datRTE")
      )
    )
  )
}

sf_rteCharacteristics_server <- function(input, output, session, suffix, datPack) {
  ns <- NS(suffix)
  id <- ns("rteCharacteristics")
  
  output[[id]] <- renderUI({ sf_rteCharacteristics_ui(id) })
  
  prefix <- "smokedfish-sidebar-inputs-"
  
  datRTE <- reactive({ generate_datRTE(input, prefix, datPack) })
  
  dataServer("sf_datRTE", data = datRTE)
  
  return(datRTE)
}

generate_datRTE <- function(input, prefix, datPack) {
  df <- sfCharacteristics(
    nLots = datPack()$nLots,
    awminSF    =get_input_value(input, prefix, "aw_min_SF"),
    awmodeSF   =get_input_value(input, prefix, "aw_mode_SF"),
    awmaxSF    =get_input_value(input, prefix, "aw_max_SF"),
    NaClminSF  =get_input_value(input, prefix, "NaCl_min_SF"), 
    NaClmodeSF =get_input_value(input, prefix, "NaCl_mode_SF"), 
    NaClmaxSF  =get_input_value(input, prefix, "NaCl_max_SF"), 
    PminSF     =get_input_value(input, prefix, "P_min_SF"), 
    PmodeSF    =get_input_value(input, prefix, "P_mode_SF"), 
    PmaxSF     =get_input_value(input, prefix, "P_max_SF"), 
    pHminSF    =get_input_value(input, prefix, "pH_min_SF"), 
    pHmodeSF   =get_input_value(input, prefix, "pH_mode_SF"), 
    pHmaxSF    =get_input_value(input, prefix, "pH_max_SF"), 
    CO2equilibriumminSF =get_input_value(input, prefix, "CO2equilibrium_min_SF"),
    CO2equilibriummodeSF =get_input_value(input, prefix, "CO2equilibrium_mode_SF"),
    CO2equilibriummaxSF =get_input_value(input, prefix, "CO2equilibrium_max_SF"),
    NITminSF    =get_input_value(input, prefix, "NIT_min_SF"),
    NITmodeSF   =get_input_value(input, prefix, "NIT_mode_SF"),
    NITmaxSF    =get_input_value(input, prefix, "NIT_max_SF"),
    aaWphminSF  =get_input_value(input, prefix, "aaWph_min_SF"),
    aaWphmodeSF =get_input_value(input, prefix, "aaWph_mode_SF"),
    aaWphmaxSF  =get_input_value(input, prefix, "aaWph_max_SF"),
    baWphminSF  =get_input_value(input, prefix, "baWph_min_SF"),
    baWphmodeSF =get_input_value(input, prefix, "baWph_mode_SF"),
    baWphmaxSF  =get_input_value(input, prefix, "baWph_max_SF"),
    caWphminSF  =get_input_value(input, prefix, "caWph_min_SF"),
    caWphmodeSF =get_input_value(input, prefix, "caWph_mode_SF"),
    caWphmaxSF  =get_input_value(input, prefix, "caWph_max_SF"),
    daWphminSF  =get_input_value(input, prefix, "daWph_min_SF"),
    daWphmodeSF =get_input_value(input, prefix, "daWph_mode_SF"),
    daWphmaxSF  =get_input_value(input, prefix, "daWph_max_SF"),
    laWphminSF  =get_input_value(input, prefix, "laWph_min_SF"),
    laWphmodeSF =get_input_value(input, prefix, "laWph_mode_SF"),
    laWphmaxSF  =get_input_value(input, prefix, "laWph_max_SF"),
    saWphminSF  =get_input_value(input, prefix, "saWph_min_SF"),
    saWphmodeSF =get_input_value(input, prefix, "saWph_mode_SF"),
    saWphmaxSF  =get_input_value(input, prefix, "saWph_max_SF")
  )
  return(df)
}

sf_rteCharacteristicsInputs_ui <- function(id) {  
  ns <- NS(id)  
  div(  
  id = ns("ColdChain"),   
#  tagList(
    sliderInput(ns("aw_min_SF"), "Minimum water activity of RTE",
                value = 0, min = 0.890, max = 1, step=0.001),
    sliderInput(ns("aw_mode_SF"), "Mode water activity of RTE",
                value = 0, min = 0.890, max = 1, step=0.001),
    sliderInput(ns("aw_max_SF"), "Maximum water activity of RTE",
                value = 0, min = 0.890, max = 1, step=0.001),
    sliderInput(ns("NaCl_min_SF"), "Minimum NaCl of RTE (%)",
                value = 0, min = 0.890, max = 1, step=0.001),
    sliderInput(ns("NaCl_mode_SF"), "Mode NaCl of RTE (%)",
                value = 0, min = 0.890, max = 1, step=0.001),
    sliderInput(ns("NaCl_max_SF"), "Maximum NaCl of RTE (%)",
                value = 0, min = 0.890, max = 1, step=0.001),
    sliderInput(ns("pH_min_SF"), "Minimum pH of RTE",
                value = 5.8, min = 5.3, max = 7.0, step=0.1),
    sliderInput(ns("pH_mode_SF"), "Mode pH of RTE",
                value = 6.1, min = 5.5, max = 7.0, step=0.1),
    sliderInput(ns("pH_max_SF"), "Maximum pH of RTE",
                value = 6.5, min = 6.3, max = 7.0, step=0.1),
    sliderInput(ns("P_min_SF"), "Minimum phenol concentration in RTE (ppm)",
                value = 5.8, min = 5.0, max = 7.0, step=0.1),
    sliderInput(ns("P_mode_SF"), "Mode phenol concentration in RTE (ppm)",
                value = 10.0, min = 6.0, max = 15.0, step=0.1),
    sliderInput(ns("P_max_SF"), "Maximum phenol concentration in RTE (ppm)",
                value = 22.0, min = 15.0, max = 30.0, step=0.1),
    sliderInput(ns("CO2equilibrium_min_SF"), "Minimum `CO_2` concentration in atmosphere in RTE (%)",
                value = 0.25, min = 0.20, max = 0.30, step=0.01),
    sliderInput(ns("CO2equilibrium_mode_SF"), "Mode of `CO_2` concentration in atmosphere in RTE (%)",
                value = 0.25, min = 0.20, max = 0.30, step=0.01),
    sliderInput(ns("CO2equilibrium_max_SF"), "Maximum `CO_2` concentration in atmosphere in RTE (%)",
                value = 0.30, min = 0.20, max = 0.30, step=0.01),
    sliderInput(ns("NIT_min_SF"), "Minimum nitrites concentration in RTE (ppm)",
                value = 0, min = 0, max = 0.30, step=0.01),
    sliderInput(ns("NIT_mode_SF"), "Mode of nitrites concentration in RTE (ppm)",
                value = 0, min = 0, max = 0.30, step=0.01),
    sliderInput(ns("NIT_max_SF"), "Maximum nitrites concentration in RTE (ppm)",
                value = 0, min = 0, max = 0.30, step=0.01),
    sliderInput(ns("aaWph_min_SF"), "Minimum acetic acid concentration in RTE (ppm)",
                value = 0, min = 0, max = 0.30, step=0.01),
    sliderInput(ns("aaWph_mode_SF"), "Mode of acetic acid concentration in RTE (ppm)",
                value = 0, min = 0, max = 0.30, step=0.01),
    sliderInput(ns("aaWph_max_SF"), "Maximum acetic acid concentration in RTE (ppm)",
                value = 0, min = 0, max = 1000, step=100),
    sliderInput(ns("baWph_min_SF"), "Minimum benzoic acid concentration in RTE (ppm)",
                value = 0, min = 0, max = 1000, step=100),
    sliderInput(ns("baWph_mode_SF"), "Mode of benzoic acid concentration in RTE (ppm)",
                value = 0, min = 0, max = 1000, step=100),
    sliderInput(ns("baWph_max_SF"), "Maximum benzoic acid concentration in RTE (ppm)",
                value = 0, min = 0, max = 1000, step=100),
    sliderInput(ns("caWph_min_SF"), "Minimum citric acid concentration in RTE (ppm)",
                value = 0, min = 0, max = 1000, step=100),
    sliderInput(ns("caWph_mode_SF"), "Mode of citric acid concentration in RTE (ppm)",
                value = 0, min = 0, max = 1000, step=100),
    sliderInput(ns("caWph_max_SF"), "Maximum citric acid concentration in RTE (ppm)",
                value = 0, min = 0, max = 1000, step=100),
    sliderInput(ns("daWph_min_SF"), "Minimum diacetate acid concentration in RTE (ppm)",
                value = 500, min = 0, max = 2500, step=100),
    sliderInput(ns("daWph_mode_SF"), "Mode of diacetate acid concentration in RTE (ppm)",
                value = 1500, min = 0, max = 2500, step=100),
    sliderInput(ns("daWph_max_SF"), "Maximum diacetate acid concentration in RTE (ppm)",
                value = 1900, min = 0, max = 2500, step=100),
    sliderInput(ns("laWph_min_SF"), "Minimum lactic acid concentration in RTE (ppm)",
                value = 6000,   min = 5000, max = 10000, step=1000),
    sliderInput(ns("laWph_mode_SF"), "Mode of lactic acid concentration in RTE (ppm)",
                value = 12000,  min = 10000, max = 15000, step=1000),
    sliderInput(ns("laWph_max_SF"), "Maximum lactic acid concentration in RTE (ppm)",
                value = 28000,  min = 20000, max = 35000, step=1000),
    sliderInput(ns("saWph_min_SF"), "Minimum sorbic acid concentration in RTE (ppm)",
                value = 0,   min = 500, max = 1000, step=100),
    sliderInput(ns("saWph_mode_SF"), "Mode of sorbic acid concentration in RTE (ppm)",
                value = 0,  min = 1000, max = 1500, step=100),
    sliderInput(ns("saWph_max_SF"), "Maximum sorbic acid concentration in RTE (ppm)",
                value = 0,  min = 2000, max = 3000, step=100)
# )
  )
}