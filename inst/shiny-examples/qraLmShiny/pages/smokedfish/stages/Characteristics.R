sf_Characteristics_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    fluidRow( h5("Smoked fish characteristics"),
      column(12,
            # h5("Smoked fish characteristics"), dataUI("rte_chars")
            h5(histUI("histograms"))
             )
      )
    )
}

sf_Characteristics_server <- function(input, output, session, suffix, datPack) {
  ns <- NS(suffix)
  id <- ns("Characteristics")
  
  output[[id]] <- renderUI({ sf_Characteristics_ui(id) })
  
  prefix <- "smokedfish-sidebar-inputs-"
  
  RTE <- reactive({ generate_RTE(input, prefix, datPack) })
  
  histServer("histograms", data = RTE)
  
  return(RTE)
}

generate_RTE <- function(input, prefix, datPack) {
  set.seed(get_input_value(input, prefix, "seed"))
    df <- sfCharacteristics(
    nLots=datPack()$nLots,
    awminSF=NULL,
    awmodeSF=NULL,
    awmaxSF=NULL,
    NaClminSF  = get_input_value(input, prefix, "NaCl_min_SF"),  # 1.5 
    NaClmodeSF = get_input_value(input, prefix, "NaCl_mode_SF"), # 3.4 
    NaClmaxSF  = get_input_value(input, prefix, "NaCl_max_SF"),  # 5.3 
    PminSF     = get_input_value(input, prefix, "P_min_SF"),  # 5.0 
    PmodeSF    = get_input_value(input, prefix, "P_mode_SF"),  # 10 
    PmaxSF     = get_input_value(input, prefix, "P_max_SF"),  # 22 
    pHminSF    = get_input_value(input, prefix, "pH_min_SF"),  # 5.8 
    pHmodeSF   = get_input_value(input, prefix, "pH_mode_SF"), #  6.1 
    pHmaxSF    = get_input_value(input, prefix, "pH_max_SF"), # 6.5 
    CO2equilibriumminSF  = get_input_value(input, prefix, "CO2equi_min_SF"), # 0.25
    CO2equilibriummodeSF = get_input_value(input, prefix, "CO2equi_mode_SF"), # 0.25
    CO2equilibriummaxSF  = get_input_value(input, prefix, "CO2equi_max_SF"), # 0.30
    NITminSF=0,
    NITmodeSF=0,
    NITmaxSF=0,
    aaWphminSF=0,
    aaWphmodeSF=0,
    aaWphmaxSF=0,
    baWphminSF=0,
    baWphmodeSF=0,
    baWphmaxSF=0,
    caWphminSF=0,
    caWphmodeSF=0,
    caWphmaxSF=0,
    daWphminSF  = get_input_value(input, prefix, "daWph_min_SF"), # 500
    daWphmodeSF = get_input_value(input, prefix, "daWph_mode_SF"), # 1500
    daWphmaxSF  = get_input_value(input, prefix, "daWph_max_SF"), # 1900
    laWphminSF  = get_input_value(input, prefix, "laWph_min_SF"), # 6000
    laWphmodeSF = get_input_value(input, prefix, "laWph_mode_SF"), # 12000
    laWphmaxSF  = get_input_value(input, prefix, "laWph_max_SF"), # 28000
    saWphminSF=0,
    saWphmodeSF=0,
    saWphmaxSF=0
  )
  return(df)
}


sf_CharacteristicsInputs_ui <- function(id) {  
  ns <- NS(id)  
   div(  
   id = ns("Characteristics"),   
#  tagList(
# SF characteristics    
    sliderInput(ns("NaCl_min_SF"),     "NaClminSF: Minimum NaCl concentration (%)",
                value = 1.5, min = 0, max = 3, step=0.1),
    sliderInput(ns("NaCl_mode_SF"),     "NaClmodeSF: Mode of NaCl concentration(%)",
                value = 3.4, min = 2.0, max = 4.0, step=0.1),
    sliderInput(ns("NaCl_max_SF"),     "NaClmaxSF: Maximum NaCl concentration (%)",
                value = 5.3, min = 4.0, max = 7.0, step=0.1),
    sliderInput(ns("P_min_SF"),     "PminSF: Minimum phenol concentration (ppm)",
                value = 5, min = 0, max = 10, step=1),
    sliderInput(ns("P_mode_SF"),     "PmodeSF: Mode of phenol concentration (ppm)",
                value = 10, min = 5, max = 10, step=1),
    sliderInput(ns("P_max_SF"),     "PmaxSF: Maximum phenol concentration (ppm)",
                value = 22, min = 10, max = 40, step=1),
    sliderInput(ns("pH_min_SF"),     "pHminSF: Minimum pH",
                value = 5.8, min = 4.0, max = 6.0, step=0.1),
    sliderInput(ns("pH_mode_SF"),     "pHmodeSF: Mode of pH",
                value = 6.1, min = 4.5, max = 6.5, step=0.1),
    sliderInput(ns("pH_max_SF"),     "pHmaxSF: Maximum pH",
                value = 6.5, min = 5.0, max = 7.0, step=0.1),
    sliderInput(ns("CO2equi_min_SF"),     "CO2equiminSF: Minimum CO2 equilibrium (%)",
                value = 0.25, min = 0, max = 1.0, step=0.05),
    sliderInput(ns("CO2equi_mode_SF"),     "CO2equimodeSF: Mode CO2 equilibrium (%)",
                value = 0.25, min = 0, max = 1.0, step=0.05),
    sliderInput(ns("CO2equi_max_SF"),     "CO2equimaxSF: Maximum CO2 equilibrium (%)",
                value = 0.30, min = 0, max = 1.0, step=0.05),
    sliderInput(ns("daWph_min_SF"),     "daWphminSF: Minimum diacetate concentration (ppm)",
                value = 0, min = 0, max = 1500, step=100),
    sliderInput(ns("daWph_mode_SF"),     "daWphmodeSF: Mode of diacetate concentration (ppm)",
                value = 0, min = 0, max = 2000, step=100),
    sliderInput(ns("daWph_max_SF"),     "daWphmaxSF: Maximum diacetate concentration (ppm)",
                value = 0, min = 0, max = 3000, step=100),
    sliderInput(ns("laWph_min_SF"),     "laWphminSF: Minimum lactic acid concentration (ppm)",
                value = 0, min = 0, max = 10000, step=100),
    sliderInput(ns("laWph_mode_SF"),     "laWphmodeSF: Mode of lactic acid concentration (ppm)",
                value = 0, min = 0, max = 20000, step=100),
    sliderInput(ns("laWph_max_SF"),     "laWphmaxSF: Maximum lactic acid concentration (ppm)",
                value = 0, min = 0, max = 35000, step=100)
#    ) 
  )
}