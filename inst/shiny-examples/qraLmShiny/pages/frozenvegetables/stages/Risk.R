fv_Risk_ui <- function(id) {
  ns = NS(id)
  
  fluidPage(
#    h3("Risk computation"),
  fluidRow(
    column(8,
           h5("Risk summary"), riskStatsUI("fv_risk_stats")
           ),
    column(8,
           h5("Risk plots"), riskDistUI("fv_risk_dist")
           )
    )
  )
}

fv_Risk_server <- function(input, output, session, suffix, datCook) {
  ns <- NS(suffix)
  id <- ns("Risk")
  
  output[[id]] <- renderUI({ fv_Risk_ui(id) })
  
  prefix <- "frozenvegetables-sidebar-inputs-"
  datRisk <- reactive({ generate_datRisk(input, prefix, datCook) })
  
  riskStatsServer("fv_risk_stats",  data = datRisk)
  riskDistServer("fv_risk_dist",    data = datRisk)
  
  return(datRisk)
}

generate_datRisk <- function(input, prefix, datCook) {
  df <- DRForModel(
                   datCook(),
                   model = get_input_value(input, prefix, "Model"),
                     if (get_input_value(input, prefix, "Model") == "JEMRA") {
                        population = get_input_value(input, prefix, "PopulationJEMRA")
                     } else if (get_input_value(input, prefix, "Model") == "EFSA") {
                       population = get_input_value(input, prefix, "PopulationEFSA")
                     } else if (get_input_value(input, prefix, "Model") == "EFSALV") {
                        population = get_input_value(input, prefix, "PopulationEFSALV")
                      } else if (get_input_value(input, prefix, "Model") == "EFSAV") {
                        population = get_input_value(input, prefix, "PopulationEFSAV")
                      } else if (get_input_value(input, prefix, "Model") == "EFSAMV") {
                        population = get_input_value(input, prefix, "PopulationEFSAMV")
                      } else if(get_input_value(input, prefix, "Model") == "Pouillot") {
                        population = get_input_value(input, prefix, "PopulationPouillot")
                      } else {
                        population = get_input_value(input, prefix, "PopulationFritsch")
                      },
                   Poisson = FALSE
                   )
  return(df)
}

fv_RiskInputs_ui <- function(id) {  
  ns <- NS(id)  
  div(  
  id = ns("Risk"),   
#  tagList(
    selectInput(ns("Model"), label = "Select DR Model:", 
                choices = list("JEMRA" = "JEMRA", 
                               "EFSA" = "EFSA",
                               "EFSALV" = "EFSALV",
                               "EFSAV" = "EFSAV",
                               "EFSAMV" = "EFSAMV",
                               "Pouillot" = "Pouillot",
                               "Fritsch" = "Fritsch"),
                selected = "JEMRA"
    ),
    conditionalPanel(sprintf("input['%s'] == 'JEMRA'", ns("Model")),
                     selectInput(ns("PopulationJEMRA"), label = "Select population:", 
                                 choices = list("Healthy population" = 1,
                                                "Increased susceptibility" = 2),
                                 selected = 2
                     )
    ),
    conditionalPanel(sprintf("input['%s'] == 'Pouillot'", ns("Model")),
                     selectInput(ns("PopulationPouillot"), "Select population",
                                 choices = list("Less than 65 years old" = 1,
                                                "More than 65 years old" = 2,
                                                "Pregnancy" = 3,
                                                "Nonhematological Cancer" = 4,
                                                "Hematological cancer" = 5,
                                                "Renal or Liver failure" = 6,
                                                "Solid organ transplant" = 7,
                                                "Inflammatory diseases" = 8,
                                                "HIV/AIDS" = 9,
                                                "Diabetes" = 10,
                                                "Hear diseases" = 11),
                                 selected = 1
                     )
    ),
    
    conditionalPanel(sprintf("input['%s'] == 'EFSA'", ns("Model")),
                     selectInput(ns("PopulationEFSA"), "Select population",
                                 choices = list("Female 1-4 yo" = 1,
                                                "Male 1-4 yo" = 2,
                                                "Female 5-14 yo" = 3,
                                                "Male 5-14 yo" = 4,
                                                "Female 15-24 yo" = 5,
                                                "Male 15-24 yo" = 6,
                                                "Female 25-44 yo" = 7,
                                                "Male 25-44 yo" = 8,
                                                "Female 45-64 yo" = 9,
                                                "Male 45-64 yo" = 10,
                                                "Female 65-74 yo" = 11,
                                                "Male 65-74 yo" = 12,
                                                "Female >75 yo" = 13,
                                                "Male >75 yo" = 14),
                                 selected = 1
                     )
    ),
    
    conditionalPanel(sprintf("input['%s'] == 'EFSALV'", ns("Model")),
                     selectInput(ns("PopulationEFSALV"), "Select population",
                                 choices = list("Female 1-4 yo" = 1,
                                                "Male 1-4 yo" = 2,
                                                "Female 5-14 yo" = 3,
                                                "Male 5-14 yo" = 4,
                                                "Female 15-24 yo" = 5,
                                                "Male 15-24 yo" = 6,
                                                "Female 25-44 yo" = 7,
                                                "Male 25-44 yo" = 8,
                                                "Female 45-64 yo" = 9,
                                                "Male 45-64 yo" = 10,
                                                "Female 65-74 yo" = 11,
                                                "Male 65-74 yo" = 12,
                                                "Female >75 yo" = 13,
                                                "Male >75 yo" = 14),
                                 selected = 1
                     )
    ),
    
    conditionalPanel(sprintf("input['%s'] == 'EFSAV'", ns("Model")),
                     selectInput(ns("PopulationEFSAV"), "Select population",
                                 choices = list("Female 1-4 yo" = 1,
                                                "Male 1-4 yo" = 2,
                                                "Female 5-14 yo" = 3,
                                                "Male 5-14 yo" = 4,
                                                "Female 15-24 yo" = 5,
                                                "Male 15-24 yo" = 6,
                                                "Female 25-44 yo" = 7,
                                                "Male 25-44 yo" = 8,
                                                "Female 45-64 yo" = 9,
                                                "Male 45-64 yo" = 10,
                                                "Female 65-74 yo" = 11,
                                                "Male 65-74 yo" = 12,
                                                "Female >75 yo" = 13,
                                                "Male >75 yo" = 14),
                                 selected = 1
                     )
    ),
    
    conditionalPanel(sprintf("input['%s'] == 'EFSAMV'", ns("Model")),
                     selectInput(ns("PopulationEFSAMV"), "Select population",
                                 choices = list("Female 1-4 yo" = 1,
                                                "Male 1-4 yo" = 2,
                                                "Female 5-14 yo" = 3,
                                                "Male 5-14 yo" = 4,
                                                "Female 15-24 yo" = 5,
                                                "Male 15-24 yo" = 6,
                                                "Female 25-44 yo" = 7,
                                                "Male 25-44 yo" = 8,
                                                "Female 45-64 yo" = 9,
                                                "Male 45-64 yo" = 10,
                                                "Female 65-74 yo" = 11,
                                                "Male 65-74 yo" = 12,
                                                "Female >75 yo" = 13,
                                                "Male >75 yo" = 14),
                                 selected = 1
                     )
    ),
    conditionalPanel(sprintf("input['%s'] == 'Fritsch'", ns("Model")),
                     selectInput(ns("PopulationFritsch"), "Select population",
                                 choices = list("Highly virulent" = 1,
                                                "Medium virulent" = 2,
                                                "Hypovirulent" = 3),
                                 selected = 1
                     )
    )
#  ) 
  )
}