smokedfish_ui <- function(id) {
  ns = NS(id)
  tabPanel(
    "Smoked Fish",
    fluidRow(
      uiOutput(ns("sidebar")),
      uiOutput(ns("stages"))
    )
  )
}

smokedfish_server <- function(input, output, session, id) {
  sf_stages <- c("Production", "Prefilleting", "Filleting",
                 "Holding", "BriningOrSalting", "Smoking", 
                 "Slicing", "Packaging", "Characteristics" ,"ColdChain", 
                 "Home", "Portioning", "Risk")
  
  source("pages/smokedfish/sidebar.R")
  sf_sidebar_server(input, output, session, id, sf_stages)

  source("pages/smokedfish/stages.R")
  sf_stages_server(input, output, session, id, sf_stages)
}
