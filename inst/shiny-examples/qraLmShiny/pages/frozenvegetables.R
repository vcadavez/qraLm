frozenvegetables_ui <- function(id) {
  ns = NS(id)
  tabPanel(
    "Frozen Vegetables",
    fluidRow(
      uiOutput(ns("sidebar")),
      uiOutput(ns("stages"))
    )
  )
}

frozenvegetables_server <- function(input, output, session, id) {
  
  fv_stages <- c("Production", "Blanching", "Partitioning", "Testing", "Portioning", "Defrosting", "Cooking", "Risk")
  
  source("pages/frozenvegetables/sidebar.R")
  
  fv_sidebar_server(input, output, session, id, fv_stages)

  source("pages/frozenvegetables/stages.R")
  
  fv_stages_server(input, output, session, id, fv_stages)
  }
