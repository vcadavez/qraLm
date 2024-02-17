fv_sidebar_ui <- function(id) {
  ns = NS(id)
  column(4,
    wellPanel(
      h3("Properties"),
      uiOutput(ns("selector")),
      uiOutput(ns("inputs"))
      )
  )
}

fv_sidebar_server <- function(input, output, session, suffix, fv_stages) {
  ns = NS(suffix)
  id = ns("sidebar")

  output[[id]] <- renderUI({
    fv_sidebar_ui(id)
  })
  
  source("pages/frozenvegetables/sidebar/stageSelector.R")
  fv_stageSelector_server(input, output, session, id, fv_stages)
  source("pages/frozenvegetables/sidebar/stageInputs.R")
  fv_stageInputs_server(input, output, session, id, fv_stages)
}