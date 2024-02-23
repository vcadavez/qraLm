sf_sidebar_ui <- function(id) {
  ns <- NS(id)
  column(4,
    wellPanel(
      uiOutput(ns("selector")),
      h3("Select Parameters"),
      uiOutput(ns("inputs"))
    )
  )
}

sf_sidebar_server <- function(input, output, session, suffix, sf_stages) {
  ns <- NS(suffix)
  id <- ns("sidebar")

  output[[id]] <- renderUI({
    sf_sidebar_ui(id)
  })

  source("pages/smokedfish/sidebar/stageSelector.R")
  sf_stageSelector_server(input, output, session, id, sf_stages)
  source("pages/smokedfish/sidebar/stageInputs.R")
  sf_stageInputs_server(input, output, session, id, sf_stages)
}
