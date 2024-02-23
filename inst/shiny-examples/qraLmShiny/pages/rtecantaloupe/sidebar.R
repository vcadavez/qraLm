ca_sidebar_ui <- function(id) {
  ns <- NS(id)
  column(4,
    wellPanel(
      uiOutput(ns("selector")),
      h3("Select Parameters"),
      uiOutput(ns("inputs"))
    )
  )
}

ca_sidebar_server <- function(input, output, session, suffix, ca_stages) {
  ns <- NS(suffix)
  id <- ns("sidebar")

  output[[id]] <- renderUI({
    ca_sidebar_ui(id)
  })

  source("pages/rtecantaloupe/sidebar/stageSelector.R")
  ca_stageSelector_server(input, output, session, id, ca_stages)
  source("pages/rtecantaloupe/sidebar/stageInputs.R")
  ca_stageInputs_server(input, output, session, id, ca_stages)
}
