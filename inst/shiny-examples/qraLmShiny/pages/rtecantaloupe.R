rtecantaloupe_ui <- function(id) {
  ns = NS(id)
  tabPanel(
    "RTE Cantaloupe",
    fluidRow(
      uiOutput(ns("sidebar")),
      uiOutput(ns("stages"))
    )
  )
}

rtecantaloupe_server <- function(input, output, session, id) {
  ca_stages <- c("Production", "Harvest","Brushing","Storing","Washing","Dicing",
                 "Partitioning","Testing","Transport","RTEStorage",
                 "ConsumersTransport","StoredDices","Risk")

  source("pages/rtecantaloupe/sidebar.R")
  ca_sidebar_server(input, output, session, id, ca_stages)

  source("pages/rtecantaloupe/stages.R")
  ca_stages_server(input, output, session, id, ca_stages)
}
