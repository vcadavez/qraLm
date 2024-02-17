create_tab <- function(stage, ns) {
  if (is.character(stage) && nchar(stage) > 0) {
    return(tabPanel(title = stage, uiOutput(outputId = ns(stage))))
  } else {
    return(NULL)
  }
}

source_all_stages <- function() {
  files <- list.files(path = "pages/rtecantaloupe/stages", pattern = "\\.R$", full.names = TRUE)
  lapply(files, source)
}
source_all_stages()

ca_stages_ui <- function(id, ca_stages) {
  ns <- NS(id)

  tab_content <- lapply(ca_stages, create_tab, ns = ns)
  tab_content <- tab_content[sapply(tab_content, function(x) !is.null(x))]

  ui_column <- column(
#    h3("Variability of contamination"),
    width = 8,
    do.call(tabsetPanel, tab_content),
    createFullscreenToggleFeature("rtecantaloupe-stages")
  )

  return(tagList(ui_column))
}

ca_stages_server <- function(input, output, session, suffix, ca_stages) {
  ns <- NS(suffix)
  id <- ns("stages")

  output[[id]] <- renderUI({
    ca_stages_ui(id, ca_stages)
  })

  datCanta              = ca_Production_server(input, output, session, id)
  datHarvest            = ca_Harvest_server(input, output, session, id, datCanta)
  datBrush              = ca_Brushing_server(input, output, session, id, datHarvest)
  datStoring            = ca_Storing_server(input, output, session, id, datBrush)
  datWashing            = ca_Washing_server(input, output, session, id, datStoring)
  datDicing             = ca_Dicing_server(input, output, session, id, datWashing)
  datPartitioning       = ca_Partitioning_server(input, output, session, id, datDicing)
  datTesting            = ca_Testing_server(input, output, session, id, datPartitioning)
  datTransport          = ca_Transport_server(input, output, session, id, datTesting)
  datRTEStorage         = ca_RTEStorage_server(input, output, session, id, datTransport)
  datConsumersTransport = ca_ConsumersTransport_server(input, output, session, id, datRTEStorage)
  datStoredDices        = ca_StoredDices_server(input, output, session, id, datConsumersTransport)
  datRisk               = ca_Risk_server(input, output, session, id, datStoredDices)
}
