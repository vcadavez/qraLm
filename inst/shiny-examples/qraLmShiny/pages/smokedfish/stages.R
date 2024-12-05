create_tab <- function(stage, ns) {
  if (is.character(stage) && nchar(stage) > 0) {
    return(tabPanel(title = stage, uiOutput(outputId = ns(stage))))
  } else {
    return(NULL)
  }
}

source_all_stages <- function() {
  files <- list.files(path = "pages/smokedfish/stages", pattern = "\\.R$", full.names = TRUE)
  lapply(files, source)
}
source_all_stages()

sf_stages_ui <- function(id, sf_stages) {
  ns <- NS(id)

  tab_content <- lapply(sf_stages, create_tab, ns = ns)
  tab_content <- tab_content[sapply(tab_content, function(x) !is.null(x))]

  ui_column <- column(
#    h3("Variability of contamination"),
    width = 8,
    do.call(tabsetPanel, tab_content),
    createFullscreenToggleFeature("smokedfish-stages")
  )

  return(tagList(ui_column))
}

sf_stages_server <- function(input, output, session, suffix, sf_stages) {
  ns <- NS(suffix)
  id <- ns("stages")

  output[[id]] <- renderUI({
    sf_stages_ui(id, sf_stages)
  })

  datFish      = sf_Production_server(input, output, session, id)
  datPrefill   = sf_Prefilleting_server(input, output, session, id, datFish)
  datFill      = sf_Filleting_server(input, output, session, id, datPrefill)
  datHold      = sf_Holding_server(input, output, session, id, datFill)
  datBrinesalt = sf_BriningOrSalting_server(input, output, session, id, datHold)
  datSmoke     = sf_Smoking_server(input, output, session, id, datBrinesalt)
  datSlice     = sf_Slicing_server(input, output, session, id, datSmoke)
  datPack      = sf_Packaging_server(input, output, session, id, datSlice)
  RTE          = sf_Characteristics_server(input, output, session, id, datPack)
  datColdchain = sf_ColdChain_server(input, output, session, id, datPack, RTE)
  datHome      = sf_Home_server(input, output, session, id, datColdchain, RTE)
  datPort      = sf_Portioning_server(input, output, session, id, datHome)
  datRisk      = sf_Risk_server(input, output, session, id, datPort)
}
