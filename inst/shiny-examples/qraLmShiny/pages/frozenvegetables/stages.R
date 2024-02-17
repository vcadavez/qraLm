create_tab <- function(stage, ns) {
  if (is.character(stage) && nchar(stage) > 0) {
    return(tabPanel(title = stage, uiOutput(outputId = ns(stage))))
  } else {
    return(NULL)
  }
}

source_all_stages <- function() {
  files <- list.files(path = "pages/frozenvegetables/stages", pattern = "\\.R$", full.names = TRUE)
  lapply(files, source)
}
source_all_stages()

fv_stages_ui <- function(id, fv_stages) {
 # Initialize namespace
  ns <- NS(id)
  
  # Create tabs
  tab_content <- lapply(fv_stages, create_tab, ns = ns)
  
  # Remove NULL elements
  tab_content <- tab_content[sapply(tab_content, function(x) !is.null(x))]
  
  # Generate UI column
  ui_column <- column(
#    h3("Variability of contamination"),
    width = 8,
    do.call(tabsetPanel, tab_content),
    createFullscreenToggleFeature("frozenvegetables-stages")
  )
  
  return(tagList(ui_column))
}

fv_stages_server <- function(input, output, session, suffix, fv_stages) {
  ns <- NS(suffix)
  id <- ns("stages")
  
  output[[id]] <- renderUI({
    fv_stages_ui(id, fv_stages)
  })

  datProd    <- fv_Production_server(input, output, session, id)
  datBlanch  <- fv_Blanching_server(input, output, session, id, datProd)
  datPart    <- fv_Partitioning_server(input, output, session, id, datBlanch)
  datTest    <- fv_Testing_server(input, output, session, id, datPart)
  datPort    <- fv_Portioning_server(input, output, session, id, datTest)
  datDefrost <- fv_Defrosting_server(input, output, session, id, datPort)
  datCook    <- fv_Cooking_server(input, output, session, id, datDefrost)
  datRisk    <- fv_Risk_server(input, output, session, id, datCook)
}