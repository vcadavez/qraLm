fv_stageInputs_ui <- function(id) {  
  ns <- NS(id)  
  div(  
    id = id,  
    shinyjs::useShinyjs(),
    tagList(  
      shinyjs::hidden(  
        div(
          id = ns("Production"), 
          fv_ProductionInputs_ui(id)
        )
      ),
      shinyjs::hidden(  
        div(
          id = ns("Blanching"), 
          fv_BlanchingInputs_ui(id)
        )
      ),  
      shinyjs::hidden(  
        div(
          id = ns("Partitioning"), 
          fv_PartitioningInputs_ui(id)
        )
      ),  
      shinyjs::hidden(  
        div(
          id = ns("Testing"), 
          fv_TestingInputs_ui(id)
        )
      ),  
      shinyjs::hidden(  
        div(
          id = ns("Portioning"), 
          fv_PortioningInputs_ui(id)
        )
      ),  
      shinyjs::hidden(  
        div(
          id = ns("Defrosting"), 
          fv_DefrostingInputs_ui(id)
        )
      ),  
      shinyjs::hidden(  
        div(
          id = ns("Cooking"), 
          fv_CookingInputs_ui(id)
        )
      ),
      shinyjs::hidden(  
        div(
          id = ns("Risk"), 
          fv_RiskInputs_ui(id)
        )
      ) 
    )  
  )
}  

fv_stageInputs_server <- function(input, output, session, suffix, fv_stages) {  
  ns <- NS(suffix)  
  id <- ns("inputs")  
  
  output[[id]] <- renderUI({  
    fv_stageInputs_ui(id)  
  })  
  
  fv_stageSelectedId <- "frozenvegetables-sidebar-selector-selection"  
  ns1 <- NS(id)  # frozenvegetables-sidebar-inputs-Production 
  
  observe({  
    req(input[[fv_stageSelectedId]])  
    cat("Selected stage:", input[[fv_stageSelectedId]], "\n")  # Debugging line
    for (stage in fv_stages) {  
      cat("Checking stage:", stage, "\n")  # Debugging line
      if (input[[fv_stageSelectedId]] == stage) { 
        cat("Showing stage: ", stage, "with ns1 ==", ns1(stage), "\n") 
        shinyjs::show(ns1(stage))  
      } else {  
        shinyjs::hide(ns1(stage))  
      }  
    }  
  })  
}